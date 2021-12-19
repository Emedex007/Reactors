module Main
  ( World
  , enemyAI
  , foodEaten
  , generateFood
  , handleEvent
  , heightGlobal
  , hitWall
  , main
  , snakeDed
  , widthGlobal
  )
  where

import Prelude

import Data.Foldable (any)
import Data.Grid (Coordinates)
import Data.List (List(..), (:), snoc, last, (\\), (!!))
import Data.List as List
import Data.Maybe (fromMaybe, Maybe(..))
import Data.Set (Set)
import Data.Set (empty, insert, member) as Set
import Data.Traversable (for_)
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Random (randomInt)
import Reactor (getW, runReactor, updateW_)
import Reactor.Events (Event(..))
import Reactor.Graphics.Colors as Color
import Reactor.Graphics.Drawing (Drawing, fill, tile)
import Reactor.Reaction (Reaction)



main :: Effect Unit
main = do
  fx <- liftEffect (randomInt 0 $ widthGlobal - 1)
  fy <- liftEffect (randomInt 0 $ heightGlobal - 1)
  let initial = { snake: ({ x: 0, y: 0 }: Nil), food: {x: fx, y: fy}, enemy: ({ x: widthGlobal - 1, y: heightGlobal - 1 } : Nil), currentTick: 0, snakeDirection: {dx: 0, dy: 0}, enemyDirection: {dx: 0, dy: 0} }
  let reactor = { initial, draw, handleEvent, isPaused: const false }
  runReactor reactor { title: "Snake", width: widthGlobal, height: heightGlobal }


type World = { snake :: List Coordinates, food :: Coordinates, enemy :: List Coordinates, currentTick :: Int, snakeDirection :: {dx :: Int, dy :: Int}, enemyDirection :: {dx :: Int, dy :: Int} }
widthGlobal :: Int
widthGlobal = 20
heightGlobal :: Int
heightGlobal = 20
wallListGlobal :: List {x :: Int, y :: Int}
wallListGlobal = 
  topbotBorder 0 (-1) <> topbotBorder 0 heightGlobal <> sidesBorder (-1) 0 <> sidesBorder widthGlobal 0
  where
    topbotBorder a b | a < widthGlobal =  {x: a, y: b} : topbotBorder (a + 1) b | otherwise = Nil
    sidesBorder a b | b < heightGlobal =  {x: a, y: b} : sidesBorder a (b + 1) | otherwise = Nil

draw :: World -> Drawing
draw { snake, food, enemy } = do
  let headSnake = fromMaybe {x: 0, y: 0} (List.head snake)
  let headEnemy = fromMaybe {x: 0, y: 0} (List.head enemy)
 
  for_ snake $ \block -> fill Color.green400 $ tile block
  fill Color.green600 $ tile headSnake  --the head is different colour
  for_ enemy $ \block -> fill Color.blue400 $ tile block
  fill Color.blue600 $ tile headEnemy
  fill Color.red400 $ tile food


handleEvent :: Event -> Reaction World
handleEvent event = do
  case event of
    KeyPress { key: "ArrowRight" } -> updateW_ {snakeDirection: {dx: 1, dy: 0}}
    KeyPress { key: "ArrowLeft" } -> updateW_ {snakeDirection: {dx: -1, dy: 0}}
    KeyPress { key: "ArrowDown" } -> updateW_ {snakeDirection: {dx: 0, dy: 1}}
    KeyPress { key: "ArrowUp" } -> updateW_ {snakeDirection: {dx: 0, dy: -1}}
    Tick {} -> do
      {currentTick} <- getW
      updateW_ {currentTick: currentTick + 1}
      if (mod currentTick 30 == 0) && (mod currentTick 20 == 0) then do
        enemyAI
        snakeTurn
      else if (mod currentTick 30 == 0) then do
        enemyAI
        snakeDed
        foodEaten
      else if (mod currentTick 20 == 0) then do
        snakeTurn
      else
        pure unit
    _ -> pure unit
snakeTurn :: Reaction World
snakeTurn = do
  hitWall
  snakeMove
  snakeDed
  foodEaten

snakeMove :: Reaction World
snakeMove = do
  {snake, snakeDirection: {dx, dy}} <- getW
  let {x, y} = fromMaybe {x: 0, y: 0} $ List.head snake
  let newTail = fromMaybe Nil $ List.init snake 
  
  updateW_ {snake: {x: x + dx, y: y + dy} : newTail}

foodEaten :: Reaction World
foodEaten = do
  {snake, food, enemy} <- getW
  let headSnake = fromMaybe {x: 0, y: 0} $ List.head snake
  let headEnemy = fromMaybe {x: 0, y: 0} $ List.head enemy
  newFood <- liftEffect $ generateFood snake enemy Set.empty
  
  if headEnemy == food then do                                           -- -
    let fedEnemy = snoc enemy $ fromMaybe {x: 0, y: 0} $ last enemy      -- checks if enemy eats food, it has priority over player
    updateW_ {food: newFood, enemy: fedEnemy}                            -- -
   
  else if headSnake == food then do                                      -- -
    let fedSnake = snoc snake $ fromMaybe {x: 0, y: 0} $ last snake      -- checks if player eats food
    updateW_ {food: newFood, snake: fedSnake}                            -- -
  
  else
    pure unit


generateFood :: List Coordinates -> List Coordinates -> Set Coordinates -> Effect Coordinates
generateFood snake enemy usedCoords = do
  fx <- randomInt 0 $ widthGlobal - 1
  fy <- randomInt 0 $ heightGlobal - 1
  
  if Set.member {x: fy, y: fy} usedCoords then                  -- využíváme Sety pro zaspsání již vyzkoušených pozic jídla, je rychlejší se ptát Setu, jestli je v něm nějaký prvek,
    generateFood snake enemy usedCoords                         -- než to checkovat v Listech
  else if any (_ == {x: fx, y: fy}) (snake <> enemy) then    -- makes sure so that the food doesnt spawn inside snake or enemy
    generateFood snake enemy $ Set.insert {x: fy, y: fy} usedCoords
  else
    (pure {x: fx, y: fy}) :: Effect Coordinates


snakeDed :: Reaction World
snakeDed = do
  {snake, enemy} <- getW
  let head = fromMaybe {x: 0, y: 0} $ List.head snake
  let tail = fromMaybe ({x: 0, y: 0} : Nil) $ List.tail snake
  when (any (_ == head) tail || any (_ == head) enemy)                                                -- check, jestli had narazí do vlastního těla NEBO do enemy
    (updateW_ {snake: ({x: 0, y: 0} : Nil), enemy: ({x: widthGlobal - 1, y: heightGlobal - 1} : Nil), snakeDirection: {dx: 0, dy: 0}})    -- update na začátek


hitWall :: Reaction World
hitWall = do
  {snake, snakeDirection: {dx, dy}} <- getW
  let {x, y} = fromMaybe {x: 0, y: 0} $ List.head snake
  when (any (_ == {x: x + dx, y: y + dy }) wallListGlobal)
    (updateW_ {snake: ({x: 0, y: 0} : Nil), enemy: ({x: widthGlobal - 1, y: heightGlobal - 1} : Nil), snakeDirection: {dx: 0, dy: 0}, enemyDirection: {dx: 0, dy: 0}})
  
-- choseBestWay :: Reaction World
-- choseBestWay = do
--   {enemy, food} <- getW
--   coin <- liftEffect $ randomInt 0 1
--   if coin == 0 then
--     moveEnemyX enemy food
--   else
--     moveEnemyY enemy food


-- moveEnemyX :: List Coordinates -> Coordinates -> Reaction World
-- moveEnemyX Nil _ = pure unit
-- moveEnemyX ({x, y} : _) {x: fx, y: fy}
--   | (fx - x) > 0 = updateW_ {enemyDirection: {dx: 1, dy: 0}}
--   | (fx - x) < 0 = updateW_ {enemyDirection: {dx: -1, dy: 0}}
--   | otherwise = updateW_ {enemyDirection: {dx: 0, dy: 0}}

-- moveEnemyY :: List Coordinates -> Coordinates -> Reaction World
-- moveEnemyY Nil _ = pure unit
-- moveEnemyY ({x, y} : _) {x: fx, y: fy} 
--   | (fy - y) > 0 = updateW_ {enemyDirection: {dx: 0, dy: 1}}
--   | (fy - y) < 0 = updateW_ {enemyDirection: {dx: 0, dy: -1}}
--   | otherwise = updateW_ {enemyDirection: {dx: 0, dy: 0}}


choseBestWay = do
  {enemy, food: {x: fx, y: fy}} <- getW
  pure ((xAxis fx enemy) : (yAxis fy enemy) : Nil)
  where
    xAxis _ Nil = {x: 0, y: 0}
    xAxis fx ({x, y} : _) | (fx - x) > 0 = {x: x + 1, y} | (fx - x) < 0 = {x: x - 1, y} | otherwise = {x, y}
    yAxis _ Nil = {x: 0, y: 0}
    yAxis fy ({x, y} : _) | (fy - y) > 0 = {x, y: y + 1} | (fy - y) < 0 = {x, y: y - 1} | otherwise = {x, y}

enemyAI :: Reaction World
enemyAI = do
  {enemy, snake} <- getW
  shortWay <- choseBestWay
  let {x, y} = fromMaybe {x: 0, y: 0} $ List.head enemy
  let possibleDirections = ({x: x + 1, y}:{x: x - 1, y}:{x, y: y + 1}:{x, y: y - 1}:Nil)
  let shortWay2 = shortWay \\ (snake <> enemy <> wallListGlobal)
  let longWay = possibleDirections \\ shortWay
  let longWay2 = longWay \\ (snake <> enemy <> wallListGlobal)
  let newTail = fromMaybe Nil $ List.init enemy
  coin <- liftEffect $ randomInt 0 1

  if List.length (shortWay2 <> longWay2) == 0 then 
    updateW_ {enemy: ({x: widthGlobal - 1, y: heightGlobal - 1} : Nil)}
  else if List.length shortWay2 == 2 then   
    case shortWay2 !! coin of
      Nothing -> pure unit
      Just way -> updateW_ {enemy: (way : newTail)}
  else if List.length shortWay2 == 1 then
    case shortWay2 !! 0 of 
      Nothing -> pure unit
      Just way -> updateW_ {enemy: (way : newTail)}
  else if List.length longWay2 == 2 then
    case longWay2 !! coin of
      Nothing -> pure unit
      Just way -> updateW_ {enemy: (way : newTail)}
  else
    case longWay2 !! 0 of 
      Nothing -> pure unit
      Just way -> updateW_ {enemy: (way : newTail)}

  

-- wallListGlobal :: List {x :: Int, y :: Int}
-- wallListGlobal = wallList 0 (-1)
-- wallList ∷ Int → Int → List { x ∷ Int , y ∷ Int }
-- wallList a b  
--   | (a < widthGlobal) && (b == -1) = {x: a, y: b} : wallList (a + 1) b
--   | (a == widthGlobal) && (b == -1) = wallList widthGlobal 0
--   | (a == widthGlobal) && (b < heightGlobal) = {x: a, y: b} : wallList a (b + 1)
--   | (a == widthGlobal) && (b == heightGlobal) = wallList 0 heightGlobal
--   | (a < widthGlobal) && (b == heightGlobal) = {x: a, y: b} : wallList (a + 1) b
--   | (a == widthGlobal) && (b == heightGlobal) = wallList (-1) 0
--   | (a == -1) && (b < heightGlobal) = {x: a, y: b} : wallList a (b + 1)
--   | otherwise = Nil
  

  
  -- if List.length usedDirections < 2  then
  --   if any (_ == {x: x + dx, y: y + dy}) (tailEnemy <> snake) then 
  --     enemyAI (List.nub $ {px: dx, py: dy} : usedDirections) (acc + 1)
  --   else 
  --     updateW_ {enemy: {x: x + dx, y: y + dy} : newTail}
  -- else
  --   choseAlternateDirection worseDirections {x, y} newTail
  -- where
  --   mapfunc {dx, dy} = do
  --     {enemy, snake} <- getW
  --     let tailEnemy = fromMaybe Nil $ List.tail enemy
  --     let {x, y} = fromMaybe {x: 0, y: 0} $ List.head enemy
  --     if aaa <$> (tailEnemy <> snake) then 
  --       unit
  --     else
  --       {dx, dy}
  --     where
  --       aaa ree = 
  --         if ree == {x: dx + x, y: dy + y} then 
  --           unit
  --         else
  --           {x: dx + x, y: dy + y}
