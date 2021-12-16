module Main
  ( World
  , draw
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

import Data.Foldable (any, length)
import Data.Grid (Coordinates)
import Data.List (List(..), (:), snoc, last)
import Data.List as List
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import Data.Set (empty, insert, member) as Set
import Data.Traversable (for_)
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Exception (throwException)
import Effect.Random (randomInt)
import Halogen.HTML (elementNS)
import Reactor (getW, runReactor, updateW_)
import Reactor.Events (Event(..))
import Reactor.Graphics.Colors as Color
import Reactor.Graphics.Drawing (Drawing, fill, tile)
import Reactor.Reaction (Reaction, executeDefaultBehavior)
import Web.TouchEvent.EventTypes (touchend)
import Web.UIEvent.WheelEvent (deltaY)


main :: Effect Unit
main = do
  fx <- liftEffect (randomInt 0 $ widthGlobal - 1)
  fy <- liftEffect (randomInt 0 $ heightGlobal - 1)
  let initial = { snake: ({ x: 0, y: 0 }: Nil), food: {x: fx, y: fy}, enemy: ({ x: widthGlobal - 1, y: heightGlobal - 1 } : Nil), currentTick: 0, snakeDirection: {dx: 0, dy: 0}, enemyDirection: {dx: 0, dy: 0}, counter: 0 }
  let reactor = { initial, draw, handleEvent, isPaused: const false }
  runReactor reactor { title: "Snake", width: widthGlobal, height: heightGlobal }


type World = { snake :: List Coordinates, food :: Coordinates, enemy :: List Coordinates, currentTick :: Int, snakeDirection :: {dx :: Int, dy :: Int}, enemyDirection :: {dx :: Int, dy :: Int}, counter :: Int }
widthGlobal :: Int
widthGlobal = 20
heightGlobal :: Int
heightGlobal = 20

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
        enemyAI Nil 0
        snakeTurn
      else if (mod currentTick 30 == 0) then do
        enemyAI Nil 0
        snakeDed
        foodEaten
      else if (mod currentTick 20 == 0) then do
        snakeTurn
      else
        pure unit
    _ -> executeDefaultBehavior
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
    executeDefaultBehavior


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
  if any (_ == head) tail || any (_ == head) enemy then                                                 -- check, jestli had narazí do vlastního těla NEBO do enemy
    updateW_ {snake: ({x: 0, y: 0} : Nil), enemy: ({x: widthGlobal - 1, y: heightGlobal - 1} : Nil)}    -- update na začátek
  else
    executeDefaultBehavior

hitWall :: Reaction World
hitWall = do
  {snake, snakeDirection: {dx, dy}} <- getW
  let {x, y} = fromMaybe {x: 0, y: 0} $ List.head snake
  when (x + dx == -1 || x + dx == widthGlobal || y + dy == -1 || y + dy == heightGlobal) 
    (updateW_ {snake: ({x: 0, y: 0} : Nil), enemy: ({x: widthGlobal - 1, y: heightGlobal - 1} : Nil), snakeDirection: {dx: 0, dy: 0}, enemyDirection: {dx: 0, dy: 0}})

choseBestWay :: Reaction World
choseBestWay = do
  {enemy, food} <- getW
  coin <- liftEffect $ randomInt 0 1
  if coin == 0 then
    moveEnemyX enemy food
  else
    moveEnemyY enemy food


moveEnemyX :: List Coordinates -> Coordinates -> Reaction World
moveEnemyX Nil _ = pure unit
moveEnemyX ({x, y} : _) {x: fx, y: fy}
  | (fx - x) > 0 = updateW_ {enemyDirection: {dx: 1, dy: 0}}
  | (fx - x) < 0 = updateW_ {enemyDirection: {dx: -1, dy: 0}}
  | otherwise = moveEnemyY ({x, y} : Nil) {x: fx, y: fy}

moveEnemyY :: List Coordinates -> Coordinates -> Reaction World
moveEnemyY Nil _ = pure unit
moveEnemyY ({x, y} : _) {x: fx, y: fy} 
  | (fy - y) > 0 = updateW_ {enemyDirection: {dx: 0, dy: 1}}
  | (fy - y) < 0 = updateW_ {enemyDirection: {dx: 0, dy: -1}}
  | otherwise = moveEnemyX ({x, y} : Nil) {x: fx, y: fy}

choseAlternateDirection :: List {px :: Int, py :: Int} -> Coordinates -> List Coordinates -> Reaction World
choseAlternateDirection Nil _ _ = pure unit
choseAlternateDirection ({px, py}:Nil) {x, y} newTail = updateW_ {enemy: {x: x + px, y: y + py}: newTail}
choseAlternateDirection ({px: px1, py: py1}:{px: px2, py: py2}:_) {x, y} newTail = do
  coin <- liftEffect $ randomInt 0 1
  if coin == 0 then
    updateW_ {enemy: {x: x + px1, y: y + py1}: newTail}
  else
    updateW_ {enemy: {x: x + px2, y: y + py2}: newTail}


enemyAI :: List {px :: Int, py :: Int} -> Int -> Reaction World
enemyAI usedDirections acc = do
  choseBestWay
  shortWay <- getFastWays Nil
  let aaab = <$> 
  {enemy, snake, enemyDirection: {dx, dy}} <- getW
  let newTail = fromMaybe Nil $ List.init enemy
  let tailEnemy = fromMaybe Nil $ List.tail enemy
  let {x, y} = fromMaybe {x: 0, y: 0} $ List.head enemy
  let possibleDirections = ({px: 1, py: 0}:{px: -1, py: 0}:{px: 0, py: 1}:{px: 0, py: -1}:Nil)
  let worseDirections = List.difference possibleDirections usedDirections

  if List.length usedDirections < 2  then
    if any (_ == {x: x + dx, y: y + dy}) (tailEnemy <> snake) then 
      enemyAI (List.nub $ {px: dx, py: dy} : usedDirections) (acc + 1)
    else 
      updateW_ {enemy: {x: x + dx, y: y + dy} : newTail}
  else
    choseAlternateDirection worseDirections {x, y} newTail
 -- Mám dva listy A B. V listu A potřebuju ke každému prvku něco přičíst. Následně potřebuju porovnat každý prvek z A s každým z B. 
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
getFastWays fastWays = do
  choseBestWay
  {enemyDirection: {dx, dy} } <- getW
  if (List.length fastWays) < 2 then
    getFastWays (List.nub $ {dx, dy} : fastWays)
  else
    pure fastWays

