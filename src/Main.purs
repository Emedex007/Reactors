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
import Data.Foldable (any)
import Data.Grid (Coordinates)
import Data.List (List(..), (:), snoc, last)
import Data.List as List
import Data.Maybe (fromMaybe)
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
import Reactor.Reaction (Reaction, executeDefaultBehavior)


main :: Effect Unit
main = do
  fx <- liftEffect (randomInt 0 $ widthGlobal - 1)
  fy <- liftEffect (randomInt 0 $ heightGlobal - 1)
  let initial = { snake: ({ x: 0, y: 0 }: Nil), food: {x: fx, y: fy}, enemy: ({ x: widthGlobal - 1, y: heightGlobal - 1 } : Nil), currentTick: 0, snakeDirection: {dx: 0, dy: 0}}
  let reactor = { initial, draw, handleEvent, isPaused: const false }
  runReactor reactor { title: "Snake", width: widthGlobal, height: heightGlobal }


type World = { snake :: List Coordinates, food :: Coordinates, enemy :: List Coordinates, currentTick :: Int, snakeDirection :: {dx :: Int, dy :: Int} }
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
        enemyAI
        snakeTurn
      else if (mod currentTick 1 == 0) then do
        enemyAI
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
    (updateW_ {snake: ({x: 0, y: 0} : Nil), enemy: ({x: widthGlobal - 1, y: heightGlobal - 1} : Nil), snakeDirection: {dx: 0, dy: 0}})

enemyAI :: Reaction World
enemyAI = do
  {enemy, food} <- getW
  let newTail = fromMaybe Nil $ List.init enemy
  coin <- liftEffect $ randomInt 0 1
  if coin == 0 then
    moveEnemyX enemy food newTail
  else
    moveEnemyY enemy food newTail 


moveEnemyX :: List Coordinates -> Coordinates -> List Coordinates -> Reaction World
moveEnemyX Nil _ _ = pure unit
moveEnemyX ({x, y} : _) {x: fx, y: fy} newTail
  | (fx - x) > 0 = updateW_ {enemy: {x: x + 1, y} : newTail}
  | (fx - x) < 0 = updateW_ {enemy: {x: x - 1, y} : newTail}
  | otherwise = moveEnemyY ({x, y} : Nil) {x: fx, y: fy} newTail

moveEnemyY :: List Coordinates -> Coordinates -> List Coordinates -> Reaction World
moveEnemyY Nil _ _ = pure unit
moveEnemyY ({x, y} : _) {x: fx, y: fy} newTail 
  | (fy - y) > 0 = updateW_ {enemy: {x, y: y + 1} : newTail}
  | (fy - y) < 0 = updateW_ {enemy: {x, y: y - 1} : newTail}
  | otherwise = (moveEnemyX ({x, y} : Nil) {x: fx, y: fy} newTail)
