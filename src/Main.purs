module Main where

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
  let initial = { snake: ({ x: 0, y: 0 }: Nil), food: {x: fx, y: fy}, enemy: ({ x: widthGlobal - 1, y: heightGlobal - 1 }: Nil)}
  let reactor = { initial, draw, handleEvent, isPaused: const true }
  runReactor reactor { title: "Snake", width: widthGlobal, height: heightGlobal }


type World = { snake :: List Coordinates, food :: Coordinates, enemy :: List Coordinates }
widthGlobal :: Int
widthGlobal = 20
heightGlobal :: Int
heightGlobal = 20


draw :: World -> Drawing
draw { snake, food, enemy } = do
  let headSnake = fromMaybe {x: 0, y: 0} (List.head snake)
  let tailSnake = fromMaybe ({x: 0, y: 0} : Nil) $ List.tail snake
  let headEnemy = fromMaybe {x: 0, y: 0} (List.head enemy)
  let tailEnemy = fromMaybe ({x: 0, y: 0} : Nil) $ List.tail enemy
  
  fill Color.green600 $ tile headSnake  --the head is different colour
  for_ tailSnake $ \block -> fill Color.green400 $ tile block
  fill Color.blue600 $ tile headEnemy
  for_ tailEnemy $ \block -> fill Color.blue400 $ tile block
  fill Color.red400 $ tile food


handleEvent :: Event -> Reaction World
handleEvent event = do
  case event of
    KeyPress { key: "ArrowRight" } ->
      interaction "ArrowRight"
    KeyPress { key: "ArrowLeft" } -> do 
      interaction "ArrowLeft"
    KeyPress { key: "ArrowDown" } -> do 
      interaction "ArrowDown"
    KeyPress { key: "ArrowUp" } -> do 
      interaction "ArrowUp"

    _ -> executeDefaultBehavior


snakeMove :: String -> Reaction World
snakeMove key = do
  {snake} <- getW
  let {x, y} = fromMaybe {x: 0, y: 0} $ List.head snake
  let newTail = fromMaybe Nil $ List.init snake 

  if key == "ArrowRight" then
    updateW_ { snake: {x: x + 1, y} : newTail}
  else if key == "ArrowLeft" then
    updateW_ { snake: {x: x - 1, y} : newTail}
  else if key ==  "ArrowDown" then
    updateW_ { snake: {x, y: y + 1} : newTail}
  else if key == "ArrowUp" then
    updateW_ { snake: {x, y: y - 1} : newTail}
  else
    executeDefaultBehavior


interaction :: String -> Reaction World
interaction key = do
  {snake} <- getW
  if hitWall key snake then
    updateW_ {snake: ({x: 0, y: 0} : Nil), enemy: ({x: widthGlobal - 1, y: heightGlobal - 1} : Nil)}
  else do
    enemyAI
    snakeMove key
    snakeDed
    foodEaten


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
  else if any (_ == {x: fx, y: fy}) snake || any (_ == {x: fx, y: fy}) enemy then    -- makes sure so that the food doesnt spawn inside snake or enemy
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


hitWall :: String -> List Coordinates -> Boolean
hitWall _ Nil = false
hitWall key ({x, y} : _) = 
  (x == (widthGlobal - 1)) && (key == "ArrowRight") ||       -- pravý border
    (x == 0) && (key == "ArrowLeft") ||                      -- levý border
      (y == (heightGlobal - 1)) && (key == "ArrowDown") ||   -- dolní border
        (y == 0) && (key == "ArrowUp")                       -- horní border


enemyAI :: Reaction World
enemyAI = do
  {enemy, food} <- getW
  let newTail = fromMaybe Nil $ List.init enemy
  enemyPathfind enemy food newTail
    where
      enemyPathfind Nil _ _ = executeDefaultBehavior
      enemyPathfind ({x, y} : _) {x: fx, y: fy} newTail
        | (fx - x) > 0 = updateW_ {enemy: {x: x + 1, y} : newTail}  -- move right
        | (fx - x) < 0 = updateW_ {enemy: {x: x - 1, y} : newTail}  -- move left
        | (fy - y) > 0 = updateW_ {enemy: {x, y: y + 1} : newTail}  -- move up
        | (fy - y) < 0 = updateW_ {enemy: {x, y: y - 1} : newTail}  -- move down
        | otherwise = executeDefaultBehavior