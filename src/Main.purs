module Main where

import Prelude

import Data.Maybe (Maybe(..))
import Effect (Effect)
import Reactor (CoordinateSystem, Reactor, executeDefaultBehavior, fill, grid, modify_, relativeTo, runReactor, tile, togglePause, utilities)
import Reactor.Events (Event(..))
import Reactor.Graphics.Colors as Color
import Reactor.Graphics.CoordinateSystem (moveDown, moveLeft, moveRight, moveUp)
import Reactor.Internal.Helpers (withJust)

main :: Effect Unit
main = runReactor reactor { title: "Moving Dot", width: 20, height: 20 }

type Point = CoordinateSystem { x :: Number, y :: Number }
type World = { player :: Point, cursor :: Maybe Point, paused :: Boolean }

reactor :: forall m. Reactor m World
reactor = { init, draw, handleEvent }
  where
  init = { player: { x: 0, y: 0 } `relativeTo` grid, cursor: Nothing, paused: false }

  draw { cursor, player } = do
    fill Color.blue400 $ tile player
    withJust cursor $ fill Color.gray200 <<< tile

  handleEvent event = do
    { bound } <- utilities
    case event of
      MouseEvent { gridCoords } -> modify_ \w -> w { cursor = Just gridCoords }

      KeypressEvent "ArrowLeft" _ ->
        modify_ \w -> w { player = bound $ moveLeft w.player }
      KeypressEvent "ArrowRight" _ ->
        modify_ \w -> w { player = bound $ moveRight w.player }
      KeypressEvent "ArrowDown" _ ->
        modify_ \w -> w { player = bound $ moveDown w.player }
      KeypressEvent "ArrowUp" _ ->
        modify_ \w -> w { player = bound $ moveUp w.player }
      KeypressEvent " " _ -> togglePause

      _ -> executeDefaultBehavior
