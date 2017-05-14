module Clock
       ( Time
       , time
       , eqTime
       , renderClock
       , animateTo
       ) where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Ref (REF, Ref, modifyRef, newRef, readRef)
import Control.Monad.Eff.Timer (TIMER, setTimeout)
import Data.Array ((..), foldM)
import Data.Int (toNumber)
import Graphics.Canvas ( CANVAS, Context2D, LineCap(..), TextAlign(..), addColorStop, arc, createRadialGradient
                       , fillPath, fillText, moveTo, lineTo, rotate, setFillStyle, setFont, setGradientFillStyle
                       , setLineCap, setLineWidth, setStrokeStyle, setTextAlign, strokePath, translate )
import Math ((%), pi)

type Time = { hours   :: Int
            , minutes :: Int }

time :: Int -> Int -> Time
time hours minutes = { hours: hours, minutes: minutes }

eqTime :: Time -> Time -> Boolean
eqTime time1 time2 = (time1.hours == time2.hours) && (time1.minutes == time2.minutes)

differenceInMinutes :: Time -> Time -> Int
differenceInMinutes from to =
    forwardOnly(toMinutes - fromMinutes)
  where
    fromMinutes = (from.hours * 60) + from.minutes
    toMinutes   = (to.hours   * 60) + to.minutes
    forwardOnly x | x < 0     = x + (12 * 60)
                  | otherwise = x

addOneMinute :: Time -> Time
addOneMinute { hours:    12, minutes: 59 } = { hours: 1,           minutes: 0 }
addOneMinute { hours: hours, minutes: 59 } = { hours: (hours + 1), minutes: 0 }
addOneMinute time1                         = time1 { minutes = time1.minutes + 1 }

renderClock :: forall eff. Time -> Number -> Context2D -> Eff (canvas :: CANVAS | eff) Context2D
renderClock time1 height ctx = do
    _ <- translate { translateX: center, translateY: center } ctx
    _ <- drawFace radius ctx
    _ <- drawNumbers radius ctx
    _ <- drawHands radius time1 ctx
    _ <- translate { translateX: -center, translateY: -center } ctx
    pure ctx
  where
    center = height / 2.0
    radius = (height / 2.0) * 0.9
    
drawFace :: forall eff. Number -> Context2D -> Eff (canvas :: CANVAS | eff) Context2D
drawFace radius ctx = do

  _ <- fillPath ctx do
    g0       <- createRadialGradient { x0: 0.0, y0: 0.0, r0: radius * 0.95, x1: 0.0, y1: 0.0, r1: radius * 1.05 } ctx
    g1       <- addColorStop 0.0 "#333"  g0
    g2       <- addColorStop 0.5 "white" g1
    gradient <- addColorStop 1.0 "#333"  g2
    _        <- setGradientFillStyle gradient ctx
    _        <- arc ctx { x: 0.0, y: 0.0, r: radius * 1.05, start: 0.0, end: 2.0 * pi }
    pure unit
  
  _ <- fillPath ctx do
    _ <- setFillStyle "white" ctx
    _ <- arc ctx { x: 0.0, y: 0.0, r: radius * 0.95, start: 0.0, end: 2.0 * pi }
    pure unit
  
  _ <- fillPath ctx do
    _ <- setFillStyle "#333" ctx
    _ <- arc ctx { x: 0.0, y: 0.0, r: radius * 0.1, start: 0.0, end: 2.0 * pi }
    pure unit
  
  pure ctx

drawNumber :: forall eff. Number -> Context2D -> Int -> Eff (canvas :: CANVAS | eff) Context2D
drawNumber radius ctx i = do
    _ <- rotate angle ctx
    _ <- translate { translateX: 0.0, translateY: (-radius * 0.85) } ctx
    _ <- rotate (-angle) ctx
    _ <- fillText ctx (show i) 0.0 (radius * 0.05)
    _ <- rotate angle ctx
    _ <- translate { translateX: 0.0, translateY: (radius * 0.85) } ctx
    _ <- rotate (-angle) ctx
    pure ctx
  where
    angle = (toNumber i) * pi / 6.0

drawNumbers :: forall eff. Number -> Context2D -> Eff (canvas :: CANVAS | eff) Context2D
drawNumbers radius ctx = do
  _ <- setFillStyle "#333" ctx
  _ <- setFont ((show (radius * 0.15)) <> "px arial") ctx
  _ <- setTextAlign ctx AlignCenter
  _ <- foldM (drawNumber radius) ctx (1..12)
  pure ctx

drawHand :: forall eff. Number -> Number -> Number -> Number -> Context2D -> Eff (canvas :: CANVAS | eff) Context2D
drawHand radius angle length width ctx = do
  _ <- strokePath ctx do
    _ <- setLineWidth width ctx
    _ <- setLineCap Round ctx
    _ <- moveTo ctx 0.0 0.0
    _ <- rotate angle ctx
    _ <- lineTo ctx 0.0 (-length)
    _ <- rotate (-angle) ctx
    pure unit
  pure ctx

drawHands :: forall eff. Number -> Time -> Context2D -> Eff (canvas :: CANVAS | eff) Context2D
drawHands radius { hours, minutes } ctx = do
    _ <- setStrokeStyle "#333" ctx
    _ <- drawHand radius hourAngle (radius * 0.5) (radius * 0.07) ctx
    _ <- drawHand radius minuteAngle (radius * 0.8) (radius * 0.07) ctx
    pure ctx
  where
    hour = (toNumber hours) % 12.0
    minute = (toNumber minutes) % 60.0
    hourAngle = (hour * pi / 6.0) + (minute * pi / (6.0 * 60.0))
    minuteAngle = minute * pi / 30.0

frameLength :: Int
frameLength = 33

animateTo :: forall eff. Time -> Time -> Number -> Context2D -> Eff (canvas :: CANVAS, timer :: TIMER, ref :: REF | eff) Unit
animateTo from to height ctx = do
    currentRef        <- newRef from
    remaining         <- pure $ differenceInMinutes from to
    remainingTicksRef <- newRef remaining
    _                 <- if remaining > 0
                           then void $ setTimeout frameLength $ advanceTime currentRef remainingTicksRef
                           else void $ renderClock from height ctx
    pure unit
  where
    scheduleNextFrame :: forall eff2. Ref Time -> Ref Int -> Eff (canvas :: CANVAS, timer :: TIMER, ref :: REF | eff2) Unit
    scheduleNextFrame currentRef remainingTicksRef = do
      remaining <- readRef remainingTicksRef
      _         <- if remaining > 0 then void (setTimeout frameLength (advanceTime currentRef remainingTicksRef)) else pure unit
      pure unit
    advanceTime :: forall eff2. Ref Time -> Ref Int -> Eff (canvas :: CANVAS, timer :: TIMER, ref :: REF | eff2) Unit
    advanceTime currentRef remainingTicksRef = do
      _         <- modifyRef currentRef addOneMinute
      current   <- readRef currentRef
      _         <- modifyRef remainingTicksRef (\x -> x - 1)
      remaining <- readRef remainingTicksRef
      _         <- renderClock current height ctx
      _         <- scheduleNextFrame currentRef remainingTicksRef
      pure unit
