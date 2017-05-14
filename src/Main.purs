module Main where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.JQuery (JQuery, JQueryEvent, getValue, on, ready, select, setText, setValue)
import Control.Monad.Eff.Random (RANDOM, randomInt)
import Control.Monad.Eff.Ref (REF, Ref, modifyRef, newRef, readRef, writeRef)
import Control.Monad.Eff.Timer (TIMER, setInterval)
import Data.Foreign (unsafeFromForeign)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..), fst, snd)
import DOM (DOM)
import Graphics.Canvas (CANVAS, getCanvasHeight, getContext2D, getCanvasElementById)
import Partial.Unsafe (unsafePartial)

import Clock (Time, time, eqTime, animateTo)

defaultTime :: Time
defaultTime = time 12 0

main :: forall eff. Eff (canvas :: CANVAS, dom :: DOM, random :: RANDOM, ref :: REF, timer :: TIMER | eff) Unit
main =
  ready $ do
    _ <- startAdvancingClock defaultTime
    _ <- startQuizClock defaultTime
    pure unit

animateClock :: forall eff. String -> Time -> Time -> Eff (canvas :: CANVAS, ref :: REF, timer :: TIMER | eff) Unit
animateClock canvasId from to =
  unsafePartial do
    Just canvas <- getCanvasElementById canvasId
    ctx         <- getContext2D canvas
    height      <- getCanvasHeight canvas
    _           <- animateTo from to height ctx
    pure unit

advancePeriodInMilliseconds :: Int
advancePeriodInMilliseconds = 5000
  
startAdvancingClock :: forall eff. Time -> Eff (canvas :: CANVAS, dom :: DOM, ref :: REF, timer :: TIMER | eff) Unit
startAdvancingClock startTime = void $ do
  timeRef <- newRef startTime
  _       <- renderAdvancingClock (Tuple startTime startTime)
  _       <- setInterval advancePeriodInMilliseconds (advanceTime timeRef >>= renderAdvancingClock)
  pure unit

renderAdvancingClock :: forall eff. Tuple Time Time -> Eff (canvas :: CANVAS, dom :: DOM, ref :: REF, timer :: TIMER | eff) Unit
renderAdvancingClock times = do
  from <- pure $ fst times
  to   <- pure $ snd times
  _    <- animateClock "advancing-clock" from to
  _    <- displayTimeAsText to
  pure unit

advanceTime :: forall eff. Ref Time -> Eff (ref :: REF | eff) (Tuple Time Time)
advanceTime timeRef = do
    from <- readRef timeRef
    _    <- modifyRef timeRef add
    to   <- readRef timeRef
    pure $ Tuple from to
  where
    add :: Time -> Time
    add { hours: 12,    minutes: 45      } = time 1           0
    add { hours: hours, minutes: 45      } = time (hours + 1) 0
    add { hours: hours, minutes: minutes } = time hours       (minutes + 15)
    
displayTimeAsText :: forall eff. Time -> Eff (dom :: DOM | eff) Unit
displayTimeAsText time = do
  timeDigitsSpan <- select "#time-digits"
  _              <- setText (timeAsDigits time) timeDigitsSpan
  timeTextSpan   <- select "#time-text"
  _              <- setText (timeAsText time) timeTextSpan
  pure unit

timeAsDigits :: Time -> String
timeAsDigits { hours: hours, minutes: 0       } = (show hours) <> ":00"
timeAsDigits { hours: hours, minutes: minutes } = (show hours) <> ":" <> (show minutes)

timeAsText :: Time -> String
timeAsText   { hours: hours, minutes: 0 }       = (hourName hours) <> " óra"
timeAsText   { hours: hours, minutes: 15 }      = "negyed "      <> (hourName (nextHour hours))
timeAsText   { hours: hours, minutes: 30 }      = "fél "         <> (hourName (nextHour hours))
timeAsText   { hours: hours, minutes: 45 }      = "háromnegyed " <> (hourName (nextHour hours))
timeAsText   _                                  = "valami valami"

nextHour :: Int -> Int
nextHour    12 = 1
nextHour hours = hours + 1

hourName :: Int -> String
hourName  1 = "egy"
hourName  2 = "kettő"
hourName  3 = "három"
hourName  4 = "négy"
hourName  5 = "öt"
hourName  6 = "hat"
hourName  7 = "hét"
hourName  8 = "nyolc"
hourName  9 = "kilenc"
hourName 10 = "tíz"
hourName 11 = "tizenegy"
hourName 12 = "tizenkettő"
hourName  _ = "mi?"


startQuizClock :: forall eff. Time -> Eff (canvas :: CANVAS, dom :: DOM, random :: RANDOM, ref :: REF, timer :: TIMER | eff) Unit
startQuizClock default = do
  targetRef <- newRef default
  _         <- nextTarget targetRef
  inputBox  <- select "#time-guess"
  _         <- on "keyup" (makeGuess targetRef) inputBox
  pure unit

nextTarget :: forall eff. Ref Time -> Eff (canvas :: CANVAS, dom :: DOM, random :: RANDOM, ref :: REF, timer :: TIMER | eff) Unit
nextTarget targetRef = do
  previous <- readRef targetRef
  target   <- generateRandomTime targetRef
  _        <- animateClock "quiz-clock" previous target
  inputBox <- select "#time-guess"
  _        <- setValue "" inputBox
  pure unit

generateRandomTime :: forall eff. Ref Time -> Eff (ref :: REF, random :: RANDOM | eff) Time
generateRandomTime targetRef = do
    previous <- readRef targetRef
    next     <- randomTime
    _        <- if eqTime previous next
                  then void $ generateRandomTime targetRef
                  else writeRef targetRef next
    readRef targetRef
  where
    randomTime :: forall eff1. Eff (random :: RANDOM | eff1) Time
    randomTime = time <$> randomInt 1 12 <*> ((*) 15 <$> (randomInt 0 3))

makeGuess :: forall eff. Ref Time -> JQueryEvent -> JQuery -> Eff (canvas :: CANVAS, dom :: DOM, random :: RANDOM, ref :: REF, timer :: TIMER | eff) Unit
makeGuess targetRef _ inputBox = do
  guessForeign <- getValue inputBox
  guess        <- pure $ unsafeFromForeign guessForeign
  target       <- timeAsDigits <$> readRef targetRef
  if target == guess
    then nextTarget targetRef
    else pure unit
  