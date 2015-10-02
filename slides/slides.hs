{-# LANGUAGE OverloadedStrings #-}

import Data.IORef

import Haste
import Haste.DOM
import Haste.Events
import Haste.Graphics.Canvas hiding (color)
import qualified Haste.Graphics.Canvas as Canvas
import Haste.Deck



--------------------------------------------------------------------------------
-- Helper functions

title :: Slide -> Slide
title = centered . color "blue" . fontSize (Pt 42)

smallTitle :: Slide -> Slide
smallTitle = centered . color "blue" . fontSize (Pt 38)

normalSize :: Slide -> Slide
normalSize = fontSize (Pt 28)

smallSize :: Slide -> Slide
smallSize = fontSize (Pt 22)

content :: Slide -> Slide
content = groupAttrs atts . normalSize
  where
    atts =
      [ style "margin-left"  =: "6em"
      , style "margin-right" =: "4em"
      , style "line-height"  =: "4em"
      ]

smallContent :: Slide -> Slide
smallContent = groupAttrs atts . smallSize
  where
    atts =
      [ style "margin-left"  =: "6em"
      , style "margin-right" =: "4em"
      , style "line-height"  =: "3em"
      ]

--------------------------------------------------------------------------------
-- Slides

first :: Slide
first = verticallyCentered $ column
    [ ""
    , ""
    , title "Functional Programming\n @ D&IT"
    , centered $ normalSize "Patrik Jansson and Emil Axelsson"
    , ""
    , ""
    , ""
    ]

whatIsFP :: Slide
whatIsFP = column
    [ title "Vad är funktionell programmering (för oss)?"
    , smallContent $ list Unnumbered
        [ "Mästerligt möte mellan matematik och maskin"
        , sublist Unnumbered "Ett fantastiskt flexibelt språk för (inte bara matematiska) uttryck"
            [ "Algebra: 3+4, 5/8, (17+3)*(2 + 1), ..."
            , "Text: `reverse \"kirtaP\"`, `\"Hej \" ++ namn ++ \"!\"`, ..."
            , "Funktioner: `area r = pi*r^2`, TODO: more examples"
            , "Kombinationer: `let   area r = pi*r^2   in  (area 2) / (area 1)`"
            ]
        , "Korta och koncisa definitioner"
        , "Typer som hjälper programmeraren att undvika fel"
        , "Kontrollerade sidoeffekter ger bra parallellism och korrekthet"
        ]
    , ""
    , ""
    ]

whatIsFP2 :: Slide
whatIsFP2 = column
    [ title "Vad är funktionell programmering (för oss)?"
    , content $ column
        [ "Några funktionella språk:"
        , list Unnumbered
            [ "Lisp, Scheme, Clojure, JavaScript, Erlang"
            , "Haskell, ML, Clean, Miranda och F#"
            ]
        ]
    , ""
    ]

logos :: Slide
logos = column
    [ title "Loads of FP-related companies"
    , centered $ withAttrs atts $ image "../AllLogos.png"
    , ""
    , ""
    , ""
    , ""
    ]
  where
    atts = ["width" =: "850"]

bouncing :: Slide
bouncing = column
    [ sized 0.08 $ title "Bouncing balls live"
    , verticallyCentered $ centered $ lift $ do
          e <- newElem "div"
          bouncingBalls e
          return e
    ]

end :: Slide
end = verticallyCentered $ title "End of presentation."

main :: IO ()
main = do
  deck <- createDeck pan
      [ first
      , whatIsFP
      , whatIsFP2
      , logos
      , bouncing
      , end
      ]
  appendChild documentBody deck
  enableDeck deck



--------------------------------------------------------------------------------

type Size = (Double, Double)

radius :: Double
radius = 15

type Ball  = [Point]
type State = [Ball]

bounce :: Size -> Point -> Int -> Ball
bounce (w, h) (x, y) v
   | v == 0 && y >= maxY = replicate 20 (x, y)
   | y' > maxY           = bounce (w, h) (x, y) (2-v)
   | otherwise           = (x, y) : bounce (w, h) (x, y') v'
 where
   maxY = h - radius
   v'   = v + 1
   y'   = y + fromIntegral v

step :: State -> State
step bs = [ps | _:ps <- bs]

ballShape :: Ball -> Shape ()
ballShape []      = return ()
ballShape (pos:_) = circle pos radius

drawBall :: Ball -> Picture ()
drawBall ball = do
    Canvas.color red $ fill $ ballShape ball
    stroke $ ballShape ball
  where red = RGB 255 0 0

animate :: Canvas -> IORef State -> IO ()
animate can state = do
    balls <- readIORef state
    writeIORef state $ step balls
    render can $ mapM_ drawBall balls
    setTimer (Once 20) $ animate can state
    return ()

----------------------------------------------------------------

-- `wrapDiv e` makes a "div" node with `e` as the only child
wrapDiv :: Elem -> IO Elem
wrapDiv e = newElem "div" `with` [children [e]]

-- `appendChildren parent children` adds a list of children to a parent element
appendChildren :: Elem -> [Elem] -> IO ()
appendChildren parent children = sequence_ [appendChild parent c | c <- children]

-- `column parent children` adds the children as a column to the parent
elemColumn :: Elem -> [Elem] -> IO ()
elemColumn parent children = do
    cs <- sequence [wrapDiv c | c <- children]
    appendChildren parent cs

-- `mkButton label` makes a clickable button with the given label
mkButton :: String -> IO Elem
mkButton label = newElem "button" `with` [prop "textContent" =: label]

-- `mkCanvas width height` makes a drawing canvas of the specified dimensions
mkCanvas :: Int -> Int -> IO Elem
mkCanvas width height =
    newElem "canvas" `with`
        [ style "border"          =: "1px solid black"
        , style "backgroundColor" =: "#EEE"
        , prop "width"            =: show width
        , prop "height"           =: show height
        ]

canWidth, canHeight :: Num a => a
canWidth  = 700
canHeight = 500

-- TODO: On my machine the balls start lower than the mouse click position (Patrik).
bouncingBalls :: Elem -> IO HandlerInfo
bouncingBalls el = do
    canvas <- mkCanvas canWidth canHeight
    clear  <- mkButton "clear"
    elemColumn el [canvas, clear]

    Just can <- getCanvas canvas

    -- Use an IORef to communicate between the animation and the event handlers
    state <- newIORef []

    -- Start the animation
    animate can state

    -- Set an event handler for clicks in the canvas
    canvas `onEvent` Click $ \evt -> do
      let (x, y) = mouseCoords evt
          pos = (fromIntegral x, fromIntegral y)
      balls <- readIORef state
      writeIORef state $ bounce (canWidth, canHeight) pos 0 : balls

    -- Set an event handler for the clear button
    clear `onEvent` Click $ \_ -> writeIORef state []

