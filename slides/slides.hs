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
title = sized 0.2 . centered . color "blue" . fontSize (Pt 42)

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
      [ style "margin-left"  =: "4em"
      , style "margin-right" =: "4em"
      , style "line-height"  =: "4em"
      ]

smallContent :: Slide -> Slide
smallContent = groupAttrs atts . smallSize
  where
    atts =
      [ style "margin-left"  =: "4em"
      , style "margin-right" =: "4em"
      , style "line-height"  =: "3em"
      ]

--------------------------------------------------------------------------------
-- Slides

first :: Slide
first = verticallyCentered $ column
    [ sized 0.3 ""
    , title "Functional Programming\n @ D&IT"
    , centered $ normalSize "Patrik Jansson and Emil Axelsson"
    , sized 0.4 ""
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
    [ sized 0.25 $ title "Vad är funktionell programmering (för oss)?"
    , content $ list Unnumbered
        [ sublist Unnumbered "Några funktionella språk:"
            [ "Lisp, Scheme, Clojure, JavaScript, Erlang"
            , "Haskell, ML, Clean, Miranda och F#"
            ]
        ]
    ]

companies :: Slide
companies = column
    [ title "FP inom industrin"
    , smallContent $ list Unnumbered
        [ "Funktionell programmering blir allt vanligare i industrin"
        , sublist Unnumbered "Används bl.a. inom"
            [ "Telekommunikation (Erlang)"
            , "Finansmarknad (Haskell, OCaml, F#)"
            , "Web-programmering"
            ]
        , sublist Unnumbered "Nyckelegenskaper"
            [ "Produktivitet: koncis kod"
            , "Parallellisering: funktioner kan beräknas parallellt utan att störa varandra"
            , "Korrekthet: tydlig kod, testbarhet och starka typsystem"
            ]
        ]
    ]

logos :: Slide
logos = column
    [ sized 0.1 ""
    , centered $ withAttrs ["width" =: "850"] $ image "../AllLogos.png"
    ]

haste1 :: Slide
haste1 = column
    [ title "Praktisk FP: Haste-kompilatorn"
    , content $ list Unnumbered
        [ sublist Unnumbered "Haste: en Haskell-till-Javascript-kompilator"
            [ "Programmera webb-sidor i Haskell"
            ]
        , sublist Unnumbered "Utvecklat av en tidigare DV-student, Anton Ekblad"
            [ "... som för närvarande är doktorand i vår grupp"
            , "Började som ett kandidatprojekt!"
            ]
        ]
    , ""
    , centered $ content "[haste-lang.org](http://haste-lang.org)"
    ]

haste2 :: Slide
haste2 = column
    [ sized 0.15 $ smallTitle "Haste-exempel: programmerbara slides"
    , content $ list Unnumbered
        [ "Dessa slides är ett Haskell-program som just nu körs i webb-läsaren!"
        , "Skrivna mha. Antons `haste-deck` bibliotek: [https://github.com/valderman/haste-deck](https://github.com/valderman/haste-deck)"
        ]
    , centered $ image "code.png"
    ]

bouncing :: Slide
bouncing = column
    [ sized 0.08 $ title "Running Haskell code in the slides"
    , sized 0.08 ""
    , sized 0.01 $ verticallyCentered $ centered $ fontSize (Pt 22) "\"Bouncing balls\""
    , verticallyCentered $ centered $ lift $ do
          e <- newElem "div"
          bouncingBalls e
          return e
    ]

testbounce :: Slide
testbounce = lift $ do
          e <- newElem "div"
          bouncingBalls e
          return e

end :: Slide
end = verticallyCentered $ title "End of presentation."

main :: IO ()
main = do
  present_ def {transition = pan}
      [ --testbounce
        first
      , whatIsFP
      , whatIsFP2
      , companies
      , logos
      , haste1
      , haste2
      , bouncing
      , end
      ]

--------------------------------------------------------------------------------

type Size = (Double, Double)

radius :: Double
radius = 15

type Ball  = [Point]
type State = [Ball]

bounce :: Size -> Point -> Int -> Ball
bounce (w, h) (x, y) v
   | v == 0 && y >= maxY = replicate 20 (x, y)
   | y' > maxY           = bounce (w, h) (x, y) (2-v)  -- inverse "velocity - 2"
   | otherwise           = (x, y) : bounce (w, h) (x, y') v'
 where
   maxY = h - radius
   v'   = v + 1
   y'   = y + fromIntegral v

step :: State -> State
step bs = [ps | _:ps <- bs] -- Note that this also removes empty lists (finished balls)

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
          pos = fixCoord (fromIntegral x, fromIntegral y)
--          fixCoord (x, y) = (x, y - 310) -- for my (Patrik's) native resolution
          fixCoord (x, y) = (x, y - 185) -- for 1024x768 presentation in full-screen mode

      -- For some reason the y-coordinate is from the top of the
      -- browser window, not from the top of the pane. The adjustment
      -- needed depends on where the canvas is in relation to the
      -- browser window. There is something wrong in the tranlation of
      -- the mouse coordinates (should use canvas.offsetTop but seems
      -- not to work).
      -- https://github.com/valderman/haste-compiler/search?utf8=%E2%9C%93&q=offsetTop

      balls <- readIORef state
      writeIORef state $ bounce (canWidth, canHeight) pos 0 : balls

    -- Set an event handler for the clear button
    clear `onEvent` Click $ \_ -> writeIORef state []
