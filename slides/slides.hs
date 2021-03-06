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

title' :: Slide -> Slide
title' = centered . color "blue" . fontSize (Pt 42)

title :: Slide -> Slide
title = sized 0.2 . groupAttrs atts . title'
  where
    atts =
      [ style "margin-left"  =: "1em"
      , style "margin-right" =: "1em"
      ]

smallTitle :: Slide -> Slide
smallTitle =
    sized 0.15 . groupAttrs atts . centered . color "blue" . fontSize (Pt 38)
  where
    atts =
      [ style "margin-left"  =: "1em"
      , style "margin-right" =: "1em"
      ]

normalSize :: Slide -> Slide
normalSize = fontSize (Pt 28)

smallSize :: Slide -> Slide
smallSize = fontSize (Pt 22)

content :: Slide -> Slide
content = groupAttrs atts . normalSize
  where
    atts =
      [ style "margin-left"  =: "3em"
      , style "margin-right" =: "3em"
      , style "line-height"  =: "4em"
      ]

smallContent :: Slide -> Slide
smallContent = groupAttrs atts . smallSize
  where
    atts =
      [ style "margin-left"  =: "3em"
      , style "margin-right" =: "3em"
      , style "line-height"  =: "3em"
      ]

--------------------------------------------------------------------------------
-- Slides

first :: Slide
first = verticallyCentered $ column
    [ sized 0.25 $ title' "Funktionell Programmering\n på Chalmers och GU"
    , sized 0.2 $ centered $ normalSize "Patrik Jansson (+ J. Svenningsson & E. Axelsson)"
    , content $ fontSize (Pt 22) $ centered $ code
        "sort :: [Int]    -> [Int]                             \n\
        \sort    []       =  []                                 \n\
        \sort    (x:xs)   =  sort smaller ++ [x] ++ sort larger  \n\
        \  where  smaller =  filter (&lt;x)  xs                   \n\
        \         larger  =  filter (&gt;=x) xs                    \n"
    ]

whatIsFP :: Slide
whatIsFP = column
    [ title "Vad är funktionell programmering?"
    , content $ list Unnumbered
        [ "En programmeringsmetodik - ett sätt att programmera"
        , sublist Unnumbered "Fokus på **abstraktion** och **komposition**"
          [ "Skapa återanvändbara byggstenar"
          , "Bygg program genom att komponera dessa byggstenar"
          ]
        , "Korta och koncisa program"
        , "Program blir enklare att testa och verifiera"
        , "Enklare att parallellisera"
        ]
    ]

whatIsFP2 :: Slide
whatIsFP2 = column
    [ sized 0.25 $ title "Vad är funktionell programmering?"
    , content $ list Unnumbered
        [ sublist Unnumbered "Några funktionella språk:"
            [ "Lisp, Scheme, Clojure, JavaScript, Erlang"
            , "Haskell, ML och F#"
            ]
        , sublist Unnumbered "Många språk har fått funktionella utökningar"
          [ "Java, C#, C++"
          ]
        , sublist Unnumbered "Nya språk med starka funktionella influenser"
          [ "Rust, Swift"
          ]
        ]
    ]

haste1 :: Slide
haste1 = column
    [ title "Praktisk FP: Haste-kompilatorn"
    , content $ list Unnumbered
        [ sublist Unnumbered "Haste: en Haskell-till-Javascript-kompilator"
            [ "Programmera webb-sidor i Haskell"
            ]
        , sublist Unnumbered "Utvecklat av en tidigare DV-student, Anton Ekblad"
            [ "... för närvarande doktorand i FP gruppen"
            , "Började som ett kandidatprojekt!"
            ]
        ]
    , ""
    , centered $ content "[haste-lang.org](http://haste-lang.org)"
    ]

haste2 :: Slide
haste2 = column
    [ smallTitle "Haste-exempel: programmerbara slides"
    , content $ list Unnumbered
        [ "Dessa slides är ett Haskell-program som \n\
          \just nu körs i webb-läsaren!"
        , "Skrivna mha. Antons `haste-deck` bibliotek: \
          \[github.com/valderman/haste-deck]\
          \(https://github.com/valderman/haste-deck)"
        ]
    , centered $ image "code.png"
    ]

bouncing :: Slide
bouncing = column
    [ sized 0.08 $ title "Köra Haskell-kod i slides"
    , sized 0.08 ""
    , sized 0.01 $ verticallyCentered $ centered $ fontSize (Pt 22) "\"Bouncing balls\""
    , verticallyCentered $ centered $ lift $ do
          e <- newElem "div"
          bouncingBalls e
          return e
    ]

fpResearch :: Slide
fpResearch = column
    [ title "Forskning i FP-gruppen"
    , content $ list Unnumbered
        [ "Haskell (!) ([kompilator från Chalmers 1990](http://wiki.portal.chalmers.se/cse/pmwiki.php/FP/Haskell20years))"
        , sublist Unnumbered "Domänspecifika programmeringsspråk"
            [ "Lava (för hårdvara), Obsidian (GPU),\n Feldspar (signalbehandling), etc."
            ]
        , sublist Unnumbered "Testning och formell verifiering"
            [ "QuickCheck (testning), SAT-lösare"
            , "Teorembevisning (Agda)"
            ]
        , sublist Unnumbered "Andra tillämpningar"
            [ "Språk, Klimateffekter, Matematikens DSL, ..."
            ]
        ]
    ]

haskell :: Slide
haskell = column
    [ title "Vår forskning: Haskell"
    , content $ list Unnumbered
        [ sublist Unnumbered "Medlemmar från FP gruppen har"
            [ "Bidragit till utformandet av Haskell"
            , "Varit med i kommitéer för Haskell-\nstandarder"
            , "Utvecklat den första Haskell-kompilatorn\n till native-kod"
            ]
        ]
    ]

dsl_Lava1 :: Slide
dsl_Lava1 = column
    [ smallTitle "Vår forskning: Lava"
    , sized 0.35 $ smallContent $ list Unnumbered
        [ "Funktionell hårdvarubeskrivning"
        , sublist Unnumbered "Exempel: parallella prefixnätverk"
            [ "Vanliga i mikroprocessorer (adderare, etc.)"
            , "Används för t.ex. sortering på\n grafikprocessorer (GPU:er)"
            ]
        ]
    , centered $ image "PP_20_abc.png"
    ]

sklanskyLava = centered $ fontSize (Pt 18) $ code
    "sklansky op [a] = [a]           \n\
    \sklansky op as  = bs ++ rs      \n\
    \  where                         \n\
    \    (as1,as2) = halveList as    \n\
    \    bs = sklansky op as1        \n\
    \    cs = sklansky op as2        \n\
    \    bn = last bs                \n\
    \    rs = [op (bn,c) | c <- cs]  \n"

dsl_Lava2 :: Slide
dsl_Lava2 = column
    [ smallTitle "Vår forskning: Lava"
    , sized 0.08 $ smallContent $ list Unnumbered ["Parallella prefixnätverk är rekursiva:"]
    , sized 0.37 $ sklanskyLava
    , sized 0.2 $ centered $ image "Sklansky_32.png"
    , sized 0.2 $ smallContent "(Samma program genererar både hårdvara och bilden ovan!)"
    ]

dsl_Lava3 :: Slide
dsl_Lava3 = column
    [ smallTitle "Vår forskning: Lava"
    , smallContent $ list Unnumbered
        [ sublist Unnumbered "Det finns massor av sätt att dela upp parallella prefix\n rekursivt"
            [ "Beslut påverkar: antal operatorer, nätverkets\n hastighet, etc."
            ]
        , sublist Unnumbered "Idé: Använd Haskell + Lava för att söka efter optimala\n nätverk (Mary Sheeran)"
            [ "Resultat: Minsta möjliga prefixnätverken"
            , "Skalar upp till nätverk av miljontals invärden"
            ]
        ]
    , sized 0.25 $ centered $ withAttrs ["width" =: "1000"] $ image "PP_256.png"
    ]

wired1 :: Slide
wired1 = column
    [ smallTitle "Vår forskning: Wired"
    , smallContent $ list Unnumbered
        [ sublist Unnumbered "Wired är en utökning av Lava för att utveckla ASICS\n (mikrochip)"
            [ "Programmeraren kontrollerar layout och routing (ledningsdragning)"
            ]
        ]
    , sized 0.14 $ smallContent "Parallellt prefix i Wired:"
    , sized 0.4 $ centered $ withAttrs ["width" =: "950"] $ image "Sklansky_Wired_16.gif"
    ]

wired2 :: Slide
wired2 = column
    [ sized 0.12 $ smallContent $ "Parallellt prefix översatt till fabricerbar layout:"
    , sized 0.36 $ centered $ withAttrs ["width" =: "600"] $ image "Sklansky_Wired_Enc.gif"
    , sized 0.4 $ centered $ withAttrs ["width" =: "500"] $ image "Sklansky_Wired_Enc_Compacted.gif"
    ]

säkerhet :: Slide
säkerhet = column
  [ smallTitle "Vår forskning: Säkerhetsbibliotek"
  , content $ list Unnumbered
    [ "I vissa program är det viktigt att säker information \
      \inte läcker ut"
    , "T.ex. password managers, bankappar, etc."
    , "I Haskell kan man implementera ett bibliotek som garanterar \
      \att information inte läcks på\n ett olämpligt sätt."
    , "För att uppnå samma sak i andra språk skulle man behöva \
      \utöka eller ändra på språket."
    ]
  ]

säkerhetAgda = column
  [ smallTitle "Vår forskning: Säkerhetsbibliotek: bevis i Agda"
  , contentLU
      [ "Hur säker är man egentligen på ett handgjort bevis?"
      , "Om man kan göra en bra modell av biblioteket \
        \kan man också specificera vad 'säker' betyder \
        \och skriva ned bevis som Agda kan kontrollera!"
      , "=> Datorstödd teorembevisning."
      , "[Alejandro Russo](http://www.cse.chalmers.se/~russo/)'s grupp jobbar på detta"
      ]
  ]

säkerhet2 :: Slide
säkerhet2 = column
  [ smallTitle "Vår forskning: Säkerhetsbibliotek"
  , smallContent $ list Unnumbered
    [ "Exempel: Alice will skriva en password manager."
    , "Ninjacoder tillhandahåller ett bibliotek för att varna för\n \
      \svaga passwords.\nDet kommunicerar med en server för att \
      \ibland hämta en\n lista med svaga passwords."
    , "Hur kan Alice var säker på att ninjacoders biblioteket inte\n stjäl passwords?"
    ]
  , sized 0.33 $ centered $ withAttrs ["width" =: "1000"] $ image "passwordninja.png"
  ]

säkerhet3 :: Slide
säkerhet3 = column
  [ smallTitle "Vår forskning: Säkerhetsbibliotek"
  , content $ list Unnumbered
    [ "I Haskell kan man använda typsystemet för att garanterat \
      \undvika informationsläckor."
    , "Vårt exempel:"
    ]
  , sized 0.2 $ centered $ fontSize (Pt 18) $ code "common :: Labeled Secret String -> MAC Public (MAC Secret Bool)"
  , centered $ withAttrs ["width" =: "1000"] $ image "passwordninja.png"
  ]

sat :: Slide
sat = column
  [ title "Vår forskning: Verifiering"
  , smallContent $ list Unnumbered
    [ sublist Unnumbered "Vår grupp har utvecklat flera verktyg för verifiering"
      [ "**MiniSAT**, en av världens bästa SAT-lösare\n\
        \Används flitigt för att verifiera hårdvarukretsar."
      , "**Paradox** och **Infinox** hittar modeller i första ordningens logik"
      , "**Equinox** bevisar teorem i första ordningens logik"
      ]
    , "Alla dessa verktyg har vunnit flera tävlingar"
    ]
  ]

sat2 :: Slide
sat2 = column
  [ title "Vår forskning: Verifiering"
  , row
      [ content $ centered $ sized 0.1 $ list Unnumbered
        [ "SAT-lösare kan användas för att enkelt skriva en sudoku-lösare"
        , "Beskriv hur ett korrekt sudoku-\nbräde ser ut"
        , "SAT-lösaren hittar en lösning på brädet"
        ]
      , centered $ image "sudoku.png"
      ]
  ]

agda :: Slide
agda = column
    [ smallTitle "Vår forskning: att programmera bevis"
    , content $ list Unnumbered
        [ sublistU "[Agda](http://wiki.portal.chalmers.se/agda/pmwiki.php) \
              \är ett språk från Gbg som stödjer"
          [  "program med beroende typer,"
          , "specifikation av programegenskaper,"
          , "samt bevis av korrekthet."
          ]
        ]
-- TODO: example
    ]

agda2 :: Slide
agda2 = column
  [ title "Exempel på program i Agda"
  , rightAgda
  , leftAgda
  ]

leftAgda :: Slide
leftAgda = sized 0.37 $ fontSize (Pt 18) $ code
    "data Nat : Set where     \n\
    \  zero : Nat             \n\
    \  suc  : Nat -> Nat      \n\
    \                         \n\
    \_+_ : Nat -> Nat -> Nat  \n\
    \zero  + m  =  m          \n\
    \suc n + m  =  suc (n + m)"

rightAgda :: Slide
rightAgda = sized 0.37 $ fontSize (Pt 18) $ code
    "data Bool : Set where \n\
    \  true  : Bool        \n\
    \  false : Bool        \n\
    \                      \n\
    \if_then_else_ : {A : Set} -> Bool -> A -> A -> A \n\
    \if true  then x else y  =  x \n\
    \if false then x else y  =  y"

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
            , "Parallellisering: funktioner kan beräknas parallellt utan\n att störa varandra"
            , "Korrekthet: tydlig kod, testbarhet och starka typsystem"
            ]
        ]
    ]

logos :: Slide
logos = column
    [ sized 0.1 ""
    , centered $ withAttrs ["width" =: "850"] $ image "AllLogos.png"
    ]

språkteknologi :: Slide
språkteknologi = column
    [ title "Vår forskning: språkteknologi"
    , contentLU
        [ "Vision: Grammatical Framework: Formalizing the Grammars of the World ([GoogleTechTalks](https://www.youtube.com/watch?v=x1LFbDQhbso))"
        , "Grammatik-baserad teknik för översättning ..."
        , "... till och från ca 30 naturliga språk"
        , "Använder funktionell programmering (och beroende typer)"
        , "Leds av Aarne Ranta - [grammaticalframework.org/](http://www.grammaticalframework.org/)"
        , "Exempel: [Kylskåpsmagneter](http://cloud.grammaticalframework.org/minibar/minibar.html)"
        ]
    ]

varförFP :: Slide
varförFP = column
    [ smallTitle "Varför lära sig om Funktionell Programmering?"
    , smallContent $ list Unnumbered
        [ "Rapid prototyping = det går *snabbt* att få fungerande program"
        , "Kraftfullt type system = datorn hjälper dig att göra rätt"
        , "Design patterns = programsnuttar kan återanvändas mycket flexibelt"
        , "Lättförståeliga program"
        , "industrial strength compilers = mycket bra kompilatorer"
        , "promising parallelisation properties = lovande egenskaper för parallellisering"
        ]
    ]

contentLU = content . list Unnumbered
-- sublistU :: Markup -> [List] -> List
sublistU = sublist Unnumbered

introFP :: String
introFP = "[Introduktion till Funktionell Programmering]\
          \(http://www.cse.chalmers.se/edu/course/TDA555/)"

kurserFP1 :: Slide
kurserFP1 = column
    [ sized 0.23 $ smallTitle "I vilka kurser lär jag mig mer om Funktionell Programmering?"
    , smallContent $ list Unnumbered
      [ sublistU "Kandidatnivån (första tre åren):"
        [ "**LP1:** [Introduktion till Funktionell Programmering](http://www.cse.chalmers.se/edu/course/TDA555/)"
        , "**LP2:** [Functional Programming](http://www.cse.chalmers.se/edu/course/TDA452/)"
        , "**LP3:** [Domain Specific Languages of Mathematics](https://github.com/DSLsofMath/DSLsofMath#dslsofmath)"
        , "**LP3-4:** Kandidatarbete &ndash; beroende på projektval"
        ]
      ]
    ]

kurserFP2 :: Slide
kurserFP2 = column
    [ sized 0.23 $ smallTitle "I vilka kurser lär jag mig mer om Funktionell Programmering?"
    , smallContent $ list Unnumbered
      [ sublistU "Masternivån (sista två åren):"
        [ "**LP1:** [Types for Programs and Proofs](http://www.cse.chalmers.se/edu/course/DAT140/)"
        , "**LP2:** [Programming Language Technology](http://www.cse.chalmers.se/edu/course/DAT151/)"
        , "**LP2:** [Models of Computation](https://sites.google.com/site/modelsofcomputation/)"
        , "**LP3:** [Advanced Functional Programming](http://www.cse.chalmers.se/edu/course/afp/)"
        , "**LP4:** [Parallel Functional Programming](http://www.cse.chalmers.se/edu/course/DAT280_Parallel_Functional_Programming/)"
        , "**LP4:** [Compiler Construction](http://www.cse.chalmers.se/edu/course/TDA283/)"
        ]
      ]
    ]

kurserFP3 :: Slide
kurserFP3 = column
    [ smallTitle "Mer om kurserna i de tre första åren"
    , smallContent $ list Unnumbered
      [ sublistU ("Introduktion till Funktionell Programmering (Dave Sands) /\n Functional Programming (Thomas Hallgren)\n År 1, LP1 för D, DV och valfri för IT (år 3, LP2)")
        [ "Språket Haskell"
        , "Rekursion, datatyper, testning, etc."
        ]
      ]
    ]

kurserFP4 :: Slide
kurserFP4 = column
    [ smallTitle "Mer om kurserna i de tre första åren"
    , smallContent $ list Unnumbered
      [ sublistU "År 2-3: [Domain Specific Languages of Mathematics](https://github.com/DSLsofMath)\n <span style=\"font-size:80%\">(Patrik Jansson & Cezar Ionescu)</span>"
          [ "Exempel på domänspecifika språk: datum, algebraiska uttryck, integraler"
          , "Förstå centrala matematiska begrepp genom programmering och tvärtom!"
          , "Fokus på lämplig notation för matematik (därav \"språk\" i titeln)"
          , "Genomföra och räkna med bevis"
          ]
      ]
    ]

kurserFP5 :: Slide
kurserFP5 = column
    [ smallTitle "Mer om kurserna i de två sista åren"
    , smallContent $ list Unnumbered
      [ sublistU "[Advanced Functional Programming](http://www.cse.chalmers.se/edu/course/afp/)\n <span style=\"font-size:80%\">(Patrik Jansson → Alejandro Russo)</span>"
          [ "Haskell i industri och teori"
          , "Domänspecifika språk som programvaruarkitektur"
          , "Strukturera program mha. matematik: *funktorer*,\n *monader*, *transformer*"
          , "Testning och bevis"
          ]
      ]
    ]

kurserFP6 :: Slide
kurserFP6 = column
    [ smallTitle "Mer om kurserna i de två sista åren"
    , smallContent $ list Unnumbered
      [ sublistU "[Programming Language Technology](http://www.cse.chalmers.se/edu/course/DAT151/) <span style=\"font-size:80%\">(Andreas Abel)</span>"
          [ "Hur implementerar man ett programspråk?"
          , "Fokus på \"front end\" &ndash; syntax, semantik, typer, etc.\n&nbsp;"
          ]
      , sublistU "[Compiler Construction](http://www.cse.chalmers.se/edu/course/TDA283/) <span style=\"font-size:80%\">(Josef Svenningsson → Alex Gerdes)</span>"
          [ "Hur implementerar man ett programspråk?"
          , "Fokus på \"back end\" &ndash; kodgenerering, optimering, etc."
          ]
      ]
    ]

frågor :: Slide
frågor = column
    [ title "Frågor?"
    , row [ withAttrs ["width" =: "500"] $ image "Ekorre_smultron_Langvind.jpg"
          , withAttrs ["width" =: "350"] $ image "Tall_i_sol_Langvind.jpg"]
    ]

end :: Slide
end = verticallyCentered $ title' "End of presentation."

main :: IO ()
main = do
  present_ def {transition = pan}
      [ first
      , whatIsFP
      , whatIsFP2
      , haste1
      , haste2
      , bouncing
      , fpResearch
      -- , haskell
      , dsl_Lava1
      , dsl_Lava2
      , dsl_Lava3
      , wired1
      , wired2
      , sat
      , sat2
      , agda
      , agda2
      , säkerhet
      , säkerhetAgda
      , språkteknologi
--      , säkerhet2
--      , säkerhet3
      , companies
      , logos
      -- , varförFP
      , kurserFP1
      , kurserFP2
      , kurserFP3
      , kurserFP4
      , kurserFP5
      , kurserFP6
      , frågor
      , end
      ]

--------------------------------------------------------------------------------

type Height = Double

radius :: Double
radius = 15

type Ball  = [Point]
type State = [Ball]

bounce :: Height -> Point -> Int -> Ball
bounce h (x, y) v
   | v == 0 && y >= maxY = replicate 20 (x, y)
   | y' > maxY           = bounce h (x, y) (2-v)  -- inverse "velocity - 2"
   | otherwise           = (x, y) : bounce h (x, y') v'
 where
   maxY = h - radius
   v'   = v + 1
   y'   = y + fromIntegral v

step :: State -> State
step bs = [ps | _:ps <- bs] -- This also removes empty lists (finished balls)

ballShape :: Ball -> Shape ()
ballShape []      = return ()
ballShape (pos:_) = circle pos radius
-- ballShape (pos:_) = circle pos (radius+3*sin (snd pos / 75))

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

bouncingBalls :: Elem -> IO HandlerInfo
bouncingBalls el = do
    canvas <- mkCanvas canWidth canHeight
    clear  <- mkButton "clear"
    elemColumn el [canvas, clear]

    Just can <- fromElem canvas

    -- Use an IORef to communicate between the animation and the event handlers
    state <- newIORef []

    -- Start the animation
    animate can state

    -- Set an event handler for clicks in the canvas
    canvas `onEvent` Click $ \evt -> do
      let (x, y) = mouseCoords evt
          pos = fixCoord (fromIntegral x, fromIntegral y)
          fixCoord (x, y) = (x, y-185)

      balls <- readIORef state
      writeIORef state $ bounce canHeight pos 0 : balls

    -- Set an event handler for the clear button
    clear `onEvent` Click $ \_ -> writeIORef state []


--------------------------------------------------------------------

-- For some reason the y-coordinate is from the top of the
-- browser window, not from the top of the pane. The adjustment
-- needed depends on where the canvas is in relation to the
-- browser window. There is something wrong in the tranlation of
-- the mouse coordinates (should use canvas.offsetTop but seems
-- not to work).
-- https://github.com/valderman/haste-compiler/search?utf8=%E2%9C%93&q=offsetTop

-- fixCoord (x, y) = (x, y - 310) -- for my (Patrik's) native resolution
-- fixCoord (x, y) = (x, y - 350) -- for 1680x1050 pixels (HB4) in full-screen mode
