import Random
import Char
import String
import Array
import Set
import Signal exposing ((<~), (~))
import Time
import Window
import Keyboard
import Mouse
import Text
import Color
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)


fps = 30


--MODEL
type Sense = Audio | Visual
type Action = Choice Sense | PullNew

type alias Card = { char: Char, pos: Int }
emptyCard = { char='0', pos= -1 }
maybeCard = Maybe.withDefault emptyCard

type alias State =
    { cards : Array.Array Card
    , side  : Int
    , timeAfterLast : Int
    , lastCorrect : Maybe Bool
    , score : Int
    , n     : Int
    , waitTime : Int
    , seed  : Random.Seed
    , debugState : Bool}
defaultState = 
    { cards = Array.empty
    , timeAfterLast = 0
    , lastCorrect = Nothing
    , score = 0
    , n = 1
    , side = 3
    , waitTime = 3
    , seed = Random.initialSeed 0
    , debugState = False }


--INPUT
type Input = Input (Set.Set Keyboard.KeyCode) (Int, Int)

ticker : Signal (Int, Int)
ticker = Signal.foldp (\x y -> (round x, 1+snd y)) (0,0) (Time.fps fps)

input : Signal Input
input = Signal.sampleOn ticker <| Input <~ Keyboard.keysDown ~ ticker


--UPDATE
doAction : State -> Action -> State
doAction state action =
    let finalCard = Array.length state.cards - 1
        correct = maybeCard (Array.get (finalCard - state.n) state.cards)
        current = maybeCard (Array.get finalCard state.cards) 
        doesMatch sense =
            case sense of
              Audio  -> correct.char == current.char
              Visual -> correct.pos == current.pos in
    case action of
      Choice sense -> 
        let (mod, newCorrect) =
                if | state.lastCorrect /= Nothing -> (0, state.lastCorrect)
                   | doesMatch sense -> (1, Just True)
                   | otherwise -> (-1, Just False)
            (newN, newScore, newCards) = 
                if state.score > 19 && mod == 1
                    then (state.n+1, 0, Array.empty)
                    else (state.n, state.score + mod, state.cards) in
        {state | score <- newScore
        , n <- newN
        , cards <- newCards
        , lastCorrect <- newCorrect }
      PullNew -> 
        let (newScore, newCorrect) =
                if (doesMatch Audio || doesMatch Visual)
                && not (Array.isEmpty state.cards)
                && state.lastCorrect == Nothing
                    then (state.score - 1, Just False)
                    else (state.score, Nothing)
            maxPos = state.side^2 - 1
            (newChar, seed') = Random.generate (Random.int 97 107) state.seed
            (newPos, seed'') = Random.generate (Random.int 0 maxPos) seed'
            newCard = {char = Char.fromCode newChar, pos = newPos}
            newCards = keep 12 <| Array.push newCard state.cards in
        {state | score <- newScore
        , timeAfterLast <- 0
        , cards <- newCards
        , seed <- seed''
        , lastCorrect <- newCorrect }

extractKeys : Set.Set Keyboard.KeyCode -> Maybe Sense
extractKeys keys = 
    let a = Char.toCode 'A'
        s = Char.toCode 'S' in
    if | Set.member a keys -> Just Audio
       | Set.member s keys -> Just Visual
       | otherwise         -> Nothing

stepGame : Input -> State -> State
stepGame (Input keysDown d) state' =
    let act = if snd d % (state'.waitTime * fps) == 0
                then Just PullNew
                else Maybe.map Choice (extractKeys keysDown)
        seed = if snd d == 1
                 then Random.initialSeed (fst d)
                 else state'.seed
        state = {state' | seed <- seed
                , timeAfterLast <- state'.timeAfterLast + 1 } in
    Maybe.withDefault state (Maybe.map (doAction state) act)


--VIEW
cardColor = Color.rgb 60 100 60
lastCardColor = Color.rgb 200 0 0

dispLastTick = fps*1

redX : Int -> Element
redX size = 
    let bar = filled lastCardColor <| rect (toFloat size/10) (toFloat size*1.4) in
    collage size size [rotate (degrees 45) bar, rotate (degrees -45) bar]

greenCheck : Int -> Element
greenCheck size' =
    let size = toFloat size'
        bigBar' = rect (size/10) (size*1.1) |> filled cardColor
        bigBar = bigBar' |> moveX (size/4)
        smallBar' = rect (size/10) (size*0.7) |> filled cardColor
        smallBar = smallBar' |> move (-size/4, -size/4) in
    collage size' size'
                [rotate (degrees -30) bigBar, rotate (degrees 45) smallBar]
    
buildLastCorrect : Int -> Maybe Bool -> Element
buildLastCorrect size correct = 
    let g = greenCheck size
        r = redX size in
    Maybe.withDefault (spacer size size) (Maybe.map (bool g r) correct)

buildLastLetter state size = 
    let textFromCard = Text.fromString << String.fromChar << .char
        textElem = centered << Text.height (toFloat size) << textFromCard
        dispText = container size size middle << textElem
        shouldDisp = state.timeAfterLast < dispLastTick
        lastChar = bool (last state.cards) Nothing shouldDisp
        lastText = Maybe.map dispText lastChar in
    Maybe.withDefault (spacer size size) lastText


buildCard : Color.Color -> Float -> Form
buildCard color size = filled color (rect size size)


buildParts : Float -> State -> (Element, Element)
buildParts maxDim state =
    let cardSize = maxDim / 5
        cardBox = round <| cardSize + cardSize/5
        lastCard = Maybe.withDefault -1 (Maybe.map .pos (last state.cards))
        lastLetter = buildLastLetter state (round cardSize)
        getColor i j =
            if i + state.side * j == lastCard
            && state.timeAfterLast < dispLastTick
                then lastCardColor
                else cardColor
        makeCard i j = buildCard (getColor i j) cardSize
        dispCard i j = collage cardBox cardBox [makeCard i j]
        makeCol i = initialize (dispCard i) state.side
        cols = initialize (flow down << makeCol) state.side
        cards = flow right cols in
    (lastLetter, cards)
        

statusText : Int -> Int -> Element
statusText score n =
    let str = String.concat ["Score: "
                            , toString score
                            , "\n"
                            , "N: "
                            , toString n] in
    leftAligned <| Text.fromString str


view : (Int, Int) -> State -> Element
view (w, h) state =
    let statusDisp = statusText state.score state.n
        (lastLetter, cards) = buildParts (toFloat <| min w h) state
        remainWidth = round <| (toFloat (w - widthOf cards)/2)
        lLWidth = max remainWidth (widthOf lastLetter)
        lLElem = container lLWidth (heightOf cards) middle lastLetter
        lCWidth = min (w - lLWidth) (widthOf lastLetter)
        lastCorrect = buildLastCorrect lCWidth state.lastCorrect
        lCElem = container (remainWidth - 1) (heightOf cards) middle lastCorrect
        board = flow right [lLElem, cards, lCElem]
        lastCards = if state.debugState
                    then centered << Text.fromString <| showCards state.cards
                    else spacer 0 0 in
    flow down [statusDisp, board, lastCards]


--MAIN
gameState : Signal State
gameState = Signal.foldp stepGame defaultState input

main = Signal.map2 view Window.dimensions gameState
                     

--DEBUG
showCard : Card -> String
showCard {char, pos} = String.fromChar char ++ ":" ++ toString pos

showCards : Array.Array Card -> String
showCards xs = String.concat << List.intersperse " | "
               << Array.toList <| Array.map showCard xs



--UTILS

initialize : (Int -> a) -> Int -> List a
initialize f n =
    let go i = if i == n then [] else f i :: go (i+1) in
    go 0

last : Array.Array a -> Maybe a
last xs = Array.get (Array.length xs - 1) xs

keep : Int -> Array.Array a -> Array.Array a
keep n xs = if Array.length xs > n then Array.slice -n (Array.length xs) xs else xs

bool : a -> a -> Bool -> a
bool x y b = if b then x else y
