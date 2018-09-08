module Events exposing
    ( decodeMouseEvent
    , decodeTouchEvent
    , decodeWheelEvent
    , onDragStart
    , onDrop
    , onSingleOrDoubleClick
    , onWithStopAndPrevent
    , prepareDoubleClick
    , preventDefault
    , setDummyDragData
    , stopPropagation
    )

import Html
import Html.Attributes as HA
import Html.Events as HE
import Json.Decode as Json
import String
import Tuple


type alias CustomDecoder msg =
    Json.Decoder
        { message : msg
        , stopPropagation : Bool
        , preventDefault : Bool
        }


type alias PairDecoder msg =
    Json.Decoder ( msg, Bool )


minusPos : ( Float, Float ) -> ( Float, Float ) -> ( Float, Float )
minusPos ( x0, y0 ) ( x1, y1 ) =
    ( x0 - x1, y0 - y1 )


decodeListLike : Json.Decoder a -> Json.Decoder (List a)
decodeListLike decoder =
    let
        decodeIndex i =
            Json.field (String.fromInt i) decoder

        list length =
            List.range 0 (length - 1)
                |> List.map decodeIndex
                |> List.foldr (Json.map2 (::)) (Json.succeed [])
    in
    Json.field "length" Json.int
        |> Json.andThen list


decodeTouchEvent : (List ( Float, Float ) -> msg) -> Json.Decoder msg
decodeTouchEvent tagger =
    let
        decodeTarget =
            Json.field "target" decodeOffset

        decodeTouches =
            Json.field "touches" <| decodeListLike decodeClientPos

        minusPositions positions base =
            List.map (\pos -> minusPos pos base) positions
    in
    Json.map tagger <|
        Json.map2 minusPositions decodeTouches decodeTarget


decodeOffset : Json.Decoder ( Float, Float )
decodeOffset =
    Json.map2 Tuple.pair
        (Json.field "offsetLeft" Json.float)
        (Json.field "offsetTop" Json.float)


decodeClientPos : Json.Decoder ( Float, Float )
decodeClientPos =
    Json.map2 Tuple.pair
        (Json.field "clientX" Json.float)
        (Json.field "clientY" Json.float)


decodeRelativePos : Json.Decoder ( Float, Float )
decodeRelativePos =
    let
        decodeTarget =
            Json.field "currentTarget" decodeOffset
    in
    Json.map2 minusPos decodeClientPos decodeTarget


decodeMouseEvent : (( Float, Float ) -> msg) -> Json.Decoder msg
decodeMouseEvent tagger =
    Json.map tagger decodeRelativePos


withFlag : Bool -> Json.Decoder msg -> PairDecoder msg
withFlag bool decoder =
    let
        wrap msg =
            ( msg, bool )
    in
    Json.map wrap decoder


stopAndPrevent : Bool -> Bool -> Json.Decoder msg -> CustomDecoder msg
stopAndPrevent stop prevent decoder =
    let
        wrap msg =
            { message = msg
            , stopPropagation = stop
            , preventDefault = prevent
            }
    in
    Json.map wrap decoder


onWithStopAndPrevent : String -> Json.Decoder msg -> Html.Attribute msg
onWithStopAndPrevent eventName decoder =
    HE.custom eventName (stopAndPrevent True True decoder)


preventDefault : String -> Html.Attribute msg
preventDefault eventAttributeName =
    HA.attribute eventAttributeName "event.preventDefault()"


stopPropagation : String -> Html.Attribute msg
stopPropagation eventAttributeName =
    HA.attribute eventAttributeName "event.stopPropagation()"


onDragStart : msg -> Html.Attribute msg
onDragStart msg =
    HE.on "dragstart" <| Json.succeed msg


onDrop : msg -> Html.Attribute msg
onDrop msg =
    HE.preventDefaultOn "drop" (withFlag True <| Json.succeed msg)


{-| HACK: Double tap support on iOS

iOS does not support `dblclick` event. To detect double tap on iOS:

  - `prepareDoubleClick` sets a flag to the clicked element's `dataset` and
    deletes it in a short amount of time.
  - `onSingleOrDoubleClick` uses a JSON decoder to check if the flag exists
    and returns a message for double click if the flag exists. Otherwise it
    just returns a message for single click.

-}
prepareDoubleClick : Html.Attribute msg
prepareDoubleClick =
    HA.attribute "onclick"
        """
        var el = event.currentTarget;
        setTimeout(function () {
          if (el.dataset.timer) { clearTimeout(parseInt(el.dataset.timer, 10)); }
          el.dataset.timer = setTimeout(function () {
            delete el.dataset.clicked;
          }, 500);
          el.dataset.clicked = 'true';
        });
        """


onSingleOrDoubleClick : msg -> msg -> Html.Attribute msg
onSingleOrDoubleClick singleMessage doubleMessage =
    let
        chooseMessage isDoubleClick =
            if isDoubleClick then
                doubleMessage

            else
                singleMessage
    in
    HE.preventDefaultOn "click"
        (withFlag True <| Json.map chooseMessage decodeClicked)


decodeClicked : Json.Decoder Bool
decodeClicked =
    let
        decodeData =
            Json.map ((==) "true") <|
                Json.at [ "currentTarget", "dataset", "clicked" ] Json.string
    in
    Json.oneOf
        [ decodeData
        , Json.succeed False
        ]


{-| Set dummy data to `event.dataTransfer` on dragstart event for cross-browser
support.

<https://github.com/timruffles/ios-html5-drag-drop-shim#cross-browser-differences-in-html5-dragndrop-api>

-}
setDummyDragData : Html.Attribute msg
setDummyDragData =
    HA.attribute "ondragstart" "event.dataTransfer.setData('text', 'dummy');"


decodeDelta : Json.Decoder ( Float, Float )
decodeDelta =
    Json.map2 Tuple.pair
        (Json.field "deltaX" Json.float)
        (Json.field "deltaY" Json.float)


decodeWheelEvent : (( Float, Float ) -> ( Float, Float ) -> msg) -> Json.Decoder msg
decodeWheelEvent tagger =
    Json.map2 tagger decodeDelta decodeRelativePos
