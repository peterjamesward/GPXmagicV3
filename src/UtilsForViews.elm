module UtilsForViews exposing
    ( colorFromElmUiColour
    , colourHexString
    , elmuiColour
    , flatBox
    , formattedTime
    , fullDepthRenderingBoxSize
    , httpErrorString
    , latitudeString
    , longitudeString
    , noPadding
    , showAngle
    , showDecimal0
    , showDecimal1
    , showDecimal2
    , showLongMeasure
    , showShortMeasure
    , showSpeed
    , uiColourHexString
    , withLeadingZeros
    )

import Angle
import BoundingBox2d exposing (BoundingBox2d)
import BoundingBox3d exposing (BoundingBox3d)
import Color
import Element exposing (Element)
import FormatNumber exposing (format)
import FormatNumber.Locales exposing (Decimals(..), usLocale)
import Hex
import Http
import Length
import Speed exposing (Speed)
import String.Interpolate
import Time


formattedTime : Maybe Time.Posix -> Element msg
formattedTime isTime =
    case isTime of
        Just yesIsTime ->
            let
                hours =
                    Time.toHour Time.utc yesIsTime

                minutes =
                    Time.toMinute Time.utc yesIsTime

                seconds =
                    Time.toSecond Time.utc yesIsTime

                millis =
                    Time.toMillis Time.utc yesIsTime
            in
            Element.text <|
                String.Interpolate.interpolate
                    "{0} : {1} : {2} . {3}"
                    [ String.fromInt hours
                    , withLeadingZeros 2 <| String.fromInt minutes
                    , withLeadingZeros 2 <| String.fromInt seconds
                    , withLeadingZeros 3 <| String.fromInt millis
                    ]

        Nothing ->
            Element.text "- - -"


elmuiColour : Color.Color -> Element.Color
elmuiColour c =
    let
        { red, green, blue } =
            Color.toRgba c
    in
    Element.rgb red green blue


colorFromElmUiColour : Element.Color -> Color.Color
colorFromElmUiColour c =
    let
        { red, green, blue } =
            Element.toRgb c
    in
    Color.fromRgba
        { red = red
        , green = green
        , blue = blue
        , alpha = 1.0
        }


withLeadingZeros : Int -> String -> String
withLeadingZeros beforePoint raw =
    String.repeat (beforePoint - String.length raw) "0"
        ++ raw


httpErrorString : Http.Error -> String
httpErrorString error =
    case error of
        Http.BadBody message ->
            "Unable to handle response: " ++ message

        Http.BadStatus statusCode ->
            "Server error: " ++ String.fromInt statusCode

        Http.BadUrl url ->
            "Invalid URL: " ++ url

        Http.NetworkError ->
            "Network error"

        Http.Timeout ->
            "Request timeout"


showLongMeasure : Bool -> Length.Length -> String
showLongMeasure imperial distance =
    if imperial then
        showDecimal2 (Length.inMiles distance)
            ++ " miles"

    else
        (showDecimal2 <| Length.inMeters distance)
            ++ "m"


showAngle : Angle.Angle -> String
showAngle angle =
    showDecimal0 <| Angle.inDegrees angle


showShortMeasure : Bool -> Length.Length -> String
showShortMeasure imperial distance =
    if imperial then
        showDecimal2 (Length.inFeet distance)
            ++ " feet"

    else
        (showDecimal2 <| Length.inMeters distance)
            ++ "m"


showSpeed : Bool -> Speed -> String
showSpeed imperial speed =
    if imperial then
        showDecimal2 (Speed.inMilesPerHour speed)
            ++ "mph"

    else
        showDecimal2 (Speed.inKilometersPerHour speed)
            ++ "kph"


showDecimal2 x =
    let
        locale =
            { usLocale
                | decimals = Exact 2
                , thousandSeparator = ""
                , negativePrefix = "-"
            }
    in
    format locale x


showDecimal0 x =
    let
        locale =
            { usLocale
                | decimals = Exact 0
                , thousandSeparator = ""
                , negativePrefix = "-"
            }
    in
    format locale x


showDecimal1 x =
    let
        locale =
            { usLocale
                | decimals = Exact 1
                , thousandSeparator = ""
                , negativePrefix = "-"
            }
    in
    format locale x


uiColourHexString : Element.Color -> String
uiColourHexString colour =
    let
        { red, green, blue } =
            Element.toRgb colour

        ( redInt, greenInt, blueInt ) =
            ( floor <| red * 255
            , floor <| green * 255
            , floor <| blue * 255
            )

        leadingZeroes str =
            String.repeat (2 - String.length str) "0" ++ str
    in
    "#"
        ++ (leadingZeroes <| Hex.toString redInt)
        ++ (leadingZeroes <| Hex.toString greenInt)
        ++ (leadingZeroes <| Hex.toString blueInt)


colourHexString : Color.Color -> String
colourHexString colour =
    let
        { red, green, blue } =
            Color.toRgba colour

        ( redInt, greenInt, blueInt ) =
            ( floor <| red * 255
            , floor <| green * 255
            , floor <| blue * 255
            )

        leadingZeroes str =
            String.repeat (2 - String.length str) "0" ++ str
    in
    "#"
        ++ (leadingZeroes <| Hex.toString redInt)
        ++ (leadingZeroes <| Hex.toString greenInt)
        ++ (leadingZeroes <| Hex.toString blueInt)


fullDepthRenderingBoxSize =
    Length.kilometers 4


flatBox : BoundingBox3d m c -> BoundingBox2d m c
flatBox box =
    let
        { minX, maxX, minY, maxY } =
            BoundingBox3d.extrema box
    in
    BoundingBox2d.fromExtrema { minX = minX, maxX = maxX, minY = minY, maxY = maxY }


noPadding =
    { left = 0, right = 0, top = 0, bottom = 0 }


longitudeString angle =
    let
        { sign, degrees, minutes, seconds } =
            Angle.toDms angle

        signString =
            case sign of
                Angle.Positive ->
                    "E"

                Angle.Negative ->
                    "W"
    in
    String.concat
        [ signString
        , " "
        , String.fromInt degrees
        , "° "
        , String.fromInt minutes
        , "′ "
        , String.fromInt <| round seconds
        , "″"
        ]


latitudeString angle =
    let
        { sign, degrees, minutes, seconds } =
            Angle.toDms angle

        signString =
            case sign of
                Angle.Positive ->
                    "N"

                Angle.Negative ->
                    "S"
    in
    String.concat
        [ signString
        , " "
        , String.fromInt degrees
        , "° "
        , String.fromInt minutes
        , "′ "
        , String.fromInt <| round seconds
        , "″"
        ]
