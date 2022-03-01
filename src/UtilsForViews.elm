module UtilsForViews exposing (..)

import Angle
import BoundingBox2d exposing (BoundingBox2d)
import BoundingBox3d exposing (BoundingBox3d)
import Color exposing (rgb)
import Color.Convert exposing (colorToHex)
import Element
import FormatNumber exposing (format)
import FormatNumber.Locales exposing (Decimals(..), usLocale)
import Hex
import Http
import Length
import Speed exposing (Speed)


elmuiColour : Color.Color -> Element.Color
elmuiColour c =
    let
        { red, green, blue, alpha } =
            Color.toRgba c
    in
    Element.rgb red green blue


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


showDecimal6 x =
    let
        locale =
            { usLocale
                | decimals = Exact 6
                , thousandSeparator = ""
                , negativePrefix = "-"
            }
    in
    format locale x


colourHexString : Element.Color -> String
colourHexString colour =
    let
        { red, green, blue, alpha } =
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


fullDepthRenderingBoxSize =
    --TODO: put box side in model
    Length.kilometers 4


flatBox : BoundingBox3d m c -> BoundingBox2d m c
flatBox box =
    let
        { minX, maxX, minY, maxY, minZ, maxZ } =
            BoundingBox3d.extrema box
    in
    BoundingBox2d.fromExtrema { minX = minX, maxX = maxX, minY = minY, maxY = maxY }
