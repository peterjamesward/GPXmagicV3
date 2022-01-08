module UtilsForViews exposing (..)

import Angle
import FormatNumber exposing (format)
import FormatNumber.Locales exposing (Decimals(..), usLocale)
import Length
import Speed exposing (Speed)

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

