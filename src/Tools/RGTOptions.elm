module Tools.RGTOptions exposing (Msg(..), Options, defaults, update, view)

{-
   <extensions>
   <rgt:parserOptions>
   <rgt:disableElevationFixes/>
   <rgt:disableAdvancedSmoothing/>
   <rgt:maxSlope>20</rgt:maxSlope>
   </rgt:parserOptions>
   </extensions>
-}

import Element exposing (Element, centerX, column, el, padding, paragraph, spacing, text)
import Element.Background as Background
import Element.Input as Input
import FlatColors.ChinesePalette
import String.Interpolate
import Tools.I18N as I18N
import Tools.I18NOptions as I18N
import ViewPureStyles exposing (commonShortHorizontalSliderStyles)


type alias Options =
    { disableElevationFixes : Bool
    , disableAdvancedSmoothing : Bool
    , maxSlope : Float
    }


type Msg
    = SetDisableElevationFixes Bool
    | SetDisableAdvancedSmoothing Bool
    | SetMaxSlope Float


defaults : Options
defaults =
    { disableElevationFixes = False
    , disableAdvancedSmoothing = False
    , maxSlope = 20.0
    }


update : Msg -> Options -> Options
update msg options =
    case msg of
        SetDisableElevationFixes newSetting ->
            { options | disableElevationFixes = newSetting }

        SetDisableAdvancedSmoothing newSetting ->
            { options | disableAdvancedSmoothing = newSetting }

        SetMaxSlope newSetting ->
            { options | maxSlope = newSetting }


view : I18N.Location -> Options -> (Msg -> msg) -> Element msg
view location options wrap =
    let
        i18n =
            I18N.text location "rgtOptions"

        elevation =
            Input.checkbox
                [ padding 5
                , spacing 5
                ]
                { onChange = wrap << SetDisableElevationFixes
                , checked = options.disableElevationFixes
                , label = Input.labelRight [] <| i18n "elevation"
                , icon = Input.defaultCheckbox
                }

        smoothing =
            Input.checkbox
                [ padding 5
                , spacing 5
                ]
                { onChange = wrap << SetDisableAdvancedSmoothing
                , checked = options.disableAdvancedSmoothing
                , label = Input.labelRight [] <| i18n "smoothing"
                , icon = Input.defaultCheckbox
                }

        maxSlope =
            Input.slider commonShortHorizontalSliderStyles
                { onChange = wrap << SetMaxSlope
                , label =
                    Input.labelBelow [] <|
                        text <|
                            String.Interpolate.interpolate
                                (I18N.localisedString location "rgtOptions" "maxSlope")
                                [ String.fromFloat options.maxSlope ]
                , min = 20.0
                , max = 30.0
                , step = Just 1.0
                , value = options.maxSlope
                , thumb = Input.defaultThumb
                }
    in
    column
        [ spacing 5
        , padding 5
        , Background.color FlatColors.ChinesePalette.antiFlashWhite
        ]
        [ paragraph [] [ i18n "info" ]
        , elevation
        , smoothing
        , el [ centerX ] maxSlope
        ]
