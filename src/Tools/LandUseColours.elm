module Tools.LandUseColours exposing (..)

import Color
import Dict
import FlatColors.AmericanPalette
import FlatColors.FlatUIPalette
import FlatColors.FrenchPalette
import UtilsForViews exposing (colorFromElmUiColour)


landUseColours =
    Dict.fromList
        [ ( "tree", Color.darkGreen )
        , ( "rock", Color.lightBrown )
        , ( "peak", Color.white )
        , ( "water", Color.lightBlue )
        , ( "wood", Color.darkGreen )
        , ( "recreation_ground", Color.rgb255 0x66 0x85 0x3E )
        , ( "grass", Color.rgb255 0x5E 0x9F 0x11 )
        , ( "meadow", colorFromElmUiColour FlatColors.FrenchPalette.auroraGreen )
        , ( "farmland", Color.rgb255 0x98 0xAE 0x35 )
        , ( "grassland", Color.rgb255 0x65 0x95 0x2C )
        , ( "forest", Color.darkGreen )
        , ( "industrial", Color.darkGray )
        , ( "residential", Color.rgb255 0xEC 0xB7 0x7D )
        , ( "retail", colorFromElmUiColour FlatColors.FlatUIPalette.carrot )
        , ( "railway", colorFromElmUiColour FlatColors.FlatUIPalette.silver )
        , ( "brownfield", Color.brown )
        ]
