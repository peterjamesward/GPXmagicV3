module Tools.LandUseColours exposing (..)

import Color
import Dict


landUseColours =
    Dict.fromList
        [ ( "tree", Color.darkGreen )
        , ( "rock", Color.lightBrown )
        , ( "peak", Color.white )
        , ( "water", Color.lightBlue )
        , ( "wood", Color.darkGreen )
        , ( "recreation_ground", Color.rgb255 0x66 0x85 0x3E )
        , ( "grass", Color.rgb255 0x5E 0x9F 0x11 )
        , ( "meadow", Color.rgb255 0xA3 0xAF 0x32 )
        , ( "farmland", Color.rgb255 0x98 0xAE 0x35 )
        , ( "grassland", Color.rgb255 0x65 0x95 0x2C )
        , ( "forest", Color.darkGreen )
        , ( "industrial", Color.darkGray )
        , ( "residential", Color.rgb255 0xC5 0xB6 0xA5 )
        , ( "retail", Color.rgb255 0xCE 0x95 0x79 )
        , ( "railway", Color.rgb255 0x87 0x7A 0x6B )
        , ( "brownfield", Color.brown )
        , ( "quarry", Color.rgb255 0x87 0x82 0x83 )
        ]
