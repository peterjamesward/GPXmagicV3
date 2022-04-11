module TipJar exposing (info, tipJar)

import Element exposing (..)
import FeatherIcons
import ViewPureStyles exposing (useIcon, useIconWithSize)


info =
    """## Tip Jar

GPXmagic is provided free for personal use.

There is no tracking, no adverts, no cookies.

If you want to show some support, I wouldn't say no.

PayPal deducts a small handling fee, but there you go.
"""


tipJar =
    row [ padding 10, spaceEvenly, centerX, centerY ]
        [ newTabLink []
            { url = "http://paypal.me/peterjamesward"
            , label =
                row [ spacing 5 ]
                    [ useIconWithSize 24 FeatherIcons.dollarSign
                    ]
            }
        ]
