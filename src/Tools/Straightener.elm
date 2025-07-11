module Tools.Straightener exposing
    ( Msg(..)
    , applyUsingOptions
    , defaultOptions
    , toolId
    , toolStateChange
    , update
    , view
    )

import Actions exposing (ToolAction(..))
import CommonToolStyles
import DomainModel exposing (EarthPoint, GPXSource, PeteTree, RoadSection, skipCount)
import Element exposing (..)
import Element.Input as Input exposing (button)
import Length
import LineSegment3d
import Point3d exposing (zCoordinate)
import PreviewData exposing (PreviewPoint, PreviewShape(..))
import Quantity
import SystemSettings exposing (SystemSettings)
import Tools.StraightenerOptions exposing (..)
import Tools.I18N as I18N
import TrackLoaded exposing (TrackLoaded, adjustAltitude)
import ViewPureStyles exposing ( neatToolsBorder)

toolId =
    "straighten"


defaultOptions : Options
defaultOptions =
    { preserveAltitude = True }


type Msg
    = SetPreserveAltitude Bool
    | ApplyWithOptions


computeNewPoints : Options -> TrackLoaded msg -> List PreviewPoint
computeNewPoints options track =
    let
        ( fromStart, fromEnd ) =
            case track.markerPosition of
                Just _ ->
                    TrackLoaded.getRangeFromMarkers track

                Nothing ->
                    ( 0, 0 )

        earthPoints =
            straighten options fromStart fromEnd track
    in
    TrackLoaded.asPreviewPoints track earthPoints


straighten : Options -> Int -> Int -> TrackLoaded msg -> List ( EarthPoint )
straighten options fromStart fromEnd track =
    let
        endIndex = skipCount track.trackTree - fromEnd

        ( startPoint, endPoint ) =
            ( DomainModel.earthPointFromIndex fromStart track.trackTree
            , DomainModel.earthPointFromIndex endIndex track.trackTree
            )

        ( startDistance, endDistance ) =
            ( DomainModel.distanceFromIndex fromStart track.trackTree
            , DomainModel.distanceFromIndex endIndex track.trackTree
            )

        trackDistance =
            endDistance |> Quantity.minus startDistance

        idealLine =
            LineSegment3d.from startPoint.space endPoint.space

        applyAdjustment :
            RoadSection
            -> ( Length.Length, List EarthPoint )
            -> ( Length.Length, List EarthPoint )
        applyAdjustment road ( distance, outputs ) =
            let
                proportionOfTrackDistance =
                    Quantity.ratio distance trackDistance

                interpolatedPoint =
                    { space = LineSegment3d.interpolate idealLine proportionOfTrackDistance
                    , time = road.startPoint.time
                    }

                newPoint =
                    if options.preserveAltitude then
                        adjustAltitude (zCoordinate road.startPoint.space) interpolatedPoint

                    else
                        interpolatedPoint
            in
            ( distance |> Quantity.plus road.trueLength
            , newPoint :: outputs
            )

        ( _, adjustedPoints ) =
            DomainModel.traverseTreeBetweenLimitsToDepth
                fromStart
                endIndex
                (always Nothing)
                0
                track.trackTree
                applyAdjustment
                ( Quantity.zero, [] )
    in
    List.reverse    adjustedPoints


applyUsingOptions :
    Options
    -> TrackLoaded msg
    -> TrackLoaded msg
applyUsingOptions options track =
    let
        ( fromStart, fromEnd ) =
            if track.markerPosition /= Nothing then
                TrackLoaded.getRangeFromMarkers track

            else
                ( 0, 0 )

        newTree =
            DomainModel.replaceRange
                (fromStart + 1)
                (fromEnd + 1)
                track.referenceLonLat
                (List.map .gpx <| computeNewPoints options track)
                track.trackTree
    in
    case newTree of
        Just isTree ->
            let
                pointerReposition =
                    identity

                ( newOrange, newPurple ) =
                    ( pointerReposition track.currentPosition
                    , Maybe.map pointerReposition track.markerPosition
                    )
            in
            { track
                | trackTree = isTree
                , currentPosition = newOrange
                , markerPosition = newPurple
                , leafIndex = TrackLoaded.indexLeaves isTree
            }

        Nothing ->
            track


toolStateChange :
    Bool
    -> Element.Color
    -> Options
    -> Maybe (TrackLoaded msg)
    -> ( Options, List (ToolAction msg) )
toolStateChange opened colour options track =
    case ( opened, track ) of
        ( True, Just theTrack ) ->
            ( options, actions options colour theTrack )

        _ ->
            -- Hide preview
            ( options, [ HidePreview "straight", HidePreview "straightProfile" ] )


actions newOptions previewColour track =
    let
        previewTree =
            .trackTree <|
                applyUsingOptions newOptions track

        profilePreview =
            ShowPreview
                { tag = "straightprofile"
                , shape = PreviewProfile previewTree
                , colour = previewColour
                , points = []
                }

        normalPreview =
            ShowPreview
                { tag = "straight"
                , shape = PreviewCircle
                , colour = previewColour
                , points = computeNewPoints newOptions track
                }
    in
    [ normalPreview, profilePreview ]


update :
    Msg
    -> Options
    -> Element.Color
    -> Maybe (TrackLoaded msg)
    -> ( Options, List (ToolAction msg) )
update msg options previewColour hasTrack =
    case ( hasTrack, msg ) of
        ( Just track, SetPreserveAltitude setting ) ->
            let
                newOptions =
                    { options | preserveAltitude = setting }
            in
            ( newOptions, actions newOptions previewColour track )


        ( Just track, ApplyWithOptions ) ->
            ( options
            , [ WithUndo (Actions.Straighten options)
              , Actions.Straighten options
              , TrackHasChanged
              ]
            )

        _ ->
            ( options, [] )


view :
    SystemSettings
    -> (Msg -> msg)
    -> Options
    -> TrackLoaded msg
    -> Element msg
view settings wrap options track =
    let
        i18n =
            I18N.text settings.location toolId


        modeChoices =
            row []
                [ Input.checkbox
                    [ padding 10
                    , spacing 5
                    ]
                    { onChange = wrap << SetPreserveAltitude
                    , checked = options.preserveAltitude
                    , label = Input.labelRight [] <| i18n "altitudes"
                    , icon = Input.defaultCheckbox
                    }
                ]

        actionButton =
            el [ centerX, width fill, spacing 5 ] <|
                button (width fill :: neatToolsBorder)
                    { onPress = Just <| wrap ApplyWithOptions
                    , label = paragraph [] [ i18n "Apply" ]
                    }

    in
    column (CommonToolStyles.toolContentBoxStyle settings)
        [ el [ centerX ] modeChoices
        , el [ centerX ] actionButton
        ]


type alias FoldState =
    { roadMinusOne : Maybe RoadSection
    , newPoints : List EarthPoint
    }

