module Tools.Tracks exposing
    ( Msg(..)
    , addSelfLoop
    , addTrack
    , addTraversal
    , boundingBox
    , defaultOptions
    , getActiveTrack
    , getKeyPlaces
    , loopCanBeAdded
    , mapOverInvisibleTracks
    , mapOverVisibleTracks
    , renameActiveTrack
    , setTrack
    , toolId
    , toolStateChange
    , traversalCanBeAdded
    , unloadActiveTrack
    , update
    , updateActiveTrack
    , view
    )

import Actions
import Angle
import Arc2d exposing (Arc2d)
import Arc3d exposing (Arc3d)
import Axis3d
import BoundingBox3d
import Color
import CommonToolStyles
import Dict
import Direction2d
import Direction3d
import DomainModel exposing (EarthPoint, GPXSource, PeteTree, trueLength)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import FeatherIcons
import FlatColors.AmericanPalette
import FlatColors.FlatUIPalette
import Geometry101
import Length exposing (Meters, inMeters, meters)
import LineSegment2d
import LineSegment3d
import List.Extra
import LocalCoords exposing (LocalCoords)
import Pixels
import Point2d
import Point3d
import Polyline3d
import PreviewData exposing (PreviewShape(..))
import Quantity exposing (Quantity)
import Scene3d exposing (Entity)
import Scene3d.Material as Material
import Set
import SketchPlane3d
import String.Interpolate
import SystemSettings exposing (SystemSettings)
import ToolTip exposing (localisedTooltip, tooltip)
import Tools.Graph as Graph
import Tools.GraphOptions as Graph exposing (Cluster, Graph)
import Tools.I18N as I18N
import Tools.Nudge
import Tools.NudgeOptions
import Tools.TracksOptions as Options exposing (Direction(..), GraphState(..), Options, OptionsUndo(..), Traversal, TraversalDisplay)
import TrackLoaded exposing (TrackLoaded)
import UtilsForViews exposing (showDecimal2, showLongMeasure, showShortMeasure)
import Vector2d
import Vector3d
import ViewPureStyles exposing (commonShortHorizontalSliderStyles, infoButton, neatToolsBorder, useIcon, useIconWithSize)


toolId =
    "tracks"


type Msg
    = SelectActiveTrack String
    | ToggleVisibility String
    | UnloadActiveTrack
    | GraphAnalyse
    | CentreLineOffset (Quantity Float Meters)
    | MinimumRadius (Quantity Float Meters)
    | ConvertFromGraph
    | HighlightTraversal Int
    | RemoveLastTraversal
    | DisplayInfo String String
    | FlipDirection Int
    | ClearRoute
    | Undo
    | SetTolerance (Quantity Float Meters)
    | SnapToNearby
    | Canonicalise
    | ToggleRoadList


defaultOptions : Options msg
defaultOptions =
    { nextTrackNumber = 1
    , activeTrackName = Nothing
    , commonReferenceGPX = Nothing
    , graph = emptyGraph
    , graphState = GraphNoTracks
    , roadListCollapsed = False
    , matchingTolerance = Length.meters 1.5
    , centreLineOffset = Length.meters 0.0
    , minimumRadiusAtPlaces = Length.meters 3.0
    , selectedTraversal = 0
    , clustersForPreview = []
    , userRoute = []
    , priors = []
    }


emptyGraph : Graph.Graph msg
emptyGraph =
    { nodes = Dict.empty
    , edges = Dict.empty
    , referenceLonLat =
        { latitude = Angle.degrees 0
        , longitude = Direction2d.positiveX
        , altitude = Quantity.zero
        , timestamp = Nothing
        }
    }


toolStateChange :
    Bool
    -> Options msg
    -> ( Options msg, List (Actions.ToolAction msg) )
toolStateChange opened options =
    if opened then
        case options.graphState of
            GraphOriginalTracks ->
                update (SetTolerance options.matchingTolerance) options

            _ ->
                ( options, [] )

    else
        -- Hide preview
        ( options, [ Actions.HidePreview "graph" ] )


update : Msg -> Options msg -> ( Options msg, List (Actions.ToolAction msg) )
update msg options =
    case msg of
        SelectActiveTrack name ->
            case Dict.get name options.graph.edges of
                Just found ->
                    let
                        track =
                            found.track
                    in
                    ( { options
                        | graph =
                            Graph.updatedEdge
                                track
                                { track | visible = True }
                                options.graph
                      }
                    , [ Actions.SetActiveTrack found.track.trackName ]
                    )

                Nothing ->
                    ( options, [] )

        ToggleVisibility name ->
            case Dict.get name options.graph.edges of
                Just found ->
                    let
                        track =
                            found.track
                    in
                    ( { options
                        | graph =
                            Graph.updatedEdge
                                track
                                { track | visible = not track.visible }
                                options.graph
                      }
                    , [ Actions.SetActiveTrack found.track.trackName ]
                    )

                Nothing ->
                    ( options
                    , [ Actions.HidePreview "graph" ]
                    )

        UnloadActiveTrack ->
            case options.activeTrackName of
                Just active ->
                    ( options, [ Actions.UnloadActiveTrack active ] )

                Nothing ->
                    ( options, [ Actions.HidePreview "graph" ] )

        SnapToNearby ->
            -- All tracks update as the snap to the clusters we have found.
            -- We delegate to graph, then pull back the updated tracks.
            let
                newGraph =
                    Graph.snapToClusters
                        options.matchingTolerance
                        options.graph

                newTracks =
                    Dict.values newGraph.edges
                        |> List.map .track
            in
            ( { options
                | graph = newGraph
                , graphState = GraphSnapped
                , priors = OptionsUndo options :: options.priors
              }
            , [ Actions.HidePreview "graph" ]
            )

        GraphAnalyse ->
            -- Note that the state rules here mean that the tracks have been "snapped".
            -- We may safely proceed with neighbour counting to find nodes and edges.
            -- The found edges become the new tracks.
            -- We delegate to graph, then pull back the updated tracks.
            -- Save the previous state for simple reversion.
            case options.graphState of
                GraphSnapped ->
                    let
                        newGraph =
                            Graph.analyzeTracksAsGraph options.graph
                    in
                    ( { options
                        | graph = newGraph
                        , graphState = GraphWithNodes
                        , userRoute = []
                        , roadListCollapsed = False
                        , priors = OptionsUndo options :: options.priors
                      }
                    , [ Actions.HidePreview "graph" ]
                    )

                _ ->
                    ( options, [] )

        Undo ->
            case options.priors of
                (OptionsUndo prior) :: _ ->
                    ( prior
                    , [ Actions.TrackHasChanged ]
                    )

                _ ->
                    ( options, [] )

        HighlightTraversal traversal ->
            ( { options | selectedTraversal = traversal }, [] )

        RemoveLastTraversal ->
            let
                newRoute =
                    List.take (List.length options.userRoute - 1) options.userRoute
            in
            ( { options
                | userRoute = newRoute
                , selectedTraversal = List.length newRoute - 1
              }
            , []
            )

        FlipDirection i ->
            let
                newOptions =
                    { options
                        | userRoute =
                            options.userRoute
                                |> List.Extra.updateAt i
                                    (\t ->
                                        { t
                                            | direction =
                                                case t.direction of
                                                    Natural ->
                                                        Reverse

                                                    Reverse ->
                                                        Natural
                                        }
                                    )
                    }
            in
            ( { newOptions
                | selectedTraversal = min options.selectedTraversal (List.length newOptions.userRoute - 1)
              }
            , []
            )

        SetTolerance tolerance ->
            lookForClusters options tolerance

        CentreLineOffset float ->
            ( { options | centreLineOffset = float }, [] )

        MinimumRadius float ->
            ( { options | minimumRadiusAtPlaces = float }, [] )

        ConvertFromGraph ->
            ( makeNewRoute options.userRoute options
            , [ Actions.RemoveAllFromMap <| Dict.keys options.graph.edges
              , Actions.TrackHasChanged
              ]
            )

        DisplayInfo tool tag ->
            ( options, [ Actions.DisplayInfo tool tag ] )

        Canonicalise ->
            case options.graphState of
                GraphWithNodes ->
                    let
                        newGraph =
                            Graph.canonicalise options.graph
                    in
                    ( { options
                        | graph = newGraph
                        , graphState = GraphWithEdges
                        , activeTrackName = Just "Road 1"
                        , priors = OptionsUndo options :: options.priors
                      }
                    , [ Actions.RemoveAllFromMap <| Dict.keys options.graph.edges
                      , Actions.TrackHasChanged
                      ]
                    )

                _ ->
                    ( { options | graphState = GraphOriginalTracks }, [] )

        ToggleRoadList ->
            ( { options | roadListCollapsed = not options.roadListCollapsed }, [] )

        ClearRoute ->
            ( { options
                | userRoute = []
                , selectedTraversal = -1
              }
            , []
            )


addTraversal : String -> Options msg -> Options msg
addTraversal newEdge options =
    case
        ( List.Extra.last options.userRoute
        , Dict.get newEdge options.graph.edges
        )
    of
        ( Just traversal, Just addedEdgeInfo ) ->
            case Dict.get traversal.edge options.graph.edges of
                Just lastEdgeInfo ->
                    let
                        newEdgeDirection =
                            -- Special case if added section is a loop.
                            if addedEdgeInfo.lowNode == addedEdgeInfo.highNode then
                                Natural

                            else
                                let
                                    finalNode =
                                        if traversal.direction == Natural then
                                            lastEdgeInfo.highNode

                                        else
                                            lastEdgeInfo.lowNode
                                in
                                if finalNode == addedEdgeInfo.lowNode then
                                    Natural

                                else
                                    Reverse
                    in
                    { options
                        | userRoute =
                            options.userRoute
                                ++ [ { edge = newEdge, direction = newEdgeDirection } ]
                        , selectedTraversal = List.length options.userRoute
                    }

                Nothing ->
                    options

        ( Nothing, Just _ ) ->
            { options
                | userRoute =
                    options.userRoute
                        ++ [ { edge = newEdge, direction = Natural } ]
            }

        _ ->
            options


renameActiveTrack : String -> Options msg -> Options msg
renameActiveTrack newName options =
    -- Prevent inadvertant duplication
    if Dict.member newName options.graph.edges then
        options

    else
        case getActiveTrack options of
            Just oldTrack ->
                let
                    renameEdge traversal =
                        if traversal.edge == oldTrack.trackName then
                            { edge = newName, direction = traversal.direction }

                        else
                            traversal

                    newGraph =
                        Graph.renameEdge oldTrack.trackName newName options.graph
                in
                { options
                    | graph = newGraph
                    , activeTrackName = Just newName
                    , userRoute = List.map renameEdge options.userRoute
                }

            Nothing ->
                options


updateActiveTrack : TrackLoaded msg -> TrackLoaded msg -> Options msg -> ( TrackLoaded msg, Options msg )
updateActiveTrack oldTrack newTrack options =
    -- Do not use for rename.
    case options.activeTrackName of
        Just index ->
            ( newTrack
            , { options
                | graph = Graph.updatedEdge oldTrack newTrack options.graph
              }
            )

        Nothing ->
            ( newTrack, options )


view : SystemSettings -> (Msg -> msg) -> Options msg -> Element msg
view settings wrapper options =
    let
        helper =
            I18N.text settings.location toolId

        listOfTracks =
            if options.roadListCollapsed then
                none

            else
                column [ spacing 5 ] <|
                    List.map
                        (\entry ->
                            displayTrackInfo entry wrapper options
                        )
                        (List.map .track <| Dict.values options.graph.edges)

        collapseExpandButton =
            if Dict.size options.graph.edges > 1 then
                el [ centerX, width fill, paddingXY 20 0 ] <|
                    Input.button
                        [ Border.width 1
                        , Border.rounded 6
                        , Border.color FlatColors.FlatUIPalette.concrete
                        , width fill
                        ]
                        { label =
                            useIcon <|
                                if options.roadListCollapsed then
                                    FeatherIcons.chevronDown

                                else
                                    FeatherIcons.chevronUp
                        , onPress = Just <| wrapper ToggleRoadList
                        }

            else
                none
    in
    el (CommonToolStyles.toolContentBoxStyle settings) <|
        column [ spacing 5 ]
            [ listOfTracks
            , collapseExpandButton
            , viewGraph settings wrapper options options.graph
            ]


viewGraph :
    SystemSettings
    -> (Msg -> msg)
    -> Options msg
    -> Graph.Graph msg
    -> Element msg
viewGraph settings wrapper options graph =
    let
        i18n =
            I18N.text settings.location toolId

        guidanceText =
            row
                [ Background.color FlatColors.FlatUIPalette.turquoise
                , Font.color (CommonToolStyles.themeForeground settings.colourTheme)
                , Border.rounded 5
                ]
                [ useIconWithSize 20 FeatherIcons.info
                , paragraph [ padding 4 ]
                    [ case options.graphState of
                        GraphNoTracks ->
                            i18n "graphNone"

                        GraphOriginalTracks ->
                            i18n "graphOriginal"

                        GraphSnapped ->
                            i18n "graphSnapped"

                        GraphWithNodes ->
                            i18n "graphAnalyzed"

                        GraphWithEdges ->
                            i18n "graphConverted"
                    ]
                ]

        helper =
            I18N.text settings.location toolId

        unloadButton =
            el [ centerX, width fill ] <|
                if options.activeTrackName /= Nothing then
                    Input.button
                        neatToolsBorder
                        { label = helper "unload"
                        , onPress = Just <| wrapper UnloadActiveTrack
                        }

                else
                    none
    in
    column
        (CommonToolStyles.toolContentBoxStyle settings)
        [ guidanceText
        , case options.graphState of
            GraphNoTracks ->
                none

            GraphOriginalTracks ->
                let
                    snapToNearbyButton =
                        row [ spacing 3, width fill ]
                            [ infoButton (wrapper <| DisplayInfo toolId "adoptInfo")
                            , Input.button neatToolsBorder
                                { onPress = Just (wrapper SnapToNearby)
                                , label = i18n "adopt"
                                }
                            ]

                    toleranceSlider =
                        row [ spacing 5 ]
                            [ none
                            , infoButton (wrapper <| DisplayInfo toolId "tolerance")
                            , Input.slider
                                commonShortHorizontalSliderStyles
                                { onChange = wrapper << SetTolerance << Length.meters
                                , label =
                                    Input.labelBelow [] <|
                                        text <|
                                            String.Interpolate.interpolate
                                                (I18N.localisedString settings.location toolId "isTolerance")
                                                [ showShortMeasure settings.imperial options.matchingTolerance ]
                                , min = 0.5
                                , max = 5.0
                                , step = Just 0.1
                                , value = Length.inMeters options.matchingTolerance
                                , thumb = Input.defaultThumb
                                }
                            ]

                    revertButton =
                        if List.length options.priors > 0 then
                            Input.button neatToolsBorder
                                { onPress = Just (wrapper Undo)
                                , label = i18n "undoNewRoute"
                                }

                        else
                            none
                in
                column [ centerX, width fill, spacing 10 ]
                    [ unloadButton
                    , toleranceSlider
                    , snapToNearbyButton
                    , revertButton
                    ]

            GraphSnapped ->
                let
                    analyseButton =
                        row [ spacing 3, width fill ]
                            [ infoButton (wrapper <| DisplayInfo toolId "info")
                            , Input.button neatToolsBorder
                                { onPress = Just (wrapper GraphAnalyse)
                                , label = i18n "find"
                                }
                            ]

                    undoButton =
                        row [ spacing 3 ]
                            [ Input.button
                                neatToolsBorder
                                { onPress = Just <| wrapper Undo
                                , label = i18n "undoSnap"
                                }
                            ]
                in
                column [ centerX, width fill, spacing 10 ]
                    [ undoButton
                    , analyseButton
                    ]

            GraphWithNodes ->
                let
                    revertButton =
                        Input.button neatToolsBorder
                            { onPress = Just (wrapper Undo)
                            , label = i18n "undoAnalyze"
                            }

                    convertButton =
                        Input.button neatToolsBorder
                            { onPress = Just (wrapper Canonicalise)
                            , label = i18n "canonicalise"
                            }
                in
                column [ width fill, padding 4, spacing 10 ]
                    [ revertButton
                    , convertButton
                    ]

            GraphWithEdges ->
                let
                    offset =
                        Length.inMeters options.centreLineOffset

                    radius =
                        Length.inMeters options.minimumRadiusAtPlaces

                    revertButton =
                        Input.button neatToolsBorder
                            { onPress = Just (wrapper Undo)
                            , label = i18n "undoCanonicalise"
                            }

                    clearRouteButton =
                        Input.button neatToolsBorder
                            { onPress = Just (wrapper ClearRoute)
                            , label = i18n "clear"
                            }

                    finishButton =
                        if not <| List.isEmpty options.userRoute then
                            row [ spacing 3 ]
                                [ infoButton (wrapper <| DisplayInfo toolId "render")
                                , Input.button
                                    neatToolsBorder
                                    { onPress = Just (wrapper ConvertFromGraph)
                                    , label = i18n "convert"
                                    }
                                ]

                        else
                            none

                    offsetSlider =
                        row [ spacing 5 ]
                            [ none
                            , infoButton (wrapper <| DisplayInfo toolId "offset")
                            , Input.slider
                                commonShortHorizontalSliderStyles
                                { onChange = wrapper << CentreLineOffset << Length.meters
                                , label =
                                    Input.labelBelow [] <|
                                        text <|
                                            String.Interpolate.interpolate
                                                (I18N.localisedString settings.location toolId "isOffset")
                                                [ showDecimal2 <| abs offset
                                                , if offset < 0.0 then
                                                    I18N.localisedString settings.location toolId "left"

                                                  else if offset > 0.0 then
                                                    I18N.localisedString settings.location toolId "right"

                                                  else
                                                    ""
                                                ]
                                , min = -5.0
                                , max = 5.0
                                , step = Just 0.25
                                , value = offset
                                , thumb = Input.defaultThumb
                                }
                            ]

                    minRadiusSlider =
                        row [ spacing 5 ]
                            [ none
                            , infoButton (wrapper <| DisplayInfo toolId "radius")
                            , Input.slider
                                commonShortHorizontalSliderStyles
                                { onChange = wrapper << MinimumRadius << Length.meters
                                , label =
                                    Input.labelBelow [] <|
                                        text <|
                                            String.Interpolate.interpolate
                                                (I18N.localisedString settings.location toolId "isRadius")
                                                [ showDecimal2 <| abs radius ]
                                , min = 1.0
                                , max = 15.0
                                , step = Just 1.0
                                , value = radius
                                , thumb = Input.defaultThumb
                                }
                            ]

                    traversalNext =
                        Input.button neatToolsBorder
                            { onPress =
                                Just <|
                                    wrapper <|
                                        HighlightTraversal <|
                                            min
                                                (List.length options.userRoute - 1)
                                                (options.selectedTraversal + 1)
                            , label = useIconWithSize 16 FeatherIcons.chevronRight
                            }

                    traversalPrevious =
                        Input.button neatToolsBorder
                            { onPress =
                                Just <|
                                    wrapper <|
                                        HighlightTraversal <|
                                            max
                                                0
                                                (options.selectedTraversal - 1)
                            , label = useIconWithSize 16 FeatherIcons.chevronLeft
                            }

                    traversals : List TraversalDisplay
                    traversals =
                        -- Display-ready version of the route.
                        options.userRoute
                            |> List.map
                                (\traversal ->
                                    case Dict.get traversal.edge graph.edges of
                                        Nothing ->
                                            { startPlace = ""
                                            , road = ""
                                            , endPlace = ""
                                            , length = Quantity.zero
                                            }

                                        Just edgeInfo ->
                                            case traversal.direction of
                                                Natural ->
                                                    { startPlace = edgeInfo.lowNode
                                                    , road = traversal.edge
                                                    , endPlace = edgeInfo.highNode
                                                    , length = trueLength edgeInfo.track.trackTree
                                                    }

                                                Reverse ->
                                                    { startPlace = edgeInfo.highNode
                                                    , road = traversal.edge
                                                    , endPlace = edgeInfo.lowNode
                                                    , length = trueLength edgeInfo.track.trackTree
                                                    }
                                )

                    dataStyles selected =
                        if selected then
                            [ Font.bold, padding 2 ]

                        else
                            [ padding 2 ]

                    traversalsTable : Element msg
                    traversalsTable =
                        let
                            totalLength =
                                traversals |> List.map .length |> Quantity.sum

                            headerAttrs =
                                [ Font.bold
                                , Border.widthEach { bottom = 2, top = 0, left = 0, right = 0 }
                                , Border.color FlatColors.FlatUIPalette.concrete
                                ]

                            footerAttrs =
                                [ Font.bold
                                , Border.widthEach { bottom = 0, top = 2, left = 0, right = 0 }
                                , Border.color FlatColors.FlatUIPalette.concrete
                                ]
                        in
                        column
                            [ width <| maximum 500 fill
                            , height <| px 300
                            , spacing 10
                            , padding 5
                            , Border.width 2
                            , Border.rounded 6
                            , Border.color FlatColors.FlatUIPalette.concrete
                            ]
                            [ row [ width fill ]
                                [ el ((width <| fillPortion 1) :: headerAttrs) <| i18n "blank"
                                , el ((width <| fillPortion 2) :: headerAttrs) <| i18n "from"
                                , el ((width <| fillPortion 2) :: headerAttrs) <| i18n "to"
                                , el ((width <| fillPortion 2) :: headerAttrs) <| i18n "along"
                                , el ((width <| fillPortion 2) :: headerAttrs) <| i18n "distance"
                                ]

                            -- workaround for a bug: it's necessary to wrap `table` in an `el`
                            -- to get table height attribute to apply
                            , el [ width fill ] <|
                                indexedTable
                                    [ width fill
                                    , height <| px 220
                                    , scrollbarY
                                    , spacing 4
                                    ]
                                    { data = traversals
                                    , columns =
                                        [ { header = none
                                          , width = fillPortion 1
                                          , view =
                                                \i t ->
                                                    row
                                                        [ spacing 2 ]
                                                        [ if i + 1 == List.length traversals then
                                                            Input.button
                                                                [ alignRight
                                                                , tooltip below (localisedTooltip settings.location toolId "remove")
                                                                ]
                                                                { onPress = Just <| wrapper RemoveLastTraversal
                                                                , label = useIcon FeatherIcons.delete
                                                                }

                                                          else
                                                            none
                                                        , if
                                                            List.length traversals
                                                                == 1
                                                                || t.startPlace
                                                                == t.endPlace
                                                          then
                                                            Input.button
                                                                [ alignRight
                                                                , tooltip below (localisedTooltip settings.location toolId "reverse")
                                                                ]
                                                                { onPress = Just <| wrapper <| FlipDirection i
                                                                , label = useIcon FeatherIcons.refreshCw
                                                                }

                                                          else
                                                            none
                                                        , Input.button
                                                            [ alignRight ]
                                                            { onPress = Just <| wrapper <| HighlightTraversal i
                                                            , label = useIcon FeatherIcons.eye
                                                            }
                                                        ]
                                          }
                                        , { header = none
                                          , width = fillPortion 2
                                          , view =
                                                \i t ->
                                                    el (dataStyles (i == options.selectedTraversal)) <|
                                                        text t.startPlace
                                          }
                                        , { header = none
                                          , width = fillPortion 2
                                          , view =
                                                \i t ->
                                                    el (dataStyles (i == options.selectedTraversal)) <|
                                                        text t.endPlace
                                          }
                                        , { header = none
                                          , width = fillPortion 2
                                          , view =
                                                \i t ->
                                                    el (dataStyles (i == options.selectedTraversal)) <|
                                                        text t.road
                                          }
                                        , { header = none
                                          , width = fillPortion 2
                                          , view =
                                                \i t ->
                                                    el (dataStyles (i == options.selectedTraversal)) <|
                                                        text <|
                                                            showLongMeasure False t.length
                                          }
                                        ]
                                    }
                            , row [ width fill ]
                                [ el ((width <| fillPortion 1) :: footerAttrs) <| text " "
                                , el ((width <| fillPortion 2) :: footerAttrs) <| text " "
                                , el ((width <| fillPortion 2) :: footerAttrs) <| text " "
                                , el ((width <| fillPortion 2) :: footerAttrs) <| text " "
                                , el ((width <| fillPortion 2) :: footerAttrs) <|
                                    text <|
                                        showLongMeasure False totalLength
                                ]
                            ]
                in
                column [ width fill, padding 4, spacing 10 ]
                    [ revertButton
                    , row [ centerX, width fill, spacing 10 ]
                        [ traversalPrevious
                        , traversalNext
                        , clearRouteButton
                        ]
                    , traversalsTable
                    , offsetSlider
                    , minRadiusSlider
                    , finishButton
                    ]
        ]


displayTrackInfo : TrackLoaded msg -> (Msg -> msg) -> Options msg -> Element msg
displayTrackInfo track wrapper options =
    row [ spacing 5 ]
        [ Input.button
            []
            { label = useIcon FeatherIcons.edit
            , onPress = Just <| wrapper (SelectActiveTrack track.trackName)
            }
        , if Just track.trackName == options.activeTrackName then
            -- Can't change visibility of active track.
            none

          else
            Input.button []
                { label =
                    useIcon <|
                        if track.visible then
                            FeatherIcons.eyeOff

                        else
                            FeatherIcons.eye
                , onPress = Just <| wrapper (ToggleVisibility track.trackName)
                }
        , text track.trackName
        ]


addTrack : TrackLoaded msg -> Options msg -> Options msg
addTrack track options =
    --If this is not the first track, we must adjust its reference point.
    --That may be inefficient but we can absorb the cost at load time.
    --If not, we (I) will have to change it. POITROAE.
    let
        unambiguousName =
            case Dict.get track.trackName options.graph.edges of
                Just _ ->
                    track.trackName ++ "-" ++ String.fromInt options.nextTrackNumber

                Nothing ->
                    track.trackName

        trackWithCommonReference =
            case options.commonReferenceGPX of
                Just commonReference ->
                    TrackLoaded.changeReferencePoint commonReference track

                Nothing ->
                    track

        trackWithUnambiguousName =
            { trackWithCommonReference | trackName = unambiguousName }

        newReferenceGPX =
            case options.commonReferenceGPX of
                Just common ->
                    Just common

                Nothing ->
                    Just <| TrackLoaded.getReferencePoint track
    in
    { options
        | nextTrackNumber = options.nextTrackNumber + 1
        , activeTrackName = Just track.trackName
        , commonReferenceGPX = newReferenceGPX
        , graph = Graph.addEdgeFromTrack trackWithUnambiguousName options.graph
        , graphState = GraphOriginalTracks
    }


setTrack : String -> Options msg -> ( Maybe (TrackLoaded msg), Options msg )
setTrack name options =
    case Dict.get name options.graph.edges of
        Just found ->
            let
                track =
                    found.track

                visibleTrack =
                    { track | visible = True }
            in
            ( Just visibleTrack
            , { options | activeTrackName = Just name }
            )

        Nothing ->
            ( Nothing, options )


getActiveTrack : Options msg -> Maybe (TrackLoaded msg)
getActiveTrack options =
    case options.activeTrackName of
        Just name ->
            Dict.get name options.graph.edges |> Maybe.map .track

        Nothing ->
            Nothing


unloadActiveTrack : Options msg -> ( Maybe (TrackLoaded msg), Options msg )
unloadActiveTrack options =
    case options.activeTrackName of
        Just active ->
            let
                graph =
                    options.graph

                newGraph =
                    { graph | edges = Dict.remove active options.graph.edges }

                newOptions =
                    { options
                        | activeTrackName =
                            -- Better to let the user see something.
                            newGraph.edges |> Dict.keys |> List.head
                        , graph = newGraph
                        , graphState =
                            if Dict.size newGraph.edges > 1 then
                                GraphOriginalTracks

                            else
                                GraphNoTracks
                    }

                newTrack =
                    case newOptions.activeTrackName of
                        Just name ->
                            Dict.get name newGraph.edges |> Maybe.map .track

                        Nothing ->
                            Nothing
            in
            ( newTrack
            , newOptions
            )

        Nothing ->
            ( Nothing, options )


mapOverVisibleTracks : (TrackLoaded msg -> Bool -> a) -> Options msg -> List a
mapOverVisibleTracks f options =
    options.graph.edges
        |> Dict.values
        |> List.indexedMap
            (\i edge ->
                if edge.track.visible then
                    Just <| f edge.track (Just edge.track.trackName == options.activeTrackName)

                else
                    Nothing
            )
        |> List.filterMap identity


mapOverInvisibleTracks : (TrackLoaded msg -> Bool -> a) -> Options msg -> List a
mapOverInvisibleTracks f options =
    options.graph.edges
        |> Dict.values
        |> List.indexedMap
            (\i edge ->
                if not edge.track.visible then
                    Just <| f edge.track (Just edge.track.trackName == options.activeTrackName)

                else
                    Nothing
            )
        |> List.filterMap identity


boundingBox : Options msg -> Maybe (BoundingBox3d.BoundingBox3d Meters LocalCoords)
boundingBox options =
    options.graph.edges
        |> Dict.values
        |> List.map (DomainModel.boundingBox << .trackTree << .track)
        |> BoundingBox3d.aggregateN


lookForClusters :
    Options msg
    -> Quantity Float Meters
    -> ( Options msg, List (Actions.ToolAction msg) )
lookForClusters options tolerance =
    let
        ( clusters, _ ) =
            Graph.identifyPointsToBeMerged tolerance options.graph

        --Return only the clusters. Wait for button click.
        --Graph.identifyPointsToBeMerged tolerance options.graph
        newOptions =
            { options
                | matchingTolerance = tolerance
                , clustersForPreview = clusters
            }
    in
    ( newOptions
    , [ makePreview newOptions ]
    )


makePreview : Options msg -> Actions.ToolAction msg
makePreview graphOptions =
    Actions.ShowPreview
        { tag = "graph"
        , shape = PreviewToolSupplied <| showNewPoints graphOptions.clustersForPreview
        , colour = FlatColors.AmericanPalette.sourLemon
        , points = []
        }


showNewPoints : List Cluster -> List (Entity LocalCoords)
showNewPoints clusters =
    let
        highlightPoint =
            Scene3d.point
                { radius = Pixels.pixels 3 }
                (Material.color Color.white)
    in
    List.map (.centroid >> highlightPoint) clusters


getKeyPlaces : Options msg -> List DomainModel.EarthPoint
getKeyPlaces options =
    Dict.values options.graph.nodes


traversalCanBeAdded : String -> Options msg -> Bool
traversalCanBeAdded newEdge options =
    -- Edge can be added if either node is same as final node of last traversal,
    -- or if there are no traversals.
    case
        ( List.Extra.last options.userRoute
        , Dict.get newEdge options.graph.edges
        )
    of
        ( Just lastTraversal, Just clickedEdge ) ->
            case Dict.get lastTraversal.edge options.graph.edges of
                Just currentLastEdge ->
                    let
                        finalNode =
                            if lastTraversal.direction == Natural then
                                currentLastEdge.highNode

                            else
                                currentLastEdge.lowNode
                    in
                    finalNode == clickedEdge.lowNode || finalNode == clickedEdge.highNode

                Nothing ->
                    False

        ( Nothing, Just _ ) ->
            -- Any edge can be the first edge used.
            True

        _ ->
            False



{-

   edgeCanBeDeleted : Int -> List Traversal -> Graph msg -> Bool
   edgeCanBeDeleted edge userRoute graph =
       -- Edge can be deleted if it's not the only edge and it's not used in the route.
       Dict.size graph.edges
           > 1
           && (not <|
                   List.any (\traversal -> traversal.edge == edge) userRoute
              )

-}


loopCanBeAdded : String -> Options msg -> Bool
loopCanBeAdded node options =
    -- Loop can be added if node is same as final node of last traversal.
    case
        List.Extra.last options.userRoute
    of
        Just traversal ->
            case Dict.get traversal.edge options.graph.edges of
                Just finalEdge ->
                    let
                        finalNode =
                            if traversal.direction == Natural then
                                finalEdge.highNode

                            else
                                finalEdge.lowNode
                    in
                    finalNode == node

                Nothing ->
                    False

        Nothing ->
            False


addSelfLoop : String -> Options msg -> Options msg
addSelfLoop node options =
    case List.Extra.last options.userRoute of
        Just traversal ->
            case Dict.get traversal.edge options.graph.edges of
                Just edgeInfo ->
                    let
                        ( _, edgeDirection, endPoint ) =
                            if traversal.direction == Natural then
                                ( edgeInfo.highNode
                                , DomainModel.getLastLeaf edgeInfo.track.trackTree |> .directionAtEnd
                                , DomainModel.earthPointFromIndex
                                    (DomainModel.skipCount edgeInfo.track.trackTree)
                                    edgeInfo.track.trackTree
                                )

                            else
                                ( edgeInfo.lowNode
                                , DomainModel.getFirstLeaf edgeInfo.track.trackTree
                                    |> .directionAtStart
                                    |> Direction2d.reverse
                                , DomainModel.startPoint edgeInfo.track.trackTree
                                )

                        loopOpposite =
                            endPoint.space
                                |> Point3d.translateBy
                                    (Vector3d.withLength
                                        (Quantity.twice options.minimumRadiusAtPlaces)
                                        (edgeDirection |> Direction3d.on SketchPlane3d.xy)
                                    )

                        loopCentre =
                            Point3d.midpoint endPoint.space loopOpposite

                        axis =
                            Axis3d.withDirection Direction3d.positiveZ loopCentre

                        ( arcStart, arcEnd ) =
                            ( endPoint.space |> Point3d.rotateAround axis (Angle.degrees 30)
                            , endPoint.space |> Point3d.rotateAround axis (Angle.degrees -30)
                            )

                        arc =
                            Arc3d.throughPoints arcStart loopOpposite arcEnd
                    in
                    case arc of
                        Just isArc ->
                            let
                                edgePoints =
                                    isArc
                                        |> Arc3d.approximate (Length.meters 0.1)
                                        |> Polyline3d.vertices
                                        |> List.map DomainModel.withoutTime
                                        |> List.map (DomainModel.gpxFromPointWithReference options.graph.referenceLonLat)

                                newEdgeTree =
                                    DomainModel.treeFromSourcesWithExistingReference
                                        edgeInfo.track.referenceLonLat
                                        edgePoints

                                newEdgeTrack =
                                    Maybe.map (TrackLoaded.newTrackFromTree edgeInfo.track.referenceLonLat)
                                        newEdgeTree
                            in
                            case newEdgeTrack of
                                Just newTrack ->
                                    let
                                        graph =
                                            options.graph

                                        newEdgeInfo =
                                            { lowNode = node
                                            , highNode = node
                                            , via = DomainModel.withoutTime loopOpposite
                                            }

                                        newEdgeIndex =
                                            "Road " ++ (String.fromInt <| 1 + Dict.size options.graph.edges)

                                        newGraph =
                                            --TODO: Should be in Graph.
                                            { graph
                                                | edges =
                                                    Dict.insert
                                                        newEdgeIndex
                                                        { lowNode = newEdgeInfo.lowNode
                                                        , highNode = newEdgeInfo.highNode
                                                        , via = Graph.nodeKey newEdgeInfo.via
                                                        , track =
                                                            { newTrack
                                                                | trackName = newEdgeIndex
                                                            }
                                                        }
                                                        graph.edges
                                            }
                                    in
                                    { options | graph = newGraph }

                                Nothing ->
                                    options

                        Nothing ->
                            options

                Nothing ->
                    options

        Nothing ->
            options


type alias Junction =
    { arc : Maybe (Arc3d Meters LocalCoords)
    , trim : Quantity Float Meters
    }


makeNewRoute : List Traversal -> Options msg -> Options msg
makeNewRoute userRoute options =
    {-
       This will walk the route, apply offset, push the old track on the Undo stack
       and then become a "trivialGraph" of the new route.
       Also note we nudge down by 1cm any edges that are revisited.
       Don't forget to push the old points on Undo stack.

        Nope, not doing a fold at traversal level since it gets unduly complicated.
        We need to consider a traversal and both its neighbours, to work out any edge
        shortening, so a triple map is conceptually easier, and a simple recursive
        function more so.
    -}
    let
        graph =
            options.graph

        useNudgeTool nudgeOption track index =
            -- Simple wrapper to use internal operation in Nudge; not efficient!
            Tools.Nudge.nudgeTrackPoint
                nudgeOption
                1.0
                index
                track

        dummyJunction : Junction
        dummyJunction =
            { arc = Nothing, trim = Quantity.zero }

        junctions : List Junction
        junctions =
            -- Took me so long to see this. Getting old?
            -- There will be one fewer junctions than edges.
            List.map2
                computeJunction
                options.userRoute
                (List.drop 1 options.userRoute)

        trim =
            -- CHANGE. Actually prune the trees to get the right leaf.
            -- Will need to do this in the traversal rendering as well.
            -- This changes the "minimum radius" semantics.
            options.minimumRadiusAtPlaces

        renderedArcs : List (List EarthPoint)
        renderedArcs =
            List.map renderJunction junctions

        isNotFirstUseOfEdge : List Bool
        isNotFirstUseOfEdge =
            -- Good practice for RGT is to depress subsequent edge pass by 1cm; avoids flicker.
            let
                ( _, flags ) =
                    List.foldl
                        (\{ edge } ( traversed, outputs ) ->
                            ( Set.insert edge traversed
                            , Set.member edge traversed :: outputs
                            )
                        )
                        ( Set.empty, [] )
                        options.userRoute
            in
            List.reverse flags

        trimmedTraversals : List (List EarthPoint)
        trimmedTraversals =
            List.map4
                trimTraversal
                (dummyJunction :: junctions)
                options.userRoute
                isNotFirstUseOfEdge
                (junctions ++ [ dummyJunction ])

        trimTraversal : Junction -> Traversal -> Bool -> Junction -> List EarthPoint
        trimTraversal preceding { edge, direction } repetition following =
            -- Emit offset points but not within the trim areas.
            case Dict.get edge graph.edges of
                Just edgeInfo ->
                    -- It's a real edge, offset flipped if reversed.
                    -- Compute offset points on unflipped edge then flip if reversed.
                    let
                        correctedOffset =
                            case direction of
                                Natural ->
                                    options.centreLineOffset

                                Reverse ->
                                    Quantity.negate options.centreLineOffset

                        ( firstOffsetIndex, lastOffsetIndex ) =
                            -- Other than start and end of route, trim back the edge to allow for the Place arc.
                            ( DomainModel.indexFromDistance trim edgeInfo.track.trackTree + 1
                            , DomainModel.indexFromDistance
                                (trueLength edgeInfo.track.trackTree |> Quantity.minus trim)
                                edgeInfo.track.trackTree
                                - 1
                            )

                        defaultNudge =
                            Tools.Nudge.defaultOptions

                        nudgeOptions : Tools.NudgeOptions.Options
                        nudgeOptions =
                            { defaultNudge
                                | horizontal = correctedOffset
                                , vertical =
                                    if repetition then
                                        Length.centimeters -1

                                    else
                                        Quantity.zero
                            }

                        nudgedPoints =
                            List.range firstOffsetIndex lastOffsetIndex
                                |> List.map (useNudgeTool nudgeOptions edgeInfo.track.trackTree)
                    in
                    case direction of
                        Natural ->
                            nudgedPoints

                        Reverse ->
                            List.reverse nudgedPoints

                Nothing ->
                    []

        newEarthPoints =
            List.take 1 trimmedTraversals
                ++ List.Extra.interweave renderedArcs (List.drop 1 trimmedTraversals)

        computeJunction : Traversal -> Traversal -> Junction
        computeJunction inbound outbound =
            -- This is the bit of new geometry. We need the "end" direction of the inbound edge
            -- (allowing for Direction) and the "start" direction of the outbound.
            -- We work out the arc needed to give the minimum radius at centre-line.
            -- We work out how much this impedes on the edges (the "trim").
            -- We compute the "arc" according to the direction and offset.
            -- Note we actually make a 2D arc, then interpolate altitudes to get 3D.
            -- First thing is all the necessary dictionary lookups.
            case ( Dict.get inbound.edge graph.edges, Dict.get outbound.edge graph.edges ) of
                ( Just inEdge, Just outEdge ) ->
                    let
                        actualVertex =
                            case inbound.direction of
                                Natural ->
                                    DomainModel.earthPointFromIndex
                                        (DomainModel.skipCount inEdge.track.trackTree)
                                        inEdge.track.trackTree

                                Reverse ->
                                    DomainModel.earthPointFromIndex
                                        0
                                        inEdge.track.trackTree

                        ( _, inboundTrimPoint ) =
                            case inbound.direction of
                                Natural ->
                                    DomainModel.interpolateTrack
                                        (trueLength inEdge.track.trackTree |> Quantity.minus trim)
                                        inEdge.track.trackTree

                                Reverse ->
                                    DomainModel.interpolateTrack
                                        trim
                                        inEdge.track.trackTree

                        ( _, outboundTrimPoint ) =
                            case outbound.direction of
                                Natural ->
                                    DomainModel.interpolateTrack
                                        trim
                                        outEdge.track.trackTree

                                Reverse ->
                                    DomainModel.interpolateTrack
                                        (trueLength outEdge.track.trackTree |> Quantity.minus trim)
                                        outEdge.track.trackTree

                        ( inboundDirection, outboundDirection ) =
                            ( Direction3d.from inboundTrimPoint.space actualVertex.space
                                |> Maybe.withDefault Direction3d.positiveZ
                                |> Direction3d.projectInto planeFor2dArc
                                |> Maybe.withDefault Direction2d.positiveX
                            , Direction3d.from actualVertex.space outboundTrimPoint.space
                                |> Maybe.withDefault Direction3d.positiveZ
                                |> Direction3d.projectInto planeFor2dArc
                                |> Maybe.withDefault Direction2d.positiveX
                            )

                        ( inboundRoad, outboundRoad ) =
                            ( LineSegment3d.from inboundTrimPoint.space actualVertex.space
                            , LineSegment3d.from actualVertex.space outboundTrimPoint.space
                            )

                        ( offsetVectorInbound, offsetVectorOutbound ) =
                            ( Vector2d.withLength options.centreLineOffset
                                (Direction2d.rotateClockwise inboundDirection)
                            , Vector2d.withLength options.centreLineOffset
                                (Direction2d.rotateClockwise outboundDirection)
                            )

                        meanHeight =
                            Quantity.half <|
                                Quantity.plus
                                    (Point3d.zCoordinate inboundTrimPoint.space)
                                    (Point3d.zCoordinate outboundTrimPoint.space)

                        -- If we now apply offset to the start and end (which we can), we
                        -- make the offset arc not the centre line arc here.
                        planeFor2dArc =
                            SketchPlane3d.xy
                                |> SketchPlane3d.translateBy (Vector3d.xyz Quantity.zero Quantity.zero meanHeight)

                        ( inboundTrim2d, outboundTrim2d ) =
                            ( inboundTrimPoint.space |> Point3d.projectInto planeFor2dArc
                            , outboundTrimPoint.space |> Point3d.projectInto planeFor2dArc
                            )

                        ( inboundRoad2d, outboundRoad2d ) =
                            ( inboundRoad |> LineSegment3d.projectInto planeFor2dArc
                            , outboundRoad |> LineSegment3d.projectInto planeFor2dArc
                            )

                        geometryPoint point =
                            Point2d.toRecord inMeters point

                        lineEquationFromSegment segment =
                            Geometry101.lineEquationFromTwoPoints
                                (geometryPoint <| LineSegment2d.startPoint segment)
                                (geometryPoint <| LineSegment2d.endPoint segment)

                        ( inboundLineEquation, outboundLineEquation ) =
                            ( lineEquationFromSegment inboundRoad2d
                            , lineEquationFromSegment outboundRoad2d
                            )

                        ( perpToInbound, perToOutbound ) =
                            ( Geometry101.linePerpendicularTo
                                inboundLineEquation
                                (geometryPoint inboundTrim2d)
                            , Geometry101.linePerpendicularTo
                                outboundLineEquation
                                (geometryPoint outboundTrim2d)
                            )

                        arcCentre =
                            Maybe.map (Point2d.fromRecord meters) <|
                                Geometry101.lineIntersection
                                    perpToInbound
                                    perToOutbound

                        arc : Maybe (Arc2d Meters LocalCoords)
                        arc =
                            case arcCentre of
                                Just centre ->
                                    let
                                        ( offsetInboundTrimPoint, _ ) =
                                            ( inboundTrim2d |> Point2d.translateBy offsetVectorInbound
                                            , outboundTrim2d |> Point2d.translateBy offsetVectorOutbound
                                            )

                                        turnAngle =
                                            Direction2d.angleFrom inboundDirection outboundDirection
                                    in
                                    Just <| Arc2d.sweptAround centre turnAngle offsetInboundTrimPoint

                                Nothing ->
                                    Nothing
                    in
                    -- We make a 3d arc through the same points.
                    case arc of
                        Just foundArc ->
                            { arc = Just <| Arc3d.on planeFor2dArc foundArc
                            , trim = trim
                            }

                        Nothing ->
                            dummyJunction

                _ ->
                    dummyJunction

        renderJunction : Junction -> List EarthPoint
        renderJunction junction =
            case junction.arc of
                Just arc ->
                    arc
                        |> Arc3d.approximate (Length.meters 0.1)
                        |> Polyline3d.vertices
                        |> List.map DomainModel.withoutTime

                Nothing ->
                    []

        newTrack : Maybe (TrackLoaded msg)
        newTrack =
            newEarthPoints
                |> List.concat
                |> List.map (DomainModel.gpxFromPointWithReference graph.referenceLonLat)
                |> DomainModel.treeFromSourcesWithExistingReference graph.referenceLonLat
                |> Maybe.map (TrackLoaded.newTrackFromTree graph.referenceLonLat)
    in
    case newTrack of
        Just track ->
            -- All has worked.
            let
                newTrackName =
                    List.map .edge options.userRoute
                        |> List.Extra.unique
                        |> String.join "-"

                trackWithUndo =
                    TrackLoaded.addToUndoStack
                        Actions.MakeRouteFromGraph
                        { track | trackName = newTrackName }
            in
            { options
                | graph = Graph.addEdgeFromTrack trackWithUndo emptyGraph
                , selectedTraversal = 0
                , graphState = GraphOriginalTracks
                , activeTrackName = Just newTrackName
                , roadListCollapsed = False
                , priors = OptionsUndo options :: options.priors
            }

        Nothing ->
            -- Not so much worked.
            options
