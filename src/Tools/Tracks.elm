module Tools.Tracks exposing
    ( Msg(..)
    , addSelfLoop
    , addTrack
    , addTraversal
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
import Arc3d
import Axis3d
import Color
import CommonToolStyles
import Dict
import Direction2d
import Direction3d
import DomainModel exposing (GPXSource, PeteTree, trueLength)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import FeatherIcons
import FlatColors.AmericanPalette
import FlatColors.FlatUIPalette
import Length exposing (Meters)
import List.Extra
import LocalCoords exposing (LocalCoords)
import Pixels
import Point3d
import Polyline3d
import PreviewData exposing (PreviewShape(..))
import Quantity exposing (Quantity)
import Scene3d exposing (Entity)
import Scene3d.Material as Material
import SketchPlane3d
import String.Interpolate
import SystemSettings exposing (SystemSettings)
import ToolTip exposing (localisedTooltip, tooltip)
import Tools.Graph as Graph
import Tools.GraphOptions as Graph exposing (Cluster, Graph)
import Tools.I18N as I18N
import Tools.TracksOptions as Options exposing (Direction(..), GraphState(..), Options, Traversal, TraversalDisplay)
import TrackLoaded exposing (TrackLoaded)
import UtilsForViews exposing (showDecimal2, showLongMeasure, showShortMeasure)
import Vector3d
import ViewPureStyles exposing (commonShortHorizontalSliderStyles, infoButton, neatToolsBorder, useIcon, useIconWithSize)


toolId =
    "tracks"


type Msg
    = SelectActiveTrack String
    | ToggleVisibility String
    | UnloadActiveTrack
    | GraphAnalyse
      --| CentreLineOffset (Quantity Float Meters)
      --| MinimumRadius (Quantity Float Meters)
      --| ConvertFromGraph
    | HighlightTraversal Int
    | RemoveLastTraversal
    | DisplayInfo String String
    | FlipDirection Int
    | ClearRoute
    | UndoAnalyze
    | SetTolerance (Quantity Float Meters)
    | SnapToNearby
    | UndoSnap
    | Canonicalise
    | UndoCanonicalise
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
                    ( options
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

                        updatedTrack =
                            { track | visible = not track.visible }
                    in
                    ( { options
                        | graph =
                            if updatedTrack.visible then
                                Graph.addEdgeFromTrack updatedTrack options.graph

                            else
                                Graph.removeEdge updatedTrack options.graph
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
                , graphState = GraphSnapped options.graph
              }
            , [ Actions.SetActiveTrack <|
                    Maybe.withDefault "" <|
                        Maybe.map .trackName <|
                            Graph.trackFromIndex 0 newGraph
              , Actions.HidePreview "graph"
              ]
            )

        GraphAnalyse ->
            -- Note that the state rules here mean that the tracks have been "snapped".
            -- We may safely proceed with neighbour counting to find nodes and edges.
            -- The found edges become the new tracks.
            -- We delegate to graph, then pull back the updated tracks.
            -- Save the previous graph for simple reversion.
            case options.graphState of
                GraphSnapped preSnapGraph ->
                    let
                        newGraph =
                            Graph.analyzeTracksAsGraph options.graph

                        newTracks =
                            Dict.values newGraph.edges
                                |> List.map .track
                    in
                    ( { options
                        | graph = newGraph
                        , graphState = GraphWithNodes options.graph preSnapGraph
                      }
                    , [ Actions.SetActiveTrack <|
                            Maybe.withDefault "" <|
                                Maybe.map .trackName <|
                                    Graph.trackFromIndex 0 newGraph
                      , Actions.HidePreview "graph"
                      ]
                    )

                _ ->
                    ( options, [] )

        UndoAnalyze ->
            case options.graphState of
                GraphWithNodes preAnalyze preSnap ->
                    ( { options
                        | graph = preAnalyze
                        , graphState = GraphSnapped preSnap
                      }
                    , [ Actions.ChangeActiveTrack 0, Actions.TrackHasChanged ]
                    )

                _ ->
                    ( options, [] )

        UndoCanonicalise ->
            case options.graphState of
                GraphWithEdges preCanon preAnalyze preSnap ->
                    ( { options
                        | graph = preCanon
                        , graphState = GraphWithNodes preAnalyze preSnap
                      }
                    , [ Actions.ChangeActiveTrack 0
                      , Actions.TrackHasChanged
                      ]
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

        {-
           CentreLineOffset float ->
               ( { options | centreLineOffset = float }, [] )
        -}
        {-
           MinimumRadius float ->
               ( { options | minimumRadiusAtPlaces = float }, [] )
        -}
        {-
           ConvertFromGraph ->
               ( options
               , [ Actions.MakeRouteFromGraph
                 , Actions.TrackHasChanged
                 , Actions.ExitRoutePlanning
                 , Actions.HidePreview toolId
                 ]
               )
        -}
        DisplayInfo tool tag ->
            ( options, [ Actions.DisplayInfo tool tag ] )

        UndoSnap ->
            case options.graphState of
                GraphSnapped previous ->
                    ( { options
                        | graph = previous
                        , graphState = GraphOriginalTracks
                      }
                    , [ Actions.SetActiveTrack <|
                            Maybe.withDefault "" <|
                                Maybe.map .trackName <|
                                    Graph.trackFromIndex 0 options.graph
                      ]
                    )

                _ ->
                    ( { options | graphState = GraphOriginalTracks }, [] )

        Canonicalise ->
            case options.graphState of
                GraphWithNodes beforeNodes beforeSnap ->
                    let
                        newGraph =
                            Graph.canonicalise options.graph
                    in
                    ( { options
                        | graph = newGraph
                        , graphState = GraphWithEdges options.graph beforeNodes beforeSnap
                      }
                    , [ Actions.SetActiveTrack <|
                            Maybe.withDefault "" <|
                                Maybe.map .trackName <|
                                    Graph.trackFromIndex 0 newGraph
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
            , unloadButton
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

                        GraphSnapped _ ->
                            i18n "graphSnapped"

                        GraphWithNodes _ _ ->
                            i18n "graphAnalyzed"

                        GraphWithEdges _ _ _ ->
                            i18n "graphConverted"
                    ]
                ]
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
                in
                column [ centerX, width fill, spacing 10 ]
                    [ toleranceSlider
                    , snapToNearbyButton
                    ]

            GraphSnapped _ ->
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
                                { onPress = Just <| wrapper UndoSnap
                                , label = i18n "undoSnap"
                                }
                            ]
                in
                column [ centerX, width fill, spacing 10 ]
                    [ undoButton
                    , analyseButton
                    ]

            GraphWithNodes snappedGraph originalGraph ->
                let
                    revertButton =
                        Input.button neatToolsBorder
                            { onPress = Just (wrapper UndoAnalyze)
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

            GraphWithEdges beforeEdges beforeNodes beforeSnap ->
                let
                    offset =
                        Length.inMeters options.centreLineOffset

                    radius =
                        Length.inMeters options.minimumRadiusAtPlaces

                    revertButton =
                        Input.button neatToolsBorder
                            { onPress = Just (wrapper UndoCanonicalise)
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
                                    { onPress = Nothing --Just (wrapper ConvertFromGraph)
                                    , label = i18n "convert"
                                    }
                                ]

                        else
                            none

                    offsetSlider =
                        row [ spacing 5 ]
                            [ none
                            , infoButton (wrapper <| DisplayInfo toolId "offset")

                            --, Input.slider
                            --    commonShortHorizontalSliderStyles
                            --    { onChange = wrapper << CentreLineOffset << Length.meters
                            --    , label =
                            --        Input.labelBelow [] <|
                            --            text <|
                            --                String.Interpolate.interpolate
                            --                    (I18N.localisedString settings.location toolId "isOffset")
                            --                    [ showDecimal2 <| abs offset
                            --                    , if offset < 0.0 then
                            --                        I18N.localisedString settings.location toolId "left"
                            --
                            --                      else if offset > 0.0 then
                            --                        I18N.localisedString settings.location toolId "right"
                            --
                            --                      else
                            --                        ""
                            --                    ]
                            --    , min = -5.0
                            --    , max = 5.0
                            --    , step = Just 0.25
                            --    , value = offset
                            --    , thumb = Input.defaultThumb
                            --    }
                            ]

                    minRadiusSlider =
                        row [ spacing 5 ]
                            [ none
                            , infoButton (wrapper <| DisplayInfo toolId "radius")

                            --, Input.slider
                            --    commonShortHorizontalSliderStyles
                            --    { onChange = wrapper << MinimumRadius << Length.meters
                            --    , label =
                            --        Input.labelBelow [] <|
                            --            text <|
                            --                String.Interpolate.interpolate
                            --                    (I18N.localisedString settings.location toolId "isRadius")
                            --                    [ showDecimal2 <| abs radius ]
                            --    , min = 1.0
                            --    , max = 15.0
                            --    , step = Just 1.0
                            --    , value = radius
                            --    , thumb = Input.defaultThumb
                            --    }
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


deleteEdgeTraversal : Int -> List Traversal -> Graph msg -> Graph msg
deleteEdgeTraversal edge userRoute graph =
    graph


addSelfLoop : String -> Options msg -> Options msg
addSelfLoop node options =
    case
        List.Extra.last options.userRoute
    of
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
                                                        , track = newTrack
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
