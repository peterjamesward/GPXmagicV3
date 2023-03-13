module Tools.Tracks exposing
    ( Msg(..)
    , addTrack
    , defaultOptions
    , getActiveTrack
    , getKeyPlaces
    , mapOverInvisibleTracks
    , mapOverVisibleTracks
    , setTrack
    , toolId
    , toolStateChange
    , unloadActiveTrack
    , update
    , updateActiveTrack
    , view
    )

import Actions
import Angle
import Color
import CommonToolStyles
import Dict
import Direction2d
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
import PreviewData exposing (PreviewShape(..))
import Quantity exposing (Quantity)
import Scene3d exposing (Entity)
import Scene3d.Material as Material
import String.Interpolate
import SystemSettings exposing (SystemSettings)
import Tools.Graph as Graph
import Tools.GraphOptions as Graph exposing (Cluster, Graph)
import Tools.I18N as I18N
import Tools.TracksOptions as Options exposing (GraphState(..), Options, Traversal)
import TrackLoaded exposing (TrackLoaded)
import UtilsForViews exposing (showDecimal2, showLongMeasure, showShortMeasure)
import ViewPureStyles exposing (commonShortHorizontalSliderStyles, infoButton, neatToolsBorder, useIcon, useIconWithSize)


toolId =
    "tracks"


type Msg
    = SelectActiveTrack Int
    | ToggleVisibility Int
    | UnloadActiveTrack
    | GraphAnalyse
      --| CentreLineOffset (Quantity Float Meters)
      --| MinimumRadius (Quantity Float Meters)
      --| ConvertFromGraph
    | HighlightTraversal Int
      --| RemoveLastTraversal
    | DisplayInfo String String
      --| FlipDirection Int
      --| ClearRoute
    | UndoAnalyze
    | SetTolerance (Quantity Float Meters)
      --| UndoDeleteRoad
    | SnapToNearby
    | UndoSnap
    | Canonicalise
    | UndoCanonicalise
    | ToggleRoadList


defaultOptions : Options msg
defaultOptions =
    { nextTrackNumber = 1
    , tracks = []
    , activeTrackIndex = Nothing
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
        SelectActiveTrack index ->
            ( options, [ Actions.SetActiveTrack index ] )

        ToggleVisibility index ->
            case List.Extra.getAt index options.tracks of
                Just found ->
                    let
                        updatedTrack =
                            { found | visible = not found.visible }
                    in
                    ( { options
                        | tracks =
                            List.Extra.updateAt
                                index
                                (always updatedTrack)
                                options.tracks
                        , graph =
                            if updatedTrack.visible then
                                Graph.addEdgeFromTrack updatedTrack options.graph

                            else
                                Graph.removeEdge updatedTrack options.graph
                      }
                    , [ Actions.SetActiveTrack <| Maybe.withDefault 0 options.activeTrackIndex ]
                    )

                Nothing ->
                    ( options
                    , if Just index == options.activeTrackIndex then
                        [ Actions.HidePreview "graph" ]

                      else
                        []
                    )

        UnloadActiveTrack ->
            case options.activeTrackIndex of
                Just active ->
                    case List.Extra.getAt active options.tracks of
                        Just track ->
                            ( options, [ Actions.UnloadActiveTrack track.trackName ] )

                        Nothing ->
                            ( options, [ Actions.HidePreview "graph" ] )

                Nothing ->
                    ( options, [] )

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
                , tracks = newTracks
                , graphState = GraphSnapped options.graph
              }
            , [ Actions.SetActiveTrack 0
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
                        , tracks = newTracks
                        , graphState = GraphWithNodes options.graph preSnapGraph
                      }
                    , [ Actions.SetActiveTrack 0
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
                        , tracks = List.map .track <| Dict.values preCanon.edges
                      }
                    , [ Actions.ChangeActiveTrack 0, Actions.TrackHasChanged ]
                    )

                _ ->
                    ( options, [] )

        HighlightTraversal traversal ->
            ( { options | selectedTraversal = traversal }, [] )

        {-
           RemoveLastTraversal ->
               let
                   graph =
                       options.graph

                   newGraph =
                       { graph
                           | userRoute =
                               List.take (List.length graph.userRoute - 1) graph.userRoute
                       }
               in
               ( { options
                   | graph = newGraph
                   , selectedTraversal = List.length newGraph.userRoute - 1
                 }
               , []
               )
        -}
        {-
           FlipDirection i ->
               let
                   graph =
                       options.graph

                   newGraph =
                       { graph
                           | userRoute =
                               graph.userRoute
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
               ( { options
                   | graph = newGraph
                   , selectedTraversal = min options.selectedTraversal (List.length newGraph.userRoute - 1)
                 }
               , []
               )
        -}
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
                    let
                        graph =
                            options.graph

                        newTracks =
                            Dict.values previous.edges
                                |> List.map .track
                    in
                    ( { options
                        | graph = previous
                        , tracks = newTracks
                        , graphState = GraphOriginalTracks
                      }
                    , [ Actions.SetActiveTrack 0 ]
                    )

                _ ->
                    ( { options | graphState = GraphOriginalTracks }, [] )

        Canonicalise ->
            case options.graphState of
                GraphWithNodes beforeNodes beforeSnap ->
                    let
                        graph =
                            options.graph

                        newGraph =
                            Graph.canonicalise graph
                    in
                    ( { options
                        | graph = newGraph
                        , tracks = List.map .track <| Dict.values newGraph.edges
                        , graphState = GraphWithEdges options.graph beforeNodes beforeSnap
                      }
                    , [ Actions.SetActiveTrack 0 ]
                    )

                _ ->
                    ( { options | graphState = GraphOriginalTracks }, [] )

        ToggleRoadList ->
            ( { options | roadListCollapsed = not options.roadListCollapsed }, [] )



{-
   ClearRoute ->
       let
           graph =
               options.graph

           newGraph =
               { graph | userRoute = [] }
       in
       ( { options
           | graph = newGraph
           , selectedTraversal = -1
         }
       , []
       )
-}
{-
   UndoDeleteRoad ->
       case options.graphUndos of
           lastVersion :: olderVersions ->
               ( { options
                   | graph = lastVersion
                   , graphUndos = olderVersions
                 }
               , []
               )

           _ ->
               ( options, [] )
-}


updateActiveTrack : TrackLoaded msg -> TrackLoaded msg -> Options msg -> ( TrackLoaded msg, Options msg )
updateActiveTrack oldTrack newTrack options =
    case options.activeTrackIndex of
        Just index ->
            ( newTrack
            , { options
                | tracks =
                    List.Extra.setAt index newTrack options.tracks
                , graph = Graph.updatedEdge oldTrack newTrack options.graph
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
                    List.indexedMap
                        (\index entry ->
                            displayTrackInfo index entry wrapper options
                        )
                        options.tracks

        unloadButton =
            el [ centerX, width fill ] <|
                if options.activeTrackIndex /= Nothing then
                    Input.button
                        neatToolsBorder
                        { label = helper "unload"
                        , onPress = Just <| wrapper UnloadActiveTrack
                        }

                else
                    none

        collapseExpandButton =
            if List.length options.tracks > 1 then
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
                            { onPress = Nothing --Just (wrapper ClearRoute)
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
                in
                column [ width fill, padding 4, spacing 10 ]
                    [ revertButton
                    , row [ centerX, width fill, spacing 10 ]
                        [ traversalPrevious
                        , traversalNext
                        , clearRouteButton
                        ]

                    --, traversalsTable
                    , offsetSlider
                    , minRadiusSlider
                    , finishButton
                    ]
        ]


displayTrackInfo : Int -> TrackLoaded msg -> (Msg -> msg) -> Options msg -> Element msg
displayTrackInfo index track wrapper options =
    row [ spacing 5 ]
        [ Input.button
            []
            { label = useIcon FeatherIcons.edit
            , onPress = Just <| wrapper (SelectActiveTrack index)
            }
        , if Just index == options.activeTrackIndex then
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
                , onPress = Just <| wrapper (ToggleVisibility index)
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
            case
                List.Extra.find
                    (\t -> t.trackName == track.trackName)
                    options.tracks
            of
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
        | tracks = trackWithUnambiguousName :: options.tracks
        , nextTrackNumber = options.nextTrackNumber + 1
        , activeTrackIndex = Just 0
        , commonReferenceGPX = newReferenceGPX
        , graph = Graph.addEdgeFromTrack trackWithUnambiguousName options.graph
        , graphState = GraphOriginalTracks
    }


setTrack : Int -> Options msg -> ( Maybe (TrackLoaded msg), Options msg )
setTrack index options =
    case List.Extra.getAt index options.tracks of
        Just found ->
            let
                visibleTrack =
                    { found | visible = True }
            in
            ( Just visibleTrack
            , { options
                | activeTrackIndex = Just index
                , tracks = List.Extra.updateAt index (always visibleTrack) options.tracks
              }
            )

        Nothing ->
            ( Nothing, options )


getActiveTrack : Options msg -> Maybe (TrackLoaded msg)
getActiveTrack options =
    case options.activeTrackIndex of
        Just index ->
            List.Extra.getAt index options.tracks

        Nothing ->
            Nothing


unloadActiveTrack : Options msg -> ( Maybe (TrackLoaded msg), Options msg )
unloadActiveTrack options =
    case options.activeTrackIndex of
        Just active ->
            let
                newOptions =
                    { options
                        | tracks = List.Extra.removeAt active options.tracks
                        , activeTrackIndex =
                            -- Better to let the user see something.
                            if List.length options.tracks > 1 then
                                Just 0

                            else
                                Nothing
                        , graph =
                            case List.Extra.getAt active options.tracks of
                                Just track ->
                                    Graph.removeEdge track options.graph

                                Nothing ->
                                    options.graph
                        , graphState =
                            if List.length options.tracks > 1 then
                                GraphOriginalTracks

                            else
                                GraphNoTracks
                    }

                newTrack =
                    case newOptions.activeTrackIndex of
                        Just index ->
                            List.Extra.getAt index newOptions.tracks

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
    options.tracks
        |> List.indexedMap
            (\i track ->
                if track.visible then
                    Just <| f track (Just i == options.activeTrackIndex)

                else
                    Nothing
            )
        |> List.filterMap identity


mapOverInvisibleTracks : (TrackLoaded msg -> Bool -> a) -> Options msg -> List a
mapOverInvisibleTracks f options =
    options.tracks
        |> List.indexedMap
            (\i track ->
                if not track.visible then
                    Just <| f track (Just i == options.activeTrackIndex)

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


traversalCanBeAdded : String -> List Traversal -> Graph.Graph msg -> Bool
traversalCanBeAdded newEdge userRoute graph =
    False



{-
   -- Edge can be added if either node is same as final node of last traversal,
   -- or if there are no traversals.
   case
       ( List.Extra.last userRoute
       , Dict.get newEdge graph.edges
       )
   of
       ( Just lastTraversal, Just clickedEdge ) ->
           case Dict.get lastTraversal.edge graph.edges of
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
-}
{-

   edgeCanBeDeleted : Int -> List Traversal -> Graph msg -> Bool
   edgeCanBeDeleted edge userRoute graph =
       -- Edge can be deleted if it's not the only edge and it's not used in the route.
       Dict.size graph.edges
           > 1
           && (not <|
                   List.any (\traversal -> traversal.edge == edge) userRoute
              )


   loopCanBeAdded : Int -> List Traversal -> Graph msg -> Bool
   loopCanBeAdded node userRoute graph =
       False
-}
{-
   -- Loop can be added if node is same as final node of last traversal.
   case
       List.Extra.last userRoute
   of
       Just traversal ->
           case Dict.get traversal.edge graph.edges of
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
-}


deleteEdgeTraversal : Int -> List Traversal -> Graph msg -> Graph msg
deleteEdgeTraversal edge userRoute graph =
    graph
