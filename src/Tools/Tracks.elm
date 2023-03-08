module Tools.Tracks exposing
    ( Msg(..)
    , addTrack
    , defaultOptions
    , getActiveTrack
    , mapOverInvisibleTracks
    , mapOverVisibleTracks
    , setTrack
    , toolId
    , unloadActiveTrack
    , update
    , updateActiveTrack
    , view
    )

import Actions
import Angle
import BoundingBox3d exposing (BoundingBox3d)
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
import ToolTip exposing (localisedTooltip, tooltip)
import Tools.Graph as Graph
import Tools.GraphOptions as Graph exposing (Cluster, Direction(..))
import Tools.I18N as I18N
import Tools.TracksOptions as Options exposing (GraphOptions, Options)
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
      --| HighlightTraversal Int
      --| RemoveLastTraversal
    | DisplayInfo String String
      --| FlipDirection Int
      --| ClearRoute
      --| RevertToTrack
    | SetTolerance (Quantity Float Meters)



--| UndoDeleteRoad
--| AdoptNewTrack


defaultGraphOptions : Options.GraphOptions msg
defaultGraphOptions =
    { matchingTolerance = Length.meters 1.5
    , centreLineOffset = Length.meters 0.0
    , minimumRadiusAtPlaces = Length.meters 3.0

    --, boundingBox = BoundingBox3d.singleton Point3d.origin
    --, selectedTraversal = 0
    , analyzed = False

    --, originalTrack = Nothing
    --, editingTrack = 0
    --, undoGraph = Nothing
    --, undoOriginalTrack = Nothing
    , clustersForPreview = []

    --, perpsForPreview = []
    --, suggestedNewTree = Nothing
    --, suggestedNewGraph = Nothing
    , graphUndos = []
    , userRoute = []
    }


defaultOptions : Options msg
defaultOptions =
    { nextTrackNumber = 1
    , tracks = []
    , activeTrackIndex = Nothing
    , commonReferenceGPX = Nothing
    , graph = emptyGraph
    , graphOptions = defaultGraphOptions
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
                                Graph.addEdge updatedTrack options.graph

                            else
                                Graph.removeEdge updatedTrack options.graph
                      }
                    , [ Actions.SetActiveTrack <| Maybe.withDefault 0 options.activeTrackIndex ]
                    )

                Nothing ->
                    ( options, [] )

        UnloadActiveTrack ->
            case options.activeTrackIndex of
                Just active ->
                    case List.Extra.getAt active options.tracks of
                        Just track ->
                            ( options, [ Actions.UnloadActiveTrack track.trackName ] )

                        Nothing ->
                            ( options, [] )

                Nothing ->
                    ( options, [] )

        {-
           AdoptNewTrack ->
               ( options
               , [ Actions.WithUndo Actions.CombineNearbyPoints
                 , Actions.CombineNearbyPoints
                 , Actions.TrackHasChanged
                 ]
               )
        -}
        GraphAnalyse ->
            ( options
            , if options.graphOptions.matchingTolerance |> Quantity.greaterThanZero then
                [ Actions.CombineNearbyPoints
                , Actions.StartRoutePlanning
                , Actions.HidePreview "graph"
                ]

              else
                [ Actions.StartRoutePlanning
                , Actions.HidePreview "graph"
                ]
            )

        {-
           RevertToTrack ->
               ( { options
                   | graph =
                       case options.originalTrack of
                           Just original ->
                               trivialGraph original

                           Nothing ->
                               -- Oh dear!
                               options.graph
                   , analyzed = False
                   , originalTrack = Nothing
                   , editingTrack = 0
                   , selectedTraversal = 0
                 }
               , [ Actions.ChangeActiveTrack 0, Actions.TrackHasChanged ]
               )
        -}
        {-
           HighlightTraversal traversal ->
               ( { options | selectedTraversal = traversal }, [] )
        -}
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
    in
    el (CommonToolStyles.toolContentBoxStyle settings) <|
        column [ spacing 5 ]
            [ listOfTracks
            , unloadButton
            , graphView settings wrapper options.graphOptions options.graph
            ]


graphView : SystemSettings -> (Msg -> msg) -> GraphOptions msg -> Graph.Graph msg -> Element msg
graphView settings wrapper options graph =
    let
        i18n =
            I18N.text settings.location toolId

        {-
           traversals : List Graph.TraversalDisplay
           traversals =
               -- Display-ready version of the route.
               options.userRoute
                   |> List.map
                       (\traversal ->
                           case Dict.get traversal.edge graph.edges of
                               Nothing ->
                                   { startPlace = -1
                                   , road = -1
                                   , endPlace = -1
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
        -}
        dataStyles selected =
            if selected then
                [ Font.bold
                , padding 2
                ]

            else
                [ padding 2 ]

        {-
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
                                                       { onPress = Nothing --Just <| wrapper RemoveLastTraversal
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
                                                       { onPress = Nothing --Just <| wrapper <| FlipDirection i
                                                       , label = useIcon FeatherIcons.refreshCw
                                                       }

                                                 else
                                                   none
                                               , Input.button
                                                   [ alignRight ]
                                                   { onPress = Nothing --Just <| wrapper <| HighlightTraversal i
                                                   , label = useIcon FeatherIcons.eye
                                                   }
                                               ]
                                 }
                               , { header = none
                                 , width = fillPortion 2
                                 , view =
                                       \i t ->
                                           el (dataStyles (i == options.selectedTraversal)) <|
                                               text <|
                                                   String.Interpolate.interpolate
                                                       (I18N.localisedString settings.location toolId "place1")
                                                       [ String.fromInt t.startPlace ]
                                 }
                               , { header = none
                                 , width = fillPortion 2
                                 , view =
                                       \i t ->
                                           el (dataStyles (i == options.selectedTraversal)) <|
                                               text <|
                                                   String.Interpolate.interpolate
                                                       (I18N.localisedString settings.location toolId "place2")
                                                       [ String.fromInt t.endPlace ]
                                 }
                               , { header = none
                                 , width = fillPortion 2
                                 , view =
                                       \i t ->
                                           el (dataStyles (i == options.selectedTraversal)) <|
                                               text <|
                                                   String.Interpolate.interpolate
                                                       (I18N.localisedString settings.location toolId "road")
                                                       [ String.fromInt t.road ]
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
        -}
        guidanceText =
            row
                [ Background.color FlatColors.FlatUIPalette.turquoise
                , Font.color (CommonToolStyles.themeForeground settings.colourTheme)
                , Border.rounded 5
                ]
                [ useIconWithSize 20 FeatherIcons.info
                , paragraph [ padding 4 ]
                    [ if options.analyzed then
                        if List.isEmpty options.userRoute then
                            i18n "guidanceNoRoute"

                        else
                            i18n "guidanceAnalyzed"

                      else
                        i18n "guidanceNotAnalyzed"
                    ]
                ]
    in
    column
        (CommonToolStyles.toolContentBoxStyle settings)
        [ guidanceText
        , if options.analyzed then
            let
                offset =
                    Length.inMeters options.centreLineOffset

                radius =
                    Length.inMeters options.minimumRadiusAtPlaces

                clearRouteButton =
                    Input.button neatToolsBorder
                        { onPress = Nothing --Just (wrapper ClearRoute)
                        , label = i18n "clear"
                        }

                revertButton =
                    Input.button neatToolsBorder
                        { onPress = Nothing --Just (wrapper RevertToTrack)
                        , label = i18n "revert"
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

                undoButton =
                    if not <| List.isEmpty options.graphUndos then
                        row [ spacing 3 ]
                            [ Input.button
                                neatToolsBorder
                                { onPress = Nothing --Just (wrapper UndoDeleteRoad)
                                , label = i18n "undo"
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
                        { onPress = Nothing

                        --Just <|
                        --    wrapper <|
                        --        HighlightTraversal <|
                        --            min (List.length traversals - 1) (options.selectedTraversal + 1)
                        , label = useIconWithSize 16 FeatherIcons.chevronRight
                        }

                traversalPrevious =
                    Input.button neatToolsBorder
                        { onPress = Nothing

                        --Just <|
                        --    wrapper <|
                        --        HighlightTraversal <|
                        --            max 0 (options.selectedTraversal - 1)
                        , label = useIconWithSize 16 FeatherIcons.chevronLeft
                        }
            in
            column [ width fill, padding 4, spacing 10 ]
                [ row [ centerX, width fill, spacing 10 ]
                    [ traversalPrevious
                    , traversalNext
                    , clearRouteButton
                    , revertButton
                    ]

                --, traversalsTable
                , undoButton
                , wrappedRow [ spacing 5 ] [ offsetSlider, minRadiusSlider ]
                , finishButton
                ]

          else
            let
                analyseButton =
                    row [ spacing 3, width fill ]
                        [ infoButton (wrapper <| DisplayInfo toolId "info")
                        , Input.button neatToolsBorder
                            { onPress = Nothing --Just (wrapper GraphAnalyse)
                            , label = i18n "find"
                            }
                        ]

                adoptTrackButton =
                    row [ spacing 3, width fill ]
                        [ infoButton (wrapper <| DisplayInfo toolId "adoptInfo")
                        , Input.button neatToolsBorder
                            { onPress = Nothing --Just (wrapper AdoptNewTrack)
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
                            , min = 0.0
                            , max = 5.0
                            , step = Nothing
                            , value = Length.inMeters options.matchingTolerance
                            , thumb = Input.defaultThumb
                            }
                        ]
            in
            column [ centerX, width fill, spacing 10 ]
                [ toleranceSlider
                , analyseButton

                --, adoptTrackButton
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
    in
    { options
        | tracks = trackWithUnambiguousName :: options.tracks
        , nextTrackNumber = options.nextTrackNumber + 1
        , activeTrackIndex = Just 0
        , commonReferenceGPX =
            case options.commonReferenceGPX of
                Just common ->
                    Just common

                Nothing ->
                    Just <| TrackLoaded.getReferencePoint track
        , graph = Graph.addEdge trackWithUnambiguousName options.graph
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
        ( clusters, enhancedTracks ) =
            ( [], [] )

        --Return only the clusters. Wait for button click.
        --Graph.identifyPointsToBeMerged tolerance options.graph
        graphOptions =
            options.graphOptions

        newGraphOptions =
            { graphOptions
                | matchingTolerance = tolerance
                , clustersForPreview = clusters
            }

        newOptions =
            { options | graphOptions = newGraphOptions }
    in
    ( newOptions
    , [ makePreview newGraphOptions ]
    )


makePreview : GraphOptions msg -> Actions.ToolAction msg
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
