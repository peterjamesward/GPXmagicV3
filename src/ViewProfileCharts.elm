module ViewProfileCharts exposing
    ( ClickZone(..)
    , initialiseView
    , update
    , view
    )

import Actions exposing (ToolAction(..))
import Dict exposing (Dict)
import DomainModel exposing (..)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import FeatherIcons
import FlatColors.AussiePalette
import FlatColors.ChinesePalette exposing (white)
import Html.Attributes exposing (id, style)
import Html.Events.Extra.Mouse as Mouse
import PaneContext exposing (PaneId, paneIdToString)
import Pixels exposing (Pixels, inPixels)
import Point2d exposing (Point2d, xCoordinate, yCoordinate)
import PreviewData exposing (PreviewData, PreviewShape(..))
import Quantity exposing (Quantity, toFloatQuantity)
import Rectangle2d
import TrackLoaded exposing (TrackLoaded)
import ViewProfileChartContext exposing (DragAction(..), Msg(..), ProfileContext)
import ViewPureStyles exposing (useIcon)


type ClickZone
    = ZoneGradient


stopProp =
    { stopPropagation = True, preventDefault = False }


zoomButtons : (Msg -> msg) -> ProfileContext -> Element msg
zoomButtons msgWrapper context =
    column
        [ alignTop
        , alignRight
        , moveDown 5
        , moveLeft 5
        , Background.color white
        , Font.size 40
        , padding 6
        , Element.spacing 8
        , Border.width 1
        , Border.rounded 4
        , Border.color FlatColors.AussiePalette.blurple
        , htmlAttribute <| Mouse.onWithOptions "click" stopProp (always ImageNoOp >> msgWrapper)
        , htmlAttribute <| Mouse.onWithOptions "dblclick" stopProp (always ImageNoOp >> msgWrapper)
        , htmlAttribute <| Mouse.onWithOptions "mousedown" stopProp (always ImageNoOp >> msgWrapper)
        , htmlAttribute <| Mouse.onWithOptions "mouseup" stopProp (always ImageNoOp >> msgWrapper)
        , htmlAttribute (style "z-index" "20")
        ]
        [ Input.button []
            { onPress = Just <| msgWrapper ImageZoomIn
            , label = useIcon FeatherIcons.plus
            }
        , Input.button []
            { onPress = Just <| msgWrapper ImageZoomOut
            , label = useIcon FeatherIcons.minus
            }
        , Input.button []
            { onPress = Just <| msgWrapper ImageReset
            , label = useIcon FeatherIcons.maximize
            }
        , Input.button []
            { onPress = Just <| msgWrapper ToggleFollowOrange
            , label =
                if context.followSelectedPoint then
                    useIcon FeatherIcons.lock

                else
                    useIcon FeatherIcons.unlock
            }
        ]


update :
    Msg
    -> (Msg -> msg)
    -> TrackLoaded msg
    -> ( Quantity Int Pixels, Quantity Int Pixels )
    -> Dict String PreviewData
    -> ProfileContext
    -> ( ProfileContext, List (ToolAction msg) )
update msg msgWrapper track ( givenWidth, givenHeight ) previews context =
    case msg of
        SetEmphasis emphasis ->
            ( { context | emphasis = toFloat emphasis }
            , []
            )

        ImageZoomIn ->
            let
                newContext =
                    { context | zoomLevel = clamp 0 10 <| context.zoomLevel + 0.5 }
            in
            ( newContext
            , [ Actions.RenderProfile newContext ]
            )

        ImageZoomOut ->
            let
                newContext =
                    { context | zoomLevel = clamp 0 10 <| context.zoomLevel - 0.5 }
            in
            ( newContext
            , [ Actions.RenderProfile newContext ]
            )

        ImageReset ->
            let
                newContext =
                    { context | zoomLevel = 0 }
            in
            ( newContext
            , [ Actions.RenderProfile newContext ]
            )

        ImageNoOp ->
            ( context, [] )

        ImageClick event ->
            -- Click moves pointer but does not re-centre view. (Double click will.)
            --if context.waitingForClickDelay then
            ( context
            , [ SetCurrent <| detectHit event track ( givenWidth, givenHeight ) context
              , TrackHasChanged
              ]
            )

        --else
        --    ( context, [] )
        ClickDelayExpired ->
            ( { context | waitingForClickDelay = False }, [] )

        ImageMouseWheel deltaY ->
            let
                maxZoom =
                    (logBase 2 <| toFloat <| skipCount track.trackTree) - 2

                increment =
                    -0.001 * deltaY

                zoomLevel =
                    clamp 0 maxZoom <|
                        context.zoomLevel
                            + increment
            in
            ( { context | zoomLevel = zoomLevel }, [] )

        ImageGrab event ->
            -- Mouse behaviour depends which view is in use...
            -- Right-click or ctrl-click to mean rotate; otherwise pan.
            let
                newContext =
                    { context
                        | dragAction = DragPan
                        , waitingForClickDelay = True
                    }
            in
            ( newContext
            , [ DelayMessage 250 (msgWrapper ClickDelayExpired) ]
            )

        ImageDrag event ->
            ( context, [] )

        ImageRelease _ ->
            ( context, [] )

        ToggleFollowOrange ->
            let
                currentDistance =
                    distanceFromIndex track.currentPosition track.trackTree
            in
            ( { context
                | followSelectedPoint = not context.followSelectedPoint
              }
            , []
            )

        ImageDoubleClick _ ->
            ( context, [] )

        MouseMove event ->
            ( { context | mouseEvent = Just event }, [] )


view :
    ProfileContext
    -> PaneId
    -> ( Quantity Int Pixels, Quantity Int Pixels )
    -> (Msg -> msg)
    -> Element msg
view context paneId ( givenWidth, givenHeight ) msgWrapper =
    -- We just declare the container for the profile canvas, the data are provided through a port.
    let
        tenPercentHeight =
            inPixels givenHeight // 10
    in
    column
        [ pointer
        , Background.color FlatColors.ChinesePalette.antiFlashWhite
        , inFront <| zoomButtons msgWrapper context
        ]
        [ el
            [ Element.width <| px <| inPixels givenWidth
            , Element.height <| px <| 7 * tenPercentHeight
            , alignLeft
            , alignTop
            , htmlAttribute (id <| "altitude" ++ paneIdToString paneId)
            ]
          <|
            el [ centerY, centerX ] (text "Altitude chart")
        , el
            [ Element.width <| px <| inPixels givenWidth
            , Element.height <| px <| 3 * tenPercentHeight
            , alignLeft
            , alignTop
            , Border.width 2
            , Border.color FlatColors.ChinesePalette.peace
            , htmlAttribute (id <| "gradient" ++ paneIdToString paneId)
            ]
          <|
            el [ centerY, centerX ] (text "Gradient chart")
        ]


initialiseView :
    String
    -> PeteTree
    -> Maybe ProfileContext
    -> ProfileContext
initialiseView suffix treeNode currentContext =
    case currentContext of
        Just context ->
            { context
                | dragAction = DragNone
                , zoomLevel = 0.0
                , defaultZoomLevel = 0.0
                , focalPoint = Quantity.half <| DomainModel.trueLength treeNode
                , metresPerPixel = 10.0
                , waitingForClickDelay = False
            }

        Nothing ->
            { contextSuffix = suffix
            , dragAction = DragNone
            , zoomLevel = 0.0
            , defaultZoomLevel = 0.0
            , focalPoint = Quantity.half <| DomainModel.trueLength treeNode
            , followSelectedPoint = False
            , metresPerPixel = 10.0
            , waitingForClickDelay = False
            , emphasis = 1.0
            , mouseEvent = Nothing
            }


detectHit :
    Mouse.Event
    -> TrackLoaded msg
    -> ( Quantity Int Pixels, Quantity Int Pixels )
    -> ProfileContext
    -> Int
detectHit event track ( w, h ) context =
    let
        ( x, y ) =
            event.offsetPos

        screenPoint =
            Point2d.pixels x y

        ( wFloat, hFloat ) =
            ( toFloatQuantity w, toFloatQuantity h )

        screenRectangle =
            Rectangle2d.from
                (Point2d.xy Quantity.zero hFloat)
                (Point2d.xy wFloat Quantity.zero)
    in
    track.currentPosition
