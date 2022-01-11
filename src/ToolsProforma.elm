module ToolsProforma exposing (..)

import AbruptDirectionChanges
import Color exposing (Color)
import DomainModel exposing (PeteTree)
import Element exposing (..)
import Element.Background as Background exposing (color)
import Element.Border as Border exposing (roundEach)
import Element.Font as Font
import Element.Input as Input
import FeatherIcons
import FlatColors.AussiePalette
import FlatColors.ChinesePalette
import FlatColors.SwedishPalette
import Json.Encode as E
import LocalCoords exposing (LocalCoords)
import Scene3d exposing (Entity)
import TrackInfoBox
import ViewPureStyles exposing (contrastingColour, useIcon)


type ToolState
    = Expanded
    | Contracted
    | Disabled


type ToolDock
    = DockUpperLeft
    | DockLowerLeft
    | DockUpperRight
    | DockLowerRight
    | DockBottom
    | DockNone


type ToolType
    = ToolTrackInfo
    | AbruptDirectionChanges


type ToolMsg
    = ToolPopupToggle ToolType
    | ToolDockSelect ToolType ToolDock
    | ToolColourSelect ToolType Element.Color
    | ToolStateToggle ToolType ToolState
    | DirectionChanges AbruptDirectionChanges.Msg


type alias ToolEntry =
    { toolType : ToolType
    , label : String
    , info : String
    , video : Maybe String
    , state : ToolState
    , dock : ToolDock
    , tabColour : Element.Color
    , textColour : Element.Color
    , isPopupOpen : Bool
    }


tools : List ToolEntry
tools =
    -- One list or five, or six? Try one. Arguably a Dict but POITROAE.
    [ trackInfoBox
    , abruptDirectionChanges
    ]


trackInfoBox : ToolEntry
trackInfoBox =
    { toolType = ToolTrackInfo
    , label = "Summary info"
    , info = "Here is some useful information"
    , video = Nothing
    , state = Expanded
    , dock = DockUpperLeft
    , tabColour = FlatColors.AussiePalette.beekeeper
    , textColour = contrastingColour FlatColors.AussiePalette.beekeeper
    , isPopupOpen = False
    }


abruptDirectionChanges : ToolEntry
abruptDirectionChanges =
    { toolType = AbruptDirectionChanges
    , label = "Direction changes"
    , info = "These may need smoothing"
    , video = Nothing
    , state = Contracted
    , dock = DockUpperRight
    , tabColour = FlatColors.AussiePalette.spicedNectarine
    , textColour = contrastingColour FlatColors.AussiePalette.spicedNectarine
    , isPopupOpen = False
    }


toggleToolPopup : ToolType -> ToolEntry -> ToolEntry
toggleToolPopup toolType tool =
    if tool.toolType == toolType then
        { tool | isPopupOpen = not tool.isPopupOpen }

    else
        tool


setToolState : ToolType -> ToolState -> ToolEntry -> ToolEntry
setToolState toolType state tool =
    if tool.toolType == toolType then
        { tool | state = state }

    else
        tool


nextToolState : ToolState -> ToolState
nextToolState state =
    case state of
        Expanded ->
            Contracted

        Contracted ->
            Expanded

        Disabled ->
            Disabled


setDock : ToolType -> ToolDock -> ToolEntry -> ToolEntry
setDock toolType dock tool =
    if tool.toolType == toolType then
        { tool | dock = dock }

    else
        tool


setColour : ToolType -> Element.Color -> ToolEntry -> ToolEntry
setColour toolType colour tool =
    if tool.toolType == toolType then
        { tool
            | tabColour = colour
            , textColour = contrastingColour colour
        }

    else
        tool


update :
    ToolMsg
    -> (ToolMsg -> msg)
    ->
        { model
            | tools : List ToolEntry
            , trackTree : Maybe PeteTree
            , directionChangeOptions : AbruptDirectionChanges.Options
        }
    ->
        ( { model
            | tools : List ToolEntry
            , trackTree : Maybe PeteTree
            , directionChangeOptions : AbruptDirectionChanges.Options
          }
        , Cmd msg
        )
update toolMsg msgWrapper model =
    case toolMsg of
        ToolPopupToggle toolType ->
            ( { model | tools = List.map (toggleToolPopup toolType) model.tools }
            , Cmd.none
            )

        ToolDockSelect toolType toolDock ->
            ( { model | tools = List.map (setDock toolType toolDock) model.tools }
            , Cmd.none
            )

        ToolColourSelect toolType color ->
            ( { model | tools = List.map (setColour toolType color) model.tools }
            , Cmd.none
            )

        ToolStateToggle toolType newState ->
            -- Record the new state, but also let the tool know!
            { model | tools = List.map (setToolState toolType newState) model.tools }
                |> toolStateHasChanged toolType newState

        DirectionChanges msg ->
            -- Delegate to tool here...
            ( model, Cmd.none )


refreshAllTools :
    { model
        | tools : List ToolEntry
        , trackTree : Maybe PeteTree
        , directionChangeOptions : AbruptDirectionChanges.Options
    }
    ->
        { model
            | tools : List ToolEntry
            , trackTree : Maybe PeteTree
            , directionChangeOptions : AbruptDirectionChanges.Options
        }
refreshAllTools model =
    -- Track, or something has changed; tool data is stale.
    -- Same impact as tools being opened, so we'll re-use that.
    -- The discarding of cmds here is questionable.
    let
        refreshOneTool entry ( updatedModel, _ ) =
            if entry.state == Expanded then
                toolStateHasChanged entry.toolType Expanded updatedModel

            else
                ( updatedModel, Cmd.none )

        ( finalModel, _ ) =
            model.tools |> List.foldl refreshOneTool ( model, Cmd.none )
    in
    finalModel


toolStateHasChanged :
    ToolType
    -> ToolState
    ->
        { model
            | tools : List ToolEntry
            , trackTree : Maybe PeteTree
            , directionChangeOptions : AbruptDirectionChanges.Options
        }
    ->
        ( { model
            | tools : List ToolEntry
            , trackTree : Maybe PeteTree
            , directionChangeOptions : AbruptDirectionChanges.Options
          }
        , Cmd msg
        )
toolStateHasChanged toolType newState model =
    case toolType of
        ToolTrackInfo ->
            ( model, Cmd.none )

        AbruptDirectionChanges ->
            case ( newState, model.trackTree ) of
                ( Expanded, Just treeNode ) ->
                    ( { model
                        | directionChangeOptions =
                            AbruptDirectionChanges.findAbruptDirectionChanges
                                model.directionChangeOptions
                                treeNode
                      }
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )



--View stuff


toolsForDock :
    ToolDock
    -> (ToolMsg -> msg)
    ->
        { model
            | tools : List ToolEntry
            , trackTree : Maybe PeteTree
            , directionChangeOptions : AbruptDirectionChanges.Options
        }
    -> Element msg
toolsForDock dock msgWrapper model =
    column [] <|
        (model.tools
            |> List.filter (\tool -> tool.dock == dock)
            |> List.map (viewTool msgWrapper model)
        )


viewTool :
    (ToolMsg -> msg)
    ->
        { model
            | trackTree : Maybe PeteTree
            , directionChangeOptions : AbruptDirectionChanges.Options
        }
    -> ToolEntry
    -> Element msg
viewTool msgWrapper model toolEntry =
    column
        [ width fill
        , spacing 0
        , Border.width 2
        , Border.color toolEntry.tabColour
        , Border.rounded 8
        , inFront <|
            column
                [ alignRight
                , moveDown 26
                ]
                [ showDockOptions msgWrapper toolEntry
                , showColourOptions msgWrapper toolEntry
                ]
        ]
        [ row
            [ width fill
            , spacing 8
            , padding 4
            , Background.color toolEntry.tabColour
            , Font.color toolEntry.textColour
            ]
            [ Input.button [ centerX ]
                { onPress =
                    Just <|
                        msgWrapper <|
                            ToolStateToggle toolEntry.toolType <|
                                nextToolState toolEntry.state
                , label = text toolEntry.label
                }
            , Input.button [ alignRight ]
                { onPress = Just <| msgWrapper <| ToolPopupToggle toolEntry.toolType
                , label = useIcon FeatherIcons.settings
                }
            ]
        , if toolEntry.state == Expanded then
            viewToolByType msgWrapper toolEntry model

          else
            none
        ]


showDockOptions : (ToolMsg -> msg) -> ToolEntry -> Element msg
showDockOptions msgWrapper toolEntry =
    if toolEntry.isPopupOpen then
        row
            [ Background.color FlatColors.ChinesePalette.antiFlashWhite
            , Border.color FlatColors.ChinesePalette.bruschettaTomato
            , Border.rounded 4
            , Border.width 2
            ]
            [ Input.button []
                { onPress = Just <| msgWrapper <| ToolDockSelect toolEntry.toolType DockUpperLeft
                , label = useIcon FeatherIcons.arrowUpLeft
                }
            , Input.button
                []
                { onPress = Just <| msgWrapper <| ToolDockSelect toolEntry.toolType DockLowerLeft
                , label = useIcon FeatherIcons.arrowDownLeft
                }
            , Input.button
                []
                { onPress = Just <| msgWrapper <| ToolDockSelect toolEntry.toolType DockBottom
                , label = useIcon FeatherIcons.arrowDown
                }
            , Input.button
                []
                { onPress = Just <| msgWrapper <| ToolDockSelect toolEntry.toolType DockLowerRight
                , label = useIcon FeatherIcons.arrowDownRight
                }
            , Input.button
                []
                { onPress = Just <| msgWrapper <| ToolDockSelect toolEntry.toolType DockUpperRight
                , label = useIcon FeatherIcons.arrowUpRight
                }
            ]

    else
        none


showColourOptions : (ToolMsg -> msg) -> ToolEntry -> Element msg
showColourOptions msgWrapper toolEntry =
    let
        colourBlock colour =
            Input.button
                [ Background.color colour, width <| px 20, height <| px 20 ]
                { label = none
                , onPress = Just <| msgWrapper <| ToolColourSelect toolEntry.toolType colour
                }
    in
    if toolEntry.isPopupOpen then
        column
            [ alignRight
            , Border.color FlatColors.ChinesePalette.bruschettaTomato
            , Border.rounded 4
            , Border.width 2
            ]
            [ row []
                [ colourBlock FlatColors.SwedishPalette.highlighterPink
                , colourBlock FlatColors.SwedishPalette.darkPeriwinkle
                , colourBlock FlatColors.SwedishPalette.megaman
                , colourBlock FlatColors.SwedishPalette.freshTurquoise
                , colourBlock FlatColors.SwedishPalette.mintyGreen
                ]
            , row []
                [ colourBlock FlatColors.SwedishPalette.sizzlingRed
                , colourBlock FlatColors.SwedishPalette.freeSpeechBlue
                , colourBlock FlatColors.SwedishPalette.spiroDiscoBall
                , colourBlock FlatColors.SwedishPalette.jadeDust
                , colourBlock FlatColors.SwedishPalette.greenTeal
                ]
            , row []
                [ colourBlock FlatColors.SwedishPalette.narenjiOrange
                , colourBlock FlatColors.SwedishPalette.yrielYellow
                , colourBlock FlatColors.SwedishPalette.sunsetOrange
                , colourBlock FlatColors.SwedishPalette.hintOfElusiveBlue
                , colourBlock FlatColors.SwedishPalette.goodNight
                ]
            , row []
                [ colourBlock FlatColors.SwedishPalette.chromeYellow
                , colourBlock FlatColors.SwedishPalette.vibrantYellow
                , colourBlock FlatColors.SwedishPalette.redOrange
                , colourBlock FlatColors.SwedishPalette.londonSquare
                , colourBlock FlatColors.SwedishPalette.blackPearl
                ]
            ]

    else
        none


viewToolByType :
    (ToolMsg -> msg)
    -> ToolEntry
    ->
        { model
            | trackTree : Maybe PeteTree
            , directionChangeOptions : AbruptDirectionChanges.Options
        }
    -> Element msg
viewToolByType msgWrapper entry model =
    case entry.toolType of
        ToolTrackInfo ->
            TrackInfoBox.trackInfoBox model.trackTree

        AbruptDirectionChanges ->
            AbruptDirectionChanges.view (msgWrapper << DirectionChanges) model.directionChangeOptions
