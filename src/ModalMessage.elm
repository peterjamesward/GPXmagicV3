module ModalMessage exposing (..)

import Model


displayInfo : String -> String -> Model.Model -> Model.Model
displayInfo tool text outerModel =
    { outerModel
        | infoText =
            case outerModel.infoText of
                Just ( isTool, isText ) ->
                    if tool == isTool && text == isText then
                        Nothing

                    else
                        Just ( tool, text )

                Nothing ->
                    Just ( tool, text )
    }
