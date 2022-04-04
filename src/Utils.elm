module Utils exposing (..)

import Http exposing (Error(..))


reversingCons : List a -> List a -> List a
reversingCons xs ys =
    -- Use this for speed when order can be ignored.
    case ( xs, ys ) of
        ( [], _ ) ->
            ys

        ( _, [] ) ->
            xs

        ( x :: moreX, _ ) ->
            reversingCons moreX (x :: ys)


combineLists : List (List a) -> List a
combineLists lists =
    List.foldl reversingCons [] lists


errorToString : Http.Error -> String
errorToString error =
    case error of
        BadUrl url ->
            "The URL " ++ url ++ " was invalid"

        Timeout ->
            "Unable to reach the server, try again"

        NetworkError ->
            "Unable to reach the server"

        BadStatus 504 ->
            "The Overpass server is busy; land use data not available"

        BadStatus 500 ->
            "The server had a problem, try again later"

        BadStatus 400 ->
            "Error 400 trying to fetch land use data"

        BadStatus _ ->
            "Unknown error"

        BadBody errorMessage ->
            errorMessage
