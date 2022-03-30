module Utils exposing (..)


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
