module Tools.OutAndBackOptions exposing (Options)

-- Putting these in a separate module means we can use with Action, without an import loop.


type alias Options =
    { offset : Float }
