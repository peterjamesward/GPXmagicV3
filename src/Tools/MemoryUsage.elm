module Tools.MemoryUsage exposing (..)


type alias HeapStatus =
    { jsHeapSizeLimit : Int
    , totalJSHeapSize : Int
    , usedJSHeapSize : Int
    }

