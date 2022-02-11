module Tools.MemoryUsage exposing (..)

{-
   In Chrome, performance.memory gives
   MemoryInfo {totalJSHeapSize: 3793082688, usedJSHeapSize: 3563533096, jsHeapSizeLimit: 4294705152}
-}


type alias HeapStatus =
    { jsHeapSizeLimit : Int
    , totalJSHeapSize : Int
    , usedJSHeapSize : Int
    }
