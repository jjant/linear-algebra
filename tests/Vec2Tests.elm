module Vec2Tests exposing (..)

import Expect exposing (Expectation, FloatingPointTolerance)
import Fuzz exposing (Fuzzer)
import Vec2 exposing (Vec2, vec2)


within : FloatingPointTolerance -> Vec2 -> Vec2 -> Expectation
within fp v1 v2 =
    Expect.all
        [ \_ -> Expect.within fp v1.x v2.x
        , \_ -> Expect.within fp v1.y v2.y
        ]
        ()


fuzzer : Fuzzer Vec2
fuzzer =
    Fuzz.map2 vec2
        Fuzz.float
        Fuzz.float
