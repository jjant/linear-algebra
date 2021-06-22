module Tests exposing (all)

import Expect exposing (FloatingPointTolerance(..))
import Fuzz exposing (Fuzzer)
import Test exposing (Test, describe)
import Vec2


all : Test
all =
    describe "all"
        [ Test.fuzz vecFuzzer "dot with itself is length squared" <|
            \vec ->
                Vec2.dot vec vec
                    |> Expect.within (Absolute 0.000000001) (Vec2.lengthSquared vec)
        ]


vecFuzzer : Fuzzer Vec2.Vec2
vecFuzzer =
    let
        bigNumber =
            1.0e6
    in
    Fuzz.map2 Vec2.vec2
        (Fuzz.floatRange -bigNumber bigNumber)
        (Fuzz.floatRange -bigNumber bigNumber)
