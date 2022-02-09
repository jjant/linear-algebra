module Mat2Tests exposing (..)

import Expect exposing (Expectation, FloatingPointTolerance(..))
import Fuzz
import Mat2 exposing (Mat2)
import Test exposing (Test, describe, test)
import Vec2 exposing (vec2)
import Vec2Tests


mulTests : Test
mulTests =
    describe "Mat2.mul"
        [ test "it works" <|
            \_ ->
                Mat2.mul (Mat2 1 2 3 4) (Mat2 5 6 0 7)
                    |> Expect.equal (Mat2 5 20 15 46)
        , test "it works2" <|
            \_ ->
                Mat2.mul (Mat2 2 4 1 4) (Mat2 1 4 1 3)
                    |> Expect.equal (Mat2 6 20 5 16)
        ]


transformTests : Test
transformTests =
    describe "Mat2.transform"
        [ test "it works" <|
            \_ ->
                Mat2.transform (Mat2.rotate pi) (vec2 1 0)
                    |> Vec2Tests.compareVec2 (vec2 -1 0)
        , Test.fuzz (Fuzz.map2 Tuple.pair Vec2Tests.vec2Fuzzer Vec2Tests.vec2Fuzzer) "scale" <|
            \( scaler, scaled ) ->
                Mat2.transform (Mat2.scale scaler) scaled
                    |> Vec2Tests.compareVec2 (vec2 (scaled.x * scaler.x) (scaled.y * scaler.y))
        ]


invertTests : Test
invertTests =
    describe "Mat2.invert"
        [ Test.fuzz Fuzz.float "it works" <|
            \angle ->
                Mat2.rotate angle
                    |> Mat2.invert
                    |> Maybe.map (compare veryClose (Mat2.rotate -angle))
                    |> Maybe.withDefault (Expect.fail "Not invertable matrix")
        , test "it works2" <|
            \_ ->
                Mat2 3 1 4 2
                    |> Mat2.invert
                    |> Maybe.map (compare veryClose (Mat2 1 (-1 / 2) -2 (3 / 2)))
                    |> Maybe.withDefault (Expect.fail "Not invertable matrix")
        ]


detTests : Test
detTests =
    describe "Mat2.det"
        [ test "it works" <|
            \_ ->
                Mat2 1 2 3 4
                    |> Mat2.det
                    |> Expect.within veryClose -2
        ]


veryClose : FloatingPointTolerance
veryClose =
    AbsoluteOrRelative 0.0001 0.0001


compare : FloatingPointTolerance -> Mat2 -> Mat2 -> Expectation
compare fp a b =
    Expect.all
        [ \_ -> Expect.within fp a.m11 b.m11
        , \_ -> Expect.within fp a.m12 b.m12
        , \_ -> Expect.within fp a.m21 b.m21
        , \_ -> Expect.within fp a.m22 b.m22
        ]
        ()
