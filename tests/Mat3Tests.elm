module Mat3Tests exposing (all)

import Expect exposing (Expectation, FloatingPointTolerance(..))
import Fuzz exposing (Fuzzer)
import Mat3
import Test exposing (Test, describe, test)
import Vec2 exposing (Vec2, vec2)
import Vec2Tests


all : Test
all =
    let
        tolerance =
            0.000001
    in
    describe "Mat3"
        [ Test.fuzz rotateTestFuzzer "rotate vector adds angle to it" <|
            \( vec, rotateAngle ) ->
                Mat3.transformPoint (Mat3.rotate rotateAngle) vec
                    |> Vec2.angle
                    |> angleWithin tolerance (Vec2.angle vec + rotateAngle)
        , Test.fuzz (Fuzz.map2 Tuple.pair vectorFuzzer vectorFuzzer) "Scale" <|
            \( vec, scaleVector ) ->
                Mat3.transformPoint (Mat3.scale scaleVector) vec
                    |> Expect.all
                        [ \res -> Expect.within (Absolute tolerance) (vec.x * scaleVector.x) res.x
                        , \res -> Expect.within (Absolute tolerance) (vec.y * scaleVector.y) res.y
                        ]
        , test "Mat3.scale" <|
            \_ ->
                let
                    m =
                        Mat3.scale (vec2 2 3)
                in
                Mat3.transformPoint m (vec2 5 8)
                    |> Vec2Tests.within veryClose (vec2 10 24)
        , test "Mat3.translate affects points" <|
            \_ ->
                let
                    m =
                        Mat3.translate (vec2 2 3)
                in
                Mat3.transformPoint m (vec2 5 8)
                    |> Vec2Tests.within veryClose (vec2 7 11)
        , test "Mat3.translate doesn't affect vectors" <|
            \_ ->
                let
                    m =
                        Mat3.translate (vec2 2 3)
                in
                Mat3.transformVector m (vec2 5 8)
                    |> Vec2Tests.within veryClose (vec2 5 8)
        ]


veryClose : FloatingPointTolerance
veryClose =
    AbsoluteOrRelative 0.0001 0.0001


angleWithin : Float -> Float -> Float -> Expectation
angleWithin tolerance angleExpected angleActual =
    let
        angleDelta =
            (angleActual - angleExpected)
                |> fmodBy (2 * Basics.pi)

        condition =
            (abs angleDelta < tolerance)
                || (abs (angleDelta - 2 * Basics.pi) < tolerance)
    in
    Expect.true "Angles are not close enough" condition


rotateTestFuzzer : Fuzzer ( Vec2, Float )
rotateTestFuzzer =
    Fuzz.map2 Tuple.pair
        unitVectorFuzzer
        angleFuzzer


angleFuzzer : Fuzzer Float
angleFuzzer =
    Fuzz.floatRange 0 (2 * Basics.pi)


unitVectorFuzzer : Fuzzer Vec2
unitVectorFuzzer =
    angleFuzzer
        |> Fuzz.map (\angle -> Vec2.vec2 (cos angle) (sin angle))


vectorFuzzer : Fuzzer Vec2
vectorFuzzer =
    Fuzz.map2 (\unitVec len -> Vec2.scale len unitVec)
        unitVectorFuzzer
        (Fuzz.floatRange 0 10)


fmodBy : Float -> Float -> Float
fmodBy mod value =
    value - mod * toFloat (floor (value / mod))
