module Mat3Tests exposing (all)

import Expect exposing (Expectation, FloatingPointTolerance(..))
import Fuzz exposing (Fuzzer)
import Mat3
import Test exposing (Test, describe)
import Vec2 exposing (Vec2)


all : Test
all =
    let
        tolerance =
            0.000001
    in
    describe "Mat3"
        [ Test.fuzz rotateTestFuzzer "rotate vector adds angle to it" <|
            \( vec, rotateAngle ) ->
                Mat3.transformPoint vec (Mat3.rotate rotateAngle)
                    |> Vec2.angle
                    |> angleWithin tolerance (Vec2.angle vec + rotateAngle)
        , Test.fuzz (Fuzz.map2 Tuple.pair vectorFuzzer vectorFuzzer) "Scale" <|
            \( vec, scaleVector ) ->
                Mat3.scale scaleVector
                    |> Mat3.transformPoint vec
                    |> Expect.all
                        [ \res -> Expect.within (Absolute tolerance) (vec.x * scaleVector.x) res.x
                        , \res -> Expect.within (Absolute tolerance) (vec.y * scaleVector.y) res.y
                        ]
        ]


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
