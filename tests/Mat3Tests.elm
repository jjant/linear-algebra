module Mat3Tests exposing (all)

import Expect exposing (Expectation, FloatingPointTolerance(..))
import Fuzz exposing (Fuzzer)
import Mat3
import Test exposing (Test, describe, test)
import Vec2 exposing (Vec2, vec2)
import Vec2Tests
import Vec3 exposing (vec3)


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
                    |> Vec2Tests.compareVec2 (vec2 10 24)
        , test "Mat3.translate affects points" <|
            \_ ->
                let
                    m =
                        Mat3.translate (vec2 2 3)
                in
                Mat3.transformPoint m (vec2 5 8)
                    |> Vec2Tests.compareVec2 (vec2 7 11)
        , test "Mat3.translate doesn't affect vectors" <|
            \_ ->
                let
                    m =
                        Mat3.translate (vec2 2 3)
                in
                Mat3.transformVector m (vec2 5 8)
                    |> Vec2Tests.compareVec2 (vec2 5 8)
        , describe "tranpose"
            [ test "transpose identity is identity" <|
                \_ ->
                    Mat3.identity
                        |> Mat3.transpose
                        |> Expect.equal Mat3.identity
            , test "transpose of a specific matrix" <|
                \_ ->
                    Mat3.fromRows
                        (vec3 5 25 1)
                        (vec3 9 43 4)
                        (vec3 9 1 0)
                        |> Mat3.transpose
                        |> Expect.equal
                            (Mat3.fromRows
                                (vec3 5 9 9)
                                (vec3 25 43 1)
                                (vec3 1 4 0)
                            )
            ]
        , mulTests
        , invertTests
        , lookAtTests
        , viewportTests
        , transformPointTests
        , transformVectorTests
        ]


mulTests : Test
mulTests =
    describe "Mat3.mul"
        [ test "Composed with point" <|
            \_ ->
                let
                    mat =
                        Mat3.mul (Mat3.rotate (Basics.pi / 4)) (Mat3.scale (vec2 2 2))
                in
                Mat3.transformPoint mat (vec2 2 6)
                    |> Expect.equal (Vec2.rotate (pi / 4) (Vec2.scale 2 (vec2 2 6)))
        , test "Composed with point 2" <|
            \_ ->
                let
                    mat =
                        Mat3.mul (Mat3.translate (vec2 1 5))
                            (Mat3.mul (Mat3.rotate (Basics.pi / 4)) (Mat3.scale (vec2 2 2)))
                in
                Mat3.transformPoint mat (vec2 2 6)
                    |> Expect.equal (Vec2.add (vec2 1 5) (Vec2.rotate (pi / 4) (Vec2.scale 2 (vec2 2 6))))
        , test "Composed with vector" <|
            \_ ->
                let
                    mat =
                        Mat3.mul (Mat3.rotate (Basics.pi / 4)) (Mat3.scale (vec2 2 2))
                in
                Mat3.transformVector mat (vec2 2 6)
                    |> Expect.equal (Vec2.rotate (pi / 4) (Vec2.scale 2 (vec2 2 6)))
        , test "Composed with vector 2" <|
            \_ ->
                let
                    mat =
                        Mat3.mul (Mat3.rotate (Basics.pi / 4)) (Mat3.scale (vec2 2 2))
                in
                Mat3.transformVector mat (vec2 2 6)
                    |> Expect.equal (Vec2.rotate (pi / 4) (Vec2.scale 2 (vec2 2 6)))
        ]


invertTests : Test
invertTests =
    describe "Mat3.invert"
        [ test "identity" <|
            \_ ->
                Mat3.identity
                    |> Mat3.invert
                    |> Expect.equal (Just Mat3.identity)
        , test "translate" <|
            \_ ->
                Mat3.translate (vec2 1 0)
                    |> Mat3.invert
                    |> Maybe.map (\invM -> Mat3.transformPoint invM (vec2 2 25))
                    |> Expect.equal (Just (vec2 1 25))
        ]


lookAtTests : Test
lookAtTests =
    describe "Mat3.lookAt"
        [ test "centerOfAttention becomes origin" <|
            \_ ->
                Mat3.transformPoint
                    (Mat3.lookAt
                        { centerOfAttention = vec2 5 5, upDirection = vec2 1 0 }
                    )
                    (vec2 5 5)
                    |> Expect.equal (vec2 0 0)
        , test "Transforms a known point" <|
            \_ ->
                Mat3.transformPoint
                    (Mat3.lookAt
                        { centerOfAttention = vec2 5 5, upDirection = vec2 1 0 }
                    )
                    (vec2 6 5)
                    |> Expect.equal (vec2 0 1)
        , test "Transforms a known point2" <|
            \_ ->
                Mat3.transformPoint
                    (Mat3.lookAt
                        { centerOfAttention = vec2 5 5, upDirection = vec2 0 1 }
                    )
                    (vec2 6 5)
                    |> Expect.equal (vec2 1 0)
        ]


viewportTests : Test
viewportTests =
    describe "Mat3.viewport"
        [ test "Origin 1080p" <|
            \_ ->
                Mat3.transformPoint (Mat3.viewport { width = 1920, height = 1080 }) (vec2 0 0)
                    |> Expect.equal (vec2 (1920 / 2) (1080 / 2))
        , test "Upper right corner 1080p" <|
            \_ ->
                Mat3.transformPoint (Mat3.viewport { width = 1920, height = 1080 }) (vec2 1 1)
                    |> Expect.equal (vec2 1920 0)
        , test "Lower right corner 1080p" <|
            \_ ->
                Mat3.transformPoint (Mat3.viewport { width = 1920, height = 1080 }) (vec2 1 -1)
                    |> Expect.equal (vec2 1920 1080)
        , test "Upper left corner 1080p" <|
            \_ ->
                Mat3.transformPoint (Mat3.viewport { width = 1920, height = 1080 }) (vec2 -1 1)
                    |> Expect.equal (vec2 0 0)
        , test "Lower left corner 1080p" <|
            \_ ->
                Mat3.transformPoint (Mat3.viewport { width = 1920, height = 1080 }) (vec2 -1 -1)
                    |> Expect.equal (vec2 0 1080)
        ]


transformPointTests : Test
transformPointTests =
    describe "Mat3.transformPoint"
        [ test "Translate" <|
            \_ ->
                Mat3.transformPoint (Mat3.translate (vec2 1 1)) (vec2 0 0)
                    |> Expect.equal (vec2 1 1)
        , test "Translate2" <|
            \_ ->
                Mat3.transformPoint (Mat3.translate (vec2 1 5)) (vec2 2 6)
                    |> Expect.equal (vec2 3 11)
        , test "Scale" <|
            \_ ->
                Mat3.transformPoint (Mat3.scale (vec2 1 5)) (vec2 2 6)
                    |> Expect.equal (vec2 2 30)
        , test "Scale 2" <|
            \_ ->
                Mat3.transformPoint (Mat3.scale (vec2 0 0)) (vec2 2 6)
                    |> Expect.equal (vec2 0 0)
        , test "Rotate" <|
            \_ ->
                Mat3.transformPoint (Mat3.rotate (Basics.pi / 2)) (vec2 2 6)
                    |> Expect.equal (Vec2.rotate (pi / 2) (vec2 2 6))
        ]


transformVectorTests : Test
transformVectorTests =
    Test.describe "Mat3.transformVector"
        [ test "Translate" <|
            \_ ->
                Mat3.transformVector (Mat3.translate (vec2 1 1)) (vec2 0 0)
                    |> Expect.equal (vec2 0 0)
        , test "Translate2" <|
            \_ ->
                Mat3.transformVector (Mat3.translate (vec2 1 5)) (vec2 2 6)
                    |> Expect.equal (vec2 2 6)
        , test "Scale" <|
            \_ ->
                Mat3.transformVector (Mat3.scale (vec2 1 5)) (vec2 2 6)
                    |> Expect.equal (vec2 2 30)
        , test "Scale 2" <|
            \_ ->
                Mat3.transformVector (Mat3.scale (vec2 0 0)) (vec2 2 6)
                    |> Expect.equal (vec2 0 0)
        , test "Rotate" <|
            \_ ->
                Mat3.transformVector (Mat3.rotate (Basics.pi / 2)) (vec2 2 6)
                    |> Expect.equal (Vec2.rotate (pi / 2) (vec2 2 6))
        , test "Translation doesn't affect point" <|
            \_ ->
                let
                    mat =
                        Mat3.mul (Mat3.translate (vec2 0 5)) Mat3.identity
                in
                Mat3.transformVector mat (vec2 2 5)
                    |> Expect.equal (vec2 2 5)
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
