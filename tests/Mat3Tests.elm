module Mat3Tests exposing (all)

import Expect exposing (Expectation, FloatingPointTolerance(..))
import Fuzz exposing (Fuzzer)
import Mat3 exposing (Mat3)
import Test exposing (Test, describe, fuzz, test)
import Util exposing (comparePrecision)
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
        , addTests
        , mulTests
        , invertTests
        , lookAtTests
        , orthographicTests
        , viewportTests
        , transformPointTests
        , transformVectorTests
        ]


addTests : Test
addTests =
    describe "Mat3.add"
        [ fuzz mat3Fuzzer "0 + m == m" <|
            \m ->
                let
                    zero =
                        Mat3.fromRows
                            (vec3 0 0 0)
                            (vec3 0 0 0)
                            (vec3 0 0 0)
                in
                Mat3.add m zero
                    |> Expect.equal m
        , test "adding known matrices" <|
            \_ ->
                Mat3.add
                    (Mat3.fromRows (vec3 1 2 3) (vec3 3 4 5) (vec3 -1 -2 -3))
                    (Mat3.fromRows (vec3 5 -1 -10) (vec3 28 0.1 0) (vec3 0 0 0))
                    |> compare
                        (Mat3.fromRows
                            (vec3 6 1 -7)
                            (vec3 31 4.1 5)
                            (vec3 -1 -2 -3)
                        )
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
        , test "non-invertible matrix" <|
            \_ ->
                Mat3.fromRows
                    (vec3 1 2 3)
                    (vec3 4 5 6)
                    (vec3 0 0 0)
                    |> Mat3.invert
                    |> Expect.equal Nothing
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


orthographicTests : Test
orthographicTests =
    describe "Mat3.orthographic"
        [ test "translate point on right edge to +1" <|
            \_ ->
                let
                    width =
                        500

                    height =
                        200
                in
                vec2 (width / 2) 37
                    |> Mat3.transformPoint (Mat3.orthographic { width = width, height = height })
                    |> .x
                    |> Util.compareFloat 1
        , test "translate point on left edge to -1" <|
            \_ ->
                let
                    width =
                        500

                    height =
                        200
                in
                vec2 -(width / 2) 37
                    |> Mat3.transformPoint (Mat3.orthographic { width = width, height = height })
                    |> .x
                    |> Util.compareFloat -1
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


compare : Mat3 -> Mat3 -> Expectation
compare =
    compareCustom 0.000001


compareCustom : Float -> Mat3 -> Mat3 -> Expectation
compareCustom fp a b =
    Expect.all
        [ \_ -> comparePrecision fp a.m11 b.m11
        , \_ -> comparePrecision fp a.m12 b.m12
        , \_ -> comparePrecision fp a.m13 b.m13
        , \_ -> comparePrecision fp a.m21 b.m21
        , \_ -> comparePrecision fp a.m22 b.m22
        , \_ -> comparePrecision fp a.m23 b.m23
        , \_ -> comparePrecision fp a.m31 b.m31
        , \_ -> comparePrecision fp a.m32 b.m32
        , \_ -> comparePrecision fp a.m33 b.m33
        ]
        ()


mat3Fuzzer : Fuzz.Fuzzer Mat3
mat3Fuzzer =
    Fuzz.constant Mat3
        |> Fuzz.andMap Fuzz.float
        |> Fuzz.andMap Fuzz.float
        |> Fuzz.andMap Fuzz.float
        |> Fuzz.andMap Fuzz.float
        |> Fuzz.andMap Fuzz.float
        |> Fuzz.andMap Fuzz.float
        |> Fuzz.andMap Fuzz.float
        |> Fuzz.andMap Fuzz.float
        |> Fuzz.andMap Fuzz.float
