module Mat4Tests exposing (..)

import Expect exposing (Expectation, FloatingPointTolerance(..))
import Fuzz
import Mat4 exposing (Mat4)
import Math.Matrix4 as Math
import Math.Vector3 as MathVec3
import Test exposing (Test, describe, fuzz, fuzz2, fuzz3, test)
import Util exposing (comparePrecision)
import Vec3 exposing (Vec3, vec3)
import Vec3Tests exposing (vec3Fuzzer)
import Vec4 exposing (vec4)


suite : Test
suite =
    describe "Mat4"
        [ test "identity" <|
            \_ ->
                compare Mat4.identity (Math.toRecord Math.identity)
        , fuzz mat4Fuzzer "invert" <|
            \r ->
                Util.compareMaybes compare
                    (r |> Mat4.invert)
                    (Math.fromRecord r |> Math.inverse |> Maybe.map Math.toRecord)
        , test "cannot invert a singular matrix" <|
            \_ ->
                Mat4.fromRows
                    (vec4 1 1 2 3)
                    (vec4 1 1 12 6)
                    (vec4 1 1 -1 23)
                    (vec4 0 0 0 0)
                    |> Mat4.invert
                    |> Expect.equal Nothing
        , fuzz2 mat4Fuzzer mat4Fuzzer "mul" <|
            \r1 r2 ->
                compare (Mat4.mul r1 r2)
                    (Math.mul (Math.fromRecord r1) (Math.fromRecord r2) |> Math.toRecord)
        , fuzz mat4Fuzzer "transpose" <|
            \r ->
                compare (r |> Mat4.transpose)
                    (r |> Math.fromRecord |> Math.transpose |> Math.toRecord)
        , fuzz2 mat4Fuzzer vec3Fuzzer "transform" <|
            \r1 v2 ->
                compareVec3 (Mat4.transformPoint r1 v2)
                    (Math.transform (Math.fromRecord r1) (MathVec3.fromRecord v2) |> MathVec3.toRecord)
        , fuzz4 positive positive positive positive "makePerspective" <|
            \f1 f2 f3 f4 ->
                compare (Mat4.perspective { fovy = f1, aspect = f2, zNear = f3, zFar = f4 })
                    (Math.makePerspective f1 f2 f3 f4 |> Math.toRecord)
        , fuzz6 Fuzz.float positive Fuzz.float positive Fuzz.float positive "makeOrtho" <|
            \f1 f2 f3 f4 f5 f6 ->
                compare (Mat4.orthographic { left = f1, right = f2, bottom = f3, top = f4, zNear = f5, zFar = f6 })
                    (Math.makeOrtho f1 f2 f3 f4 f5 f6 |> Math.toRecord)
        , test "lookAt when eye equals centerOfAttention" <|
            \_ ->
                let
                    eye =
                        vec3 0 0 1
                in
                Mat4.lookAt
                    { eye = eye
                    , centerOfAttention = eye
                    , up = Vec3.j
                    }
                    |> Expect.equal Nothing
        , fuzz3 vec3Fuzzer vec3Fuzzer vec3Fuzzer "makeLookAt" <|
            \eye centerOfAttention up ->
                Mat4.lookAt { eye = eye, centerOfAttention = centerOfAttention, up = up }
                    |> Maybe.map
                        (compareCustom 0.000002
                            (Math.makeLookAt (MathVec3.fromRecord eye) (MathVec3.fromRecord centerOfAttention) (MathVec3.fromRecord up) |> Math.toRecord)
                        )
                    |> Maybe.withDefault Expect.pass

        -- , fuzz3 Fuzz.float vec3Fuzzer mat4Fuzzer "rotate" <|
        --     \f v r ->
        --         Expect.all
        --             [ compare (Mat4.rotate f v.adt (Mat4.fromRecord r) |> Mat4.toRecord)
        --             , compare (Record.rotate f v.mat4Fuzzer (Record.fromRecord r) |> Record.toRecord)
        --             , compare (Tuple.rotate f v.tuple (Tuple.fromRecord r) |> Tuple.toRecord)
        --             ]
        --             (Math.rotate f v.math (Math.fromRecord r) |> Math.toRecord)
        -- , fuzz2 vec3Fuzzer mat4Fuzzer "scale" <|
        --     \v r ->
        --         compare (Mat4.scale v r)
        --             (Math.scale (MathVec3.fromRecord v) (Math.fromRecord r) |> Math.toRecord)
        -- , fuzz2 vec3Fuzzer mat4Fuzzer "translate" <|
        --     \v r ->
        --         Expect.all
        --             [ compare (Mat4.translate v.adt (Mat4.fromRecord r) |> Mat4.toRecord)
        --             , compare (Record.translate v.mat4Fuzzer (Record.fromRecord r) |> Record.toRecord)
        --             , compare (Tuple.translate v.tuple (Tuple.fromRecord r) |> Tuple.toRecord)
        --             ]
        --             (Math.translate v.math (Math.fromRecord r) |> Math.toRecord)
        , fuzz2 Fuzz.float Vec3Tests.nonZeroFuzzer "makeRotate" <|
            \f v -> compare (Mat4.rotate f v) (Math.makeRotate f (MathVec3.fromRecord v) |> Math.toRecord)
        , fuzz vec3Fuzzer "makeScale" <|
            \v ->
                compare (Mat4.scale v)
                    (Math.makeScale (MathVec3.fromRecord v) |> Math.toRecord)
        , fuzz vec3Fuzzer "makeTranslate" <|
            \v ->
                compare (Mat4.translate v) (Math.makeTranslate (MathVec3.fromRecord v) |> Math.toRecord)
        , addTests
        , transformVectorTests
        ]


addTests : Test
addTests =
    describe "Mat4.add"
        [ fuzz mat4Fuzzer "0 + m == m" <|
            \m ->
                let
                    zero =
                        Mat4.fromRows
                            (vec4 0 0 0 0)
                            (vec4 0 0 0 0)
                            (vec4 0 0 0 0)
                            (vec4 0 0 0 0)
                in
                Mat4.add m zero
                    |> Expect.equal m
        , test "adding known matrices" <|
            \_ ->
                Mat4.add
                    (Mat4.fromRows (vec4 1 2 3 2)
                        (vec4 3 4 5 0)
                        (vec4 -1 -2 -3 1)
                        (vec4 1 1 1 1)
                    )
                    (Mat4.fromRows (vec4 5 -1 -10 1)
                        (vec4 28 0.1 0 1)
                        (vec4 0 0 0 1)
                        (vec4 -1 1 -1 1)
                    )
                    |> compare
                        (Mat4.fromRows
                            (vec4 6 1 -7 3)
                            (vec4 31 4.1 5 1)
                            (vec4 -1 -2 -3 2)
                            (vec4 0 2 0 2)
                        )
        ]


transformVectorTests : Test
transformVectorTests =
    Test.describe "Mat4.transformVector"
        [ test "Translate" <|
            \_ ->
                Mat4.transformVector (Mat4.translate (vec3 1 1 1)) (vec3 0 0 0)
                    |> Expect.equal (vec3 0 0 0)
        , test "Translate2" <|
            \_ ->
                Mat4.transformVector (Mat4.translate (vec3 1 5 3)) (vec3 2 6 2)
                    |> Expect.equal (vec3 2 6 2)
        , test "Scale" <|
            \_ ->
                Mat4.transformVector (Mat4.scale (vec3 1 5 0)) (vec3 2 6 19)
                    |> Expect.equal (vec3 2 30 0)
        , test "Scale 2" <|
            \_ ->
                Mat4.transformVector (Mat4.scale (vec3 0 0 0)) (vec3 2 6 2)
                    |> Expect.equal (vec3 0 0 0)
        , test "Translation doesn't affect point" <|
            \_ ->
                let
                    mat =
                        Mat4.mul (Mat4.translate (vec3 0 5 2)) Mat4.identity
                in
                Mat4.transformVector mat (vec3 2 5 1)
                    |> Expect.equal (vec3 2 5 1)
        ]


compare : Mat4 -> Mat4 -> Expectation
compare mat1 mat2 =
    compareCustom 0.000001 mat1 mat2


errorMessage : String -> Float -> Float -> String
errorMessage str f1 f2 =
    str ++ ":\n" ++ "\tMat1: " ++ String.fromFloat f1 ++ "\n\tMat2: " ++ String.fromFloat f2


compareCustom : Float -> Mat4 -> Mat4 -> Expectation
compareCustom precision mat1 mat2 =
    Expect.all
        [ \_ -> comparePrecision precision mat1.m11 mat2.m11 |> Expect.onFail (errorMessage "m11" mat1.m11 mat2.m11)
        , \_ -> comparePrecision precision mat1.m21 mat2.m21 |> Expect.onFail (errorMessage "m21" mat1.m21 mat2.m21)
        , \_ -> comparePrecision precision mat1.m31 mat2.m31 |> Expect.onFail (errorMessage "m31" mat1.m31 mat2.m31)
        , \_ -> comparePrecision precision mat1.m41 mat2.m41 |> Expect.onFail (errorMessage "m41" mat1.m41 mat2.m41)
        , \_ -> comparePrecision precision mat1.m12 mat2.m12 |> Expect.onFail (errorMessage "m12" mat1.m12 mat2.m12)
        , \_ -> comparePrecision precision mat1.m22 mat2.m22 |> Expect.onFail (errorMessage "m22" mat1.m22 mat2.m22)
        , \_ -> comparePrecision precision mat1.m32 mat2.m32 |> Expect.onFail (errorMessage "m32" mat1.m32 mat2.m32)
        , \_ -> comparePrecision precision mat1.m42 mat2.m42 |> Expect.onFail (errorMessage "m42" mat1.m42 mat2.m42)
        , \_ -> comparePrecision precision mat1.m13 mat2.m13 |> Expect.onFail (errorMessage "m13" mat1.m13 mat2.m13)
        , \_ -> comparePrecision precision mat1.m23 mat2.m23 |> Expect.onFail (errorMessage "m23" mat1.m23 mat2.m23)
        , \_ -> comparePrecision precision mat1.m33 mat2.m33 |> Expect.onFail (errorMessage "m33" mat1.m33 mat2.m33)
        , \_ -> comparePrecision precision mat1.m43 mat2.m43 |> Expect.onFail (errorMessage "m43" mat1.m43 mat2.m43)
        , \_ -> comparePrecision precision mat1.m14 mat2.m14 |> Expect.onFail (errorMessage "m14" mat1.m14 mat2.m14)
        , \_ -> comparePrecision precision mat1.m24 mat2.m24 |> Expect.onFail (errorMessage "m24" mat1.m24 mat2.m24)
        , \_ -> comparePrecision precision mat1.m34 mat2.m34 |> Expect.onFail (errorMessage "m34" mat1.m34 mat2.m34)
        , \_ -> comparePrecision precision mat1.m44 mat2.m44 |> Expect.onFail (errorMessage "m44" mat1.m44 mat2.m44)
        ]
        ()


compareVec3 : Vec3 -> Vec3 -> Expectation
compareVec3 vec1 vec2 =
    Expect.all
        [ .x >> Expect.within (Absolute 0.0000000001) vec1.x
        , .y >> Expect.within (Absolute 0.0000000001) vec1.y
        , .z >> Expect.within (Absolute 0.0000000001) vec1.z
        ]
        vec2



--
--compareVec3Custom precision vec1 vec2 =
--    Expect.all
--        [ .x >> comparePrecision precision vec1.x
--        , .y >> comparePrecision precision vec1.y
--        , .z >> comparePrecision precision vec1.z
--        ]
--        vec2


positive : Fuzz.Fuzzer Float
positive =
    Fuzz.floatRange 0.000001 100


fuzz6 :
    Fuzz.Fuzzer a5
    -> Fuzz.Fuzzer a4
    -> Fuzz.Fuzzer a3
    -> Fuzz.Fuzzer a2
    -> Fuzz.Fuzzer a1
    -> Fuzz.Fuzzer a
    -> String
    -> (a5 -> a4 -> a3 -> a2 -> a1 -> a -> Expect.Expectation)
    -> Test.Test
fuzz6 a1 a2 a3 a4 a5 a6 desc fn =
    Test.fuzz
        (Fuzz.constant fn
            |> Fuzz.andMap a1
            |> Fuzz.andMap a2
            |> Fuzz.andMap a3
            |> Fuzz.andMap a4
            |> Fuzz.andMap a5
            |> Fuzz.andMap a6
        )
        desc
        identity


fuzz4 :
    Fuzz.Fuzzer a5
    -> Fuzz.Fuzzer a4
    -> Fuzz.Fuzzer a3
    -> Fuzz.Fuzzer a2
    -> String
    -> (a5 -> a4 -> a3 -> a2 -> Expect.Expectation)
    -> Test.Test
fuzz4 a1 a2 a3 a4 desc fn =
    Test.fuzz (Fuzz.map fn a1 |> Fuzz.andMap a2 |> Fuzz.andMap a3 |> Fuzz.andMap a4) desc identity


mat4Fuzzer : Fuzz.Fuzzer Mat4
mat4Fuzzer =
    Fuzz.constant Mat4
        |> Fuzz.andMap Fuzz.float
        |> Fuzz.andMap Fuzz.float
        |> Fuzz.andMap Fuzz.float
        |> Fuzz.andMap Fuzz.float
        |> Fuzz.andMap Fuzz.float
        |> Fuzz.andMap Fuzz.float
        |> Fuzz.andMap Fuzz.float
        |> Fuzz.andMap Fuzz.float
        |> Fuzz.andMap Fuzz.float
        |> Fuzz.andMap Fuzz.float
        |> Fuzz.andMap Fuzz.float
        |> Fuzz.andMap Fuzz.float
        |> Fuzz.andMap Fuzz.float
        |> Fuzz.andMap Fuzz.float
        |> Fuzz.andMap Fuzz.float
        |> Fuzz.andMap (Fuzz.oneOf [ Fuzz.floatRange -10000000 -0.0000001, Fuzz.floatRange 0.0000001 10000000 ])
