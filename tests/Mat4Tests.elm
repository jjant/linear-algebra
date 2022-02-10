module Mat4Tests exposing (..)

import Expect exposing (Expectation, FloatingPointTolerance(..))
import Fuzz
import Mat4 exposing (Mat4)
import Math.Matrix4 as Math
import Math.Vector3 as MathVec3
import Test exposing (Test, describe, fuzz, fuzz2, fuzz3, skip, test)
import Vec3
import Vec3Tests exposing (vec3Fuzzer)


suite : Test
suite =
    describe "Matrix4!"
        [ test "identity" <|
            \_ ->
                compare Mat4.identity (Math.toRecord Math.identity)
        , fuzz mat4Fuzzer "invert" <|
            \r ->
                compareMaybes compare
                    (r |> Mat4.invert)
                    (Math.fromRecord r |> Math.inverse |> Maybe.map Math.toRecord)

        -- , fuzz mat4Fuzzer "inverseOrthonormal" <|
        --     \r ->
        --         Expect.all
        --             [ compare (r |> Mat4.inverseOrthonormal |> Mat4.toRecord)
        --             , compare (r |> Record.fromRecord |> Record.inverseOrthonormal |> Record.toRecord)
        --             , compare (r |> Tuple.fromRecord |> Tuple.inverseOrthonormal |> Tuple.toRecord)
        --             ]
        --             (r |> Math.fromRecord |> Math.inverseOrthonormal |> Math.toRecord)
        , fuzz2 mat4Fuzzer mat4Fuzzer "mul" <|
            \r1 r2 ->
                compare (Mat4.mul r1 r2)
                    (Math.mul (Math.fromRecord r1) (Math.fromRecord r2) |> Math.toRecord)

        -- , fuzz2 mat4Fuzzer mat4Fuzzer "mulAffine" <|
        --     \r1 r2 ->
        --         Expect.all
        --             [ compare (Mat4.mulAffine (Mat4.fromRecord r1) (Mat4.fromRecord r2) |> Mat4.toRecord)
        --             , compare (Record.mulAffine (Record.fromRecord r1) (Record.fromRecord r2) |> Record.toRecord)
        --             , compare (Tuple.mulAffine (Tuple.fromRecord r1) (Tuple.fromRecord r2) |> Tuple.toRecord)
        --             ]
        --             (Math.mulAffine (Math.fromRecord r1) (Math.fromRecord r2) |> Math.toRecord)
        -- , fuzz mat4Fuzzer "transpose" <|
        --     \r ->
        --         Expect.all
        --             [ compare (r |> Mat4.transpose |> Mat4.toRecord)
        --             , compare (r |> Record.fromRecord |> Record.transpose |> Record.toRecord)
        --             , compare (r |> Tuple.fromRecord |> Tuple.transpose |> Tuple.toRecord)
        --             ]
        --             (r |> Math.fromRecord |> Math.transpose |> Math.toRecord)
        -- , fuzz3 vec3Fuzzer vec3Fuzzer vec3Fuzzer "makeBasis" <|
        --     \v1 v2 v3 ->
        --         Expect.all
        --             [ compare (Mat4.makeBasis v1.adt v2.adt v3.adt |> Mat4.toRecord)
        --             , compare (Record.makeBasis v1.mat4Fuzzer v2.mat4Fuzzer v3.mat4Fuzzer |> Record.toRecord)
        --             , compare (Tuple.makeBasis v1.tuple v2.tuple v3.tuple |> Tuple.toRecord)
        --             ]
        --             (Math.makeBasis v1.math v2.math v3.math |> Math.toRecord)
        -- , fuzz2 mat4Fuzzer vec3Fuzzer "transform" <|
        --     \r1 v2 ->
        --         Expect.all
        --             [ compareVec3 (Mat4.transform (Mat4.fromRecord r1) v2.adt |> ADTVec3.toRecord)
        --             , compareVec3 (Record.transform (Record.fromRecord r1) v2.mat4Fuzzer |> RecordVec3.toRecord)
        --             , compareVec3 (Tuple.transform (Tuple.fromRecord r1) v2.tuple |> TupleVec3.toRecord)
        --             ]
        --             (Math.transform (Math.fromRecord r1) v2.math |> MathVec3.toRecord)
        -- , fuzz6 Fuzz.float positive Fuzz.float positive Fuzz.float positive "makeFrustum" <|
        --     \f1 f2 f3 f4 f5 f6 ->
        --         Expect.all
        --             [ compare (Mat4.makeFrustum f1 f2 f3 f4 f5 f6 |> Mat4.toRecord)
        --             , compare (Record.makeFrustum f1 f2 f3 f4 f5 f6 |> Record.toRecord)
        --             , compare (Tuple.makeFrustum f1 f2 f3 f4 f5 f6 |> Tuple.toRecord)
        --             ]
        --             (Math.makeFrustum f1 f2 f3 f4 f5 f6 |> Math.toRecord)
        -- , fuzz4 positive positive positive positive "makePerspective" <|
        --     \f1 f2 f3 f4 ->
        --         Expect.all
        --             [ compare (Mat4.makePerspective f1 f2 f3 f4 |> Mat4.toRecord)
        --             , compare (Record.makePerspective f1 f2 f3 f4 |> Record.toRecord)
        --             , compare (Tuple.makePerspective f1 f2 f3 f4 |> Tuple.toRecord)
        --             ]
        --             (Math.makePerspective f1 f2 f3 f4 |> Math.toRecord)
        -- , fuzz6 Fuzz.float positive Fuzz.float positive Fuzz.float positive "makeOrtho" <|
        --     \f1 f2 f3 f4 f5 f6 ->
        --         Expect.all
        --             [ compare (Mat4.makeOrtho f1 f2 f3 f4 f5 f6 |> Mat4.toRecord)
        --             , compare (Record.makeOrtho f1 f2 f3 f4 f5 f6 |> Record.toRecord)
        --             , compare (Tuple.makeOrtho f1 f2 f3 f4 f5 f6 |> Tuple.toRecord)
        --             ]
        --             (Math.makeOrtho f1 f2 f3 f4 f5 f6 |> Math.toRecord)
        -- , fuzz4 Fuzz.float positive Fuzz.float positive "makeOrtho2D" <|
        --     \f1 f2 f3 f4 ->
        --         Expect.all
        --             [ compare (Mat4.makeOrtho2D f1 f2 f3 f4 |> Mat4.toRecord)
        --             , compare (Record.makeOrtho2D f1 f2 f3 f4 |> Record.toRecord)
        --             , compare (Tuple.makeOrtho2D f1 f2 f3 f4 |> Tuple.toRecord)
        --             ]
        --             (Math.makeOrtho2D f1 f2 f3 f4 |> Math.toRecord)
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
        --         Expect.all
        --             [ compare (Mat4.scale v.adt (Mat4.fromRecord r) |> Mat4.toRecord)
        --             , compare (Record.scale v.mat4Fuzzer (Record.fromRecord r) |> Record.toRecord)
        --             , compare (Tuple.scale v.tuple (Tuple.fromRecord r) |> Tuple.toRecord)
        --             ]
        --             (Math.scale v.math (Math.fromRecord r) |> Math.toRecord)
        -- , fuzz4 Fuzz.float Fuzz.float Fuzz.float mat4Fuzzer "scale3" <|
        --     \f1 f2 f3 r ->
        --         Expect.all
        --             [ compare (Mat4.scale3 f1 f2 f3 (Mat4.fromRecord r) |> Mat4.toRecord)
        --             , compare (Record.scale3 f1 f2 f3 (Record.fromRecord r) |> Record.toRecord)
        --             , compare (Tuple.scale3 f1 f2 f3 (Tuple.fromRecord r) |> Tuple.toRecord)
        --             ]
        --             (Math.scale3 f1 f2 f3 (Math.fromRecord r) |> Math.toRecord)
        -- , fuzz2 vec3Fuzzer mat4Fuzzer "translate" <|
        --     \v r ->
        --         Expect.all
        --             [ compare (Mat4.translate v.adt (Mat4.fromRecord r) |> Mat4.toRecord)
        --             , compare (Record.translate v.mat4Fuzzer (Record.fromRecord r) |> Record.toRecord)
        --             , compare (Tuple.translate v.tuple (Tuple.fromRecord r) |> Tuple.toRecord)
        --             ]
        --             (Math.translate v.math (Math.fromRecord r) |> Math.toRecord)
        -- , fuzz4 Fuzz.float Fuzz.float Fuzz.float mat4Fuzzer "translate3" <|
        --     \f1 f2 f3 r ->
        --         Expect.all
        --             [ compare (Mat4.translate3 f1 f2 f3 (Mat4.fromRecord r) |> Mat4.toRecord)
        --             , compare (Record.translate3 f1 f2 f3 (Record.fromRecord r) |> Record.toRecord)
        --             , compare (Tuple.translate3 f1 f2 f3 (Tuple.fromRecord r) |> Tuple.toRecord)
        --             ]
        --             (Math.translate3 f1 f2 f3 (Math.fromRecord r) |> Math.toRecord)
        -- , fuzz2 Fuzz.float vec3Fuzzer "makeRotate" <|
        --     \f v ->
        --         Expect.all
        --             [ compare (Mat4.makeRotate f v.adt |> Mat4.toRecord)
        --             , compare (Record.makeRotate f v.mat4Fuzzer |> Record.toRecord)
        --             , compare (Tuple.makeRotate f v.tuple |> Tuple.toRecord)
        --             ]
        --             (Math.makeRotate f v.math |> Math.toRecord)
        , fuzz vec3Fuzzer "makeScale" <|
            \v ->
                compare (Mat4.scale v)
                    (Math.makeScale (MathVec3.fromRecord v) |> Math.toRecord)
        , fuzz vec3Fuzzer "makeTranslate" <|
            \v ->
                compare (Mat4.translate v) (Math.makeTranslate (MathVec3.fromRecord v) |> Math.toRecord)
        ]


compareMaybes : (a -> b -> Expectation) -> Maybe a -> Maybe b -> Expectation
compareMaybes f ma mb =
    case ( ma, mb ) of
        ( Just a, Just b ) ->
            f a b

        ( Nothing, Nothing ) ->
            Expect.pass

        ( _, _ ) ->
            Expect.fail "Differing maybes"


compare : Mat4 -> Mat4 -> Expectation
compare mat1 mat2 =
    compareCustom 0.000001 mat1 mat2


comparePrecision : Float -> Float -> Float -> Expectation
comparePrecision precision a b =
    if isNaN a && isNaN b then
        Expect.pass

    else
        Expect.within (Absolute precision) a b


errorMessage str f1 f2 =
    str ++ ":\n" ++ "\tMat1: " ++ String.fromFloat f1 ++ "\n\tMat2: " ++ String.fromFloat f2


compareCustom : Float -> Mat4 -> Mat4 -> Expectation
compareCustom precision mat1 mat2 =
    Expect.all
        [ .m11 >> comparePrecision precision mat1.m11 >> Expect.onFail (errorMessage "m11" mat1.m11 mat2.m11)
        , .m21 >> comparePrecision precision mat1.m21 >> Expect.onFail (errorMessage "m21" mat1.m21 mat2.m21)
        , .m31 >> comparePrecision precision mat1.m31 >> Expect.onFail (errorMessage "m31" mat1.m31 mat2.m31)
        , .m41 >> comparePrecision precision mat1.m41 >> Expect.onFail (errorMessage "m41" mat1.m41 mat2.m41)
        , .m12 >> comparePrecision precision mat1.m12 >> Expect.onFail (errorMessage "m12" mat1.m12 mat2.m12)
        , .m22 >> comparePrecision precision mat1.m22 >> Expect.onFail (errorMessage "m22" mat1.m22 mat2.m22)
        , .m32 >> comparePrecision precision mat1.m32 >> Expect.onFail (errorMessage "m32" mat1.m32 mat2.m32)
        , .m42 >> comparePrecision precision mat1.m42 >> Expect.onFail (errorMessage "m42" mat1.m42 mat2.m42)
        , .m13 >> comparePrecision precision mat1.m13 >> Expect.onFail (errorMessage "m13" mat1.m13 mat2.m13)
        , .m23 >> comparePrecision precision mat1.m23 >> Expect.onFail (errorMessage "m23" mat1.m23 mat2.m23)
        , .m33 >> comparePrecision precision mat1.m33 >> Expect.onFail (errorMessage "m33" mat1.m33 mat2.m33)
        , .m43 >> comparePrecision precision mat1.m43 >> Expect.onFail (errorMessage "m43" mat1.m43 mat2.m43)
        , .m14 >> comparePrecision precision mat1.m14 >> Expect.onFail (errorMessage "m14" mat1.m14 mat2.m14)
        , .m24 >> comparePrecision precision mat1.m24 >> Expect.onFail (errorMessage "m24" mat1.m24 mat2.m24)
        , .m34 >> comparePrecision precision mat1.m34 >> Expect.onFail (errorMessage "m34" mat1.m34 mat2.m34)
        , .m44 >> comparePrecision precision mat1.m44 >> Expect.onFail (errorMessage "m44" mat1.m44 mat2.m44)
        ]
        mat2


compareVec3 : { x : Float, y : Float, z : Float } -> { x : Float, y : Float, z : Float } -> Expectation
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
        |> Fuzz.andMap Fuzz.float
