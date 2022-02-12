module Vec3Tests exposing (nonZeroFuzzer, suite, vec3Fuzzer)

import Expect exposing (Expectation)
import Fuzz
import Math.Vector3 as Math
import Test exposing (..)
import Util exposing (compareFloat)
import Vec3 exposing (Vec3, vec3)


suite : Test
suite =
    describe "Vec3"
        [ fuzzWrapFloatVec "setX" Math.setX Vec3.setX
        , fuzzWrapFloatVec "setY" Math.setY Vec3.setY
        , fuzzWrapFloatVec "setZ" Math.setZ Vec3.setZ
        , fuzzWrap2 "distance" Math.distance Vec3.distance
        , fuzzWrap2 "distanceSquared" Math.distanceSquared Vec3.distanceSquared
        , fuzzWrap2 "dot" Math.dot Vec3.dot
        , fuzzWrap2Vec "cross" Math.cross Vec3.cross
        , fuzzWrap "length" Math.length Vec3.length
        , fuzzWrap "lengthSquared" Math.lengthSquared Vec3.lengthSquared
        , fuzzWrap2Vec "add" Math.add Vec3.add
        , fuzzWrap2Vec "sub" Math.sub Vec3.sub
        , fuzzWrap2Vec "direction" Math.direction (\a b -> Vec3.direction { from = b, to = a })
        , fuzzWrapVec "negate" Math.negate Vec3.negate
        , test "normalize 0 == 0" <|
            \_ ->
                compareVec3 (Vec3.normalize (vec3 0 0 0)) (vec3 0 0 0)
        , fuzzWrapVec "normalize"
            -- Vec3.normalize and Math.normalize differ in how they treat zero.
            (\v ->
                if v == Math.vec3 0 0 0 then
                    Math.vec3 0 0 0

                else
                    Math.normalize v
            )
            Vec3.normalize
        , fuzzWrapFloatVec "scale" Math.scale Vec3.scale
        , test "i is the x unit vector" <|
            \_ ->
                Vec3.i
                    |> Expect.equal (vec3 1 0 0)
        , test "j is the y unit vector" <|
            \_ ->
                Vec3.j
                    |> Expect.equal (vec3 0 1 0)
        , test "k is the z unit vector" <|
            \_ ->
                Vec3.k
                    |> Expect.equal (vec3 0 0 1)
        , fuzz vec3Fuzzer "fromHomogeneous is inverse of point" <|
            \v ->
                v
                    |> Vec3.point
                    |> Vec3.fromHomogeneous
                    |> compareVec3 v
        , fuzz vec3Fuzzer "fromHomogeneous is inverse of vector" <|
            \v ->
                v
                    |> Vec3.vector
                    |> Vec3.fromHomogeneous
                    |> compareVec3 v
        ]


vec3Fuzzer : Fuzz.Fuzzer Vec3
vec3Fuzzer =
    Fuzz.map3 (\x y z -> { x = x, y = y, z = z })
        Fuzz.float
        Fuzz.float
        Fuzz.float


nonZeroFuzzer : Fuzz.Fuzzer Vec3
nonZeroFuzzer =
    Fuzz.map3 (\x y z -> { x = x, y = y, z = z })
        (Fuzz.oneOf [ Fuzz.floatRange -100000 -0.000001, Fuzz.floatRange 0.000001 100000 ])
        (Fuzz.oneOf [ Fuzz.floatRange -100000 -0.000001, Fuzz.floatRange 0.000001 100000 ])
        (Fuzz.oneOf [ Fuzz.floatRange -100000 -0.000001, Fuzz.floatRange 0.000001 100000 ])


fuzzWrap : String -> (Math.Vec3 -> Float) -> (Vec3.Vec3 -> Float) -> Test
fuzzWrap name fn0 fn1 =
    fuzz vec3Fuzzer name <|
        \r1 ->
            compareFloat (fn1 r1) (fn0 (Math.fromRecord r1))


fuzzWrapVec : String -> (Math.Vec3 -> Math.Vec3) -> (Vec3.Vec3 -> Vec3.Vec3) -> Test
fuzzWrapVec name fn0 fn1 =
    fuzz vec3Fuzzer name <|
        \r1 ->
            compareVec3 (fn1 r1) (fn0 (Math.fromRecord r1) |> Math.toRecord)


fuzzWrap2 : String -> (Math.Vec3 -> Math.Vec3 -> Float) -> (Vec3.Vec3 -> Vec3.Vec3 -> Float) -> Test
fuzzWrap2 name fn0 fn1 =
    fuzz2 vec3Fuzzer vec3Fuzzer name <|
        \r1 r2 -> compareFloat (fn1 r1 r2) (fn0 (Math.fromRecord r1) (Math.fromRecord r2))


fuzzWrap2Vec : String -> (Math.Vec3 -> Math.Vec3 -> Math.Vec3) -> (Vec3.Vec3 -> Vec3.Vec3 -> Vec3.Vec3) -> Test
fuzzWrap2Vec name fn0 fn1 =
    fuzz2 vec3Fuzzer vec3Fuzzer name <|
        \r1 r2 -> compareVec3 (fn1 r1 r2) (fn0 (Math.fromRecord r1) (Math.fromRecord r2) |> Math.toRecord)


fuzzWrapFloatVec : String -> (Float -> Math.Vec3 -> Math.Vec3) -> (Float -> Vec3.Vec3 -> Vec3.Vec3) -> Test
fuzzWrapFloatVec name fn0 fn1 =
    fuzz2 vec3Fuzzer Fuzz.float name <|
        \r1 fl -> compareVec3 (fn1 fl r1) (fn0 fl (Math.fromRecord r1) |> Math.toRecord)


compareVec3 : Vec3 -> Vec3 -> Expectation
compareVec3 v1 v2 =
    Expect.all
        [ \_ ->
            compareFloat v1.x v2.x
                |> Expect.onFail "x"
        , \_ ->
            compareFloat v1.y v2.y
                |> Expect.onFail "y"
        , \_ ->
            compareFloat v1.z v2.z
                |> Expect.onFail "z"
        ]
        ()
