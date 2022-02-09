module Vec3Tests exposing (suite, vec3Fuzzer)

import Expect exposing (Expectation, FloatingPointTolerance(..))
import Fuzz
import Math.Vector3 as Math
import Test exposing (..)
import Util exposing (compareFloat)
import Vec3 exposing (Vec3)


suite : Test
suite =
    describe "Vector3!"
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
                compareVec3 (Vec3.normalize (Vec3.vec3 0 0 0)) (Vec3.vec3 0 0 0)
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
        ]


vec3Fuzzer : Fuzz.Fuzzer Vec3
vec3Fuzzer =
    Fuzz.map3 (\x y z -> { x = x, y = y, z = z })
        Fuzz.float
        Fuzz.float
        Fuzz.float


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
        [ \_ -> compareFloat v1.x v2.x
        , \_ -> compareFloat v1.y v2.y
        , \_ -> compareFloat v1.z v2.z
        ]
        ()
