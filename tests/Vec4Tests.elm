module Vec4Tests exposing (suite)

import Expect exposing (Expectation)
import Fuzz
import Math.Vector4 as Math
import Test exposing (..)
import Util exposing (compareFloat)
import Vec4 exposing (Vec4)


suite : Test
suite =
    describe "Vec4"
        [ fuzzWrapFloatVec "setW" Math.setW Vec4.setW
        , fuzzWrapFloatVec "setX" Math.setX Vec4.setX
        , fuzzWrapFloatVec "setY" Math.setY Vec4.setY
        , fuzzWrapFloatVec "setZ" Math.setZ Vec4.setZ
        , fuzzWrap2 "distance" Math.distance Vec4.distance
        , fuzzWrap2 "distanceSquared" Math.distanceSquared Vec4.distanceSquared
        , fuzzWrap2 "dot" Math.dot Vec4.dot
        , fuzzWrap "length" Math.length Vec4.length
        , fuzzWrap "lengthSquared" Math.lengthSquared Vec4.lengthSquared
        , fuzzWrap2Vec "add" Math.add Vec4.add
        , fuzzWrap2Vec "sub" Math.sub Vec4.sub
        , fuzzWrap2Vec "direction" (Util.ignoreEquals (Math.vec4 0 0 0 0) Math.direction) (\a b -> Vec4.direction { from = b, to = a })
        , fuzzWrapVec "negate" Math.negate Vec4.negate
        , fuzzWrapVec "normalize" (Util.ignoreZero (Math.vec4 0 0 0 0) Math.normalize) Vec4.normalize
        , fuzzWrapFloatVec "scale" Math.scale Vec4.scale
        ]


fuzzWrap : String -> (Math.Vec4 -> Float) -> (Vec4.Vec4 -> Float) -> Test
fuzzWrap name fn0 fn1 =
    fuzz vec4Fuzzer name <|
        \r1 ->
            compareFloat (fn1 r1) (fn0 (Math.fromRecord r1))


fuzzWrapVec : String -> (Math.Vec4 -> Math.Vec4) -> (Vec4.Vec4 -> Vec4.Vec4) -> Test
fuzzWrapVec name fn0 fn1 =
    fuzz vec4Fuzzer name <|
        \r1 -> compareVec4 (fn1 r1) (fn0 (Math.fromRecord r1) |> Math.toRecord)


fuzzWrap2 : String -> (Math.Vec4 -> Math.Vec4 -> Float) -> (Vec4.Vec4 -> Vec4.Vec4 -> Float) -> Test
fuzzWrap2 name fn0 fn1 =
    fuzz2 vec4Fuzzer vec4Fuzzer name <|
        \r1 r2 -> compareFloat (fn1 r1 r2) (fn0 (Math.fromRecord r1) (Math.fromRecord r2))


fuzzWrap2Vec : String -> (Math.Vec4 -> Math.Vec4 -> Math.Vec4) -> (Vec4.Vec4 -> Vec4.Vec4 -> Vec4.Vec4) -> Test
fuzzWrap2Vec name fn0 fn1 =
    fuzz2 vec4Fuzzer vec4Fuzzer name <|
        \r1 r2 -> compareVec4 (fn1 r1 r2) (fn0 (Math.fromRecord r1) (Math.fromRecord r2) |> Math.toRecord)


fuzzWrapFloatVec : String -> (Float -> Math.Vec4 -> Math.Vec4) -> (Float -> Vec4.Vec4 -> Vec4.Vec4) -> Test
fuzzWrapFloatVec name fn0 fn1 =
    fuzz2 vec4Fuzzer Fuzz.float name <|
        \r1 fl -> compareVec4 (fn1 fl r1) (fn0 fl (Math.fromRecord r1) |> Math.toRecord)


compareVec4 : Vec4 -> Vec4 -> Expectation
compareVec4 v1 v2 =
    Expect.all
        [ \_ ->
            compareFloat v1.x v2.x
        , \_ ->
            compareFloat v1.y v2.y
        , \_ ->
            compareFloat v1.z v2.z
        , \_ ->
            compareFloat v1.w v2.w
        ]
        ()
        |> Expect.onFail (vec4Error v1 v2)


vec4Error : Vec4 -> Vec4 -> String
vec4Error v1 v2 =
    "v1:\n\t" ++ Vec4.toString v1 ++ "\nv2:\n\t" ++ Vec4.toString v2


vec4Fuzzer : Fuzz.Fuzzer Vec4
vec4Fuzzer =
    Fuzz.map4 (\x y z w -> { x = x, y = y, z = z, w = w })
        Fuzz.float
        Fuzz.float
        Fuzz.float
        Fuzz.float
