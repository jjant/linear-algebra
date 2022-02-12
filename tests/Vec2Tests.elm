module Vec2Tests exposing (compareVec2, suite, vec2Fuzzer)

import Expect exposing (Expectation)
import Fuzz
import Math.Vector2 as Math
import Test exposing (..)
import Util exposing (compareFloat)
import Vec2 exposing (Vec2)


suite : Test
suite =
    describe "Vec2"
        [ fuzzWrapFloatVec "setX" Math.setX Vec2.setX
        , fuzzWrapFloatVec "setY" Math.setY Vec2.setY
        , fuzzWrap2 "distance" Math.distance Vec2.distance
        , fuzzWrap2 "distanceSquared" Math.distanceSquared Vec2.distanceSquared
        , fuzzWrap2 "dot" Math.dot Vec2.dot
        , fuzzWrap "length" Math.length Vec2.length
        , fuzzWrap "lengthSquared" Math.lengthSquared Vec2.lengthSquared
        , fuzzWrap2Vec "add" Math.add Vec2.add
        , fuzzWrap2Vec "sub" Math.sub Vec2.sub
        , fuzzWrap2Vec "direction" (Util.ignoreEquals (Math.vec2 0 0) Math.direction) (\a b -> Vec2.direction { from = b, to = a })
        , fuzzWrapVec "negate" Math.negate Vec2.negate
        , test "normalize 0 == 0" <|
            \_ ->
                compareVec2 (Vec2.normalize Vec2.zero) Vec2.zero
        , fuzzWrapVec "normalize" (Util.ignoreZero (Math.vec2 0 0) Math.normalize) Vec2.normalize
        , fuzzWrapFloatVec "scale" Math.scale Vec2.scale
        ]


record : Fuzz.Fuzzer { x : Float, y : Float }
record =
    Fuzz.map2 (\x y -> { x = x, y = y })
        Fuzz.float
        Fuzz.float


fuzzWrap : String -> (Math.Vec2 -> Float) -> (Vec2.Vec2 -> Float) -> Test
fuzzWrap name fn0 fn1 =
    fuzz record name <|
        \r1 -> compareFloat (fn1 r1) (fn0 (Math.fromRecord r1))


fuzzWrapVec : String -> (Math.Vec2 -> Math.Vec2) -> (Vec2.Vec2 -> Vec2.Vec2) -> Test
fuzzWrapVec name fn0 fn1 =
    fuzz record name <|
        \r1 -> compareVec2 (fn1 r1) (fn0 (Math.fromRecord r1) |> Math.toRecord)


fuzzWrap2 : String -> (Math.Vec2 -> Math.Vec2 -> Float) -> (Vec2.Vec2 -> Vec2.Vec2 -> Float) -> Test
fuzzWrap2 name fn0 fn1 =
    fuzz2 record record name <|
        \r1 r2 -> compareFloat (fn1 r1 r2) (fn0 (Math.fromRecord r1) (Math.fromRecord r2))


fuzzWrap2Vec : String -> (Math.Vec2 -> Math.Vec2 -> Math.Vec2) -> (Vec2.Vec2 -> Vec2.Vec2 -> Vec2.Vec2) -> Test
fuzzWrap2Vec name fn0 fn1 =
    fuzz2 record record name <|
        \r1 r2 -> compareVec2 (fn1 r1 r2) (fn0 (Math.fromRecord r1) (Math.fromRecord r2) |> Math.toRecord)


fuzzWrapFloatVec : String -> (Float -> Math.Vec2 -> Math.Vec2) -> (Float -> Vec2.Vec2 -> Vec2.Vec2) -> Test
fuzzWrapFloatVec name fn0 fn1 =
    fuzz2 record Fuzz.float name <|
        \r1 fl -> compareVec2 (fn1 fl r1) (fn0 fl (Math.fromRecord r1) |> Math.toRecord)


compareVec2 : Vec2 -> Vec2 -> Expectation
compareVec2 v1 v2 =
    Expect.all
        [ \_ ->
            compareFloat v1.x v2.x
        , \_ ->
            compareFloat v1.y v2.y
        ]
        ()
        |> Expect.onFail (vec2Error v1 v2)


vec2Error : Vec2 -> Vec2 -> String
vec2Error v1 v2 =
    "v1:\n\t" ++ Vec2.toString v1 ++ "\nv2:\n\t" ++ Vec2.toString v2


vec2Fuzzer : Fuzz.Fuzzer Vec2
vec2Fuzzer =
    Fuzz.map2 (\x y -> { x = x, y = y })
        Fuzz.float
        Fuzz.float
