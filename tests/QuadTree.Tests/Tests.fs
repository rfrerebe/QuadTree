module QuadTree.Tests

open QuadTree
open NUnit.Framework
open FsUnit
open FsCheck
open FsCheck.NUnit

type Within5 =
  static member Point() =
      Arb.generate<float>
      |> Gen.two
      |> Gen.filter (fun (t,t') -> abs(t) <= 5.0 && abs(t') <= 5.0 )
      |> Gen.map (fun (t,t') -> {Lat = t; Lng =t' })
      |> Arb.fromGen
  
type Outside5 =
  static member Point()=
    Arb.generate<float>
    |> Gen.two
    |> Gen.filter (fun (t,t') -> abs(t) > 5.0 || abs(t') > 5.0 )
    |> Gen.map (fun (t,t') -> {Lat = t; Lng =t' })
    |> Arb.fromGen

type SWPoint =
  static member Point() =
      Arb.generate<float>
      |> Gen.two
      |> Gen.filter (fun (t,t') -> t >= -5.0  && t < 0.0 && t' >= -5.0 && t' < 0.0 )
      |> Gen.map (fun (t,t') -> {Lat = t; Lng =t' })
      |> Arb.fromGen

type NEPoint =
  static member Point() =
      Arb.generate<float>
      |> Gen.two
      |> Gen.filter (fun (t,t') -> t <= 5.0  && t > 0.0 && t' <= 5.0 && t' > 0.0 )
      |> Gen.map (fun (t,t') -> {Lat = t; Lng =t' })
      |> Arb.fromGen

type NWPoint =
  static member Point() =
      Arb.generate<float>
      |> Gen.two
      |> Gen.filter (fun (t,t') -> t >= -5.0  && t < 0.0 && t' <= 5.0 && t' > 0.0 )
      |> Gen.map (fun (t,t') -> {Lat = t; Lng =t' })
      |> Arb.fromGen
let defaultQuadTree () = init  {Lat = 0.0; Lng = 0.0 } 5.0 1

type SEPoint =
  static member Point() =
      Arb.generate<float>
      |> Gen.two
      |> Gen.filter (fun (t,t') -> t <= 5.0  && t > 0.0 && t' >= -5.0 && t' < 0.0 )
      |> Gen.map (fun (t,t') -> {Lat = t; Lng =t' })
      |> Arb.fromGen

[<Property( Arbitrary=[| typeof<Within5> |] )>]
let ``Can insert point from QuadTree ``(p : Point) =
  insert p (defaultQuadTree ())


[<Property( Arbitrary=[| typeof<Within5> |] )>]
let ``Can insert and remove  point from QuadTree ``(p : Point) =
  let q = defaultQuadTree ()
  insert p q
  |> should  be True
  remove p q

[<Property( Arbitrary=[| typeof<NEPoint> |], MaxTest = 10)>]
let ``Closest point from SW is origin`` (p: Point) =
  let q = defaultQuadTree ()
  insert ({Lat = 0.0; Lng = 0.0}) q
  |> ignore
  insert p q
  |> should  be True
  let result  = findClosest ({Lat = -1.0; Lng = -1.0}) q

  result
  |> Option.isSome 
  |> should be True

  result
  |> Option.get
  |> should equal {Lat = 0.0; Lng = 0.0}



[<Property( Arbitrary=[| typeof<SEPoint> |], MaxTest = 10)>]
let ``Closest point from NW is origin`` (p: Point) =
  let q = defaultQuadTree ()
  insert ({Lat = 0.0; Lng = 0.0}) q
  |> ignore
  insert p q
  |> should  be True
  let result  = findClosest ({Lat = -1.0; Lng = 1.0}) q
  
  result
  |> Option.isSome 
  |> should be True

  result
  |> Option.get
  |> should equal {Lat = 0.0; Lng = 0.0}



[<Property( Arbitrary=[| typeof<NWPoint> |], MaxTest = 10)>]
let ``Closest point from SE is origin`` (p: Point) =
  let q = defaultQuadTree ()
  insert ({Lat = 0.0; Lng = 0.0}) q
  |> ignore
  insert p q
  |> should  be True
  let result  = findClosest ({Lat = 1.0; Lng = -1.0}) q
  
  result
  |> Option.isSome 
  |> should be True

  result
  |> Option.get
  |> should equal {Lat = 0.0; Lng = 0.0}



[<Property( Arbitrary=[| typeof<SWPoint> |] , MaxTest = 10)>]
let ``Closest point from NE is origin`` (p: Point) =
  let q = defaultQuadTree ()
  insert ({Lat = 0.0; Lng = 0.0}) q
  |> ignore
  insert p q
  |> should  be True
  let result  = findClosest ({Lat = 1.0; Lng = 1.0}) q
  
  result
  |> Option.isSome 
  |> should be True

  result
  |> Option.get
  |> should equal {Lat = 0.0; Lng = 0.0}


[<Property( Arbitrary=[| typeof<Within5> |] )>]
let ``Bound contains Point``(p : Point) =
  contains (defaultQuadTree ()).Bound p
  |> should be True

[<Property( Arbitrary=[| typeof<Outside5> |] )>]
let ``Bound doesn't contains Point``(p : Point) =
  contains (defaultQuadTree ()).Bound p
  |> should be False