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

let defaultQuadTree = init  {Lat = 0.0; Lng = 0.0 } 5.0 1


[<Property( Arbitrary=[| typeof<Within5> |] )>]
let ``Can insert point from QuadTree ``(p : Point) =
  insert p defaultQuadTree


[<Property( Arbitrary=[| typeof<Within5> |] )>]
let ``Can insert and remove  point from QuadTree ``(p : Point) =
  insert p defaultQuadTree
  |> should  be True
  remove p defaultQuadTree

