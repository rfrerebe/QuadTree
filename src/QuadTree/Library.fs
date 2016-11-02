module QuadTree
  
  open System.Collections.Concurrent

  type Point = { Lat :float; Lng : float}
  type Bound = { Center : Point; HalfDimension : float }

  type Direction =
      | NW 
      | NE
      | SW
      | SE
      
  type Node = { NW: QuadTree option; NE: QuadTree option; SE: QuadTree option; SW: QuadTree option }
  and QuadTree = { Bound : Bound; Max : int; Data : ConcurrentBag<Point>; mutable Node : Node option }

  let emptyNode = {NW = None; NE = None; SW = None; SE = None }

  let contains  (b: Bound)  (p: Point)  =
      match abs(p.Lat - b.Center.Lat) <= b.HalfDimension, abs(p.Lng- b.Center.Lng)<= b.HalfDimension with
      | true, true -> true
      | _, _ -> false

  let intersect (b:Bound) (b': Bound) =
      let distance = b'.HalfDimension + b.HalfDimension
      match abs(b'.Center.Lat - b.Center.Lat)<= distance, abs(b'.Center.Lng - b.Center.Lng)<=distance with
      | true,_ -> true
      | _, true -> true
      | _, _ -> false 

  let init p halfDimension max =
      { Bound = { Center = p; HalfDimension = halfDimension};
          Max = max;
          Data = new ConcurrentBag<Point>();
          Node = None }

  let createSubQuadTree (q: QuadTree) (dir : Direction) =
      let quarter = q.Bound.HalfDimension / 2.0
      match dir with
      | NW ->
        let center = { Lat = q.Bound.Center.Lat + quarter ; Lng = q.Bound.Center.Lng + quarter }
        init center quarter q.Max
      | SW -> 
          let center = { Lat = q.Bound.Center.Lat + quarter ; Lng = q.Bound.Center.Lng - quarter }
          init center quarter q.Max
      | NE ->
          let center = { Lat = q.Bound.Center.Lat - quarter ; Lng = q.Bound.Center.Lng + quarter }
          init center quarter q.Max
      | SE ->
          let center = { Lat = q.Bound.Center.Lat - quarter ; Lng = q.Bound.Center.Lng - quarter }
          init center quarter q.Max

  let subdivide (q : QuadTree) (dir : Direction) =
      match q.Node with
      | Some n ->
          match dir with
          | NW -> 
              match n.NW with
              | Some q' ->  q'
              | None -> 
                  let nq = createSubQuadTree q NW
                  q.Node <- Some {n with NW = Some(nq) }
                  nq
          | NE -> 
              match n.NE with 
              | Some q' -> q'
              | None -> 
                  let nq = createSubQuadTree q NE
                  q.Node <- Some {n with NE = Some(nq) }
                  nq
          | SW -> 
              match n.SW with
              | Some q' -> q'
              | None ->
                  let nq = createSubQuadTree q SW
                  q.Node <- Some {n with SW = Some(nq) }
                  nq
          | SE -> 
              match n.SE with
              | Some q' -> q'
              | None ->
                  let nq = createSubQuadTree q SE
                  q.Node <- Some {n with SE = Some(nq) }
                  nq
      | None -> 
          match dir with
          | NW -> 
              let nq = createSubQuadTree q NW
              q.Node <- Some {emptyNode with NW = Some(nq) }
              nq
          | NE -> 
              let nq = createSubQuadTree q NE
              q.Node <- Some {emptyNode with NE = Some(nq) }
              nq
          | SW ->
              let nq = createSubQuadTree q SW
              q.Node <- Some {emptyNode with SW = Some(nq) }
              nq
          | SE -> 
              let nq = createSubQuadTree q SE
              q.Node <- Some {emptyNode with SE = Some(nq) }
              nq

  let getSubQuadTree (n: Node option) (d :Direction) : QuadTree option =
      match n with
      | None -> None
      | Some n' ->
          match d with
          | NW -> n'.NW
          | NE -> n'.NE
          | SW -> n'.SW
          | SE -> n'.SE


  let rec insert (p: Point)(q : QuadTree)  =
      match p |> contains q.Bound with
      | false -> false
      | true ->
          match q.Data.Count < q.Max with
          | true -> 
              q.Data.Add p
              true
          | false ->
              match p.Lat, p.Lng with
              | x, y when x >= q.Bound.Center.Lat && y >= q.Bound.Center.Lng
                  -> subdivide q NW
                      |> insert p
              | x, y when x >= q.Bound.Center.Lat && y < q.Bound.Center.Lng
                  -> subdivide q SW
                      |> insert p
              | x, y when x < q.Bound.Center.Lat && y >= q.Bound.Center.Lng
                  -> subdivide q NE
                      |> insert p
              | _, _ -> subdivide q SE
                      |> insert p


  let rec remove (p : Point) (q: QuadTree) =
      match p |> contains q.Bound with
      | false -> false
      | true ->
          match q.Data.TryTake (ref p) with
          | true -> true
          | false ->
              match p.Lat, p.Lng with
              | x, y when x >= q.Bound.Center.Lat && y >= q.Bound.Center.Lng
                  ->  match getSubQuadTree q.Node NW with
                      | None -> true // or should it be false ?
                      | Some q ->  remove p q
              | x, y when x >= q.Bound.Center.Lat && y < q.Bound.Center.Lng
                  ->  match getSubQuadTree q.Node SW with
                      | None -> true // or should it be false ?
                      | Some q ->  remove p q
              | x, y when x < q.Bound.Center.Lat && y >= q.Bound.Center.Lng
                  ->  match getSubQuadTree q.Node NE with
                      | None -> true // or should it be false ?
                      | Some q ->  remove p q
              | _, _ 
                  ->  match getSubQuadTree q.Node SE with
                      | None -> true // or should it be false ?
                      | Some q ->  remove p q


  let findClosest (p: Point) (q: QuadTree) =
      let distance p' =
          (p'.Lat - p.Lat)**2.0 + (p'.Lng - p.Lng)**2.0
      let minimum (m : Point option) ( m' : Point option) =
          match m, m' with
          | None, None -> None
          | None, _ -> m'
          | _, None -> m
          | Some point, Some point' ->
              seq [point; point']
              |> Seq.minBy distance
              |> Option.Some
      let rec findClosest' (min : Point option)  (q' : QuadTree) =
          match q.Data.IsEmpty with
          | true -> min
          | false ->
              let min' =
                  q'.Data
                  |> Seq.minBy distance
                  |> Option.Some
                  |> minimum min
              match p.Lat, p.Lng with
              | x, y when x >= q'.Bound.Center.Lat && y >= q'.Bound.Center.Lng
                  ->  match getSubQuadTree q'.Node NW with
                          | None -> min'
                          | Some q -> findClosest' min' q
              | x, y when x >= q'.Bound.Center.Lat && y < q'.Bound.Center.Lng
                  ->  match getSubQuadTree q'.Node SW with
                      | None -> min'
                      | Some q -> findClosest' min' q
              | x, y when x < q'.Bound.Center.Lat && y >= q'.Bound.Center.Lng
                  ->  match getSubQuadTree q'.Node NE with
                      | None -> min'
                      | Some q -> findClosest' min' q
              | _, _ 
                  ->  match getSubQuadTree q'.Node SE with
                      | None -> min'
                      | Some q -> findClosest' min' q

      match p |> contains q.Bound with
      | false -> None
      | true -> findClosest' None q
      

