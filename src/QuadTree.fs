module QuadTree

type Point = { Lat :float; Lng : float}
type Bound = { Center : Point; HalfDimension :float }


type Direction =
    | NW 
    | NE
    | SW
    | SE
    
type Node = { NW: QuadTree option ; NE: QuadTree option ; SE: QuadTree option; SW: QuadTree option }
and QuadTree = { Bound : Bound; Max : int; mutable Index : int; mutable Data : Point array; mutable Node : Node option }

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
        Index = 0;
        Data = Array.zeroCreate max;
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

let rec insert (p: Point) (q : QuadTree)  =
    q |> ignore
    match p |> contains q.Bound with
    | false -> false
    | true ->
        match q.Index < q.Max with
        | true -> 
            q.Data.[q.Index] <- p
            q.Index <- q.Index + 1
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
            | x, y -> subdivide q SE
                    |> insert p

