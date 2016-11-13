module QuadTree
  
  open System.Collections.Concurrent

  type Point = { Lat :float; Lng : float}

  type Direction =
      | NW 
      | NE
      | SW
      | SE

  type IBound<'T> =  
    abstract member Contains : Point -> bool
    abstract member Split : Direction -> 'T
    abstract member GetDirection : Point -> Direction

  /// Rectangle type defined with
  /// * a Point as a Center
  /// * a Height
  /// * a Width
  type Rectangle =
    { Center : Point; 
    HalfHeight : float;
    HalfWidth : float } 
    interface IBound<Rectangle> with 
      member self.Contains (p: Point)  =
        match abs(p.Lat - self.Center.Lat) <= self.HalfHeight, abs(p.Lng- self.Center.Lng)<= self.HalfWidth with
        | true, true -> true
        | _, _ -> false
      member self.Split (d :Direction)  =
        let quarterHeight = self.HalfHeight / 2.0
        let quaterWidth = self.HalfWidth / 2.0
        match d with
        | NW ->
            let center = { Lat = self.Center.Lat + quarterHeight ; Lng = self.Center.Lng + quaterWidth }
            { Center = center; HalfHeight =  quarterHeight; HalfWidth = quaterWidth } 
        | SW -> 
            let center = { Lat = self.Center.Lat + quarterHeight ; Lng = self.Center.Lng - quaterWidth }
            { Center = center; HalfHeight =  quarterHeight; HalfWidth = quaterWidth }
        | NE ->
            let center = { Lat = self.Center.Lat - quarterHeight ; Lng = self.Center.Lng + quaterWidth }
            { Center = center; HalfHeight =  quarterHeight; HalfWidth = quaterWidth }
        | SE ->
            let center = { Lat = self.Center.Lat - quarterHeight ; Lng = self.Center.Lng - quaterWidth }
            { Center = center; HalfHeight =  quarterHeight; HalfWidth = quaterWidth }
      member self.GetDirection (p: Point) =
        match p.Lat, p.Lng with
        | x, y when x >= self.Center.Lat && y >= self.Center.Lng  ->  NW
        | x, y when x >= self.Center.Lat && y < self.Center.Lng -> SW
        | x, y when x < self.Center.Lat && y >=self.Center.Lng -> NE
        | _, _ -> SE


  /// Square type defined with
  /// * a Point as a Center
  /// * a Half Dimension
  type Square = 
    { Center : Point; 
    HalfDimension : float } 
    interface IBound<Square> with 
      member self.Contains (p: Point)  =
        match abs(p.Lat - self.Center.Lat) <= self.HalfDimension, abs(p.Lng- self.Center.Lng)<= self.HalfDimension with
        | true, true -> true
        | _, _ -> false
      member self.Split (d :Direction)  =
        let quarter = self.HalfDimension / 2.0
        match d with
        | NW ->
            let center = { Lat = self.Center.Lat + quarter ; Lng = self.Center.Lng + quarter }
            { Center = center; HalfDimension =  quarter } 
        | SW -> 
            let center = { Lat = self.Center.Lat + quarter ; Lng = self.Center.Lng - quarter }
            { Center = center; HalfDimension =  quarter }
        | NE ->
            let center = { Lat = self.Center.Lat - quarter ; Lng = self.Center.Lng + quarter }
            { Center = center; HalfDimension =  quarter }
        | SE ->
            let center = { Lat = self.Center.Lat - quarter ; Lng = self.Center.Lng - quarter }
            { Center = center; HalfDimension =  quarter }
      member self.GetDirection (p: Point) =
        match p.Lat, p.Lng with
        | x, y when x >= self.Center.Lat && y >= self.Center.Lng  ->  NW
        | x, y when x >= self.Center.Lat && y < self.Center.Lng -> SW
        | x, y when x < self.Center.Lat && y >=self.Center.Lng -> NE
        | _, _ -> SE

   
  type Node<'T when 'T :> IBound<'T>> = { NW: QuadTree<'T> option; NE: QuadTree<'T> option; SE: QuadTree<'T> option; SW: QuadTree<'T> option }
  and QuadTree<'T when 'T :> IBound<'T>> = { Bound : IBound<'T>; Max : int; Data : ConcurrentBag<Point>; mutable Node : Node<'T> option }

  let private emptyNode = {NW = None; NE = None; SW = None; SE = None }
  let init (bound : IBound<'T>)  max =
      { Bound = bound;
          Max = max;
          Data = new ConcurrentBag<Point>();
          Node = None }
  let private getSubQuadTree (n: Node<'T> option) (d :Direction) : QuadTree<'T> option =
      match n with
      | None -> None
      | Some n' ->
          match d with
          | NW -> n'.NW
          | NE -> n'.NE
          | SW -> n'.SW
          | SE -> n'.SE
  let private createSubQuadTree (q: QuadTree<'T>) (dir : Direction) =
      init (q.Bound.Split dir) q.Max

  let private initializeNode n =
     match n with 
     | Some n -> n
     | None -> emptyNode
  let private createNode q subq dir =
      let node = initializeNode q.Node
      match dir with
      | NW -> q.Node <- Some { node with NW = Some(subq)}
      | NE -> q.Node <- Some { node with NE = Some(subq)}
      | SW -> q.Node <- Some { node with SW = Some(subq)}
      | SE -> q.Node <- Some { node with SE = Some(subq)}
  let private subdivide (q : QuadTree<'T>) (dir : Direction) = 
     match getSubQuadTree q.Node dir with
     | Some q' ->  q'
     | None ->
         let subQuadTree = createSubQuadTree q dir
         createNode q subQuadTree dir
         subQuadTree

  /// <summary>
  /// Insert a Point in a QuadTree
  /// </summary>
  /// <returns>
  /// True if Point was inserted in QuadTree
  /// False otherwise
  /// </returns     
  let rec insert (q : QuadTree<'T>) (p: Point)  =
      match q.Bound.Contains p with
      | false -> false
      | true ->
          match q.Data.Count < q.Max with
          | true -> 
              q.Data.Add p
              true
          | false ->
              q.Bound.GetDirection p
              |> subdivide q 
              |> insert  <| p

  /// <summary>
  /// Remove a Point from a QuadTree
  /// </summary>
  /// <returns>
  /// True if Point was removed from QuadTree
  /// False otherwise
  /// </returns  
  let rec remove (q: QuadTree<'T>)  (p : Point) =
      match q.Bound.Contains p with
      | false -> false
      | true ->
          match q.Data.TryTake (ref p) with
          | true -> true
          | false ->
              let dir  = q.Bound.GetDirection p
              match getSubQuadTree q.Node dir with
              | None -> false
              | Some q ->  remove q p


  /// <summary>
  /// Returns the closest Point in a QuadTree
  /// </summary>
  /// <returns>
  /// Some(Point) if a closest match is found
  /// None otherwise
  /// </returns 
  let findClosest (q: QuadTree<'T>) (p: Point) =
      let distance p' =
          (p'.Lat - p.Lat)**2.0 + (p'.Lng - p.Lng)**2.0
      let minimum (m : Point option) ( m' : Point) =
          match m with
          | None -> Some m'
          | Some point ->
              seq [point; m']
              |> Seq.minBy distance
              |> Option.Some
      let rec findClosest' (min : Point option)  (q' : QuadTree<'T>) =
          match q'.Data.IsEmpty with
          | true -> min
          | false ->
              let min' =
                  q'.Data
                  |> Seq.minBy distance
                  |> minimum min
              let dir  = q'.Bound.GetDirection p
              match getSubQuadTree q'.Node dir with
              | None -> min'
              | Some q -> findClosest' min' q
      match  q.Bound.Contains p  with
      | false -> None
      | true -> findClosest' None q
      

