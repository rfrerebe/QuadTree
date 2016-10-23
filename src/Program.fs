open QuadTree

[<EntryPoint>]
let main argv =
    let origin = {Lat = 0.0; Lng = 0.0}
    let point1 =  {Lat = 1.0; Lng = 1.0}
    let point2 =  {Lat = 2.0; Lng = 1.0}
    let box = { Center = origin; HalfDimension = 1.0 }

    let quadTree = init origin 2.0 1

    let result = insert origin quadTree 
    quadTree |> ignore
    let result2 = insert point1 quadTree 
    quadTree |> ignore
    let result3 = insert point2 quadTree 
    quadTree |> ignore
    0 // return an integer exit code