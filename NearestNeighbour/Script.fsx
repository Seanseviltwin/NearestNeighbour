module NearestNeighbour

type Point = float * float

type Distance = float

//Point -> Point -> Distance 
let getDistance (p1 : Point) (p2 : Point):Distance = 
    let (x1, y1) = p1
    let (x2, y2) = p2
    sqrt ((x2 - x1) ** 2.0 + (y2 - y1) ** 2.0)

//Point->Point->Point
let getMidPoint (p1 : Point) (p2 : Point):Point =
    let (x1, y1) = p1
    let (x2, y2) = p2
    let x = (x1 + x2) / 2.0
    let y = (y1 + y2) / 2.0
    (x,y)

let getAllDistances (points : seq<Point>) =
    Seq.map getDistance points
    let firstPoint = Seq.head points
    let distances = Seq.skip 1 points |> Seq.map (fun x -> getDistance firstPoint, x)
     