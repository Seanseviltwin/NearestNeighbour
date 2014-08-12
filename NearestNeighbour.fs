module NearestNeighbour
//Nearest neighbours
open System
open System.Collections.Generic

let addBothResultsToTheMap (mapy : Map<'a*'a,'b>) (tuple : 'a*'a*'b) : Map<'a*'a,'b>=
    let first, second, result = tuple
    let map2 = mapy.Add ((first,second), result)
    map2.Add((second,first),result)

let addOneResultToTheMap (mapy : Map<'a*'a,'b>) (tuple : 'a*'a*'b) : Map<'a*'a,'b>=
    let first, second, result = tuple
    mapy.Add ((first,second), result)
    
//split because I'm going to need to do this to individual rows at some point
let mapOfOneValueFuncedWithARangeOfValues (pairFunc : 'a->'a->'b)  (values : 'a list) (value :'a): Map<'a*'a,'b>=
    (values |> List.fold (fun x y-> addOneResultToTheMap x (value, y , (pairFunc value y))) Map.empty) 

let rec upperLeftRec (map : Map<'a*'a,'b>) (pairFunc : 'a->'a->'b)  (values : 'a list) : Map<'a*'a,'b>= 
    let head = values.Head
    let tail = values.Tail
    match tail with 
    | [] -> map
    | _ -> 
        let newMap = mapOfOneValueFuncedWithARangeOfValues pairFunc tail head
                        |> Map.toSeq 
                        |> Seq.fold (fun (x:Map<'a*'a,'b>) y -> x.Add(y)) map //There must be a nicer way of doing this.
        upperLeftRec newMap pairFunc tail

let upperLeft (pairFunc : 'a->'a->'b)  (values : 'a list) = upperLeftRec Map.empty pairFunc values

type Point = string * float * float
type Distance = float
type DistanceMatrix = Map<Point * Point, Distance>

let distanceBetweenPoints (a:Point) (b:Point):Distance=
    let aLabel, a1, a2 = a
    let bLabel, b1, b2 = b
    sqrt((b2-a2)**2. + (b1-a1)**2.)
    
let pointsToDistanceMap (points : Point list) =
    upperLeft distanceBetweenPoints points

let pointsWithShortestDistance (map:Map<Point*Point,Distance>) : Point * Point =
    map |> Map.toSeq|> Seq.minBy snd |> fst

let midpoint point1 point2:Point =
    let label1, x1,y1 = point1
    let label2, x2,y2 = point2
    ("{"+label1+":"+label2+")",0.5*(x1+x2),0.5*(y1+y2))

let nearestNeighbour points : Point*Point =
    pointsWithShortestDistance (pointsToDistanceMap points)

let rec FindNearestNeighboursRec map (listOfReplacedPoints: (Point * Point * Point) list) points : (Point * Point * Point) list =
    match points with
    | [] -> listOfReplacedPoints
    | _  ->
         let point1,point2 = pointsWithShortestDistance map
         let newPoint = midpoint point1 point2
         let newListOfReplacedPoints = (point1, point2, newPoint) :: listOfReplacedPoints
         let pointsWithoutNewPoint = points |> List.filter (fun x -> x.Equals(point1)||x.Equals(point2))
         let newMap = mapOfOneValueFuncedWithARangeOfValues distanceBetweenPoints pointsWithoutNewPoint newPoint
                        |> Map.toSeq 
                        |> Seq.fold (fun (x:Map<'a*'a,'b>) y -> x.Add(y)) map
                        |> Map.filter (fun (x,y) z -> x.Equals(point1)||x.Equals(point2)|| y.Equals(point1)||y.Equals(point2))
         let newPoints = newPoint::pointsWithoutNewPoint
         FindNearestNeighboursRec newMap newListOfReplacedPoints newPoints



let FindNearestNeighbours points =
    FindNearestNeighboursRec (pointsToDistanceMap points) List.empty points