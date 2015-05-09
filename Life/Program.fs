// Copyright (c)  2015 Christopher Nikkel
open FSharp.Charting
open System
open System.Drawing
open System.Windows.Forms

[<EntryPoint>]
let main argv = 
    let combine (a, b) = (fst a + fst b, snd a + snd b) 

    let surrounding = 
        [for i in -1..1 -> [for j in -1..1 -> (i, j) ] ] 
        |> List.concat

//use collect instead of this
    let findSurrounding d = 
        surrounding 
        |> List.map(fun cell -> combine(d, cell)) 
        |> List.filter(fun x -> x<>d)

    let categorizeNeighbors d centerCell = 
        List.partition (fun cell -> (List.exists (fun c -> c = cell) d)) (findSurrounding centerCell)   
   
    let countNeighbors d centerCell =
        List.length (fst (categorizeNeighbors d centerCell))
    
   
    // Any live cell with fewer than two live neighbours dies, as if caused by under-population.
    // Any live cell with two or three live neighbours lives on to the next generation.
    // Any live cell with more than three live neighbours dies, as if by overcrowding.
    // Any dead cell with exactly three live neighbours becomes a live cell, as if by reproduction.
    let survives neighborCount =
        match neighborCount with
            | 2 | 3 -> true
            | _ -> false

    let born neighborCount = if neighborCount = 3 then true else false

    let iterateDeadGeneration previousData deadCells = 
        deadCells |> List.filter(fun deadCell -> born (countNeighbors previousData deadCell))

    let sortAndRemoveDuplicates data = List.ofSeq (set data)      

    let rec iterateAliveGeneration previousData newData cellIndex =
        if cellIndex < List.length previousData then
            let aliveCell = previousData.[cellIndex] 
            let (aliveNeighbors, deadNeighbors) = categorizeNeighbors previousData aliveCell
            let newAliveData = 
                if survives (List.length aliveNeighbors)
                then aliveCell :: newData
                else newData
            iterateDeadGeneration previousData deadNeighbors 
                |> List.append (iterateAliveGeneration previousData (newAliveData) (cellIndex + 1))
        else
            sortAndRemoveDuplicates newData 

    let data1 = [(1, 1); (1, 2); (1, 3)]   
     
    let data2 = iterateAliveGeneration data1 [] 0
    

    let chart1 = data1 |> Chart.FastPoint
    let chart2 = data2 |> Chart.FastPoint
    
    let chart = [chart1; chart2] |> Chart.Combine |> Chart.WithXAxis(Enabled=false) |> Chart.WithYAxis(Enabled=false)
    
    let form = chart.ShowChart()

    System.Windows.Forms.Application.Run(form)

    0 // return an integer exit code
