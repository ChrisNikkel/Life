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

    //TODO: use collect instead of this
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

    let removeDups data = data |> Set.ofList |> Set.toList

    let rec iterateAliveGeneration previousData newData cellIndex =
        if cellIndex < List.length previousData then
            let aliveCell = previousData.[cellIndex] 
            let (aliveNeighbors, deadNeighbors) = categorizeNeighbors previousData aliveCell
            let newAliveData = 
                if survives (List.length aliveNeighbors)
                then aliveCell :: newData |> removeDups
                else newData
            iterateDeadGeneration previousData deadNeighbors 
                |> List.append (iterateAliveGeneration previousData (newAliveData) (cellIndex + 1)) |> removeDups
        else
            newData 

    let doIteration initialData = (iterateAliveGeneration initialData [] 0)

    let setItemPosition x y item = item |> List.map (fun (ix, iy) -> (ix + x, iy + y))
    let addItem = List.append
    let crossItem = [(1, 1); (1, 2); (1, 3)]
    let gliderItem = [(1, 1); (2, 1); (3, 1); (3, 2); (2, 3)]
    let toadItem = [(1, 1); (2, 1); (3, 1); (2, 2); (3, 2); (4, 2)]
    let blockItem = [(1, 1); (1, 2); (2, 1); (2, 2)]
    let beaconItem = [(4, 1); (4, 2); (3, 1); (1, 3); (1, 4); (2, 4)]

    let initialData = 
        blockItem
        |> addItem (gliderItem |> setItemPosition 10 85)
        |> addItem (crossItem |> setItemPosition 20 50)
        |> addItem (toadItem |> setItemPosition 50 50)

    let createTimer interval =
        let timer = new System.Windows.Forms.Timer(Interval = int(interval * 1000.0), Enabled = true)
        timer.Start()
        timer.Tick
    
    let events = createTimer 0.1
    let eventStream = events |> Observable.scan (fun data _ -> doIteration data) initialData
    let chart = LiveChart.Point(eventStream, Name = "Life").WithXAxis(Enabled = false, Min = 0.0, Max = 100.0).WithYAxis(Enabled = false, Min = 1.0, Max = 100.0)
    let form = chart.ShowChart()
    form.Width <- 800
    form.Height <- 800

    System.Windows.Forms.Application.Run(form)
    0 // return an integer exit code
