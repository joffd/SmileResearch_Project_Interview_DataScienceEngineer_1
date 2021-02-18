#r "nuget: Deedle"
#r "nuget: FSharp.Stats"
// third party .net packages 
#r "nuget: Plotly.NET, 2.0.0-beta5"
#r "nuget: FSharp.Data"
#I "data"

open System
open Deedle

/// DOMAIN

type ProductDetails = {
    StartDate: DateTimeOffset
    NbOfDays: int
    ResetTime: TimeSpan
    EndDate: DateTimeOffset
    GammaD: float
    ThetaD: float
}


type Reset = {
    DateTime: DateTimeOffset
    ResetIndex: float
}

type Resets = List<Reset>

type Holding = {
    Product: ProductDetails
    Resets: Resets
} with
    member this.addReset (reset: Reset) =
        { this with Resets = reset :: this.Resets}
    member this.addResets (resets: Resets) =
        { this with Resets = resets @ this.Resets}
    
    static member Create(product: ProductDetails, initLevel) =
        {
            Product = product
            Resets = [{DateTime = product.StartDate ; ResetIndex = initLevel}]
        }

/// CONST

// Min Reset Interval
let MIN_RESET_INTERVAL = TimeSpan(0,1,0)
let [<Literal>] CSV_FILE = @"C:\\Users\\joffr\\source\\repos\\SmileResearch_Project_Interview_DataScienceEngineer_1\\data\btc-perp.csv"

/// FUNCTIONS

// Create a standardized (midnight reset) product
let createProduct (startD: DateTimeOffset) (nb: int) (gammaD: float) (thetaD: float) =
    let startMidnight = startD.Subtract(startD.TimeOfDay)
    
    {
        StartDate = startMidnight
        NbOfDays = nb
        ResetTime = TimeSpan(0,0,0)
        EndDate = startMidnight.AddDays(float nb)
        GammaD = gammaD
        ThetaD = thetaD
    }


// Calc Realised Gamma PnL
// Complixity: O(n)
let calcRealisedGamma (h: Holding) =
    h.Resets
    |> List.pairwise
    |> List.sumBy (fun (x,y) -> 50. * h.Product.GammaD * pown (((y.ResetIndex - x.ResetIndex) / x.ResetIndex)) 2)

// Calc Realised PnL
let calcRealisedPnl (h: Holding) =
    fun (dt: DateTimeOffset) ->
        let nbDaysStarted = 
            if dt >= h.Product.EndDate then h.Product.NbOfDays
            else
                Math.Floor(dt.Subtract(h.Product.StartDate).TotalDays + 1.)
                |> int
            
        (calcRealisedGamma h) + (float nbDaysStarted) * h.Product.ThetaD

// 

/// Data Analysis

// Loading Data into a Deedle Frame
let btcPerp = 
    Frame.ReadCsv(CSV_FILE, hasHeaders=true, separators=";")
    
let seriesDT : Series<int, DateTimeOffset> =
    btcPerp
    |> Frame.getCol "UtcUnixTimeInMilliSeconds"
    |> Series.map (fun _ x -> DateTimeOffset.FromUnixTimeMilliseconds(x))
// Printing

btcPerp.ReplaceColumn("UtcUnixTimeInMilliSeconds",seriesDT.Values)
let newFrame = 
    Frame.indexRowsDateOffs "UtcUnixTimeInMilliSeconds" btcPerp
    |> Frame.sortRowsByKey
btcPerp.Print()
newFrame.Print()
let closes = newFrame?Close
let closesMidnight = closes |> Series.filter (fun k _ -> k.TimeOfDay = TimeSpan(0,0,0))


// Charting Price


//// TESTS
// Test product creation
let gammaD = 1825000.
let thetaD = -250000.
let nbDays = 5

let product1 = createProduct (DateTimeOffset.UtcNow) nbDays gammaD thetaD

let rnd = System.Random(Seed = 0)
let randoms = [for i in 0..4 do yield rnd.NextDouble()]


let resets =
    List.init 5 (fun i -> DateTimeOffset.UtcNow.AddHours(float (8 * i)), float i)
    |> List.map (fun (d,i) -> {DateTime = d ; ResetIndex = (50000. * (1.0 + i * 0.05)) |> round})

let initH = Holding.Create(product1, 50000.)

let calcRGamma = calcRealisedGamma (initH.addResets(resets))

let calcRPnl = calcRealisedPnl (initH.addResets(resets))
DateTimeOffset.UtcNow.AddHours(33.)
|> calcRPnl

