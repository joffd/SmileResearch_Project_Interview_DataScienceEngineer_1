

open System

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

// Min Reset Interval
let MIN_RESET_INTERVAL = TimeSpan(0,1,0)

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

//// TESTS
// Test product creation
let gammaD = 1825000.
let thetaD = -250000.
let nbDays = 2

let product1 = createProduct (DateTimeOffset.UtcNow) nbDays gammaD thetaD

