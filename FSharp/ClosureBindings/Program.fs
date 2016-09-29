let outerFunc outerParam =
    let outerLocal = outerParam

    let innerFunc innerParam =
        outerLocal * innerParam + outerParam

    innerFunc

// What type is innerFunc?

// What type is outerFunc?

let closureFunc = outerFunc 5
closureFunc 10 |> printfn "%d"
// What do we expect to print?


let outerFunc2 outerParam =
    let mutable outerLocal = outerParam

    let innerFunc innerParam =
        outerLocal * innerParam + outerParam

    let returnValue = innerFunc
    outerLocal <- 100
    returnValue

let closure2 = outerFunc2 5
closure2 10 |> printfn "%d"