// We use "let" to define functions, too.
let square x = x * x
// The parameters to the function follow its name, with no commas.
// The = sign starts the function body.
// There is no "return" in F#; the last expression in the function body is the return value.
// Jump to "main" below to see how to call this function.

// Note we did not define a type for x... so what type is it, and how is that determined?

// If the context of the function makes it clear, F# can infer the type of each parameter.
// In this case, F# defaults to "int" for x, because we perform x * x in the body.

// If the context is unclear, then other hints may be needed.

// Like variables, "square" has a type. Its type is "int -> int", which indicates a function
// taking an int variable and returning an int.




// Apply the given interest rate to the given principal amount for the given number of years,
// then return the resulting balance.
let interest principal rate numYears =
    let mutable year = 1
    let mutable balance = principal
    while year <= numYears do
        balance <- balance * (1.0 + rate)
        year <- year + 1
    balance

// What types are the three parameters? Infer it based on the body of the function.


// The type of "interest" is float -> float -> int -> float; a function taking a float, float,
// and int; and returning float.



// Convert Fahrenheit degrees to Celcius
let toCelcius tempF = (tempF - 32.0) * 5.0 / 9.0


// Challenges:
// Write a function that concatenates a string with itself.
let concat str = str + str




// Write a function that finds the harmonic mean of two floating-point numbers.
let harmonicMean x y = (1.0/ ((1.0/x) + (1.0/y)))




// Write a function that repeats a given string N times, with commas separating each
// copy of the string.
let rec repeatString str N =
    if N > 1 then
        str + (repeatString str (N - 1))
    else
        str

printfn "%s" (repeatString "foo" 5)




[<EntryPoint>]
let main argv = 
    printfn "5 squared is %d" (square 5)
    
    // "Other hints" about the type of a parameter:
    // The next line only works if the previous line is commented out.
    // printfn "5.0 squared is %d" (square 5.0)
    
    // Lacking any other information about what type the parameter to "square" is, F#
    // can examine where the function is called and infer the type of the parameter based
    // on the type of the argument.



    printfn "$10,000 after 10 years at 0.01%% interest yields %0.2f" (interest 10000.0 0.0001 10)

    printfn "90 degrees F = %0.1f degrees C" (toCelcius 90.0)    

    
    0 // return an integer exit code
