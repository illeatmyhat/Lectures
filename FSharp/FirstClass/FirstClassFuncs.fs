// A language construct is "first class" if it can be:
// 1. Assigned to a variable
// 2. Passed as a parameter
// 3. Returned from a function
// 4. Dynamically created (debatable)

let square x = x * x

// Show that functions in F# are first class.

let var = square // Function can be assigned to a variable!
// What does this let us do?
// For one, we can call the "square" function through the variable "var"
printfn "5 squared is %d" (var 5)



// This function takes "fn" and "v" as parameters. What types are they?
// Examine how they are used...
let applyFunction fn v =
    fn v

// The type signature for applyFunction is ('a -> 'b) -> 'a -> 'b. 
// What on earth does that mean?

printfn "7 squared is %d" (applyFunction square 7)
// So we passed "square" to "applyFunction". applyFunction needs two params:
// one function that takes a "a" parameter and returns a "b"; and a "a" value.
// Did we satisfy that?
// Therefore functions can be passed as parameters.


// () is for functions with no parameters.
let returnFunc() =
    square

printfn "10 squared is %d" (returnFunc() 10)


// So why bother? What does this DO for us?

// Let's write a function to find the sum of all the even numbers in a sequence
let sumEvens list =
    let mutable sum = 0
    for n in list do
        if n % 2 = 0 then
            sum <- sum + n
    sum

// Let's write a function to find the sum of all the ODD numbers in a sequence
let sumOdds list =
    let mutable sum = 0
    for n in list do
        if n % 2 = 1 then
            sum <- sum + n
    sum

// How about the sum of all multiples of 3?
// Etc...

// These functions all apply the same pattern: iterate through a sequence and sum any value
// from the sequence that makes a particular predicate function true.
// The actual function doesn't really matter as long as it returns a boolean. We can thus
// abstract the sum procedure to a single function, asking the caller to provide a boolean
// function to select which elements should be summed...
let sumList pred list =
    let mutable sum = 0
    for n in list do
        if pred n then
            sum <- sum + n
    sum

// Along with a predicate function isEven...
let isEven n = 
    n % 2 = 0

// To be applied as
printfn "The sum of all evens from 1 to 10 is %d" (sumList isEven (seq {1 .. 10}))
// isEven is passed as the parameter "pred". It is invoked on the line "if pred n then...".
// When the function isEven returns true for some n, that n is used in the sum.


// All those parentheses are ugly and can get in the way of type inference and readability.
// F# has a special "pipe forward" operator to solve the issue!
seq {1 .. 10}
|> sumList isEven
|> printfn "The sum of all evens from 1 to 10 is %d"
// Pipe forward takes a value and a function; feeds the value as the LAST argument to the function, and
// gives the result. That result can then be piped forward if necessary.





// Take a breather...



// Finally, how can functions be "dynamically created"? Two ways:
// As inner functions:
let getAdder baseVal =
    let adder x =
        baseVal + x
    adder

// Examine the type of getAdder: int -> (int -> int)
// getAdder returns a function that takes an int and returns an int...
// but how is the return value of that function calculated? It uses baseVal's value
// when getAdder is actually invoked. That is a DYNAMIC value that is not known until
// runtime, so the behavior of the resulting function won't be known until runtime. 


// To use it:
let addBy5 = getAdder 5
addBy5 10 |> printfn "5 + 10 = %d"

// Calling getAdder multiple times will result in multiple copies of "adder" being
// created, each with a different value of "baseVal".
let addBy100 = getAdder 100
addBy100 10 |> printfn "100 + 10 = %d"


// This may be troubling... how does it POSSIBLY work?







// The other way to dynamically create a function at runtime: the "fun" keyword.
// Say I want to call the earlier sumList function to find the sum of multiples of 3.
// I COULD write a new predicate "isMultOf3"... but I don't really need that function
// to have a name. I really just need an anonymous function that does what I need
// when I call sumList. That's what "fun" is for:
seq {1 .. 10}
|> sumList (fun x -> x % 3 = 0) // a function of 1 parameter, returning what?
|> printfn "Sum of multiples of 3 from 1 to 10 = %d"