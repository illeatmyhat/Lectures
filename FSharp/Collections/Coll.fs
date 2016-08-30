// Arrays are fixed-size, mutable collections of homogenous data.
let array = [|1; 2; 3; 4; 5|] // semicolons to separate
let first = array.[0] // . operator

let printArray (arr:int[]) =
    // The type annotation is necessary to tell the compiler that arr is an array.

    // the F# "for" loop runs while the var is <= the limit.
    for i = 0 to arr.Length - 1 do
        printf "%d " arr.[i]
    printfn ""
    // another way
    for i in arr do
        printf "%d " i
    printfn ""
// Jump down to main...


// A list is an immutable collection of homogenous data.
let list1 = ["a"; "b"] // note the difference with arrays
let list2 = "c" :: list1 // :: is called "cons" ; create a new list by placing a new 
                         // head on an existing list

let list3 = list1 @ list2 // @ is "concat": concatenate/append two lists

let printList li =
    // Lists do have .[] indexing like arrays, but lists are secretly LINKED LISTS...
    // and what is the O(_) runtime complexity of indexing a linked list?



    // For that reason, we don't typically index lists. We can iterate through them instead.
    for i in li do
        printf "%s " i
    printfn ""
// Jump down to main...


// Finally, sequences.
// A sequence is an immutable iterable collection of values, with no concern for how the values
// are represented in the sequence. Arrays are sequences. Lists are sequences too; in a way, they 
// are both subclasses of the "seq" type.

// The seq keyword and the yield keyword can define sequences.
let mySeq = seq {yield 1; yield 2; yield 3}
// The seq above has 3 elements, but there is no specification for how they are represented in a 
// data structure. This has an important effect: all 3 values don't necessarily have to be in memory
// at the same time.

// Seq can be defined with .. as well.
let longSeq = seq {1L .. 1000000000000L}
// Comment out the long list in main, then examine the memory consumption. 
// Even though we still have this VERY LONG SEQUENCE, our memory consumption is way down... why?

[<EntryPoint>]
let main argv = 
    // Demonstrate passing arrays to functions
    printArray array
    printfn "\n"
    array.[0] <- 100
    printArray array

    printfn "\n"
    // Declare a mutable list and mutate it
    let mutable myList = ["s" ; "h" ; "a" ; "r"]
    printList myList
    myList <- "f" :: myList
    printList myList
    myList <- myList @ ["p"]
    printList myList

    // The List module (think: class) has many methods for working with lists.
    let numbers = [5; 2; 9; 1; 0; 4]
    printfn "The sum is %d" (List.sum numbers)

    // Useful methods: List.length, List.contains, List.isEmpty, List.sum, List.max


    // Lists can be constucted over subranges with [ .. ] syntax.
    let lotsOfNumbers = [ 1 .. 40000000 ]
    printfn "The largest of 1 .. 40000000 is %d" (List.max lotsOfNumbers)

    // Run this program and monitor its MEMORY CONSUMPTION -- WOW!!!
    // Where is that coming from?





    // The Seq module has similar methods for working with sequences.
    // printfn "The largest of 1 .. 1000000000000L is %d" (Seq.max longSeq)
    // So what's the difference?



    System.Console.ReadLine() // wait for user to press Enter to close
    0
    
