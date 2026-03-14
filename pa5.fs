
// 11 April 2022

// pa5.fs
// This assignment will be to write an expression evaluation function that take as arguments a 
// list of variable bindings and a string that is composed of letters and operators from the set [+ - * /$@].  
// It will scan the string from left to right, interpreting the string as an expression in 
// postfix Polish notation (except for the $ and @ special characters). 
//------------------------------------------------------------------------------------------------------

// All of the code you turn in must have been written by you without immediate 
// reference to another solution to the problem you are solving.  That means that you can look at 
// other programs to see how someone solved a similar problem, but you shouldn't have any code 
// written by someone else visible when you write yours (and you shouldn't have looked at a 
// solution just a few seconds before you type!).  You should compose the code you write based on 
// your understanding of how the features of the language you are using can be used to implement 
    // the algorithm you have chosen to solve the problem you are addressing.  Doing i/cutting and pasting 
// stuff you don't actually understand.  It is the only way to achieve the learning objectives of the 
// course. 

open System

// Retrieves the val with the letter and push it on the stack
let rec numFinder element vars =
    match vars with
    | [] -> 0
    | (a, b) :: tl -> 
        if char a = element then b
        else numFinder element tl

// Updates stack based on the operator
let exprCheck element vars (stack : int list) =
    match element with
    | '$' -> stack.Tail.Head :: stack.Head :: stack.Tail.Tail       //swap
    | '*' -> (stack.Tail.Head * stack.Head) :: stack.Tail.Tail      //multiply
    | '+' -> (stack.Tail.Head + stack.Head) :: stack.Tail.Tail      //add
    | '-' -> (stack.Tail.Head - stack.Head) :: stack.Tail.Tail      //subtracts
    | '/' -> (stack.Tail.Head / stack.Head) :: stack.Tail.Tail      //divides
    | ' ' -> stack                                                  //skip
    | _   -> (numFinder element vars) :: stack                      //find num to add to stack

let eval vars expr = 
    let rec innerEval vars (stack : int list) expr = 
        match expr with
        | [] -> stack.Head
        | hd :: (tl : char list) -> 
            if hd = '@' then       //updates vars if hd = @
                let newVars = (string tl.Head, stack.Head) :: vars
                innerEval newVars stack tl.Tail
            else //calls exprCheck
                let newStack = exprCheck hd vars stack 
                innerEval vars newStack tl
    innerEval vars [] (Seq.toList expr)

//------------------------------------------- End of program

// Provided EXPR
let testEval = eval [("a",5);("b",2);("c",9)]
let exprList =["ab+"; "cab+-"; "cab+$-"; "ab+cb-"; "ab+cb- @d bd+"; 
                "ab-abab+--"; "bbb @q bqbq**"; "ca- @b bc$-"]
let resultList = List.map testEval exprList
resultList |>  List.iter (fun item -> printfn "%i" item)

// The answer is
// 7
//  2
// -2
// 49
// 51
// 0
// 64
// 5
