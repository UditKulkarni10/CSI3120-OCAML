(*** CSI 3120 Assignment 2 ***)
(*** Udit Kulkarni ***)
(*** 300053077 ***)
(*** 4.05.0 ***)
(* If you use the version available from the lab machines via VCL, the
   version is 4.05.0 ***)

(*************)
(* PROBLEM 1 *)
(*************)

(* For each part of problem 1, explain in the given string why the code
   will not typecheck, then follow the instructions for that part to
   change the code so that it typechecks while keeping the same values
   as intended by the erroneous code. Once you have done so, uncomment
   the fixed expression for submission.
*)

(* Problem 1a - Give your explanation in exp1a and then fix the
   right-hand-side of the assignment to match the listed type.
   (Do not change the left-hand-side.)
*)


let exp1a : string = "The defined type is a tuple within a list and so the contents of the list must be encomapssed in parenthesis 
in order to make it a tuple"

let prob1a : (string * int * char) list = [("7", 8, '9')];;


(* Problem 1b - Give your explanation in exp1b and then fix the type
   of variable prob1b to match the type of the expression on the
   right-hand-side of the assignment. (Do not change the
   right-hand-side.)
 *)


let exp1b : string = "The right hand side suggests that the tuple consists of a list of strings and a list of integers however the left hand type suggests 
that there exists a list with tuples containing a string element and an integer element"

let prob1b : (string list * int list) = (["apples";"bananas";"carrots"],[3;2;1]);;


(* Problem 1c - Give your explanation in exp1c and then fix the
   right-hand-side of the expression to match the variable prob1c's
   listed type.  (Do not change the left-hand-side.)
 *)


let exp1c : string = "The code would not run because the left side defines the list as being list containg sub string lists where as the code was 
creating a list of lists conataining sub lists of strings"

let prob1c : string list list = ["2"; "b"] :: ["or"; "not"; "2b"] :: ["that is" ; "the"] :: ["question"] :: []


(*************)
(* PROBLEM 2 *)
(*************)

(* Fill in expressions to satisfy the following types:
 *
 * NOTE: for option, list, and function types, you must
 * provide a nontrivial answer. For a list that means a
 * non-empty one, for an option type that means a Some
 * construction, and for a function, that means using
 * its arguments to generate the return value.
 * example problems:
 *   let x : int option = ???
 *   let y : int list = ???
 *   let f (x: int) (y: int) : int = ???
 * incorrect answers:
 *   let x : int option = None
 *   let y : int list = []
 *   let f (x: int) (y: int) : int = 7
 * possible correct answers:
 *   let x : int option = Some 1
 *   let y : int list = [1]
 *   let y : int list = [1; 2]
 *   let y : int list = 1 :: [2]
 *   let f (x: int) (y: int) : int = x + y
 *   let f (x: int) (y: int) : int =
 *         String.length  ((string_of_int x) ^ (string_of_int y))
 *)

(* Problem 2a *)

let prob2a : (int * ((string * float) option list)) list = [(12, [Some("hello", 8.0)])];;


(* Problem 2b *)
(* a pet is a (name, animal_type, age option) tuple *)

type pet = string * string * int option


let prob2b : string * pet list option = ("petsmart", Some [("max", "dog", Some 7)]);;



(* Problem 2c *)
(* Fill in a valid function call to f to make prob2c typecheck *)

let prob2c =
  let rec f arg =
    match arg with
    | (a, b) :: xs -> if a then (b ^ (f xs)) else f xs
    | _ -> ""
  in f [(true, "We"); (false, "hate"); (true, " love");(true, " coding") ;(true, " in Ocaml")]


(*************)
(* PROBLEM 3 *)
(*************)

(* Problem 3a.  You have been asked to write a text filter,
   where you want to find all search characters in your text
   if they appear the right order.

   Write a function text_filter that takes two lists of characters
   and checks to see if all the characters in the first list are
   included in the second list AND in the same order, BUT possibly
   with other characters in between.  For example

   text_filter ['a';'m';'z'] ['1';'a';'2';'m';'3';'z'] = true
   text_filter ['a';'m';'z'] ['1';'a';'3';'z'] = false
   text_filter ['a';'m';'z'] ['1';'z';'2';'m';'3';'a'] = false

*)
let rec text_filter (xs:char list) (ys:char list) : bool =
	match xs with
		| [] -> (match ys with
                | [] -> true
                |h1::_ -> false)
		| head::tail -> (match ys with
					| [] -> false
					|a::b -> if head = a then text_filter tail b else text_filter xs b)


(* Problem 3b. Rewrite the function above so that is is polymorphic,
   i.e., it should work on lists whose elements are any types.  Give
   at least one test case (call your function at least once) with a
   type that is different from chars. *)
let rec text_filter_poly (xs:_ list) (ys:_ list) : bool =
	match xs with
		| [] -> (match ys with
                | [] -> true
                |h1::_ -> false)
		| head::tail -> (match ys with
					| [] -> false
					|a::b -> if head = a then text_filter_poly tail b else text_filter_poly xs b)
 
 let test_textfilter_poly = text_filter_poly ["why";"hello";"you"] ["he"; "runs"; "why" ; "where"; "hello"; "when"; "there"; "you"]
 let test_textfilter_poly2 = text_filter_poly [1;7;4;5] [1;4;8;7;0;12;34;76;5;98]
 let test_textfilter_poly3 = text_filter_poly [2;4] [1;3;5;7;9;11;13;15]

(*************)
(* PROBLEM 4 *)
(*************)

(* Write a function (int_to_whole) that converts an integer
   into a whole number if one exists
   (a whole number is 1, 2, 3, ...).
   Use an option type because not all integer inputs can
   be converted. *)

type whole =  One | Succ of whole


let rec int_to_whole (integer:int) : whole option = 
   match integer with
   |0 -> None
   |1 -> Some One
   |integer -> int_to_whole(integer-1)



