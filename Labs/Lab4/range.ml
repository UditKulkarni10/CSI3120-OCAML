(* A condensed version of the signature in range.mli.  Your first step is to study the contents of range.mli. *)
module type RANGE =
sig
  type t
  type e
  val singleton : e -> t
  val range : e -> e -> t
  val sadd : t -> e -> t
  val smult : t -> e -> t
  val bridge : t -> t -> t
  val size : t -> int
  val contains : t -> e -> bool
  val rless : t -> t -> bool option
end

(* An implementation of the RANGE datatype with int as range type and
   pairs representing a range *)
(* module LoHiPairRange : RANGE with type e = int =
struct
  type e = int
  type t = e * e
  let singleton (i:e) : t = (i,i)
  let range (i:e) (j:e) : t = ((min i j), (max i j))
  let sadd (x:t) (i:e) : t = let (lo,hi) = x in (lo+i,hi+i)
  let smult (x:t) (i:e) : t =
    let (lo, hi) = x in
    if i >= 0 then (lo*i,hi*i)
    else (hi*i,lo*i)
  let bridge (x:t) (y:t) : t =
    let (lx, hx) = x in
    let (ly, hy) = y in
    ((min lx ly), (max hx hy))
  let size (x:t) : int =
    let (lo,hi) = x in
    hi - lo - (-1)
  let contains (x:t) (i:e) : bool =
    let (lo,hi) = x in
    (lo <= i) && (i <= hi)
  let rless (x:t) (y:t) : bool option =
    let (lx, hx) = x in
    let (ly, hy) = y in
    if hx < ly then Some true
    else if hy < lx then Some false
    else None
end*)

(* Exercise 1: Complete the new implementation of RANGE in the
     ListRange module below.  The part that is already implemented
     should give you enough information to implement the rest.  Add
     some test code to test your implementation. *)
    
(* An implementation of the RANGE datatype with int as range type and
   lists representing a range *)
module ListRange : RANGE with type e = int =
struct
  type e = int
  type t = e list

  (* auxiliary functions *)
  let minmax (l:t) : (e*e) option =
      let rec max (t:t) (e:e) : e =
          match t with
          | [] -> e
          | h::r -> max r h
      in
      match l with
      | [] -> None
      | h::r -> Some (h, (max r h))
  let rec build (i:e) (j:e) : e list =
    if i = j then [j]
    else i :: build (i+1) j
  
  let singleton (i:e) : t = [i]
  let range (i:e) (j:e) : t = build (min i j) (max i j)
  (* TODO Exercise 1: Replace all the code below with correct implementations of the operations. *)
  let sadd (x:t) (i:e) : t = 
    (match minmax(x) with  
      |Some(a, b) -> range(a+i)(b+i)
      |None -> [])
      
  let smult (x:t) (i:e) : t = 
    (match minmax(x) with 
      |None -> []
      |Some(a, b) -> range(a*i)(b*i))
      
  let bridge (x:t) (y:t) : t = 
    (match minmax(x) with  
      |None -> []
      |Some(a,b) -> (match minmax(y) with 
                          |None -> []
                          |Some(a1, b1) -> range a b1))
  let size (x:t) : int = 
    (match minmax(x) with
      |None -> 0
      |Some(a, b) -> b-a)

  let contains (x:t) (i:e) : bool = 
    (match minmax(x) with 
      |None -> false
      |Some(a,b) -> if i > a && i < b then true else false)

  let rless (x:t) (y:t) : bool option = 
    (match minmax(x) with 
      |None -> None 
      |Some(a, b) -> (match minmax(y) with
        |None -> None
        |Some(a1, b1) -> if a > a1 then None else Some(b < a1)))
end

(* TODO Exercise 1: Add some test code to test your new implementation. *)
let s = ListRange.range 3 15
let b = ListRange.range (-5) 6
let t = ListRange.smult s 2
let j = ListRange.contains b 4
let b = ListRange.bridge s b

(* Exercise 2: Design an imperative version of RANGE.  Do so by
   copying range.mli here and changing the types as necessary.  And
   then copy the implementation of LoHiPairRange and modify the code
   as necessary.  All the operations should remain the same as in the
   functional version.  The singleton and range operations should each
   create a new range.  The sadd and smult operations should modify
   existing ranges. Consider the design choices and design your own
   version of bridge. *)

module type RANGE =
sig
  (* types *)
  (* RANGE type *)
  type t
  (* element type *)
  type e
    
  (* constructors *)
  (* construct a one-item range *)
  val singleton : e -> t
  (* construct a range with two endpoints, inclusive *)
  val range : e -> e -> t

  (* modifiers *)
  (* scalar add range, e.g. if r is a range from -4 through 6, 
     sadd r 1 produces a range from  -3 through 7. 
     This operation does not change the size of a range. *)
  val sadd : t -> e -> unit
  (* scalar multiply range, e.g. if r is a range from 2 through 4,
     smult r 3 produces a range from 6 through 12. 
     This operation may change the size of a range. *)                        
  val smult : t -> e -> unit
  (* create a new range that spans both given ranges, e.g.
     if given a range from -4 through 6 and a range from 10 through 12, 
     produces a range from -4 through 12. *)
  val bridge : t -> t -> t

  (* observers *)
  (* how many elements are in the range? *)
  val size : t -> int
  (* does t contain e? *)
  val contains : t -> e -> bool
  (* is an arbitrary element of the first range 
      less than an arbitrary element of the second range?
     if the ranges overlap, return None, because 
      answers differ depending on the element chosen
     otherwise return whether the first range's max < second range's min
   *)
  val rless : t -> t -> bool option
      
end

module LoHiPairRange : RANGE with type e = int =
  struct
      type e = int
      type t = (e * e) ref
      let singleton (i:e) : t = ref (i,i)
      let range (i:e) (j:e) : t = ref ((min i j), (max i j))

      let sadd (x:t) (i:e) : unit = 
        let (lo,hi) = !x in x := (lo+i, hi+i)

      let smult (x:t) (i:e) : unit =
        let (lo, hi) = !x in
        if i >= 0 then x := (lo*i,hi*i)
        else x := (hi*i,lo*i)


      let bridge (x:t) (y:t) : t =
        let (lx, hx) = !x in
        let (ly, hy) = !y in
        ref ((min lx ly), (max hx hy))

      let size (x:t) : int =
        let (lo,hi) = !x in
        hi - lo - (-1)
      let contains (x:t) (i:e) : bool =
        let (lo,hi) = !x in
        (lo <= i) && (i <= hi)
      let rless (x:t) (y:t) : bool option =
        let (lx, hx) = !x in
        let (ly, hy) = !y in
        if hx < ly then Some true
        else if hy < lx then Some false
        else None
    end