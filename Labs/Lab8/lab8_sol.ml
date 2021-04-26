(* Object-Oriented Programming in OCaml (Real World OCaml Chapter 12):
   Representing Grammars as Classes and Expressions as Objects *)

(* We can represent expressions given by the grammar:

   e ::= num | e + e

   by using objects from a class called "expression".  We begin with
   an abstract class (using the keyword "virtual" in OCaml) called
   "expression".  Although this class has no instances, it lists the
   operations common to all the different kinds of expressions.  These
   operations include a predicate "is_atomic" indicating whether there
   are subexpressions or not, operations to retrieve the left and
   right subexpressions (if the expression is not atomic), and a
   method computing the value of the expression.
 *)

class virtual expression = object
  method virtual is_atomic : bool
  method virtual left_sub : expression option
  method virtual right_sub : expression option
  method virtual value : int
end

(* Because the grammar has two cases, we have two subclasses of
   "expression", one for numbers, and one for sums.
 *)

class number_exp (n:int) = object
  inherit expression as super
  val mutable number_val = n
  method is_atomic = true
  method left_sub = None
  method right_sub = None
  method value = number_val
end               

class sum_exp (e1:expression) (e2:expression) = object
  inherit expression as super
  val mutable left_exp = e1
  val mutable right_exp = e2
  method is_atomic = false
  method left_sub = Some left_exp
  method right_sub = Some right_exp
  method value = left_exp#value + right_exp#value
end

(* QUESTION 1. Product Class and Method Calls *)
(* 1(a). Extend this class hierarchy by writing a "prod_exp" class to
   represent product expressions of the form:

   e ::= ... | e * e
 *)

(* Solution to 1(a): *)
class prod_exp (e1:expression) (e2:expression) = object
  inherit expression as super
  val mutable left_exp = e1
  val mutable right_exp = e2
  method is_atomic = false
  method left_sub = Some left_exp
  method right_sub = Some right_exp
  method value = left_exp#value * right_exp#value
end               

(* 1(b). Create objects representing the following expressions:
   - An expression "a" representing the number 3
   - An expression "b" representing the number 0
   - An expression "c" representing the number 5
   - An expression "d" representing the sum of "a" and "b"
   - An expression "e" representing the product of "d" and "c"
   Then send the message "value" to "e".
   Note that "e" represents the expression (3+0)*5.
 *)

(* Solution to 1(b): *)
let a = new number_exp 3
let b = new number_exp 0
let c = new number_exp 5
let d = new sum_exp a b
let e = new prod_exp d c
let _ = e#value

(* QUESTION 2. Unary Expressions *)
(* Extend the class hierarchy further by writing a "square_exp".
   The expression below written e^2 means "e squared":

   e ::= ... | e^2

   Changes will be required to the "expression" interface, so you will
   need to reimplement all the classes from above with these changes.
   Try to make as few changes as possible to the program. *)

(* Solution to Question 2 that also takes into account the response
   needed for Question 3 *)
class virtual expression = object
  method virtual is_atomic : bool
  method virtual sub_exp1 : expression option
  method virtual sub_exp2 : expression option
  method virtual sub_exp3 : expression option
  method virtual value : int
end

class number_exp (n:int) = object
  inherit expression as super
  val mutable number_val = n
  method is_atomic = true
  method sub_exp1 = None
  method sub_exp2 = None
  method sub_exp3 = None
  method value = number_val
end               

class sum_exp (e1:expression) (e2:expression) = object
  inherit expression as super
  val mutable left_exp = e1
  val mutable right_exp = e2
  method is_atomic = false
  method sub_exp1 = Some left_exp
  method sub_exp2 = Some right_exp
  method sub_exp3 = None
  method value = left_exp#value + right_exp#value
end               

class prod_exp (e1:expression) (e2:expression) = object
  inherit expression as super
  val mutable left_exp = e1
  val mutable right_exp = e2
  method is_atomic = false
  method sub_exp1 = Some left_exp
  method sub_exp2 = Some right_exp
  method sub_exp3 = None
  method value = left_exp#value * right_exp#value
end               

class square_exp (e:expression) = object
  inherit expression as super
  val mutable only_exp = e
  method is_atomic = false
  method sub_exp1 = Some only_exp
  method sub_exp2 = None
  method sub_exp3 = None
  method value = only_exp#value * only_exp#value
end               

(* QUESTION 3. Ternary Expressions and More Method Calls *)
(* 3(a). Extend this class heirarchy by writing a "cond_exp" class to
   represent conditionals of the form

   e ::= ... | e?e:e

   In a conditional expression a?b:c, evaluate "a" and if the value is
   not 0, then return the value of "b".  If the value of "a" is 0,
   then return the value of "c".

   Again, try to make as few changes as possible to the program.  If
   necessary, redesign the class hierarchy you created for Question 2
   so that it handles both unary and ternary expressions.

   3(b). Re-create all the objects a,b,c,d,e above and create new
   objects:
   - An expression "f" representing the square of "c"
   - An expression "g" representing the conditional b?e:f
   Then send the message "value" to "g".
 *)

(* Solution to Quesiton 3(a): *)
class cond_exp (e:expression) (e1:expression) (e2:expression) = object
  inherit expression as super
  val mutable test_exp = e
  val mutable then_exp = e1
  val mutable else_exp = e2
  method is_atomic = false
  method sub_exp1 = Some test_exp
  method sub_exp2 = Some then_exp
  method sub_exp3 = Some else_exp
  method value =
    let test_val = test_exp#value in
    if test_val <> 0 then then_exp#value else else_exp#value
end               

(* Solution to Quesiton 3(b): *)
let a = new number_exp 3
let b = new number_exp 0
let c = new number_exp 5
let d = new sum_exp a b
let e = new prod_exp d c
let _ = e#value
let f = new square_exp c
let g = new cond_exp b e f
let _ = g#value

(* 3(c) Enter the following expressions (for a,b,c,d,e,f,g) into OCaml
   so that you can see what is printed by the OCaml interpreter.  In
   each case, the type of the expression will be printed. Note that
   they are not all the same.

   Then enter the expression defining the value of "e_list".  Note
   that this is a list containing elements of type "expression", which
   is a different type than the ones printed out for a,b,c,d,e,f,g.
   Explain why these elements are all allowed to have more than one
   type in OCaml.
 *)

let _ = a
let _ = b
let _ = c
let _ = d
let _ = e
let _ = f
let _ = g
let e_list : expression list = [a;b;c;d;e;f;g]

(* Solution to 3(c): This is an example of subtyping.  All of the
   types "number_exp", "sum_exp", "prod_exp", "square_exp", and
   "cond_exp" are all subtypes of "expression". All the elements of a
   list must have the same type.  And this is the type that is common
   to them all.  *)

(* QUESTION 4. Redesign the entire hierarchy again, so that it
   includes a new operation that takes one argument (x:int) and
   modifies an expression object so that all of its leaves are
   incremented by the value of x.  (The leaves of an expression are
   all the subexpressions belonging to the "number_exp" class.)  This
   operation should not return a new instance of an "expression".  It
   should modify the instances that it is applied to.

   Re-create all the objects a,b,c,d,e,f,g again.  Then send the
   message "value" to "g".  Then apply the new operation with any
   argument value greater than 0.  Then send the message "value" to
   "g". The new value should be different than the original one.
   Verify that your implementation gives the expected new value.  *)

(* Solution to Question 4: *)
class virtual expression = object
  method virtual is_atomic : bool
  method virtual sub_exp1 : expression option
  method virtual sub_exp2 : expression option
  method virtual sub_exp3 : expression option
  method virtual value : int
  method virtual add_to_leaves : int -> unit
end

class number_exp (n:int) = object
  inherit expression as super
  val mutable number_val = n
  method is_atomic = true
  method sub_exp1 = None
  method sub_exp2 = None
  method sub_exp3 = None
  method value = number_val
  method add_to_leaves (x:int) = number_val <- n + x
end               

class sum_exp (e1:expression) (e2:expression) = object
  inherit expression as super
  val mutable left_exp = e1
  val mutable right_exp = e2
  method is_atomic = false
  method sub_exp1 = Some left_exp
  method sub_exp2 = Some right_exp
  method sub_exp3 = None
  method value = left_exp#value + right_exp#value
  method add_to_leaves (x:int) = (left_exp#add_to_leaves x;
                                  right_exp#add_to_leaves x)
end               

class prod_exp (e1:expression) (e2:expression) = object
  inherit expression as super
  val mutable left_exp = e1
  val mutable right_exp = e2
  method is_atomic = false
  method sub_exp1 = Some left_exp
  method sub_exp2 = Some right_exp
  method sub_exp3 = None
  method value = left_exp#value * right_exp#value
  method add_to_leaves (x:int) = (left_exp#add_to_leaves x;
                                  right_exp#add_to_leaves x)
end               

class square_exp (e:expression) = object
  inherit expression as super
  val mutable only_exp = e
  method is_atomic = false
  method sub_exp1 = Some only_exp
  method sub_exp2 = None
  method sub_exp3 = None
  method value = only_exp#value * only_exp#value
  method add_to_leaves (x:int) = only_exp#add_to_leaves x
end               

class cond_exp (e:expression) (e1:expression) (e2:expression) = object
  inherit expression as super
  val mutable test_exp = e
  val mutable then_exp = e1
  val mutable else_exp = e2
  method is_atomic = false
  method sub_exp1 = Some test_exp
  method sub_exp2 = Some then_exp
  method sub_exp3 = Some else_exp
  method value =
    let test_val = test_exp#value in
    if test_val <> 0 then then_exp#value else else_exp#value
  method add_to_leaves (x:int) = (test_exp#add_to_leaves x;
                                  then_exp#add_to_leaves x;
                                  else_exp#add_to_leaves x)
end

let a = new number_exp 3
let b = new number_exp 0
let c = new number_exp 5
let d = new sum_exp a b
let e = new prod_exp d c
let f = new square_exp c
let g = new cond_exp b e f
let _ = g#value (* 25 *)
let _ = g#add_to_leaves 2
let _ = g#value (* 49 *)
