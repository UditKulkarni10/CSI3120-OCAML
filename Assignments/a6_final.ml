(*** CSI 3120 Assignment 6 ***)
(*** Udit Kulkarni***)
(*** 300053077***)
(*** 4.05.0 ***)
(* If you use the version available from the lab machines via VCL, the
   version is 4.05.0 ***)

(*********************************************)
(* PROBLEM 1: Function-Oriented Organization *)
(*********************************************)

(* Consider the OCaml code below. *)

type fBalance = float
type fInterestRate = float
type fMonthlyPayment = float

type loan =
  | NotePayable of fBalance
  | CreditCard of fBalance * fInterestRate
  | BankLoan of fBalance * fMonthlyPayment

(* Problem 1(a) *)
(* Write a function that takes a list of loans and returns a
   list of balances.  Your function must preserve order.  For
   example, the first element of the result must be the balance of the
   first loan in the input list *)

(* let get_fBalances l =
   or
   let rec get_fBalances l = *)

let get_fBalances (loans: loan list) : fBalance list = 
   let (bal_list : fBalance list) = [] in 
      let rec add_to_bal (loans: loan list)(bal_list: fBalance list) : fBalance list =
         match loans with 
            |[] -> bal_list
            |head :: tail -> match head with 
                                 |NotePayable x -> add_to_bal tail (List.append bal_list [x])
                                 |CreditCard (a, b) -> add_to_bal tail (List.append bal_list [a])
                                 |BankLoan (a, b) -> add_to_bal tail (List.append bal_list [a]) 
         in
   add_to_bal loans bal_list



(* Problem 1(b)  *)
(* Write some test code: Create a list containing 3 loan accounts (one
   of each kind).  Set the monthly payment for the bank loan
   to 0. Apply your function from part 1(a) to your list of accounts *)

let np = NotePayable(75.54)
let cc = CreditCard(2478.76, 0.5)
let bl = BankLoan(1500000., 400.)

let test1 = get_fBalances[cc;np;bl]

(*******************************************)
(* PROBLEM 2: Object-Oriented Organization *)
(*******************************************)

(* Your code for parts 2(a) and (b) go here *)


exception Overpayed of float

 class note_payable = 
   object(self)
      val mutable fBalance :float = 0.
      method get_balance :float = fBalance
      method payback (a: float) = 
         if a >= fBalance then raise (Overpayed fBalance) else fBalance <- (fBalance -. a)
      method borrow (a:float) = fBalance <- (fBalance +. a)
      method to_loan : loan = NotePayable(fBalance) 
   end
      

class credit_card = 
   object(self)
      inherit note_payable as super
      val mutable fInterestRate:float = 0.2
      method get_interest:float = fInterestRate
      method set_interest (rate : float) = fInterestRate <- rate
      method add_interest = self#borrow (self#get_balance *. fInterestRate)
      method to_loan: loan = CreditCard(self#get_balance, fInterestRate)
   end


class bank_loan = 
   object(self)
      inherit note_payable as super
      val mutable fMonthlyPayment : float = 0.
      method get_mpayment: float = fMonthlyPayment
      method borrow (amount :float) = super#borrow (self#get_balance +. amount);
                                       fMonthlyPayment <- fMonthlyPayment +. (amount *. 0.1)
      method payback_monthly_amount = super#payback (self#get_balance -. fMonthlyPayment)
      method to_loan: loan = BankLoan(self#get_balance,fMonthlyPayment)
   end


(* Problem 2(a)  *)
(* Implement an inheritance hierarchy of loans (an object-oriented
   version of the data type in Problem 1). The credit_card and
   bank_loan classes should be subclasses of the note_payable class.
   The arguments to the NotePayable, CreditCard, and BankLoan
   constructors of the loan data type should become instance
   variables.  The initial values should be 0.0 for the balance, 0.2
   (representing 20%) for the interest rate, and 0 for the monthly
   payment.  Define a method called get_balance that returns the
   balance amount.

   Use the following programming conventions.
   - Do not use abstract classes.
   - The instance variables and methods should go in the highest class
     possible in the hierarchy to maximize inheritance.  Only override
     methods when necessary.  (In this hierarchy credit cards are
     the only kind of loan with an interest rate and bank
     loans are the only kind of loan with a monthly payment.)
   - In a subclass, do not use instance variables of the super class
     directly.  For example, if the implementation of a method in
     credit_card needs to access the balance amount, then it must
     call "get_balance".

   Implement methods called "borrow" and "payback" that take one
   argument, the amount to add to (borrow) or subtract from (payback)
   the balance.  If the amount of the payback is more than the balance,
   raise an exception that takes one argument. The data returned
   when this exception is raised should be the loan balance.

   Add methods in credit_card to get and set the interest rate.
   Also add an "add_interest" method that modifies the balance by
   adding interest to the balance using the interest rate.

   In the bank_loan class, override the borrow method so that
   it also increases the monthly payment by 10% (0.1) of the borrowed
   amount.  So borrowing an additional 100.00 would add 10.00 to the
   monthly payment.  Add a "payback_monthly_amount" method that
   reduces the balance by the monthly payment amount.
   Also add a method to get the value of the monthly payment,
   but do not allow clients to set it. *)


(* Problem 2(b)  *)
(* Add a method "to_loan" to every class that transforms an
   object to the corresponding value of type loan (where
   loan is the data type defined at the beginning of this file
   just before the statement of Problem 1(a)).  (It must return an
   element of type loan where the values of the arguments are
   determined from the values of the instance variables. *)


(*********************************************)
(* PROBLEM 3: Object-Oriented "Constructors" *)
(*********************************************)
(* Problem 3(a)  *)
(* Write a function that takes an argument, creates a note_payable
   object, and then uses the argument to update the balance.  Do the
   same for credit_card and bank_loan.  The function that
   creates a credit card must take an additional argument used to
   set the interest rate. *)

let construct_note_payable (amount :float) : note_payable= 
   let np = new note_payable in 
      np#borrow amount;
   np
      

let construct_credit_card (amount :float) (rate :float) : credit_card= 
   let cc =  new credit_card in
      cc#borrow amount;
      cc#set_interest rate;
   cc


let construct_bank_loan (amount : float) : bank_loan = 
   let bl = new bank_loan in 
      bl#borrow amount;
   bl


(* Problem 3(b)  *)
(* Using the same data that you used to create your solution to 1(b),
   create one object of each class, and then create a list containing
   all of them.  You may have to use the coercion operator from
   Chapter 12 of "Real World OCaml".  (See the course notes.) *)

let np = construct_note_payable 75.54
let cc = construct_credit_card 2478.76 0.5
let bl = construct_bank_loan 1500000. 

let obj_list = [np; (cc :> note_payable); (bl :> note_payable)]



(* Problem 3(c)  *)
(* Redo Problem 1(a), writing the object-oriented version this time (a
   function that takes a list of objects of type note_payable and
   returns a list of balances.  Call your function on your list from
   Problem 3(b). *)

let get_oBalances1 (loans : note_payable list) : fBalance list = 
   let (bal_list : fBalance list) = [] in
      let rec add_to_bal (loans : note_payable list)(bal_list : fBalance list): fBalance list = 
         match loans with 
            |[] -> bal_list
            |head :: tail -> add_to_bal tail (List.append bal_list [head#get_balance])
         in
   add_to_bal loans bal_list



let _ = get_oBalances1 obj_list
(****************************************************)
(* PROBLEM 4: Conversion to Function-Oriented Style *)
(****************************************************)
(* Write a function that returns a list of loan balances with the
   same return values as your solution to Problem 3(c), but this time,
   your function must first take a list of note_payable objects,
   convert them to elements of type loan, and then call your
   function from Problem 1(a) *)

let get_oBalances2 (np_objects: note_payable list) : fBalance list = 
   let (loan_list : loan list)  = [] in
      let rec add_to_loan (np_objects: note_payable list) (loan_list: loan list): loan list = 
         match np_objects with
            |[] -> loan_list
            | head :: tail -> let to_loan = head#to_loan in add_to_loan tail (List.append loan_list [to_loan])
         in
   get_fBalances (add_to_loan np_objects loan_list)
            


let _ = get_oBalances2 obj_list