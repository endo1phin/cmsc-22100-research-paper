(*  
      SML Code for Scrap Your Boilerplate: 
A Practical Design Pattern for Generic Programming 
*)



(*==== Declare data structures ====*)

(* Note: sml type decleration looks kinda ugly because of 
the lack of inplicit recursive declaration... *)

type Name = string
type Address = string
datatype Person = P of Name * Address
datatype Salary = S of real 
datatype Employee = E of Person * Salary
type Manager = Employee
datatype Dept 
  = D of Name * Manager * SubUnit list
and SubUnit 
  = PU of Employee 
  | DU of Dept
and Company = C of Dept list

(*==== Construct example data ====*)

val ralf = E(P("ralf", "Amsterdam"), S 8000.0)
val joost = E (P ("Joost", "Amsterdam"), S 1000.0)
val marlow = E (P ("Marlow", "Cambridge"), S 2000.0)
val blair = E (P ("Blair", "London"), S 100000.0)
val genCom = C ([
  D ("Research", ralf, [PU joost, PU marlow]), 
  D ("Strategy", blair, [])
])


(*==== Boilerplace example ====*)

fun incS k (S s) = S (s * (1.0+k))
fun incE k (E (p, s)) = E (p, (incS k s))
fun incU k (PU e) = PU (incE k e)
  | incU k (DU d) = DU (incD k d)
and incD k (D (nm, mgr, us)) = D (nm, (incE k mgr), (map (incU k) us))

fun increase k (C ds) = C (map (incD k) ds)

(* 
  increase 0.1 genCom; 
*)


(*==== Type extension ====*)


datatype any = V of exn

fun mkAnyFuncs () = 
  let exception Tag of 'a
    fun mkV v = V (Tag v)
    fun getV (V (Tag v)) = SOME v
      | getV _ = NONE
  in (mkV, getV)
  end

datatype transform = T of any -> any
and 'a transformOps = TransOps of {
  mkT : ('a -> 'a) -> transform,
  useT : transform -> ('a -> 'a)
}

val idT = T (fn x => x)

  

