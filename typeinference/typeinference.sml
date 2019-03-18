(*  Type Inference --- Skeleton File  *)
(*  Original version by Geoffrey Smith - http://users.cs.fiu.edu/~smithg/  *)

use "type.sml";

Control.Print.printDepth:= 100;

datatype term
  = AST_ID of string
  | AST_NUM of int
  | AST_BOOL of bool
  | AST_FUN of (string * term)
  | AST_APP of (term * term)
  | AST_SUCC
  | AST_PRED
  | AST_ISZERO
  | AST_IF of (term * term * term)

(*  A substitution is just a function typ -> typ.  *)

fun identity (t : typ) = t

(*  replace (a, t) is the substitution that just replaces a with t.  *)

fun replace (a, t) (VAR b) =
      if a = b then t else VAR b
|   replace (a, t) (ARROW (t1, t2)) =
      ARROW (replace (a, t) t1, replace (a, t) t2)
|   (* No effect on other types. *)
    replace (a, t) t1 = t1

(*  occurs : string * typ -> bool  *)

fun occurs (a, VAR b)         = (a = b)
|   occurs (a, ARROW(t1, t2)) = occurs (a, t1) orelse occurs (a, t2)
|   occurs (a, _)             = false

exception Circularity
exception Mismatch

(*  unify : typ * typ -> (typ -> typ)  *)

fun unify (VAR a, t) =
      if VAR a = t then identity
      else if occurs (a, t) then raise Circularity
      else replace (a, t)
|   unify (t, VAR a)   = unify (VAR a, t)
|   unify (INT, INT)   = identity
|   unify (BOOL, BOOL) = identity
|   unify (ARROW(t1, t2), ARROW(t3, t4)) =
      let val s1 = unify (t1, t3)
	  val s2 = unify (s1 t2, s1 t4)
      in
	  s2 o s1
      end
|   unify (_, _) = raise Mismatch


(*  New type variable generator:  newtypevar () and reset ()  *)

local
  val letters = ["a","b","c","d","e","f","g","h","i","j","k","l","m",
		 "n","o","p","q","r","s","t","u","v","w","x","y","z"]
  val cnt = ref 0
  val typevars = ref letters
in
  fun newtypevar () = (
    if null (! typevars) then (
      cnt := ! cnt + 1;
      typevars := map (fn s => s ^ Int.toString (! cnt)) letters
    ) else ();
    VAR (hd (! typevars)) before (typevars := tl (! typevars))
  )

  fun reset () = (
    cnt := 0;
    typevars := letters
  )
end

(*  Milner's algorithm W : (string -> typ) * term -> (typ -> typ) * typ
 *
 *  I have included the code for AST_NUM and AST_IF; you must complete
 *  the other cases!
 *)
exception Unimplemented

fun W (E, AST_ID s)          = (identity, E s)
  | W (E, AST_NUM n)         = (identity, INT)
  | W (E, AST_BOOL b)        = (identity, BOOL)
  | W (E, AST_FUN (s,e))     =
    let val (s1, t1) = W (E, AST_ID s)
        val s2 = unify(t1, E s)
        val (s3, t2) = W (s2 o s1 o E, e)
        val s4 = unify(s3 t1, t2)
    in
      (s4 o s3 o s2 o s1, s4 t2)
    end
  | W (E, AST_APP (e1,e2))   =
    let val (s1, t1) = W (E, e1)
    in
        case t1 of
          ARROW(a, b) =>
            let val s2 = unify(t1, ARROW(a, b))
            val (s3, t2) = W (s2 o s1 o E, e2)
            val s4 = unify(s3 t1, t2)
            in
              (s4 o s3 o s2 o s1, s4 t2)
            end
    end
  | W (E, AST_SUCC)          = (identity, ARROW(INT, INT))
  | W (E, AST_PRED)          = (identity, ARROW(INT, INT))
  | W (E, AST_ISZERO)        = (identity, ARROW(INT, BOOL))
  | W (E, AST_IF (e1,e2,e3)) =
      let val (s1, t1) = W (E, e1)
	  val s2 = unify(t1, BOOL)
	  val (s3, t2) = W (s2 o s1 o E, e2)
	  val (s4, t3) = W (s3 o s2 o s1 o E, e3)
	  val s5 = unify(s4 t2, t3)
      in
	(s5 o s4 o s3 o s2 o s1, s5 t3)
      end

(*  Here's a driver program that uses W to find the principal type of e
 *  and then displays it nicely, using typ2str.
 *)

fun infer e =
  let val (s, t) = (reset (); W (emptyenv, e))
  in
    print ("The principal type is\n  " ^ typ2str t ^ "\n")
  end;
