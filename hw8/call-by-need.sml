(*  Here's a skeleton file to help you get started on Interpreter 1.
 * Original version by Geoffrey Smith - http://users.cs.fiu.edu/~smithg/
 *)

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
  | AST_SET of (string * term)
  | AST_ADD of (term * term)
  | AST_SEQ of (term * term)
  | AST_LET of (string * term * term)

datatype result
  = RES_ERROR of string
  | RES_NUM   of int
  | RES_BOOL  of bool
  | RES_SUCC
  | RES_PRED
  | RES_ISZERO
  | RES_CLOS  of (string * term * env)
  | RES_UNIT
and thunk = EV of result | UN of (env * term)
and env = Env of string -> thunk ref


exception UnboundID
fun emptyenvFun  (x : string) : thunk ref = raise UnboundID;
val emptyenv = Env emptyenvFun

(* update : (string -> thunk ref) -> string -> (string -> thunk ref) *)
fun update (Env e) x t y
  = if x = y then t else e y

exception Unimplemented

(*  interp : (env * term) -> result. *)
fun interp (Env e, AST_ID i)        = raise Unimplemented
  | interp (env, AST_SET (i, e))    = raise Unimplemented
  | interp (env, AST_APP (e1,e2))   = raise Unimplemented
  | interp (env, AST_NUM n)         = RES_NUM n
  | interp (env, AST_BOOL b)        = RES_BOOL b
  | interp (env, AST_FUN (i,e))     = RES_CLOS (i,e,env)
  | interp (env, AST_SUCC)          = RES_SUCC
  | interp (env, AST_PRED)          = RES_PRED
  | interp (env, AST_ISZERO)        = RES_ISZERO
  | interp (env, AST_IF (e1,e2,e3)) =
    (case interp (env,e1) of
         RES_BOOL true =>
         interp (env,e2)
       | RES_BOOL false =>
         interp (env,e3))
  | interp (env, AST_ADD (e1,e2))   =
    (case (interp (env, e1),interp (env,e2)) of
         (RES_NUM n,RES_NUM m) => RES_NUM (n + m))
  | interp (env, AST_SEQ (e1,e2))   =
    (interp (env, e1); interp (env, e2))
  | interp (env, AST_LET (s,e1,e2)) =
    (interp (Env (update env s (ref (UN (env,e1)))), e2))
