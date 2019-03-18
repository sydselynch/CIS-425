(*  Here's a skeleton file to help you get started on Interpreter 1.
 * Original version by Geoffrey Smith - http://users.cs.fiu.edu/~smithg/
 *)

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

datatype result
  = RES_ERROR of string
  | RES_NUM   of int
  | RES_BOOL  of bool
  | RES_SUCC
  | RES_PRED
  | RES_ISZERO
  | RES_FUN   of (string * term);

(*  An environment is a function string -> result.
 *  I've included code for the empty environment
 *  and code that produces the updated environment E[x : t].
 *)

exception UnboundID
datatype env = Env of (string -> result)
fun emptyenvFun  (x : string) : result = raise UnboundID;
val emptyenv = Env emptyenvFun

(*  update : (string -> result) -> string -> result -> string -> result  *)
fun update (Env e) (x : string) (ty : result) y = if x = y then ty else e y

(*  Here's a partial skeleton of interp : (env * term) -> result.
    I've done the first case for you
*)
fun interp (env, AST_ID i)          = env(i)
  | interp (env, AST_NUM n)         = RES_NUM(n)
  | interp (env, AST_BOOL b)        = RES_BOOL(b)
  | interp (env, AST_FUN (i,e))     = RES_FUN(i, e)
  | interp (env, AST_APP (e1,e2))   =
    let val first = interp(env, e1)
    in
      case first of
        RES_ISZERO =>
          let
            val (RES_NUM num) = interp(env, e2)
          in
            case num of
              0 => RES_BOOL true
              | _ => RES_BOOL false
          end
        | RES_PRED =>
            let
                val (RES_NUM num) = interp(env, e2)
            in
                case num of
                    0 => RES_NUM 0
                    | _ => RES_NUM(num-1)
            end
        | RES_SUCC =>
            let
                val (RES_NUM num) = interp(env, e2)
            in
                RES_NUM(num+1)
            end
        | RES_FUN(i, e) =>
            let
                val new_env = update (emptyenv) (i) (interp(env, e))
            in
                interp(new_env, e2)
            end
        | _ => RES_ERROR "Invalid Type"
    end
  | interp (env, AST_SUCC)          = RES_SUCC
  | interp (env, AST_PRED)          = RES_PRED
  | interp (env, AST_ISZERO)        = RES_ISZERO
  | interp (env, AST_IF (e1,e2,e3)) =
    let
        val (RES_BOOL v1) = interp(env, e1)
    in
        case v1 of
            true => interp(env, e2)
            | false => interp(env, e3)
    end
