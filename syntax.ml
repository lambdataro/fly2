(*==================================================*
 * syntax.ml - Fly 2.1
 * 2017/04/17 @lambdataro
 *==================================================*)

(* 位置情報 *)
type pos = Lexing.position

(* タグ *)
type tag = string

(* 文 *)
type stmt =
  | Expr of expr
  | Let of pos * tag option * pat * expr
  | Cond of pos * tag option * expr

(* 式 *)
and expr =
  | Nil
  | Num of float
  | Var of pos * string
  | App of pos * expr * expr
  | Block of tag option * stmt list
  | Shift of pos * tag option * string * expr

(* パターン *)
and pat =
  | PWild
  | PVar of string
  | POr of pat * pat
  | PAs of pat * string
  | PNum of float
  | PNil
  | PCons of pat * pat

(* 値 *)
type value =
  | NUM of float
  | LIST of value list
  | CONT of cont
  | PRIM1 of prim1_id
  | PRIM2 of prim2_id
  | PRIM2_1 of prim2_id * value

(* 1引数プリミティブ *)
and prim1_id =
  | Neg | IsNum | IsList
  | ToStr | Write | Floor

(* 2引数プリミティブ *)
and prim2_id =
  | Add | Sub | Mul | Div
  | Eq | Lt | Cons

(* 継続 *)
and cont = value -> trail -> value

(* トレイル *)
and trail = trail_item list
and trail_item =
  | Tag of string
  | Cont of cont
