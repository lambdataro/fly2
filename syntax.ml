(*==================================================*
 * syntax.ml - Fly 2.0
 * Naoki Takashima
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
  | Put | Get | ToStr | Write
  | Floor | Flush

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

(* エラーメッセージを表示して終了 *)
let errorf pos fmt =
  let open Printf in
  ksprintf (fun str ->
    if pos = Lexing.dummy_pos then
      eprintf "エラー: %s\n"
        str
    else
      eprintf "エラー %s(%d): %s\n"
        pos.Lexing.pos_fname
        pos.Lexing.pos_lnum
        str;
    exit 1) fmt


