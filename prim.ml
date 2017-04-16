(*==================================================*
 * prim.ml - Fly 2.1
 * 2017/04/17 @lambdataro
 *==================================================*)

open Error
open Syntax
open Printf

(* 真偽値 *)
let true_val = NUM 1.0
let false_val = NUM 0.0

(* 文字列を数値のリストに変換 *)
let list_of_string str =
  let lis = ref [] in
  String.iter (fun c ->
    let n = float (int_of_char c) in 
    lis := NUM n :: !lis) str;
  LIST (List.rev !lis)

(* 数値のリストを文字列に変換 *)
let string_of_list pos vs =
  let buf = Buffer.create 32 in
  List.iter (function
    | NUM n ->
      let i = int_of_float n in
      if i >= 0 && i <= 255 then
        Buffer.add_char buf (char_of_int i)
      else
        errorf pos "文字コードが範囲外: to_string"
    | _ ->
      errorf pos "型エラー: to_string"
  ) vs;
  Buffer.contents buf

(* 値の文字列表現 *)
let rec string_of_value v =
  let open Printf in
  match v with
  | NUM n -> sprintf "%g" n
  | LIST vs ->
    sprintf "[%s]" @@
    String.concat ", " @@
    List.map string_of_value vs
  | _ -> "<fun>"

(* 符号反転 *)
let eval_neg pos = function
  | NUM n -> NUM (~-.n)
  | _ -> errorf pos "型エラー: neg"

(* 数値であるかの判定 *)
let eval_is_num _ = function
  | NUM _ -> true_val
  | _ -> false_val

(* リストであるかの判定 *)
let eval_is_list _ = function
  | LIST _ -> true_val
  | _ -> false_val

(* 文字列化 *)
let eval_to_str pos = function
  | v ->
    list_of_string (string_of_value v)

(* 文字列の表示 *)
let eval_write pos = function
  | LIST vs ->
    Global.print (string_of_list pos vs);
    LIST vs
  | _ -> errorf pos "型エラー: write"

(* 切り捨て *)
let eval_floor pos = function
  | NUM v -> NUM (floor v)
  | _ -> errorf pos "型エラー: floor"

(* 1引数プリミティブの評価 *)
let eval_prim1 pos op v =
  let f =
    match op with
    | Neg -> eval_neg
    | IsNum -> eval_is_num
    | IsList -> eval_is_list
    | ToStr -> eval_to_str
    | Write -> eval_write
    | Floor -> eval_floor
  in
  f pos v

(* 加算 *)
let eval_add pos v1 v2 =
  match v1, v2 with
  | NUM n1, NUM n2 -> NUM (n1 +. n2)
  | _ -> errorf pos "型エラー: add"

(* 減算 *)
let eval_sub pos v1 v2 =
  match v1, v2 with
  | NUM n1, NUM n2 -> NUM (n1 -. n2)
  | _ -> errorf pos "型エラー: sub"

(* 乗算 *)
let eval_mul pos v1 v2 =
  match v1, v2 with
  | NUM n1, NUM n2 -> NUM (n1 *. n2)
  | _ -> errorf pos "型エラー: mul"

(* 除算 *)
let eval_div pos v1 v2 =
  match v1, v2 with
  | NUM n1, NUM n2 -> NUM (n1 /. n2)
  | _ -> errorf pos "型エラー: div"

(* クロージャかどうかの判定 *)
let is_closure = function
  | NUM _ | LIST _ -> false
  | _ -> true

(* 値の比較(=) *)
let rec value_equal pos v1 v2 =
  if is_closure v1 || is_closure v2 then
    errorf pos "関数を比較しようとした"
  else
    match v1, v2 with
    | NUM n1, NUM n2 -> n1 = n2
    | LIST vs1, LIST vs2 when
        List.length vs1 = List.length vs2 ->
      List.for_all2 (value_equal pos) vs1 vs2
    | _ -> false

(* 等しい *)
let eval_eq pos v1 v2 =
  if value_equal pos v1 v2 then
    true_val
  else
    false_val

(* 値の比較(<) *)
and value_less_than pos v1 v2 =
  match v1, v2 with
  | NUM n1, NUM n2 -> n1 < n2
  | _ -> errorf pos "数値以外を比較しようとした"

(* より小さい *)
let eval_lt pos v1 v2 =
  if value_less_than pos v1 v2 then
    true_val
  else
    false_val

(* コンス *)
let eval_cons pos v1 v2 =
  match v1, v2 with
  | v, LIST vs -> LIST (v :: vs)
  | _ -> errorf pos "型エラー: cons"

(* 2引数プリミティブの評価 *)
let eval_prim2 pos op v1 v2 =
  let f =
    match op with
    | Add -> eval_add
    | Sub -> eval_sub
    | Mul -> eval_mul
    | Div -> eval_div
    | Eq -> eval_eq
    | Lt -> eval_lt
    | Cons -> eval_cons
  in
  f pos v1 v2
