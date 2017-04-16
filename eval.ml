(*==================================================*
 * eval.ml - Fly 2.1
 * 2017/04/17 @lambdataro
 *==================================================*)

open Error
open Syntax
open Prim

(* 環境の探索 *)
let lookup pos x env =
  try List.assoc x env
  with Not_found -> errorf pos "変数\'%s\'は未定義" x
    
(* 空の継続 *)
let rec null_cont v = function
  | [] -> v
  | Tag _ :: tr -> null_cont v tr
  | Cont k :: tr -> k v tr

(* 継続の分割 *)
let rec split_trail pos p = function
  | [] ->
    errorf pos "タグ\'%s\'が見つからない" p
  | Tag q :: tr when p = q -> ([], tr)
  | k :: tr ->
    let (f, b) = split_trail pos p tr in 
    (k :: f, b)

(* 最も内側のタグを返す *)
let rec inner_tag = function
  | [] -> failwith "can't happen"
  | Tag p :: _ -> p
  | _ :: tr -> inner_tag tr

(* 評価 *)
let rec eval expr env cont trail =
  match expr with
  | Nil -> cont (LIST []) trail
  | Num n -> cont (NUM n) trail
  | Var (pos, x) -> cont (lookup pos x env) trail
  | App (pos, e1, e2) ->
    eval e1 env (fun v1 tr1 ->
      eval e2 env (fun v2 tr2 ->
        eval_app pos v1 v2 cont tr2) tr1) trail
  | Block (opt, stmts) ->
    let p =
      match opt with
      | None -> ""
      | Some p -> p
    in
    eval_block (NUM 0.) stmts env null_cont
      (Tag p :: Cont cont :: trail)
  | Shift (pos, opt, x, e) ->
    let (c, behind) = eval_shift pos opt cont trail in
    let env' = (x, CONT c) :: env in
    eval e env' null_cont behind

(* 関数適用の評価 *)
and eval_app pos v1 v2 cont trail =
  match v1 with
  | CONT k -> k v2 (Cont cont :: trail)
  | PRIM1 op -> cont (eval_prim1 pos op v2) trail
  | PRIM2 op -> cont (PRIM2_1 (op, v2)) trail
  | PRIM2_1 (op, v) -> cont (eval_prim2 pos op v v2) trail
  | _ -> errorf pos "関数でないものを値に適用"

and eval_shift pos opt cont trail =
  let p =
    match opt with
    | None -> inner_tag trail
    | Some p -> p
  in
  let (front, behind) = split_trail pos p trail in
  let c x tr' = cont x (front @ Tag p :: tr') in
  (c, Tag p :: behind)

(* ブロックの評価 *)
and eval_block value stmts env cont trail =
  match stmts with
  | [] -> cont value trail
  | Expr e :: rest ->
    eval e env (fun v tr -> 
      eval_block v rest env cont tr
    ) trail
  | Let (pos, opt, pat, e) :: rest ->
    eval e env (fun v tr ->
      match pat_match pat v with
      | Some env' ->
        eval_block v rest (env' @ env) cont tr
      | None -> 
        let (c, behind) = eval_shift pos opt cont tr in
        null_cont v behind
    ) trail
  | Cond (pos, opt, e) :: rest ->
    eval e env (fun v tr ->
      if v = NUM 0. || v = LIST [] then
        let (c, behind) = eval_shift pos opt cont tr in
        null_cont v behind
      else
        eval_block v rest env cont tr
    ) trail

(* パターンマッチ *)
and pat_match pat value =
  match pat, value with
  | PWild, _ -> Some []
  | PVar x, v -> Some [(x, v)]
  | POr (p1, p2), v ->
    begin
      match pat_match p1 v with
      | None -> None
      | Some opt1 ->
        match pat_match p2 v with
        | None -> None
        | Some opt2 -> Some (opt1 @ opt2)
    end
  | PAs (p, x), v ->
    begin
      match pat_match p v with
      | None -> None
      | Some opt -> Some ((x, v) :: opt)
    end
  | PNum n1, NUM n2 when n1 = n2 -> Some []
  | PNil, LIST [] -> Some []
  | PCons (p1, p2), LIST (v :: vs) ->
    begin
      match pat_match p1 v with
      | None -> None
      | Some opt1 ->
        match pat_match p2 (LIST vs) with
        | None -> None
        | Some opt2 -> Some (opt1 @ opt2)
    end 
  | _ -> None

(* 初期環境 *)
let init_env =
  [
    ("__prim_neg", PRIM1 Neg);
    ("__prim_is_num", PRIM1 IsNum);
    ("__prim_is_list", PRIM1 IsList);
    ("__prim_to_str", PRIM1 ToStr);
    ("__prim_write", PRIM1 Write);
    ("__prim_floor", PRIM1 Floor);
    ("__prim_add", PRIM2 Add);
    ("__prim_sub", PRIM2 Sub);
    ("__prim_mul", PRIM2 Mul);
    ("__prim_div", PRIM2 Div);
    ("__prim_eq", PRIM2 Eq);
    ("__prim_lt", PRIM2 Lt);
    ("__prim_cons", PRIM2 Cons);
  ]

(* 評価の開始 *)
let start_eval e =
  eval e init_env null_cont []
