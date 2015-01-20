/*==================================================*
 * parser.mly - Fly 2.0
 * Naoki Takashima
 *==================================================*/
%{
open Syntax

(* 位置情報を取得 *)
let pos n =
  Parsing.rhs_start_pos n

(* 単行演算子を構成 *)
let make_uniop pos x e =
  App (pos, Var (pos, x), e)

(* 二項演算子を構成 *)
let make_binop pos x e1 e2 =
  App (pos, App (pos, Var (pos, x), e1), e2)

(* 式のリストをconsの列に変換 *)
let rec expr_list_to_cons pos = function
  | [] -> Nil
  | e :: es ->
    make_binop pos "__prim_cons" e (expr_list_to_cons pos es)

(* 文字列をリスト化 *)
let string_to_list pos str =
  let lis = ref [] in
  String.iter (fun c ->
    let n = float (int_of_char c) in 
    lis := Num n :: !lis) str;
  expr_list_to_cons pos (List.rev !lis)

(* パターンのリストをPConsの列に変換 *)
let rec pat_list_to_cons = function
  | [] -> PNil
  | p :: ps -> PCons (p, pat_list_to_cons ps)
%}

%token PLS MNS AST SLS
%token LT GT LE GE EQ NE
%token LP RP LBK RBK LBR RBR
%token AT COL COLCOL SC COM
%token BANG QUE DOLL DOLLDOLL
%token UBAR ARROW VBAR AND

%token <string> ID
%token <string> TAG
%token <string> STRING
%token <float> NUM

%token EOF

/* 演算子優先順位 */
%nonassoc ARROW
%left VBAR
%right AND
%nonassoc LT GT LE GE EQ NE
%right COLCOL
%left PLS MNS
%left AST SLS
%nonassoc UNARY
%nonassoc LP ID NUM STRING
          LBK LBR DOLLDOLL

%start main
%type <Syntax.stmt list> main

%%

/* メイン */
main:
  | stmt_list sc_opt EOF
    { List.rev $1 }
  | EOF
    { [] }
;

/* 省略可能なセミコロン */
sc_opt:
  | /* empty */
    { () }
  | SC
    { () }
;

/* 式 */
expr:
  | arg_expr
    { $1 }
  | expr arg_expr
    { App (pos 1, $1, $2) }
  | DOLL tag_opt var_opt ARROW expr
    { Shift (pos 1, $2, $3, $5) }
  | MNS expr %prec UNARY
    { make_uniop (pos 1) "__op_neg" $2 }
  | AT expr %prec UNARY
    { make_uniop (pos 1) "__op_fix" $2 }
  | expr PLS expr
    { make_binop (pos 1) "__op_add" $1 $3 }
  | expr MNS expr
    { make_binop (pos 1) "__op_sub" $1 $3 }
  | expr AST expr
    { make_binop (pos 1) "__op_mul" $1 $3 }
  | expr SLS expr
    { make_binop (pos 1) "__op_div" $1 $3 }
  | expr EQ expr
    { make_binop (pos 1) "__op_eq" $1 $3 }
  | expr NE expr
    { make_binop (pos 1) "__op_ne" $1 $3 }
  | expr LT expr
    { make_binop (pos 1) "__op_lt" $1 $3 }
  | expr GT expr
    { make_binop (pos 1) "__op_gt" $1 $3 }
  | expr LE expr
    { make_binop (pos 1) "__op_le" $1 $3 }
  | expr GE expr
    { make_binop (pos 1) "__op_ge" $1 $3 }
  | expr COLCOL expr
    { make_binop (pos 1) "__op_cons" $1 $3 }
;

/* 省略可能なタグ */
tag_opt:
  | /* empty */
    { None }
  | TAG COL
    { Some $1 }
;

/* 省略可能な変数 */
var_opt:
  | ID
    { $1 }
  | /* empty */
    { "" }
;

/* 括弧なしで関数適用の引数になる式 */
arg_expr:
  | LP expr RP
    { $2 }
  | ID
    { Var (pos 1, $1) }
  | NUM
    { Num $1 }
  | LBK RBK
    { Nil }
  | LBK expr_list RBK
    { expr_list_to_cons (pos 1) (List.rev $2) }
  | STRING
    { string_to_list (pos 1) $1 }
  | LBR tag_opt stmt_list sc_opt RBR
    { Block ($2, List.rev $3) }
  | DOLLDOLL
    { Shift (pos 1, None, "__k", Var (pos 1, "__k")) }
;

/* 式の列 */
expr_list:
  | expr
    { [$1] }
  | expr_list COM expr
    { $3 :: $1 }
;

/* ブロック文の列 */
stmt_list:
  | stmt
    { [$1] }
  | stmt_list SC stmt
    { $3 :: $1 }
;

/* ブロック文 */
stmt:
  | expr
    { Expr $1 }
  | BANG tag_opt pat EQ expr
    { Let (pos 1, $2, $3, $5) }
  | QUE tag_opt expr
    { Cond (pos 1, $2, $3) }
;

/* パターン */
pat:
  | UBAR
    { PWild }
  | ID
    { PVar $1 }
  | pat VBAR pat
    { POr ($1, $3) }
  | pat AND ID
    { PAs ($1, $3) }
  | NUM
    { PNum $1 }
  | LBK RBK
    { PNil }
  | pat COLCOL pat
    { PCons ($1, $3) }
  | LBK pat_list RBK
    { pat_list_to_cons (List.rev $2) }
;

/* パターンの列 */
pat_list:
  | pat
    { [$1] }
  | pat_list COM pat
    { $3 :: $1 }
;

