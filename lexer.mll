(*==================================================*
 * lexer.mll - Fly 2.0
 * Naoki Takashima
 *==================================================*)
{
open Lexing
open Syntax
open Parser

let buf = Buffer.create 40
let reset () = Buffer.clear buf
let add c = Buffer.add_char buf c
let get () = Buffer.contents buf

}

let digit = ['0'-'9']
let upper = ['A'-'Z']
let lower = ['a'-'z''_']
let alnum = digit | upper | lower | '\''
let space = [' ''\t''\r']

let id = lower alnum*
let tag = upper alnum*
let num = digit+ ('.' digit*)? (['e' 'E'] ['+' '-']? digit+)?

rule token = parse
  | '+'     { PLS }
  | '-'     { MNS }
  | '*'     { AST }
  | '/'     { SLS }
  | '<'     { LT }
  | '>'     { GT }
  | "<="    { LE }
  | ">="    { GE }
  | '='     { EQ }
  | "<>"    { NE }
  | '('     { LP }
  | ')'     { RP }
  | '['     { LBK }
  | ']'     { RBK }
  | '{'     { LBR }
  | '}'     { RBR }
  | '@'     { AT }
  | ':'     { COL }
  | "::"    { COLCOL } 
  | ';'     { SC }
  | ','     { COM }
  | '!'     { BANG }
  | '?'     { QUE }
  | '$'     { DOLL }
  | "$$"    { DOLLDOLL }
  | '_'     { UBAR }
  | "->"    { ARROW }
  | '|'     { VBAR }
  | '&'     { AND }
  
  | id as str       { ID str }
  | tag as str      { TAG str }
  | '\"'            { reset (); string lexbuf; STRING (get ()) }
  | num as str      { NUM (float_of_string str) }
  | '\n'            { new_line lexbuf; token lexbuf }
  | space           { token lexbuf }
  | "(*"            { comment lexbuf; token lexbuf }
  | eof             { EOF }
  | _               { errorf lexbuf.lex_curr_p "文法エラー" }

and string = parse
  | '\"'            { () }
  | "\\n"           { add '\n'; string lexbuf }
  | "\\t"           { add '\t'; string lexbuf }
  | "\\r"           { add '\r'; string lexbuf }
  | "\\\\"          { add '\\'; string lexbuf }
  | '\\' (_ as c)   { add c; string lexbuf }
  | eof             { errorf lexbuf.lex_curr_p "文字列が閉じていない" }
  | _ as c          { add c; string lexbuf }

and comment = parse 
  | "*)"            { () }
  | "(*"            { comment lexbuf }
  | '\n'            { new_line lexbuf; comment lexbuf }
  | eof             { errorf lexbuf.lex_curr_p "コメントが閉じていない" }
  | _               { comment lexbuf }

