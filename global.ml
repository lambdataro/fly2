(*==================================================*
 * global.ml - Fly 2.1
 * 2017/04/17 @lambdataro
 *==================================================*)

(* 文字列出力 *)
let global_print_ref = ref (fun (_: string) -> ())
let print str = (!global_print_ref) str
