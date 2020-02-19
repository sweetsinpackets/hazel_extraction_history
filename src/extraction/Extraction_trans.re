open Extraction_declear;
open Extraction_tool;
open Extraction_decons;

//translate to string
let rec pass_trans = (~type1: pass_t) : option(string) =>
  switch (type1){
    | Bool => Some("bool")
    | Number => Some("int")
    | Unit => Some("()")
    | List(a) => option_string_concat(~strs=[pass_trans(~type1=a), Some(" list")])
    | APP(a, b) => option_string_concat(~strs=[pass_trans(~type1=a), Some(" -> "), pass_trans(~type1=b)])
    | EMPTY => None
    | _ => None
  };


// translate a variable to the annotated string
// using option case of var to make future cases easy
let add_var_annotation = (~var:option(string), ~set:variable_set_t) : option(string) =>
  switch(var){
    | None => None
    | Some(s) => option_string_concat_ignoreNone(~strs=[var, option_string_concat(~strs=[Some(":"), pass_trans(~type1=find_variable_set(~var=s, ~set=set))])])
  };



//================================
//  UHPat
//================================


let rec trans_uhpat_pass = (~t:UHPat.t, ~set:variable_set_t) : pass_t => 
    switch(opseq_operand_uhpat(~t=t)){
        | EmptyHole(_) => HOLE
        | Wild(_) => CANNOT_INFER
        | Var(_, _, s) => find_variable_set(~var=s, ~set=set)
        | NumLit(_) => Number
        | BoolLit(_) => Bool
        | ListNil(_) => List(CANNOT_INFER)
        | Parenthesized(a) => trans_uhpat_pass(~t=a, ~set=set)
        | Inj(_, _, a) => trans_uhpat_pass(~t=a, ~set=set)
    };