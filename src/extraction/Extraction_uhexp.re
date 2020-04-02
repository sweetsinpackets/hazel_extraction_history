open Extraction_declear;
open Extraction_trans;
open Extraction_tool;
open Extraction_uhpat
open Extraction_uhtyp


// This file will extract UHExp

// directly use UHExp.string_of_operator to translate the operator
//Js.log(UHExp.string_of_operator(Space));


//can use same structure as old version
let rec uhexp_trans = (~t : UHExp.t, ~vs:variable_set_t) : extract_t =>
    switch(t){
        | [] => (Some(""), UNK)
        | [l] => line_trans(~l=l, ~vs=vs)
        | [h,...t] => extract_t_combine(~ex1 = line_trans(~l=h, ~vs=vs), ~ex2 = uhexp_trans(~t=t, ~vs=vs))
    }
and line_trans = (~l:UHExp.line, ~vs:variable_set_t) : extract_t =>
