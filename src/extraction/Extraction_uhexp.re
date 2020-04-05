open Extraction_declear;
open Extraction_trans;
open Extraction_tool;
open Extraction_uhpat
open Extraction_uhtyp


// This file will extract UHExp

// directly use UHExp.string_of_operator to translate the operator
//Js.log(UHExp.string_of_operator(Space));


// The extract_t is prepared for ExpLine
// In Letline, we can't actually evaluate the type for a let
//      but we can evaluate the block in let
//      Letline itself should have UNK, because we can't let forever
let rec uhexp_trans = (~t : UHExp.t, ~vs:variable_set_t) : extract_t =>
    switch(t){
        | [] => (Some(""), UNK)
        | [l] => extract_t_combine(~ex1 = fst(line_trans(~l=l, ~vs=vs)), ~ex2 = (Some("\n"), UNK))
        | [h,...t] => {let res = line_trans(~l=h, ~vs=vs);
            extract_t_combine(~ex1 = extract_t_combine(~ex1=fst(res), ~ex2 = (Some("\n"), UNK)), ~ex2 = uhexp_trans(~t=t, ~vs=snd(res)))}
    }
//return the modified variable set
and line_trans = (~l:UHExp.line, ~vs:variable_set_t) : (extract_t, variable_set_t) =>
    switch(l){
        | EmptyLine => ((Some(""), UNK), vs)
        //uht is option given, hence if given, don't need to inference
        | LetLine(uhp, uht, t) => {
            // snd should be EMPTY since not defined, here use p as a string
            let p = (fst(uhpat_trans(~t=uhp, ~vs=vs)), UNK);    
            let exp = uhexp_trans(~t=t, ~vs=vs);
            switch(uht) {
                | Some(a) => {
                    let typ = uhtyp_trans(~t=a);
                    let new_vs = add_variable(~v=(fst(p), snd(typ)), ~env=vs);
                    let e = extract_t_concat(~le = [
                        (Some("let "), UNK),
                        p,
                        (Some(" = "), UNK),
                        (fst(exp), UNK),
                        (Some(":"), UNK),
                        (fst(typ), UNK),
                        (Some(" in "), UNK)
                    ]);
                    (e, new_vs);
                }
                // need to first evaluate t=block and get the type
                | None => {
                    let new_vs = add_variable(~v=(fst(p), snd(exp)), ~env=vs);
                    let e = extract_t_concat(~le = [
                        (Some("let "), UNK),
                        p,
                        (Some(" = "), UNK),
                        (fst(exp), UNK),
                        (Some(":"), UNK),
                        (pass_trans(~type1=snd(exp)), UNK),
                        (Some(" in "), UNK)
                    ]);
                    (e, new_vs);
                }
            }
        }
        | ExpLine(opseq) => switch(opseq){
            | OpSeq(_oprand, a) => (uhexp_seq_trans(~t=a, ~vs=vs), vs)
        }
    }
and uhexp_seq_trans = (~t: Seq.t('operand, 'operator), ~vs : variable_set_t) : extract_t =>
    switch(t){
        | S(operand, affix_e) => switch(affix_e){
            | E => uhexp_operand_trans(~ope=operand, ~vs=vs)
            | A(operator, seqt) => uhexp_const(~ope1=operand, ~op=operator, ~ope2=uhexp_seq_trans(~t=seqt, ~vs=vs), ~vs=vs)
        }
    }
and uhexp_operand_trans = (~ope: UHExp.operand, ~vs:variable_set_t) : extract_t =>
    switch(ope) {
        | EmptyHole(_) => (Some(""), HOLE)
        | Var(a, b, c) => switch(a, b){
            | (NotInHole, NotInVarHole) => var_annotate(~var=c, ~vs=vs)
            | _ => (Some(""), HOLE)
        } 
        | NumLit(a, b) => 
    }
and uhexp_const(~ope1:UHExp.operand, ~op:UHExp.operator, ~ope2:extract_t, ~vs:variable_set_t)