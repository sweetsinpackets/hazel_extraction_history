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
        | Var(err, v_err, s) => switch(err, v_err){
            | (NotInHole, NotInVarHole) => var_annotate(~var=s, ~vs=vs)
            | _ => (Some(""), HOLE)
        } 
        | NumLit(err, s) => switch(err){
            | NotInHole => (Some(string_of_int(s)), Number)
            | _ => (Some(""), HOLE)
        }
        | BoolLit(err, s) => switch(err){
            | NotInHole => (Some(string_of_bool(s)), Bool)
            | _ => (Some(""), HOLE)
        }
        | ListNil(err) => switch(err){
            | NotInHole => (Some("[]"), List(UNK))
            | _ => (Some(""), HOLE)
        }
        | Lam(err, uhp, uht, t) => switch(err) {
            | NotInHole => lam_trans(~uhp=uhp, ~uht=uht, ~t=t, ~vs=vs)
            | _ => (Some(""), HOLE)
        }
        // just give the result, i.e. the picked type
        | Inj(err, side, t) => switch(err) {
            | NotInHole => inj_trans(~side=side, ~t=t, ~vs=vs)
            | _ => (Some(""), HOLE)
        }
        | Case(err, t, rules, uht) => switch(err){
            | NotInHole => case_trans(~t=t, ~rules=rules, ~uht=uht, ~vs=vs)
            | _ => (Some(""), HOLE)
        }
    }
//note that lambda will be the type (A->B)
and lam_trans = (~uhp: UHPat.t, ~uht:option(UHTyp.t), ~t:UHExp.t, ~vs:variable_set_t) : extract_t => 
    switch(uht) {
        | Some(typ) => {
            let v = (fst(uhpat_trans(~t=uhp, ~vs=vs)), UNK);
            let x_t = uhtyp_trans(~t=typ);
            let new_vs = add_variable(~v=(fst(v), snd(x_t)), ~env=vs);
            let e_t = uhexp_trans(~t=t, ~vs=new_vs);            
            let str = option_string_concat(~strs = [
                Some("(fun "),
                fst(v),
                Some(":"),
                fst(x_t),
                Some(" -> "),
                fst(e_t),
                Some(")")
            ]);
            (str, ARROW(snd(x_t), snd(e_t)));
        }
        | None => {
            // FIXME: how to infer the type of x from expression?
            // now use 'a and UNK, hoping it will CONFLICT when apply the lambda
            let v = (fst(uhpat_trans(~t=uhp, ~vs=vs)), UNK);
            let x_t = (Some("'a"), UNK);
            let new_vs = add_variable(~v=(fst(v), snd(x_t)), ~env=vs);
            let e_t = uhexp_trans(~t=t, ~vs=new_vs);
            let str = option_string_concat(~strs=[
                Some("(fun "),
                fst(v),
                Some(":"),
                fst(x_t),
                Some(" -> "),
                fst(e_t),
                Some(")")
            ]);
            (str, ARROW(snd(x_t), snd(e_t)));
        }
    }
and inj_trans = (~side:InjSide.t, ~t:UHExp.t, ~vs:variable_set_t) : extract_t =>
{
    // should accpet a sum type, and the expression is evaluated like no injection
    // the type should be injected due to side
    let exp = uhexp_trans(~t=t, ~vs=vs);
    switch(snd(exp)){
        | SUM(t1, t2) => switch(side) {
            | L => (fst(exp), t1)
            | R => (fst(exp), t2)
        }
        | _ => (Some("Not a sum type"), CONFLICT)
    }
}
//ocaml doesn't support gradual type, so every case should have exactly the same type
and case_trans = (~t:UHExp.t, ~rules:UHExp.rules, ~uht:option(UHTyp.t), ~vs:variable_set_t) : extract_t =>
{
    let x = uhexp_trans(~t=t, ~vs=vs);
    let r = rules_trans(~x_t=snd(x), ~rules=rules, ~uht=uht, ~vs=vs);
    extract_t_concat(~le=[
        (Some("((match "), UNK),
        (fst(x), UNK),
        (Some(" with\n"), UNK),
        r,
        (Some(") : "), UNK),
        (pass_trans(~type1=snd(r)), UNK),
        (Some(")"), UNK)
    ]);  
}
//divide rules into rule_trans and check if all types are same
and rules_trans = (~x_t : pass_t, ~rules : UHExp.rules, ~uht:option(UHTyp.t), ~vs:variable_set_t) : extract_t =>
{
    let ext = switch(rules){
        | [] => (Some(""), UNK)
        | [h] => rule_trans(~x_t=x_t, ~rule=h, ~vs=vs)
        | [h, ...t] => extract_t_combine(~ex1=rule_trans(~x_t=x_t, ~rule=h, ~vs=vs), ~ex2=rules_trans(~x_t=x_t,~rules=t,~uht=uht,~vs=vs))
    };
    switch(uht) {
        | Some(t) => extract_t_combine(~ex1=ext, ~ex2=(Some(""), snd(uhtyp_trans(~t=t))))
        | None => ext
    };
}
//check whether matches is same type with x_t
//note add a \n to each rule
and rule_trans = (~x_t : pass_t, ~rule : UHExp.rule, ~vs:variable_set_t) :extract_t =>
{
    
}
and uhexp_const = (~ope1:UHExp.operand, ~op:UHExp.operator, ~ope2:extract_t, ~vs:variable_set_t) : extract_t =>
{}