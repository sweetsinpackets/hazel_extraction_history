open Extraction_declear;
open Extraction_tool;



let rec uhpat_trans = (~t : UHPat.t, ~vs:variable_set_t) : extract_t =>
    switch(t){
        | OpSeq(_oprand, a) => uhpat_seq_trans(~t=a, ~vs=vs)
    }
and uhpat_seq_trans = (~t : Seq.t('operand, 'operator), ~vs:variable_set_t) : extract_t =>        
    switch(t){   //I don't think oprand is necessary
        | S(operand, affix_e) => switch(affix_e){
            | E => uhpat_operand_trans(~ope=operand, ~vs=vs)//only an operand
            | A(operator, seqt) => (None, CONFLICT)  //uhtyp_const(~ope1=operand, ~op=operator, ~ope2=uhtyp_seq_trans(~t=seqt))
        }
    }
and uhpat_operand_trans = (~ope : UHPat.operand, ~vs:variable_set_t) : extract_t =>
    switch(ope){
        | EmptyHole(_) => (None, HOLE)  //it's a HOLE
        | Wild(er) => switch(er){
            | NotInHole => (Some("_"), UNK)
            | _ => (None, HOLE)
        } 
        | Var(er, ve, var) => switch(er, ve){
            | (NotInHole, NotInVarHole) => (Some(var), find_variable_set(~var=var, ~set=vs))
            | _ => (None, HOLE)
        }
        | NumLit(a, b) => switch(a){
            | NotInHole => (Some(string_of_int(b)), Number)
            | _ => (None, HOLE)
        }
        | BoolLit(a, b) => switch(a){
            | NotInHole => (Some(string_of_bool(b)), Bool)
            | _ => (None, HOLE)
        }
        | ListNil(a) => switch(a){
            | NotInHole => (Some("[]"), List(UNK))
            | _ => (None, HOLE)
        }
        | Parenthesized(t) => extract_t_concat(~le=[(Some("("), UNK), uhpat_trans(~t=t, ~vs=vs), (Some(")"), UNK)])
        | Inj()
    }