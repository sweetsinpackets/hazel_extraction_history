//============================
// FIXME: Get Rid of it!!!
//  Opseq destruction
// I don't want to do so, 
//    but... I need to extract argument for a dependent type
//    at least for now
//============================

// UHPat.t -> operand
let opseq_operand_uhpat = (~t : UHPat.t) : UHPat.operand =>
  switch(t){
    | opseq => switch(opseq){
      | OpSeq.OpSeq(_, b) => switch(b){
        | S(op, _) => op
      }
    }
  };

let opseq_operand_uhtyp = (~t : UHTyp.t) : UHTyp.operand =>
  switch(t){
    | opseq => switch(opseq){
      | OpSeq.OpSeq(_, b) => switch(b){
        | S(op, _) => op
      }
    }
  };


