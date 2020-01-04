type op =
  | Arrow
  | Prod
  | Sum;


type skel_t = Skel.t(op);


type t =
  /* outer nodes */
  | Hole
  | Unit
  | Num
  | Bool
  /* inner nodes */
  | Parenthesized(t)
  | List(t)
  | OpSeq(skel_t, opseq)
and opseq = OperatorSeq.opseq(t, op);

exception SkelInconsistentWithOpSeq(skel_t, opseq);