type op =
  | Comma
  | Space
  | Cons;

type skel_t = Skel.t(op);

type t =
  /* outer nodes */
  | EmptyHole(MetaVar.t)
  | Wild(ErrStatus.t)
  | Var(ErrStatus.t, VarErrStatus.t, Var.t)
  | NumLit(ErrStatus.t, int)
  | BoolLit(ErrStatus.t, bool)
  | ListNil(ErrStatus.t)
  /* inner nodes */
  | Parenthesized(t)
  | OpSeq(skel_t, opseq)
  | Inj(ErrStatus.t, InjSide.t, t)
and opseq = OperatorSeq.opseq(t, op);