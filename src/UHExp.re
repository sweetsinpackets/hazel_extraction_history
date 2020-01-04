type op =
  | Space
  | Plus
  | Minus
  | Times
  | LessThan
  | GreaterThan
  | Equals
  | Comma
  | Cons
  | And
  | Or;


type skel_t = Skel.t(op);


type block =
  | Block(lines, t)
and lines = list(line)
and line =
  | ExpLine(t)
  | EmptyLine
  | LetLine(UHPat.t, option(UHTyp.t), block)
and t =
  /* outer nodes */
  | EmptyHole(MetaVar.t)
  | Var(ErrStatus.t, VarErrStatus.t, Var.t)
  | NumLit(ErrStatus.t, int)
  | BoolLit(ErrStatus.t, bool)
  | ListNil(ErrStatus.t)
  /* inner nodes */
  | Lam(ErrStatus.t, UHPat.t, option(UHTyp.t), block)
  | Inj(ErrStatus.t, InjSide.t, block)
  | Case(ErrStatus.t, block, rules, option(UHTyp.t))
  | Parenthesized(block)
  | OpSeq(skel_t, opseq) /* invariant: skeleton is consistent with opseq */
  | ApPalette(ErrStatus.t, PaletteName.t, SerializedModel.t, splice_info)
and opseq = OperatorSeq.opseq(t, op)
and rules = list(rule)
and rule =
  | Rule(UHPat.t, block)
and splice_info = SpliceInfo.t(block)
and splice_map = SpliceInfo.splice_map(block);