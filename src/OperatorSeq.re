type opseq('tm, 'op) =
  | ExpOpExp('tm, 'op, 'tm)
  | SeqOpExp(opseq('tm, 'op), 'op, 'tm);
