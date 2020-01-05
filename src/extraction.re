open UHExp;

// an example of
// let x=3 in
// x
let example_let =
  Block(
    [
      LetLine(
        UHPat.Var(NotInHole, NotInVarHole, "x"),
        None,
        Block([], NumLit(NotInHole, 3)),
      ),
    ],
    Var(NotInHole, NotInVarHole, "x"),
  );

// an example of simple 123
let example_123 = Block(
  [],
  NumLit(NotInHole, 123)
);

let example_true = Block(
  [],
  BoolLit(NotInHole, true)
);

let example_emptyhole = Block(
  [],
  EmptyHole(45)
);

let example_listnil = Block(
  [],
  ListNil(NotInHole)
);

//===============================
//working functions
//TODO: first write all the "t" handlers, 
//      then line recursive
//===============================

//==============================
//  UHTyp.re
//==============================

// translate type constructor into ocaml type
//opseq is when using bi-argument operations like plus,
//skel.t use BinOp to indicate the operation(op) and two placeholder
//  in UHTyp, it's one the type, like Num -> Num
//  It's like skel.t will always receive NotInHole, so ignore it
//opseq is the operation, ExpOpExp is to do calculation,
//  SeqOpExp is to do sequence operation, like 1+2+3
let rec uhtyp_translater = (~t : UHTyp.t) : option(string) =>
  switch(t) {
    | Num => Some("int")
    | Bool => Some("bool")
    | Unit => Some("()") //written as (), actually is unit
    | List(a) => switch(uhtyp_translater(a)){
      | None => None
      | Some(s) => Some(s ++ " list")
    }
    | Parenthesized(a) => switch(uhtyp_translater(a)) {
      | None => None
      | Some(s) => Some("(" ++ s ++ ")")
    }
    | Hole => None
    | OpSeq(skel_t, opseq) => switch(skel_t){
      | BinOp(NotInHole, _, _, _) => opseq_handler(~opseq=opseq)
      | _ => None
    }
  }
  and opseq_handler = (~opseq) : option(string) => switch(opseq){
        | ExpOpExp(tm1, op, tm2) => switch((uhtyp_translater(tm1), uhtyp_translater(tm2)))
        {
          | (Some(a), Some(b)) => Some(a ++ " " ++ uhtyp_op_translater(op) ++ " " ++ b)
          | _ => None
        }
        | SeqOpExp(seq, op, tm) => switch(opseq_handler(~opseq=seq), uhtyp_translater(tm)) {
          | (Some(a), Some(b)) => Some(a ++ " " ++ uhtyp_op_translater(op) ++ " " ++ b)
          | _ => None
        }
  } and uhtyp_op_translater = (~op : UHTyp.op) : string =>
  switch(op) {
    | Arrow => " -> "
    | Prod => " * " //int*int in ocaml
    | Sum => " | "
  };



// let uhtyp_example1 : UHTyp.t= Parenthesized(OpSeq(
//     BinOp(NotInHole, Arrow, Placeholder(1), Placeholder(2)),
//     ExpOpExp(Num, Arrow, Num)));

// let uhtyp_example2 : UHTyp.t= Parenthesized(OpSeq(
//     BinOp(NotInHole, Arrow, BinOp(NotInHole, Arrow, Placeholder(0), Placeholder(1)), Placeholder(2)),
//     SeqOpExp(ExpOpExp(Unit, Arrow, Bool), Arrow, Num)));

// switch(uhtyp_translater(~t=uhtyp_example2)){
//   | None => ()
//   | Some(s) => print_endline(s)
// };


//==============================
// UHPat.re
//==============================

let rec uhpat_translater = (~t : UHPat.t) : option(string) =>
  switch(t) {
    | EmptyHole(_) => None
    | Wild(a) => switch(a) {
      | NotInHole => Some("_")
      | _ => None
    }
    | Var(a, b, c) => switch(a, b) {
      | (NotInHole, NotInVarHole) => Some(c)
      | _ => None
    }
    | NumLit(a, b) => switch(a) {
      | NotInHole => Some(string_of_int(b))
      | _ => None
    }
    | BoolLit(a, b) => switch(a) {
      | NotInHole => Some(string_of_bool(b))
      | _ => None
    }
    | ListNil(a) => switch(a) {
      | NotInHole => Some("[]")
      | _ => None
    }
    | Parenthesized(t) => switch(uhpat_translater(t)) {
      | None => None
      | Some(s) => Some("(" ++ s ++ ")")
    }
    //TODO: currently we use polymorphic type ('a) for it, 
    // better to reconstruct for a type
    // (though the inference is good for that)
    | Inj(a, b, c) => switch(a) {
      | NotInHole => uhpat_translater(~t = c)
      | _ => None
    }
    | OpSeq(skel_t, opseq) => switch(skel_t){
      | BinOp(NotInHole, _, _, _) => uhpat_opseq_handler(~opseq=opseq)
      | _ => None
    }
  } and uhpat_opseq_handler = (~opseq) : option(string) => switch(opseq){
        | ExpOpExp(tm1, op, tm2) => switch((uhpat_translater(tm1), uhpat_translater(tm2)))
        {
          | (Some(a), Some(b)) => Some(a ++ " " ++ uhpat_op_translater(op) ++ " " ++ b)
          | _ => None
        }
        | SeqOpExp(seq, op, tm) => switch(uhpat_opseq_handler(~opseq=seq), uhpat_translater(tm)) {
          | (Some(a), Some(b)) => Some(a ++ " " ++ uhpat_op_translater(op) ++ " " ++ b)
          | _ => None
        }
  } and uhpat_op_translater = (~op : UHPat.op) : string => switch(op) {
    | Comma => ", "
    | Space => " "
    | Cons => " :: "
  };


//testcases
let uhpat_example1 : UHPat.t = Parenthesized(OpSeq(
    BinOp(NotInHole, Cons, BinOp(NotInHole, Cons, Placeholder(0), Placeholder(1)), Placeholder(2)),
    SeqOpExp(ExpOpExp(NumLit(NotInHole, 1), Cons, NumLit(NotInHole, 2)), Cons, ListNil(NotInHole))));

let uhpat_example2 : UHPat.t = 

switch(uhpat_translater(~t=uhpat_example1)){
  | None => ()
  | Some(s) => print_endline(s)
};

//==============================
//  UHExp.re
//==============================

// OUTER NODES

// it means there's empty hole, so it will fail
let emptyhole_handler = () : option(string) => None;

let var_handler = (~errstatus : ErrStatus.t, ~varerrstatus : VarErrStatus.t, ~value : Var.t) =>
  switch(errstatus, varerrstatus) {
    | (NotInHole, NotInVarHole) => Some(value)
    | _ => None
  };

let numlit_handler = (~errstatus : ErrStatus.t, ~value : int) : option(string) =>
  switch(errstatus) {
    | NotInHole => Some(string_of_int(value))
    | _ => None
  };

let boollit_handler = (~errstatus : ErrStatus.t, ~value : bool) : option(string) =>
  switch(errstatus) {
    | NotInHole => Some(string_of_bool(value))
    | _ => None
  };


let listnil_handler = (~errstatus : ErrStatus.t) : option(string) =>
  switch(errstatus) {
    | NotInHole => Some("[]")
    | _ => None
  };

// INNER NODES
//TODO:Start here, from Lam





let type_handler = (~t : t) : option(string) =>
  switch(t) {
    | EmptyHole(_) => emptyhole_handler()
    | Var(a, b, c) => var_handler(~errstatus=a, ~varerrstatus=b, ~value=c)
    | NumLit(a, b) => numlit_handler(~errstatus=a, ~value=b)
    | BoolLit(a, b) => boollit_handler(~errstatus=a, ~value=b)
    | ListNil(a) => listnil_handler(~errstatus=a)
    | _ => None
  };

//=============================

let block_handler=(~block : block) : option(string) => 
  switch(block) {
    | Block(lines, t) => type_handler(~t=t)
  };



//used to handle None errors, or incomplete codes
let extraction_caller=(~block : block) : string =>
  switch(block_handler(~block=block)) {
    | None => "It's not completed!"
    | Some(s) => s
  };

//print_endline(extraction_caller(~block=example_listnil));