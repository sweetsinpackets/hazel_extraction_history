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

// Helper Functions

let uhtyp_op_translater = (~op : UHTyp.op) : string =>
  switch(op) {
    | Arrow => " -> "
    | Prod => " * " //int*int in ocaml
    | Sum => " | "
  }


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
    | OpSeq(skel_t, opseq) => switch()
  };


let uhtyp_example : UHTyp.t= Parenthesized(List(Num));

switch(uhtyp_translater(~t=uhtyp_example)){
  | None => ()
  | Some(s) => print_endline(s)
};


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