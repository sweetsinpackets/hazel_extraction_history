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

print_endline(extraction_caller(~block=example_listnil));