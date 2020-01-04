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

//===============================
//working functions
//TODO: first write all the "t" handlers, 
//      then line recursive
//===============================

let numlit_handler = (~errstatus : ErrStatus.t, ~value : int) : option(string) =>
  switch(errstatus) {
    | NotInHole => Some(string_of_int(value))
    | _ => None
  };


let type_handler = (~t : t) : option(string) =>
  switch(t) {
    | NumLit(a, b) => numlit_handler(~errstatus=a, ~value=b)
    | _ => None
  };


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

print_endline(extraction_caller(~block=example_123));