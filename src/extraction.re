open UHExp;



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
    | Hole => None    
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

//FIXME: Here is a version with type inference
//  such as (\lambda x:?.x+1) 1 can return 2, but leave a ? hole as "Hole"
//ocaml can do type inference, so just change hole to 'a
// let rec uhtyp_translater = (~t : UHTyp.t) : option(string) =>
//   switch(t) {
//     | Hole => "'a"
//     | Num => Some("int")
//     | Bool => Some("bool")
//     | Unit => Some("()") //written as (), actually is unit
//     | List(a) => switch(uhtyp_translater(a)){
//       | None => None
//       | Some(s) => Some(s ++ " list")
//     }
//     | Parenthesized(a) => switch(uhtyp_translater(a)) {
//       | None => None
//       | Some(s) => Some("(" ++ s ++ ")")
//     }
//     | OpSeq(skel_t, opseq) => switch(skel_t){
//       | BinOp(NotInHole, _, _, _) => opseq_handler(~opseq=opseq)
//       | _ => None
//     }
//   }
//   and opseq_handler = (~opseq) : option(string) => switch(opseq){
//         | ExpOpExp(tm1, op, tm2) => switch((uhtyp_translater(tm1), uhtyp_translater(tm2)))
//         {
//           | (Some(a), Some(b)) => Some(a ++ " " ++ uhtyp_op_translater(op) ++ " " ++ b)
//           | _ => None
//         }
//         | SeqOpExp(seq, op, tm) => switch(opseq_handler(~opseq=seq), uhtyp_translater(tm)) {
//           | (Some(a), Some(b)) => Some(a ++ " " ++ uhtyp_op_translater(op) ++ " " ++ b)
//           | _ => None
//         }
//   } and uhtyp_op_translater = (~op : UHTyp.op) : string =>
//   switch(op) {
//     | Arrow => " -> "
//     | Prod => " * " //int*int in ocaml
//     | Sum => " | "
//   };


// TESTCASE

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
    //FIXME: currently we use polymorphic type ('a) for it in "Let" assignment, 
    // better to reconstruct for a type
    // (though the inference is good for that)
    // in Hazel, a type Num | Bool can't have value 1, 
    //   it should be type inj[L or R, Bool](1)
    // in ocaml, sum type should be directly give value by constructor
    //   and need a type declaration for sum type
    // Hence change "x : (A | B) = inj[L](val)" where val:A into
    //   "x : 'a = val" temporarily, :'a can be ignored
    //   here we ignore 'a, because imagine assignment is legal
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


//TESTCASE

// let uhpat_example1 : UHPat.t = Parenthesized(OpSeq(
//     BinOp(NotInHole, Cons, BinOp(NotInHole, Cons, Placeholder(0), Placeholder(1)), Placeholder(2)),
//     SeqOpExp(ExpOpExp(NumLit(NotInHole, 1), Cons, NumLit(NotInHole, 2)), Cons, ListNil(NotInHole))));

// let uhpat_example2 : UHPat.t = Parenthesized(Inj(NotInHole, L, Parenthesized(OpSeq(
//     BinOp(NotInHole, Cons, BinOp(NotInHole, Cons, Placeholder(0), Placeholder(1)), Placeholder(2)),
//     SeqOpExp(ExpOpExp(NumLit(NotInHole, 1), Cons, NumLit(NotInHole, 2)), Cons, ListNil(NotInHole))))))

// switch(uhpat_translater(~t=uhpat_example2)){
//   | None => ()
//   | Some(s) => print_endline(s)
// };

//==============================
//  UHExp.re
//==============================

// using "fun list(_ : type) -> expr" as lambda expression
// nested is ok, fun x -> fun y -> ... -> expr


//TODO: complete the block and line part
let rec block_handler=(~block : block) : option(string) => 
  switch(block) {
    | Block(lines, t) => type_handler(~t=t)
  }
and //The t part
type_handler = (~t : t) : option(string) =>
  switch(t) {
    // outer nodes
    | EmptyHole(_) => None  //an empty hole always means incomplete
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
    // inner nodes
    | Lam(a, b, c, d) => lam_handler(~errstatus=a, ~uhpat=b, ~uhtyp=c, ~block=d)
    | Parenthesized(b) => switch(block_handler(b)) {
      | None => None
      | Some(s) => Some("(" ++ s ++ ")")
    }
    | _ => None
  }
and   // Lam helper function
lam_handler = (~errstatus : ErrStatus.t, ~uhpat : UHPat.t, ~uhtyp : option(UHTyp.t), ~block : block) : option(string) => 
  //UHTyp we receive a Some(Hole), it maybe legal to inference
  //Just use another version of uhtyp_translator
  switch(errstatus, uhpat, uhtyp){
    | (NotInHole, pat, None) => switch(uhpat_translater(pat), block_handler(~block=block)) {
      //Maybe here need ()
      | (Some(s), Some(b)) => Some("fun " ++ s ++ " -> " ++ b) 
      | _ => None
    }
    | (NotInHole, pat, Some(typ)) => switch(uhpat_translater(pat), uhtyp_translater(typ), block_handler(~block=block)) {
      //Maybe here need ()
      | (Some(s), Some(t), Some(b)) => Some("fun " ++ s ++ ":" ++ t ++ " -> " ++ b) 
      | _ => None
    }
    | _ => None
  }
and
inj_handler = (~errstatus : ErrStatus.t, ~injside : InjSide.t, ~block : block) : option(string) =>
  //TODO:


//=============================


//used to handle None errors, or incomplete codes
let extraction_caller=(~block : block) : string =>
  switch(block_handler(~block=block)) {
    | None => "There could be some error in the code. Most possible is incomplete holes."
    | Some(s) => s
  };


//TESTCASE

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

let example_lam1 = Block(
  [],
  Lam(NotInHole, 
      Var(NotInHole, NotInVarHole, "x"),
      None,
      Block(
        [],
        Var(NotInHole, NotInVarHole, "x")
      )
  )
);

let example_lam2 = Block(
  [],
  Lam(NotInHole, 
      Var(NotInHole, NotInVarHole, "x"),
      Some(Num),
      Block(
        [],
        Lam(NotInHole,
            Var(NotInHole, NotInVarHole, "y"),
            Some(Num),
            Block(
              [],
              Var(NotInHole, NotInVarHole, "xy")  //need to modify
            )
        )
      )
  )
);

print_endline(extraction_caller(~block=example_lam2));
