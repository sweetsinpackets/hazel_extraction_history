//===================
// Tools
//===================

// option_string_concat([s1, Some(constant)])
let rec option_string_concat = (~strs : list( option(string))) : option(string) =>
  switch (strs) {
    | [] => Some("")
    | [a, ...rest] => switch(a, option_string_concat(~strs=rest)) {
      | (Some(s1), Some(s2)) => Some(s1 ++ s2)
      | _ => None
    }
  };

//================================

// Add indent levels and pass into the handlers
// level starts from 0, +1 means double space
let rec indent_space = (~level: int): string =>
  if (level > 0) {
    "  " ++ indent_space(~level=level - 1);
  } else {
    "";
  };

//===============================
// Type Decleration
//===============================


//The type used to indicate the 
type pass_t =
  | Bool
  | Number
  | Unit
  | List(pass_t)
  | APP(pass_t, pass_t) //pass_t -> pass_t
  | CANNOT_INFER  //I think useless
  | CONFLICT;  //can't use in ocaml(include gradual typing) or detect error

type extract_t = (option(string), pass_t);


//check whether is equal
let rec pass_eq = (~type1: pass_t, ~type2: pass_t) : bool =>
  switch (type1, type2){
    | (Unit, Unit) => true
    | (Bool, Bool) => true
    | (Number, Number) => true
    | (List(a), List(b)) => pass_eq(~type1=a, ~type2=b)
    | (APP(a1, b1), APP(a2, b2)) => pass_eq(~type1=a1, ~type2=a2) && pass_eq(~type1=b1, ~type2=b2)
    | _ => false
  };

//check conflict and pass on
let pass_check = (~type1: pass_t, ~type2: pass_t) : pass_t =>
  switch (type1, type2){
    | (Unit, Unit) => Unit
    | (Bool, Bool) => Bool
    | (Number, Number) => Number
    | (List(a), List(b)) => (if (pass_eq(~type1=a,~type2=b)) {List(a);} else {CONFLICT;})
    | (APP(_), APP(_)) => (if (pass_eq(~type1=type1,~type2=type2)) {type1;} else {CONFLICT;})
    | (_, CANNOT_INFER) => CANNOT_INFER
    | (CANNOT_INFER, _) => CANNOT_INFER
    | _ => CONFLICT
  };

//translate to string
let rec pass_trans = (~type1: pass_t) : option(string) =>
  switch (type1){
    | Bool => Some("bool")
    | Number => Some("int")
    | Unit => Some("()")
    | List(a) => option_string_concat(~strs=[pass_trans(~type1=a), Some(" list")])
    | APP(a, b) => option_string_concat(~strs=[pass_trans(~type1=a), Some(" -> "), pass_trans(~type1=b)])
    | _ => None
  };






