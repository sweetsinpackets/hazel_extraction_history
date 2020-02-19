// this is the tool functions 

open Extraction_declear;


let rec option_string_concat = (~strs : list( option(string))) : option(string) =>
  switch (strs) {
    | [] => Some("")
    | [a, ...rest] => switch(a, option_string_concat(~strs=rest)) {
      | (Some(s1), Some(s2)) => Some(s1 ++ s2)
      | _ => None
    }
  };

let rec option_string_concat_ignoreNone = (~strs : list( option(string))) : option(string) =>
  switch (strs) {
    | [] => Some("")
    | [a, ...rest] => switch(a, option_string_concat_ignoreNone(~strs=rest)) {
      | (Some(s1), Some(s2)) => Some(s1 ++ s2)
      | (Some(s1), None) => Some(s1)
      | (None, Some(s2)) => Some(s2)
      | _ => None
    }
  };

//insert indent_space just like previous work
let rec indent_space = (~level: int): string =>
  if (level > 0) {
    "  " ++ indent_space(~level=level - 1);
  } else {
    "";
  };

//=========================================
// PASS_T
//=========================================

// Check whether two types are EQUAL
// Logic: if Conflict appears in any subcases, pass Conflict
//        if CANNOT_INFER appears with some other type, use other type
//        FIXME: EMPTY case
//        FIXME: add Hole cases

let rec pass_eq = (~type1: pass_t, ~type2: pass_t) : bool =>
  switch (type1, type2){
    | (CONFLICT, _) => false
    | (_, CONFLICT) => false
    | (CANNOT_INFER, _) => true
    | (_, CANNOT_INFER) => true
    | (EMPTY, _) => true
    | (_, EMPTY) => true
    | (HOLE, HOLE) => true
    | (Unit, Unit) => true
    | (Bool, Bool) => true
    | (Number, Number) => true
    | (List(a), List(b)) => pass_eq(~type1=a, ~type2=b)
    | (APP(a1, b1), APP(a2, b2)) => pass_eq(~type1=a1, ~type2=a2) && pass_eq(~type1=b1, ~type2=b2)
    | _ => false
  };

// use whenever need to combine two type_t into one
let rec pass_check = (~type1: pass_t, ~type2: pass_t) : pass_t =>
  switch (type1, type2){
    | (EMPTY, a) => a
    | (a, EMPTY) => a 
    | (a, CANNOT_INFER) => a
    | (CANNOT_INFER, b) => b    
    | (CONFLICT, _) => CONFLICT
    | (_, CONFLICT) => CONFLICT
    //other cases
    | (Unit, Unit) => Unit
    | (Bool, Bool) => Bool
    | (Number, Number) => Number   
    //EMTPY don't allow to appear in dependency type 
    | (List(a), List(b)) => switch(pass_check(~type1=a, ~type2=b)){
      | CONFLICT => CONFLICT
      | EMPTY => CONFLICT
      | _ => List(pass_check(~type1=a, ~type2=b))
    }
    | (APP(a1, b1), APP(a2, b2)) => switch(pass_check(~type1=a1, ~type2=a2), pass_check(~type1=b1, ~type2=b2)){
      | (CONFLICT, _) => CONFLICT
      | (_, CONFLICT) => CONFLICT
      | (EMPTY, _) => CONFLICT
      | (_, EMPTY) => CONFLICT
      | _ => APP(pass_check(~type1=a1, ~type2=a2), pass_check(~type1=b1, ~type2=b2))
    }
    | _ => CONFLICT
  };

// a sugar for pass check for multiconditions
let rec pass_concat = (~types: list(pass_t)) : pass_t =>
    switch (types){
        | [] => EMPTY
        | [a] => a
        | [h,...t] => pass_check(~type1=h, ~type2=pass_concat(~types=t))
    };


//===================================
// Vairable Set
//===================================

// assume won't duplicate
// find a variable in the set, if don't find, return EMPTY
let rec find_variable_set = (~var:string, ~set:variable_set_t) : pass_t=>
  switch(set){
    | [] => EMPTY
    | [a, ...l] => if (var == fst(a)) {snd(a);} else {find_variable_set(~var=var,~set=l);}
  };


// add a new variable to the current variable set
// if already exists, apply override 
let rec add_variable = (~v:extract_t, ~env:variable_set_t) : variable_set_t =>
    switch (fst(v)){
        | None => env
        | Some(s) => switch(env){
            | [] => []
            | [h,...t] => if (fst(h) == s) {[(s, snd(v)),...add_variable(~v=v, ~env=t)]} 
                        else {[h,...add_variable(~v=v, ~env=t)]}
        }
    };



