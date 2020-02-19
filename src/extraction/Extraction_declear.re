/* This is a decleration of the inner types used in the extraction
most base but useless code can be directly re-used in the old version
    - Note that opseq had been rewrite to a top-level expression, need to re-write indeed.

TODO: things in order need to be done
    1. declear the inner type used and functions
    2. declear the vocabulary_set functions 
    3. include the error type and pass rules*/


type pass_t =
    | HOLE
    | Bool
    | Number
    | Unit
    | List(pass_t)
    | APP(pass_t, pass_t)
    //| Pair(pass_t, pass_t)
    | EMPTY //as None, or currently NO type
    | CANNOT_INFER // dependency type like list(a)
    | CONFLICT;     //it's an error


// the return type of most function
// (extracted string, the passing type)
type extract_t = (option(string), pass_t)


// the variable environment 
// from top level to down levels, each level insert new ones
// (name, type), if local bind, override as rules
type variable_set_t = list((string, pass_t))

