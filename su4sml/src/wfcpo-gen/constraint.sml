(** A syntactic or semantic constraint for a UML/OCL model.
 * Every constraint consists of at leas a Constraint and Constraint info.
 * 
 *)
signature CONSTRAINT =
sig 
    type constraint = { name      : string,
		    description   : string,
		    generator     : Rep.Model -> Rep_OclTerm.OclTerm list }
			
    exception ConstraintError of string
end

(** The common type for every constraint *)
structure Constraint : CONSTRAINT =
struct

(* su4sml *)
open Rep_Core
open Rep
open Rep_OclTerm

type constraint = { name          : string,
		    description   : string,
		    generator     : Rep.Model -> OclTerm list }

exception ConstraintError of string

end;
