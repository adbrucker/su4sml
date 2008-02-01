signature PLUGIN_CONSTRAINT =
sig 
   include CONSTRAINT
   val getConstraint : Constraint.constraint
   val info : string
   val generate_po   : Rep.Model -> Rep_OclTerm.OclTerm list
   exception Plugin_Constraint_Error of string
end
structure Plugin_Constraint : PLUGIN_CONSTRAINT =
struct

(* su4sml *)
open Rep_Core
open Rep
open Rep_OclTerm

(* wfcpo-gen *)
open Constraint

type constraint = Constraint.constraint 

exception ConstraintError of string
exception Plugin_Constraint_Error of string

fun generate_po (model:Rep.Model) = [Predicate (Literal("true",Boolean),Boolean,["DUMMY_PREDICATE"],[])]

val getConstraint = {name="plugin_constraint",description="A plugin_constraint cannot be instantiated itself. Use a corresponding plugin.\n",generator=generate_po}:constraint

val info = "This is just the structure. For creating a new plugin constraint, write a functor implementing the PLUGIN_CONSTRAINT signature. The a new constraint can be registered in the WFCPO_Registry by add the structure NEW_FUNCTOR(Plugin) and calling the value newConstraint."
end;
