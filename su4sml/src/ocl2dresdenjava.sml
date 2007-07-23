(*****************************************************************************
 * su4sml --- a SML repository for managing (Secure)UML/OCL models
 *             http://projects.brucker.ch/su4sml/
 *                                                                            
 * ocl2dresdenjava.sml --- 
 * This file is part of su4sml.
 *
 * Copyright (c) 2005-2007, ETH Zurich, Switzerland
 *
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are
 * met:
 *
 *     * Redistributions of source code must retain the above copyright
 *       notice, this list of conditions and the following disclaimer.
 *
 *     * Redistributions in binary form must reproduce the above
 *       copyright notice, this list of conditions and the following
 *       disclaimer in the documentation and/or other materials provided
 *       with the distribution.
 *
 *     * Neither the name of the copyright holders nor the names of its
 *       contributors may be used to endorse or promote products derived
 *       from this software without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 * "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
 * LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
 * A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
 * OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
 * SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
 * LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
 * DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
 * THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
 * OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 ******************************************************************************)
(* $Id$ *)

(**
 * A counter for the variables which returns a new number on each
 * next()/nextstr() call.
 *)
structure varcounter = struct
val count = ref ~1

(** Increment and return the counter value. *)
fun next() = (count := !count + 1; !count)

(** Increment and return the counter value as a string. *)
fun nextStr() = Int.toString (next())

(** Reset the counter to -1. *)
fun reset() = (count := ~1)

(** Get the current value. *)
fun current() = !count

(** Get the current value as a string. *)
fun currentStr() = Int.toString(current())
end


(**
 * A map to store the relation of values calculated before the function
 * call which are used in @pre terms.
 *)
structure preMap = struct open library
val entries : (string * int) list ref = ref nil

(** Insert a new entry - if an entry with the same key already exists, it will be removed. *)
fun put (key : string) (value : int) = entries := (key,value)::(List.filter (fn (entry) => (fst entry) <> key) (!entries))

(** Get the entry with the given key. *)
fun get (key : string) = case (List.filter (fn (entry) => (fst entry) = key) (!entries)) of
			     [result] => snd result
			   | _ => ~1

(** Check if an entry with the given key exists. *)
fun has (key : string) = foldr (fn (a,b) => ((fst a) = key) orelse b) false (!entries)

(** Clear the list. *)
fun clear () = entries := nil

(** Get the current size of the list. *)
fun size () = List.length (!entries)
end


(** 
 * Conversion of OCL expressions to Java code which makes use of the
 * Dresden standard ocl library. 
 *)
structure Ocl2DresdenJava = struct open library open Rep_OclType open Rep_OclTerm open Rep_Core open Ocl2String

(** 
 * Convert an oclterm to Java. 'on' should be the object which represents
 * self, so in most cases it will be 'this'. The result is string * int
 * where string is the generated code and int is the id of the oclNode which
 * contains the result of the generated code (ie. the last node which got
 * assigned a value in the generated code).
 *) 
fun ocl2java' oclterm on =
    let
	(* Get the oclNode number as a string.  *)
	fun count res = Int.toString(snd res)

	(* Get the generated code. *)
	fun code res = fst res

	(* Generate a new node of type ntype which gets assigned the result of ncode. *)
	fun newNode ntype ncode = "final "^ntype^" oclNode"^(varcounter.nextStr())^" = "^ncode^";\n"

	(* Generate a new UmlOclFactory. *)
	fun newFact () = ("final UmlOclFactory oclFact"^(varcounter.nextStr())^" = UmlOclFactory.getInstance();\n", 
			  varcounter.current())

	(* Convert a node id to oclNode<id>. *)
	fun node nid = "oclNode"^(count nid)

	(* Convert a node id to oclFact<id>. *)
	fun fact fid = "oclFact"^(count fid)

	(* Generate code for an if statement by evaluating condition and the two branches. *)
	fun ifStmt cond thenb elseb rest =
	    let 
		val condition = ocl2java' cond on
		val thenbranch = ocl2java' thenb on
		val elsebranch = ocl2java' elseb on
	    in
		((code condition)^
		 (code thenbranch)^
		 (code elsebranch)^
		 (newNode ("Ocl"^(string_of_OclType rest)) ("Ocl.toOcl"^(string_of_OclType rest)^"("^(node condition)^".ifThenElse("^(node thenbranch)^", "^(node elsebranch)^"))")),
		 varcounter.current())
	    end

	(* Get an object which represents the given OclType. *)
	fun typeObj ptype = 
	    case ptype of (Classifier p) =>
			  let
			      val factory = newFact ()
			      val oclmodeltype = newNode "OclModelType" ((fact factory)^".getOclModelTypeFor(\""^(string_of_OclType_colon ptype)^"\")")
			  in
			      ((code factory)^
			       oclmodeltype,
			       varcounter.current())
			  end
			| (Set s) =>
			  let
			      val setType = typeObj s
			  in
			      ((code setType)^
			       (newNode "OclCollectionType" ((node setType)^".getOclSetType()")),
			       varcounter.current())
			  end
			| OclAny =>
			  ((newNode "OclType" ("OclType.getOclAny()")),
			   varcounter.current())
			| _ =>
			  (newNode "OclPrimitiveType" ("OclPrimitiveType.getOcl"^(string_of_OclType ptype)^"()"),varcounter.current())

	(* Get the node id of the node which stores the result of src which was used in an @pre expression. *)
	fun atPre src = ("", preMap.get(ocl2string true src))

	(* Generate code for an attribute/association end call. *)
	fun attrCall src path ptype =
	    case src of OperationCall (osrc,styp,["oclLib","OclAny","atPre"],[],_) => atPre oclterm
		      | _ =>
			let
			    val target = ocl2java' src on
			    val typeObject = typeObj ptype
			    fun node' typ typobj = (newNode ("Ocl"^typ) ("Ocl.toOcl"^typ^"("^(node target)^".getFeature("^(node typobj)^", \""^(hd (rev path))^"\"))"))
			in
			    case ptype of (Classifier p) => 
					  ((code target)^
					   (code typeObject)^
					   (node' "ModelObject" typeObject),
					   varcounter.current())
					| (Set s) => 
					  ((code target)^
					   (code typeObject)^
					   (node' "Set" typeObject),
					   varcounter.current())
					| _      => 
					  ((code target)^
					   (code typeObject)^
					   (node' (string_of_OclType ptype) typeObject),
					   varcounter.current())
			end

	(* Access to a variable - this is either self, result or an argument to the function call. *)
	fun var name t = 
	    let 
		val factory = newFact ()
		val vname = if name = "self" then on else name
		val vtype = typeObj t
		fun node' typ typobj = newNode ("Ocl"^typ) ("(Ocl"^typ^")"^(fact factory)^".getOclRepresentationFor("^(node typobj)^", "^vname^")")
	    in
		case t of (Classifier p) =>
			  ((code factory)^
			   (code vtype)^
			   (node' "ModelObject" vtype),
			   varcounter.current())
			| _ => 
			  ((code factory)^
			   (code vtype)^
			   (node' (string_of_OclType t) vtype),
			   varcounter.current())
	    end

	(* Generate code for binary operations on basic types. *)
	fun string_of_binop src bop arg rtype =
	    let
		val left = ocl2java' src on
		val right = ocl2java' arg on
	    in
		((code left)^
		 (code right)^
		 (newNode ("Ocl"^(string_of_OclType rtype)) ((node left)^"."^bop^"("^(node right)^")")),
		 varcounter.current())
	    end

	(* Generate code for unary operations on basic types. *)
	fun string_of_unop src sop rtype =
	    let 
		val right = ocl2java' src on
	    in
		((code right)^
		 (newNode ("Ocl"^(string_of_OclType rtype)) ((node right)^"."^sop^"()")),
		 varcounter.current())
	    end

	(* Get an empty set. *)
	fun emptySet () = (newNode "OclSet" "OclSet.getEmptyOclSet()",
			   varcounter.current())

	(* Insert the result of src into an empty set. *)
	fun oclset src =
	    let
		val src' = ocl2java' src on
		val set = emptySet ()
	    in
		((code src')^
		 (code set)^
		 (node set)^".setToInclude("^(node src')^");\n",
		 varcounter.current())
	    end
	    
	(* Generate code for the ->notEmpty() operation. *)
	fun oclnotempty src = 
	    let
		val src' = ocl2java' src on
	    in
		((code src')^
		 (newNode "OclBoolean" ((node src')^".notEmpty()")),
		 varcounter.current()) 
	    end

	(* Generate code for the ->isEmpty() operation. *)
	fun oclempty src = 
	    let
		val src' = ocl2java' src on
	    in
		((code src')^
		 (newNode "OclBoolean" ((node src')^".isEmpty()")),
		 varcounter.current()) 
	    end

	(* Generate code for the ->size() operation. *)
	fun oclsize src = 
	    let
		val src' = ocl2java' src on
	    in
		((code src')^
		 (newNode "OclInteger" ((node src')^".size()")),
		 varcounter.current()) 
	    end

	(* Generate code for a function call. Evaluate parameters and pass them to the call. *)
	fun opCall src op_name args rtype =
	    let
		val src' = ocl2java' src on
		val resultTypeObj = typeObj rtype
		(* Evaluate arguments, generate new node with the result. *)
		fun evalArg (arg,atype) = 
		    let
			val acode = ocl2java' arg on
			fun umltype utype = case utype of
						Integer => "UmlType.INT"
					      | Real => "UmlType.REAL"
					      | String => "UmlType.STRING"
					      | Boolean => "UmlType.BOOLEAN"
					      | _ => "UmlType.MODELTYPE"
		    in
			((code acode)^
			 (newNode "OclParameter" ("new OclParameter("^(umltype atype)^", "^(node acode)^")")),
			 varcounter.current())
		    end
		val arguments = map evalArg args
		val argcode = join "" (map fst arguments)
		(* Evaluated arguments *)
		val argsEvald = join ", " (map (fn a => node a) arguments)
		fun node' typ typobj = argcode^(newNode ("Ocl"^typ) ("Ocl.toOcl"^typ^"("^(node src')^".getFeature("^(node typobj)^", \""^(hd (rev op_name))^"\", new OclParameter[]{"^argsEvald^"}))"))
	    in
		case rtype of (Classifier p) => 
			      ((code src')^
			       (code resultTypeObj)^
			       (node' "ModelObject" resultTypeObj),
			       varcounter.current())
			    | _      => 
			      ((code src')^
			       (code resultTypeObj)^
			       (node' (string_of_OclType rtype) resultTypeObj),
			       varcounter.current())
	    end

	(* Generate code for the ->oclAsType() operation. *)
	fun oclAsType src oclType rtyp =
	    let
		val src' = ocl2java' src on
		val typeObject = typeObj rtyp
	    in
		case oclType of
		    OclAny => 
		    ((code src')^
		     (code typeObject)^
		     (newNode "OclAny" ("Ocl.toOclAny("^(node src')^".oclAsType("^(node typeObject)^"))")),
		     varcounter.current())
		  | _ =>
		    ((code src')^
		     (code typeObject)^
		     (newNode "OclModelObject" ("Ocl.toOclModelObject("^(node src')^".oclAsType("^(node typeObject)^"))")),
		     varcounter.current())
	    end

	(* Generate code for the ->oclIsUndefined() operation. *)
	fun oclIsUndefined src =
	    let
		val src' = ocl2java' src on
	    in 
		((code src')^
		 (newNode "OclBoolean" ("OclBoolean.getOclRepresentationFor("^(node src')^".isUndefined())")),
		  varcounter.current())
	    end

	(* Generate code for the ->oclIsDefined() operation. *)
	fun oclIsDefined src = 
	    let
		val src' = oclIsUndefined src
	    in
		((code src')^
		 (newNode "OclBoolean" ((node src')^".not()")),
		 varcounter.current())
	    end
    in
	case oclterm of 
	    (* Literals *)
	    Literal ("true",Boolean)  => (newNode "OclBoolean" "OclBoolean.TRUE",varcounter.current())
	  | Literal ("false",Boolean) => (newNode "OclBoolean" "OclBoolean.FALSE",varcounter.current())
	  | Literal (l,Integer)       => (newNode "OclInteger" ("new OclInteger("^l^")"),varcounter.current())
	  | Literal (s,String)        => (newNode "OclString" ("new OclString(\""^s^"\")"),varcounter.current())
	  | Literal (r,Real)          => (newNode "OclReal" ("new OclReal("^r^")"),varcounter.current())
	  (* Logical operators *)
	  | OperationCall (src,Boolean,["oclLib","Boolean","and"],[(arg,Boolean)],rtype) => string_of_binop src "and" arg rtype
	  | OperationCall (src,Boolean,["oclLib","Boolean","or"],[(arg,Boolean)],rtype)  => string_of_binop src "or" arg rtype
	  | OperationCall (src,Boolean,["oclLib","Boolean","xor"],[(arg,Boolean)],rtype) => string_of_binop src "xor" arg rtype
	  | OperationCall (src,Boolean,["oclLib","Boolean","not"],[],rtype)              => string_of_unop src "not" rtype
	  | OperationCall (src,Boolean,["oclLib","Boolean","implies"],[(arg,Boolean)],rtype) => string_of_binop src "implies" arg rtype
	  (* Comparison operators *)
	  | OperationCall (src,styp,["oclLib",classifier,"="],[(arg,atyp)],rtype)   => string_of_binop src "isEqualTo"  arg rtype
	  | OperationCall (src,styp,["oclLib",classifier,"<>"],[(arg,atyp)],rtype)  => string_of_binop src "isNotEqualTo" arg rtype
	  | OperationCall (src,styp,["oclLib",classifier,"=="],[(arg,atyp)],rtype)  => string_of_binop src "isEqualTo" arg rtype
	  | OperationCall (src,styp,["oclLib",classifier,"~="],[(arg,atyp)],rtype)  => string_of_binop src "isNotEqualTo" arg rtype
	  (* OCL Real *)
	  | OperationCall (src,styp,["oclLib",classifier,"round"],[],rtype)         => string_of_unop src "round" rtype
	  | OperationCall (src,styp,["oclLib",classifier,"floor"],[],rtype)         => string_of_unop src "floor" rtype
	  | OperationCall (src,styp,["oclLib",classifier,"min"],[(arg,atyp)],rtype) => string_of_binop src "min" arg rtype
	  | OperationCall (src,styp,["oclLib",classifier,"max"],[(arg,atyp)],rtype) => string_of_binop src "max" arg rtype
	  | OperationCall (src,styp,["oclLib",classifier,"/"],[(arg,atyp)],rtype)   => string_of_binop src "divide" arg rtype
	  | OperationCall (src,styp,["oclLib",classifier,"abs"],[],rtype)           => string_of_unop src "abs" rtype
	  | OperationCall (src,styp,["oclLib",classifier,"-"],[(arg,atyp)],rtype)   => string_of_binop src "subtract" arg rtype
	  | OperationCall (src,styp,["oclLib",classifier,"+"],[(arg,atyp)],rtype)   => string_of_binop src "add" arg rtype
	  | OperationCall (src,styp,["oclLib",classifier,"*"],[(arg,atyp)],rtype)   => string_of_binop src "multiply" arg rtype  
	  (* OCL Integer *)
	  | OperationCall (src,styp,["oclLib",classifier,"mod"],[(arg,atyp)],rtyp)  => string_of_binop src "mod" arg rtyp
	  | OperationCall (src,styp,["oclLib",classifier,"div"],[(arg,atyp)],rtyp)  => string_of_binop src "div" arg rtyp
	  | OperationCall (src,styp,["oclLib",classifier,"-"],[],rtyp)              => string_of_unop src "negative" rtyp
	  (* OCL Numerals *)
	  | OperationCall (src,styp,["oclLib",classifier,"<"],[(arg,atyp)],rtyp)    => string_of_binop src "isLessThan" arg rtyp
	  | OperationCall (src,styp,["oclLib",classifier,"<="],[(arg,atyp)],rtyp)   => string_of_binop src "isLessEqual" arg rtyp
	  | OperationCall (src,styp,["oclLib",classifier,">"],[(arg,atyp)],rtyp)    => string_of_binop src "isGreaterThan" arg rtyp
	  | OperationCall (src,styp,["oclLib",classifier,">="],[(arg,atyp)],rtyp)   => string_of_binop src "isGreaterEqual" arg rtyp
	  (* Some collection operations *)
	  | OperationCall (src,styp,["oclLib",_,"asSet"],[],rtyp)    => oclset src
	  | OperationCall (src,styp,["oclLib",_,"notEmpty"],[],rtyp) => oclnotempty src
	  | OperationCall (src,styp,["oclLib",_,"isEmpty"],[],rtyp)  => oclempty src
	  | OperationCall (src,styp,["oclLib",_,"size"],[],rtyp)     => oclsize src
	  (* oclIs(Und|D)efined *)
	  | OperationCall (src,styp,["oclIsDefined"],[],rtyp)        => oclIsDefined src
	  | OperationCall (src,styp,["oclIsUndefined"],[],rtyp)      => oclIsUndefined src
	  (* @pre *)
	  | OperationCall (src,styp,["oclLib","OclAny","atPre"],[],_) => atPre src
	  (* Unsupported call - TODO: maybe replace by error "..."? *)
	  | OperationCall (src,styp,[opname],[],rtyp) => (("/* Unsupported OCL operation "^opname^". */\n"),varcounter.current())
	  (* If *)
	  | If (cond,condt,thenb,thent,elseb,elset,rest) => ifStmt cond thenb elseb rest
	  (* Access to attributes *)
	  | AttributeCall (src,stype,path,ptype) => attrCall src path ptype
	  (* Access association ends *)
	  | AssociationEndCall (src,stype,path,ptype) => attrCall src path ptype
	  (* Access to variables *)
	  | Variable (name, t) => var name t
	  (* Function calls *)
	  | OperationCall (src,styp,op_name,args,rtype) => opCall src op_name args rtype
	  (* oclAsType *)
	  | OperationWithType (src,styp,"oclAsType",oclType,rtyp) => oclAsType src oclType rtyp
	  (* Print currently unknown stuff using ocl2string. TODO: maybe replace by error "..."? *)
	  | _ => ("/* "^(ocl2string true oclterm)^" */\n", 0)
    end

(** Return the Java code which evaluates the oclterm. *)
fun ocl2java oclterm on = fst (ocl2java' oclterm on)

(** Convert list of arguments ((string * Rep_OclType.OclType) list) to a comma separated string. *)
fun opargs2string args = 
    let 
	fun arg2string (name,typ) = (Rep_OclType.string_of_OclType typ)^" "^name
    in
	join ", " (List.map arg2string args )
    end
    
(** Return ocl formula as a Java comment. *)
fun oclComment formula = "/* "^(ocl2string false formula)^" */\n"

(** Check the result of checking a condition. *)
fun checkConditionResult condition name condType ex uut = 
    let 
	val name' = case name of SOME t => " "^t
			       | NONE => ""
    in
	(fst condition)^
	"if(!oclNode"^(Int.toString(snd condition))^".isTrue()){"^
	"\n\tthrow new "^ex^"(\""^condType^name'^" of "^uut^" failed!\");\n"^
	"}\n"
    end

(** Extract @pre operations from the postconditions, generate code to save @pre values. *)
fun preExtract env on curOp =
    let
	fun getPres precond = 
	    let
		(* Save the result of attribute/association end calls. *)
		fun attSave src stype path ptype call = case src of OperationCall (asrc,styp,["oclLib","OclAny","atPre"],[],_) => 
								    let
									val condstr = ocl2string true precond
								    in
									if not (preMap.has(condstr)) then
									    let
										val code = ocl2java' (call (asrc,stype,path,ptype)) on
									    in 
										((preMap.put condstr (snd code));
										 (fst code))
									    end
									else
									    ""    
								    end
								  | _ => getPres src
		(* Save the result of function calls. *)
		fun resSave src styp op_name args rtype = case src of OperationCall (asrc,styp,["oclLib","OclAny","atPre"],[],_) => 
								      let
									  val condstr = ocl2string true asrc
								      in
									  if not (preMap.has(condstr)) then
									      let
										  val code = ocl2java' asrc on
									      in 
										  ((preMap.put condstr (snd code));
										   (fst code))
									      end
									  else
									      ""
								      end
								    | _ => (getPres src)^(join "\n" (map (getPres o fst) args))
	    in
		case precond of
		    OperationCall (src,styp,["oclLib","OclAny","atPre"],[],_) => error "atPre()-operation should not be reached."
		  | OperationCall (src,styp,op_name,args,rtype) => resSave src styp op_name args rtype
		  | Literal (_,_) => ""
		  | If (cond,_,thenb,_,elseb,_,_) => (getPres cond)^(getPres thenb)^(getPres elseb)
		  | AttributeCall (src,stype,path,ptype) => attSave src stype path ptype AttributeCall
		  | AssociationEndCall (src,stype,path,ptype) => attSave src stype path ptype AssociationEndCall
		  | Variable (_,_) => ""
		  | OperationWithType (src,_,_,_,_) => getPres src
		  | _ => ""
	    end
    in
	join "\n" (List.map (getPres o snd) (Rep_Core.postcondition_of_op curOp))
    end

(** Create the string which checks preconditions. *)
fun precondString env on curOp ex = 
    let 
	fun getPrecond precond = 
	    (oclComment (snd precond))^
	    (checkConditionResult (ocl2java' (snd precond) on) (fst precond) "Precondition" ex ((Rep_Core.name_of_op curOp)^"("^(opargs2string (Rep_Core.arguments_of_op curOp))^")"))
    in
	(preMap.clear();
	(join "\n" (List.map getPrecond (Rep_Core.precondition_of_op curOp)))^
	("\n/* Save values used in @pre-expressions of the postcondition */\n"^
	 (preExtract env on curOp)))
    end

(** Create the string which checks postconditions. *)
fun postcondString env on curOp ex =
    let 
	fun getPostcond postcond = 
	    (oclComment (snd postcond))^
	    (checkConditionResult (ocl2java' (snd postcond) on) (fst postcond) "Postcondition" ex ((Rep_Core.name_of_op curOp)^"("^(opargs2string (Rep_Core.arguments_of_op curOp))^")"))
    in
	join "\n" (List.map getPostcond (Rep_Core.postcondition_of_op curOp))
    end

(** Create the string which checks invariants. *)
fun invString env on curCl ex =
    let 
	fun getInvariant invariant = 
	    (oclComment (snd invariant))^
	    (checkConditionResult (ocl2java' (snd invariant) on) (fst invariant) "Invariant" ex (Rep_Core.short_name_of curCl))
    in
	join "\n" (List.map getInvariant (Rep_Core.invariant_of curCl))
    end

end
