(** 
 * Conversion of OCL expressions to Java code which makes use of the
 * Dresden standard ocl library. 
 *)

structure varcounter = struct
val count = ref ~1
fun next() = (count := !count + 1; !count)
fun nextStr() = Int.toString (next())
fun reset() = (count := ~1)
fun current() = !count
fun currentStr() = Int.toString(current())
end

structure Ocl2DresdenJava = struct open library open Rep_OclType open Rep_OclTerm open Rep_Core
									       
fun ocl2java' oclterm on = 
    let
	fun count res = Int.toString(snd res)
	fun code res = fst res
	fun newNode ntype ncode = "final "^ntype^" oclNode"^(varcounter.nextStr())^" = "^ncode^";\n"
	fun newFact () = ("final UmlOclFactory oclFact"^(varcounter.nextStr())^" = UmlOclFactory.getInstance();\n", 
			  varcounter.current())
	fun node nid = "oclNode"^(count nid)
	fun fact fid = "oclFact"^(count fid)
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
	fun attrCall src path ptype =
	    let
		val target = ocl2java' src on
		fun node' typ typobj = (newNode ("Ocl"^typ) ("Ocl.toOcl"^typ^"("^(node target)^".getFeature("^(node typobj)^", \""^(hd (rev path))^"\"))"))
	    in
		case ptype of (Classifier p) => 
			      let
				  val factory = newFact ()
				  val oclmodeltype = (newNode "OclModelType" ((fact factory)^".getOclModelTypeFor(\""^(string_of_OclType_colon ptype)^"\")"),varcounter.current())
			      in
				  ((code factory)^
				   (code target)^
				   (code oclmodeltype)^
				   (node' "ModelObject" oclmodeltype),
				   varcounter.current())
			      end
			    | _      => 
			      let 
				  val oclprimtype = (newNode "OclPrimitiveType" ("OclPrimitiveType.getOcl"^(string_of_OclType ptype)^"()"),varcounter.current())
			      in
				  ((code target)^
				   (code oclprimtype)^
				   (node' (string_of_OclType ptype) oclprimtype),
				   varcounter.current())
			      end
	    end
		
	fun var name t = 
	    let 
		val factory = newFact ()
		val vname = if name = "self" then on else name
	    in
		case t of Integer => 
			  let 
			      val integertype = (newNode "OclPrimitiveType" "OclPrimitiveType.getOclInteger()",varcounter.current())
			  in
			      ((code factory)^
			       (code integertype)^
			       (newNode "OclInteger" ("(OclInteger)"^(fact factory)^".getOclRepresentationFor("^(node integertype)^", "^vname^")")),
			       varcounter.current())
			  end
			| _ =>
			  let
			      val modeltype = (newNode "OclModelType" ((fact factory)^".getOclModelTypeFor(\""^(string_of_OclType_colon t)^"\")"),
						varcounter.current())
			  in
			      ((code factory)^
			       (code modeltype)^
			       (newNode "OclModelObject" ("(OclModelObject)"^(fact factory)^".getOclRepresentationFor("^(node modeltype)^", "^vname^")")),
			       varcounter.current())
			  end
	    end
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
	fun string_of_unop src sop rtype =
	    let 
		val right = ocl2java' src on
	    in
		((code right)^
		 (newNode ("Ocl"^(string_of_OclType rtype)) ((node right)^"."^sop^"()")),
		 varcounter.current())
	    end
	fun emptySet () = (newNode "OclSet" "OclSet.getEmptyOclSet()",
			   varcounter.current())
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
	fun oclnotempty src = 
	    let
		val src' = ocl2java' src on
	    in
		((code src')^
		 (newNode "OclBoolean" ((node src')^".notEmpty()")),
		 varcounter.current()) 
	    end
	fun oclempty src = 
	    let
		val src' = ocl2java' src on
	    in
		((code src')^
		 (newNode "OclBoolean" ((node src')^".isEmpty()")),
		 varcounter.current()) 
	    end
	fun oclsize src = 
	    let
		val src' = ocl2java' src on
	    in
		((code src')^
		 (newNode "OclInteger" ((node src')^".size()")),
		 varcounter.current()) 
	    end
		
    in
    case oclterm of 
	Literal ("true",Boolean) => (newNode "OclBoolean" "OclBoolean.TRUE",varcounter.current())
      | Literal ("false",Boolean) => (newNode "OclBoolean" "OclBoolean.FALSE",varcounter.current())
      | Literal (l,Integer) => (newNode "OclInteger" ("new OclInteger("^l^")"),varcounter.current())
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
      | OperationCall (src,styp,["oclLib",_,"asSet"],[],rtyp) => oclset src
      | OperationCall (src,styp,["oclLib",_,"notEmpty"],[],rtyp) => oclnotempty src
      | OperationCall (src,styp,["oclLib",_,"isEmpty"],[],rtyp) => oclempty src
      | OperationCall (src,styp,["oclLib",_,"size"],[],rtyp) => oclsize src
      (* If *)
      | If (cond,condt,thenb,thent,elseb,elset,rest) => ifStmt cond thenb elseb rest
      (* Access to attributes *)
      | AttributeCall (src,styp,path,ptype) => attrCall src path ptype
      (* Access to variables *)
      | Variable (name, t) => var name t
      | _ => (Ocl2String.ocl2string true oclterm, 0)
    end

fun ocl2java oclterm on = fst (ocl2java' oclterm on)

(* Convert list of arguments ((string * Rep_OclType.OclType) list) to a comma separated string *)
fun opargs2string args = 
    let 
	fun arg2string (name,typ) = (Rep_OclType.string_of_OclType typ)^" "^name
    in
	join ", " (List.map arg2string args )
    end
	    

(* Check the result of checking a condition *)
fun checkConditionResult condition name condType uut = 
    let 
	val name' = case name of SOME t => " "^t
			       | NONE => ""
    in
	(fst condition)^
	"if(!oclNode"^(Int.toString(snd condition))^".isTrue()){"^
	"\n\tthrow new "^condType^"FailedException(\""^condType^name'^" of "^uut^" failed!\");\n"^
	"}\n"
    end
    
(* Create the string which checks preconditions *)
fun precondString env on curOp = 
    let fun getPrecond precond = checkConditionResult (ocl2java' (snd precond) on) (fst precond) "Precondition" ((Rep_Core.name_of_op curOp)^"("^(opargs2string (Rep_Core.arguments_of_op curOp))^")")
    in
	join "\n" (List.map getPrecond (Rep_Core.precondition_of_op curOp))
    end

(* Create the string which checks postconditions *)
fun postcondString env on curOp =
    let fun getPostcond postcond = checkConditionResult (ocl2java' (snd postcond) on) (fst postcond) "Postcondition" ((Rep_Core.name_of_op curOp)^"("^(opargs2string (Rep_Core.arguments_of_op curOp))^")")
    in
	join "\n" (List.map getPostcond (Rep_Core.postcondition_of_op curOp))
    end

(* Create the string which checks invariants *)
fun invString env on curCl =
    let fun getInvariant invariant = checkConditionResult (ocl2java' (snd invariant) on) (fst invariant) "Invariant" (Rep_Core.short_name_of curCl)
    in
	join "\n" (List.map getInvariant (Rep_Core.invariant_of curCl))
    end

end
