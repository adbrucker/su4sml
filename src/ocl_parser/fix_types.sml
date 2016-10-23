(*****************************************************************************
 * su4sml --- an SML repository for managing (Secure)UML/OCL models
 *             http://projects.brucker.ch/su4sml/
 *                                                                            
 * fix_types.sml --- 
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


signature FixTypes = sig 
    val type_of : Rep_OclTerm.OclTerm -> Rep_OclTerm.OclType
    val src_type_of : Rep_OclTerm.OclTerm -> Rep_OclTerm.OclType
    val IntegerLiteralToReal : Rep_OclTerm.OclTerm -> Rep_OclTerm.OclTerm
    val downcastCollection  : Rep_OclTerm.OclTerm -> Rep_OclTerm.OclTerm
    val transformForHolOcl  : Rep_OclTerm.OclTerm -> Rep_OclTerm.OclTerm
    val upCastClassifier : Rep_OclTerm.OclTerm -> Rep_OclTerm.OclTerm
									 
    val transform_invariant:  (Rep_OclTerm.OclTerm -> Rep_OclTerm.OclTerm) -> Rep_Core.Classifier -> Rep_Core.Classifier  
    val transform_invariants:  (Rep_OclTerm.OclTerm -> Rep_OclTerm.OclTerm) -> Rep_Core.Classifier list -> Rep_Core.Classifier list  
exception FixTyping of string
end

structure FixTyping = struct
open Rep_OclTerm

exception FixTyping of string

fun error t = let val _ = print (t^"\n") in raise FixTyping t end

fun type_of (Literal (_,t )) = t
  | type_of (CollectionLiteral (_,t)) = t 
  | type_of (If (_,_,_,_,_,_,t))  = t
  | type_of (AssociationEndCall (_,_,_,t)) = t
  | type_of (AttributeCall(_,_,_,t)) = t
  | type_of (OperationCall (_,_,_,_,t)) = t
  | type_of (OperationWithType (_,_,_,_,t)) = t
  | type_of (Variable (_,t)) = t
  | type_of (Iterate (_,_,_,_,_,_,_,_,t)) = t
  | type_of (Iterator (_,_,_,_,_,_,t)) = t

fun src_type_of (Literal (_,_)) = error "Literal: src_type_of"
  | src_type_of (CollectionLiteral (_,_)) = error "CollectionLitearal: src_type_of"
  | src_type_of (If (_,_,_,_,_,_,_))  = error "if: src_type_of"
  | src_type_of (AssociationEndCall (_,t,_,_)) = t
  | src_type_of (AttributeCall(_,t,_,_)) = t
  | src_type_of (OperationCall (_,t,_,_,_)) = t
  | src_type_of (OperationWithType (_,t,_,_,_)) = t
  | src_type_of (Variable (_,_)) = error "Variable: src_type_of"
  | src_type_of (Iterate (_,_,_,_,_,_,_,_,_)) = error "Iterate: src_type_of"
  | src_type_of (Iterator (_,_,_,_,_,_,_)) = error "Iterator: src_type_of"

fun downcastCollection  (exp as Literal (_,_)) = exp
  | downcastCollection (exp as (CollectionLiteral (_,_))) 
    = exp
  | downcastCollection (If (c,ct,t,tt,e,et,typ))  
    = (If (downcastCollection c,ct,downcastCollection t,tt,downcastCollection e,et,typ))
  | downcastCollection (AssociationEndCall (src,srcT,a,t)) 
    =(AssociationEndCall(downcastCollection src,srcT,a,t))
  | downcastCollection (AttributeCall(src,srcT,a,t))  
    = (AttributeCall(downcastCollection src,srcT,a,t))
  | downcastCollection (OperationCall (src,st,p,params,typ)) 
    = (let 
	   fun dca (a, at) = (downcastCollection a, at)	
       in
	   (case st of 
		Collection _ => (let 
				    val dc_src = downcastCollection src
				in 
				    (OperationCall (dc_src,type_of (dc_src),p ,map dca params,typ))
				end)
	      | _            =>  (OperationCall (downcastCollection src, st,p ,map dca params,typ)) 
	   )
       end
      )

  | downcastCollection (OperationWithType (src,srcT,n,argTyp,typ))
    = (OperationWithType (downcastCollection src,srcT,n,argTyp,typ))
  | downcastCollection (exp as (Variable (_,_))) 
    = exp 
  | downcastCollection (Let (var, varT, rhs, rhsT, term, typ))
    = (Let (var, varT, downcastCollection rhs, rhsT, downcastCollection  term, typ))
  | downcastCollection (Iterate (vars, result, resultT, resultTerm, src,srct,body,bodyT,typ)) 
    = (Iterate (vars, result, resultT,resultTerm, downcastCollection  src,srct,downcastCollection body,bodyT,typ)) 
  | downcastCollection (Iterator (name, vars, src,srct,body,bodyT,typ)) 
    = (Iterator (name, vars,  downcastCollection  src,srct,downcastCollection body,bodyT,typ)) 


fun setTypeToReal (Literal(v,Real)) = Literal(v,Real)
  | setTypeToReal (Literal(v,Integer)) = Literal(v,Real)
  | setTypeToReal exp  = let 
	val src = IntegerLiteralToReal exp
    in
	if (type_of src = Integer) 
	then OperationWithType (exp,Integer,"oclAsType",Real,Real)
	else exp
    end
and IntegerLiteralToReal (exp as Literal (_,_)) = exp   
  | IntegerLiteralToReal (exp as (CollectionLiteral (_,_))) 
    = exp
  | IntegerLiteralToReal (If (c,ct,t,tt,e,et,typ))  
    = (If (IntegerLiteralToReal c,ct,IntegerLiteralToReal t,tt,IntegerLiteralToReal e,et,typ))
  | IntegerLiteralToReal (AssociationEndCall (src,srcT,a,t)) 
    =(AssociationEndCall(IntegerLiteralToReal src,srcT,a,t))
  | IntegerLiteralToReal (AttributeCall(src,srcT,a,t))  
    = (AttributeCall(IntegerLiteralToReal src,srcT,a,t))
  | IntegerLiteralToReal (OperationCall (src,Real,p,[(arg, Real)],typ)) 
    = let 
	val srcT = type_of src
	val argT = type_of arg
    in
	case (srcT, argT) of 
	    (Real, Real)       => (OperationCall (IntegerLiteralToReal src,Real,p,
						 [(IntegerLiteralToReal arg, Real)],typ))
	  | (Integer, Real)    => (OperationCall (IntegerLiteralToReal(setTypeToReal src),Real,p,
						 [(arg, Real)],typ))
	  | (Real, Integer)     => (OperationCall (IntegerLiteralToReal src,Real,p,
						  [(setTypeToReal arg, Real)],typ))
	  | (Integer, Integer) => (OperationCall (IntegerLiteralToReal src,Integer,p,
						 [(arg, Integer)],typ))

    end

  | IntegerLiteralToReal (OperationCall (src,srcT',["oclLib","OclAny","="],[(arg, argT')],typ))
    = let 
	val src =  IntegerLiteralToReal src
	val arg =  IntegerLiteralToReal arg
	val srcT = type_of (src )
	val argT = type_of arg
    in
	(case (srcT, argT) of 
	     (Real, Real)      => (OperationCall (src,srcT,["oclLib","OclAny","="],[(arg, argT)],typ))
	  | (Integer, Real)    => (OperationCall ((setTypeToReal src),Real,["oclLib","OclAny","="],
						 [(arg, Real)],typ))
	  | (Real, Integer)    => (OperationCall (src,Real,["oclLib","OclAny","="],
						  [(setTypeToReal arg, Real)],typ))
	  | (Integer, Integer) => (OperationCall (src,srcT,["oclLib","OclAny","="],[(arg, argT)],typ))
	  | (_, _) => (OperationCall (src,srcT,["oclLib","OclAny","="],[(arg, argT)],typ)))
    end
  | IntegerLiteralToReal (OperationCall (src,srcT,p,args,typ)) 
    = (OperationCall (IntegerLiteralToReal src,srcT, p, 
		      map (fn (a, aT) => (IntegerLiteralToReal a, aT)) args ,typ)) 

  | IntegerLiteralToReal (OperationWithType (src,srcT,n,argTyp,typ))
    = (OperationWithType (IntegerLiteralToReal src,srcT,n,argTyp,typ))
  | IntegerLiteralToReal (exp as (Variable (_,_))) 
    = exp 
  | IntegerLiteralToReal (Let (var, varT, rhs, rhsT, term, typ))
    = (Let (var, varT, IntegerLiteralToReal rhs, rhsT, IntegerLiteralToReal  term, typ))
  | IntegerLiteralToReal (Iterate (vars, result, resultT, resultTerm, src,srct,body,bodyT,typ)) 
    = (Iterate (vars, result, resultT,resultTerm, IntegerLiteralToReal  src,srct,IntegerLiteralToReal body,bodyT,typ)) 
  | IntegerLiteralToReal (Iterator (name, vars, src,srct,body,bodyT,typ)) 
    = (Iterator (name, vars,  IntegerLiteralToReal  src,srct,IntegerLiteralToReal body,bodyT,typ)) 
 


fun transform_body_in_post Op =  
    let 
	fun transform_body_in_postconstraint body =
	    let 
		val result_type = type_of body
		val result = Rep_OclTerm.Variable ("result",result_type)
		val equal = ["oclLib","OclAny","="]
		val body_type = result_type
	    in
		Rep_OclTerm.OperationCall (result, result_type,
							equal,[(body,body_type)],
							Rep_OclType.Boolean)
	    end
	val post_representation = map (fn (n,c) => (n,transform_body_in_postconstraint c)) (Rep_Core.body_of_op Op)
	val post = Rep_Core.postcondition_of_op Op
    in
	Rep_Core.update_postcondition (post@post_representation) Op
    end


fun transform_body_in_post_of_c c = 
    let
	val Ops =  map (transform_body_in_post) (Rep_Core.operations_of c)
    in
	Rep_Core.update_operations Ops c
    end


fun transformForHolOcl t = (IntegerLiteralToReal o downcastCollection) t

fun transform_pre f Op = 
    let
	val pre = map (fn (n,i) => (n, f i)) (Rep_Core.precondition_of_op Op)
    in
	Rep_Core.update_precondition pre Op
    end

fun transform_post f Op = 
    let
	val post = map (fn (n,i) => (n, f i)) (Rep_Core.postcondition_of_op Op)
    in
	Rep_Core.update_postcondition post Op
    end

fun transform_pre_of_c f c = 
    let
	val Ops =  map (transform_pre f) (Rep_Core.operations_of c)
    in
	Rep_Core.update_operations Ops c
    end

fun transform_post_of_c f c = 
    let
	val Ops =  map (transform_post f) (Rep_Core.operations_of c)
    in
	Rep_Core.update_operations Ops c
    end




 
fun transform_invariant f c = 
    let
	val invariant = map (fn (n,i) => (n, f i)) (Rep_Core.invariant_of c)
    in
	Rep_Core.update_invariant invariant c
    end

fun transform_invariants f cl = map (transform_invariant f) cl

fun transform_pres  f cl = map (transform_pre_of_c f) cl
fun transform_posts f cl = map (transform_post_of_c f) cl
fun transform_bodys_in_posts cl = map (transform_body_in_post_of_c) cl

fun transform_ocl_spec f = 
    (transform_pres f) o (transform_posts f) o (transform_invariants f) o transform_bodys_in_posts

end


(* 
fun print_depth n =
 (Control.Print.printDepth := n div 2;
  Control.Print.printLength := n);

val cl = ModelImport.import 
	     "/home/brucker/infsec/teaching/theses/semesterarbeiten/2006_mkrucker_ocl/examples/company/company.zargo"  
             "/home/brucker/infsec/teaching/theses/semesterarbeiten/2006_mkrucker_ocl/examples/company/company.ocl" []

*)
