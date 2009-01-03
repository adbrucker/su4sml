(*****************************************************************************
 * su4sml --- a SML repository for managing (Secure)UML/OCL models
 *             http://projects.brucker.ch/su4sml/
 *                                                                            
 * base_cartridge.sml --- 
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
 * This cartridge knows about the basic elements of UML class models.
 * The elements are classifiers, attributes, and operations with their
 * parameters in terms of the Rep interface
 *) 
signature BASE_CARTRIDGE =
sig
    include CARTRIDGE
    
(** returns the current classifier. *)
val curClassifier: environment -> Rep.Classifier option
				  
(** returns the current attribute *)
val curAttribute: environment -> Rep.attribute option

(** returns the current association end *)
val curAssociationEnd : environment -> Rep.associationend option 
     
(** returns the current operation *)
val curOperation: environment -> Rep.operation option

(** returns the current operation parameter *)
val curArgument  : environment -> (string * Rep_OclType.OclType) option

				  
end


structure Base_Cartridge : BASE_CARTRIDGE =
struct

(* translation functions *)
(* type translation table *)


fun oclType2Native t  = Rep_OclType.string_of_OclType t
			       
fun visibility2Native XMI.public    = "public"
  | visibility2Native XMI.private   = "private"
  | visibility2Native XMI.protected = "protected"
  | visibility2Native XMI.package   = "package"
				       
fun scope2Native XMI.ClassifierScope = "ClassifierScope"
  | scope2Native XMI.InstanceScope   = "InstanceScope"
				       
				       
(*type Model = Rep.Classifier list*)
type Model = Rep_Core.transform_model
             
type environment = { model	  : Model,
                     counter      : int ref,
		     curClassifier: Rep_Core.Classifier option,
		     curInvariant : (string option * Rep_OclTerm.OclTerm) option, 
                     curAssocEnd  : Rep_Core.associationend option,
		     curOperation : Rep_Core.operation option,
		     curPrecondition : (string option * Rep_OclTerm.OclTerm) option, 
		     curPostcondition: (string option * Rep_OclTerm.OclTerm) option, 
		     curAttribute : Rep_Core.attribute option ,
		     curArgument  : (string * Rep_OclType.OclType) option
		   }
		   
(* service functions for other cartridges to have access to the current
 * list items
 *)
fun getModel      (env : environment) = #model env
fun getClassifiers  (env: environment) = #1 (getModel env)
fun getAssociations (env: environment) = #2 (getModel env)
fun curClassifier (env : environment) = (#curClassifier env)
fun curAttribute  (env : environment) = (#curAttribute env)
fun curAssociationEnd  (env : environment) = (#curAssocEnd env)
fun curOperation  (env : environment) = (#curOperation env)
fun curArgument   (env : environment) = (#curArgument env)

fun curClassifier' (env : environment) = Option.valOf((#curClassifier env))
fun curAttribute'  (env : environment) = Option.valOf((#curAttribute env))
fun curInvariant'  (env : environment) = Option.valOf((#curInvariant env))
fun curPrecondition'  (env : environment) = Option.valOf((#curPrecondition env))
fun curPostcondition'  (env : environment) = Option.valOf((#curPostcondition env))
fun curAssociationEnd'  (env : environment) = Option.valOf((#curAssocEnd env))
fun curOperation'  (env : environment) = Option.valOf((#curOperation env))
fun curArgument'   (env : environment)  = Option.valOf((#curArgument env))

fun initEnv model = { model         = model,
                      counter       = ref 0,
		      curClassifier = NONE,
		      curInvariant  = NONE,
                      curAssocEnd   = NONE,
		      curOperation  = NONE,
		      curPrecondition  = NONE,
		      curPostcondition = NONE,
		      curAttribute  = NONE,
		      curArgument   = NONE } : environment

fun curClassifierPackageToString env p2sfun = (case (#curClassifier env) of 
                                                   NONE  =>  p2sfun
                                                                 (Rep.package_of 
                                                                      (hd (getClassifiers env)))
                                                 | SOME c => p2sfun
                                                                 (Rep.package_of 
                                                                      (curClassifier' env)))

(* FIX: check for NONEs in arguments environment *)
(** 
 * lookup base cartridge specific string-valued variables 
 * The base cartridge knows the following variables:
 * classifier_name, classifier_package, classifier_parent,
 * attribute_name, attribute_type, attribute_visibility,
 * attribute_scope, operation_name, operation_result_type,
 * operation_visibility, operation_scope, argument_name, argument_type  
 *)
fun lookup env "classifier_name"  	 = Rep_Core.short_name_of (curClassifier' env)
  | lookup env "classifier_package"      = curClassifierPackageToString env Rep_OclType.string_of_path 
  | lookup env "classifier_package_path" = curClassifierPackageToString env Rep_OclType.pathstring_of_path 
  | lookup env "classifier_parent"       = Rep_Core.parent_short_name_of (curClassifier' env)
  | lookup env "attribute_name"          = #name (curAttribute' env)
  | lookup env "attribute_type" 	 = oclType2Native (#attr_type (curAttribute' env))
  
  | lookup env "inv_name"                = (case #1 (curInvariant' env) of 
  						NONE => "" 
						| SOME n => n )
  | lookup env "inv_cs" 	         = (Ocl2String.ocl2string false) (#2 (curInvariant' env))
  | lookup env "post_name"               = (case #1 (curPostcondition' env) of 
  						NONE => "" 
						| SOME n => n )
  | lookup env "post_cs" 	         = (Ocl2String.ocl2string false) (#2 (curPostcondition' env))
  
  | lookup env "pre_name"                = (case #1 (curPrecondition' env) of 
  						NONE => "" 
						| SOME n => n )
  | lookup env "pre_cs" 	         = (Ocl2String.ocl2string false) (#2 (curPrecondition' env))

  | lookup env "attribute_visibility"    = visibility2Native(#visibility 
                                                               (curAttribute' env))
  | lookup env "attribute_scope" 	 = scope2Native (#scope (curAttribute' env))
  | lookup env "assocend_name"           = (Rep_OclType.string_of_path o #name o valOf o #curAssocEnd) env
  | lookup env "assocend_type"           = (oclType2Native o #aend_type o valOf o #curAssocEnd) env
  | lookup env "operation_name" 	 = Rep.name_of_op (curOperation' env)
  | lookup env "operation_result_type"   = oclType2Native (Rep.result_of_op 
                                                               (curOperation' env))
  | lookup env "operation_visibility"    = visibility2Native (#visibility 
                                                                  (curOperation' env))
  | lookup env "operation_scope"	 = scope2Native (#scope (curOperation' env))
  | lookup env "argument_name" 	         = #1 (curArgument' env)
  | lookup env "argument_type" 		 = oclType2Native (#2 (curArgument' env))
  | lookup env "counter"                 = Int.toString (!(#counter env))
  | lookup env "counter_next"            = ((#counter env) := !(#counter env)+1; 
                                            Int.toString (!(#counter env)))
  | lookup _ s = (Logger.warn ("in Base_Cartridge.lookup: unknown variable \""^s^"\"."); "$"^s^"$")


(** 
 * evaluate base cartridge specific predicates.
 * The base cartridge supports the following predicates:
 * isClass, isInterface, isEnumeration, isPrimitive, hasParent,
 * first_classifier, first_attribute, first_operation, first_argument,
 * last_classifier, last_attribute, last_operation, last_argument,
 * attribute_isPublic, attribute_isProtected, attribute_isPrivate,
 * attribute_isPackage, attribute_isStatic, operation_isPublic,
 * operation_isPrivate, operation_isProtected, operation_isPackage,
 * operation_isStatic, 
 *)		 
fun test env "isClass"       = (case (#curClassifier env)  of 
				    SOME (Rep.Class{...}) => true 
				  | _                     => false)
  | test env "notClass"      = not (test env "isClass")
  | test env "isInterface"   = (case (#curClassifier env)  of 
				    SOME (Rep.Interface{...}) => true 
				  | _                         => false)
  | test env "notInterface"      = not (test env "isInterface")
  | test env "isEnumeration" = (case (#curClassifier env)  of 
				    SOME (Rep.Enumeration{...}) => true 
				  | _ 	                        => false)
  | test env "isPrimitive"   = (case (#curClassifier env)  of 
                                    SOME (Rep.Primitive{...}) => true 
                                  | _ 	                      => false)
  | test env "hasParent" = let val parentName = 
                                   Rep_OclType.string_of_path 
                                       (Rep.parent_name_of (curClassifier' env))
                           in
                               (parentName <> "oclLib.OclAny")
                           end
  | test env "hasOperations" = (length (Rep_Core.operations_of (curClassifier' env))) > 0
  | test env "hasInvariants" = (length (Rep_Core.invariant_of (curClassifier' env))) > 0
  | test env "hasOpSpec"     = ((length (Rep_Core.precondition_of_op (curOperation' env))) 
                               +(length (Rep_Core.postcondition_of_op (curOperation' env)))) > 0
  | test env "hasAttributes" = (length (Rep_Core.attributes_of (curClassifier' env))) > 0
  | test env "first_classifier" = (curClassifier' env = hd  (getClassifiers env))
  | test env "first_attribute" = (curAttribute'  env 
                                  = hd (Rep_Core.attributes_of (curClassifier' env)))
  | test env "first_operation" = (curOperation'  env 
                                  = hd (Rep_Core.operations_of (curClassifier' env)))
  | test env "first_argument"  = (curArgument'   env 
                                  = hd (Rep_Core.arguments_of_op (curOperation' env)))
  | test env "last_classifier" = (curClassifier' env = List.last (getClassifiers env))
  | test env "last_attribute"  = (curAttribute' env = 
                                  List.last (Rep_Core.attributes_of 
                                                 (curClassifier' env)))
  | test env "last_operation" = (curOperation' env  = 
                                 List.last (Rep_Core.operations_of 
                                                (curClassifier' env)))
  | test env "last_argument" = (curArgument' env  
                                = List.last (Rep_Core.arguments_of_op 
                                                 (curOperation' env)))
  | test env "attribute_isPublic"    = ((#visibility (curAttribute' env)) = XMI.public)
  | test env "attribute_isPrivate"   = ((#visibility (curAttribute' env)) = XMI.private)
  | test env "attribute_isProtected" = ((#visibility (curAttribute' env)) = XMI.protected)
  | test env "attribute_isPackage"   = ((#visibility (curAttribute' env)) = XMI.package)
  | test env "attribute_isStatic"    = ((#scope (curAttribute' env)) = XMI.ClassifierScope)
  | test env "operation_isPublic"    = ((#visibility (curOperation' env)) = XMI.public)
  | test env "operation_isPrivate"   = ((#visibility (curOperation' env)) = XMI.private)
  | test env "operation_isProtected" = ((#visibility (curOperation' env)) = XMI.protected)
  | test env "operation_isPackage"   = ((#visibility (curOperation' env)) = XMI.package)
  | test env "operation_isStatic"    = ((#scope (curOperation' env)) = XMI.ClassifierScope)
  | test env "operation_isQuery"     = #isQuery (curOperation' env)
  | test env  s = Logger.error ("in Base_Cartridge.test: undefined predicate: \""^s^"\".")

		  
(* fun foreach_classifier: environment -> environment list *)
fun foreach_classifier (env : environment) 
  = let val cl = (getClassifiers env)
	fun env_from_classifier c = { model        = #model env,
                                      counter      = #counter env,
				      curClassifier= SOME c,
		                      curInvariant  = NONE,
                                      curAssocEnd  = NONE,
				      curOperation = NONE,
		                      curPrecondition  = NONE,
		                      curPostcondition = NONE,
				      curAttribute = NONE,
				      curArgument  = NONE }
    in 
	List.map env_from_classifier cl
    end
    

(* Only iterate over non-primitive classifiers such as Class, Interface, Enum *)
fun foreach_nonprimitive_classifier (env : environment) 
  = let val cl = List.filter (fn cenv => (case cenv of
					      Rep.Primitive{...} => false
					    | _                  => true)) (getClassifiers env)
	fun env_from_classifier c = { model = (#model env),
                                      counter = #counter env,
				      curClassifier = SOME c,
		                      curInvariant  = NONE,
                                      curAssocEnd  = NONE,
				      curOperation = NONE,
		                      curPrecondition  = NONE,
		                      curPostcondition = NONE,
				      curAttribute = NONE,
				      curArgument  = NONE }
    in 
	List.map env_from_classifier cl
    end
				  
fun foreach_invariant (env : environment) 
  = let val invs = Rep_Core.invariant_of (curClassifier' env)
	fun env_from_inv inv = { model = (#model env),
                                      counter = #counter env,
				      curClassifier = SOME (curClassifier' env),
		                      curInvariant  = SOME inv,
                                      curAssocEnd  = NONE,
				      curOperation = NONE,
		                      curPrecondition  = NONE,
		                      curPostcondition = NONE,
				      curAttribute = NONE,
				      curArgument  = NONE }
    in 
	List.map env_from_inv invs
    end
				  

fun foreach_precondition (env : environment) 
  = let val pres = Rep_Core.precondition_of_op (curOperation' env)
	fun env_from_pre pre = { model = (#model env),
                                      counter = #counter env,
				      curClassifier = SOME (curClassifier' env),
		                      curInvariant  = NONE,
                                      curAssocEnd  = NONE,
				      curOperation = NONE,
		                      curPrecondition  = SOME pre,
		                      curPostcondition = NONE,
				      curAttribute = NONE,
				      curArgument  = NONE }
    in 
	List.map env_from_pre pres
    end
				  

fun foreach_postcondition (env : environment) 
  = let val posts = Rep_Core.postcondition_of_op (curOperation' env)
	fun env_from_post post = { model = (#model env),
                                      counter = #counter env,
				      curClassifier = SOME (curClassifier' env),
		                      curInvariant  = NONE,
                                      curAssocEnd  = NONE,
				      curOperation = NONE,
		                      curPrecondition  = NONE,
		                      curPostcondition = SOME post,
				      curAttribute = NONE,
				      curArgument  = NONE }
    in 
	List.map env_from_post posts
    end
				  



fun foreach_attribute (env : environment) 
  = let val attrs = Rep_Core.attributes_of (curClassifier' env)
	fun env_from_attr a = { model = #model env,
                                counter      = #counter env,
				curClassifier = SOME (curClassifier' env),
		                curInvariant  = NONE,
                                curAssocEnd  = NONE,
				curOperation = NONE,
		                curPrecondition  = NONE,
		                curPostcondition = NONE,
				curAttribute = SOME a,
				curArgument  = NONE	}
    in 
	List.map env_from_attr attrs
    end
    
fun foreach_operation (env : environment) 
  = let val ops = Rep_Core.operations_of (curClassifier' env)
	fun env_from_op operation = { model = #model env,
                                      counter      = #counter env,
				      curClassifier = SOME (curClassifier' env),
		                      curInvariant  = NONE,
				      curOperation = SOME operation,
		      curPrecondition  = NONE,
		      curPostcondition = NONE,
                                      curAssocEnd  = NONE,
				      curAttribute = NONE,
				      curArgument  = NONE }
    in 
	List.map env_from_op ops
    end
    
fun foreach_argument (env : environment) 
  = let val args = Rep_Core.arguments_of_op (curOperation' env)
	fun env_from_argument arg = { model = #model env,
                                      counter      = #counter env,
				      curClassifier = SOME (curClassifier' env),
				      curInvariant  = NONE,
				      curOperation = SOME (curOperation' env),
		      curPrecondition  = NONE,
		      curPostcondition = NONE,
                                      curAssocEnd  = NONE,
				      curAttribute = NONE,
				      curArgument  = SOME arg }
    in 
	List.map env_from_argument args
    end

fun foreach_assocend (env : environment) 
  = let val associations = getAssociations env
	val aends = Rep_Core.associationends_of associations (curClassifier' env)
	fun env_from_argument arg = { model = #model env,
                                      counter      = #counter env,
				      curClassifier = SOME (curClassifier' env),
				      curInvariant  = NONE,
                                      curAssocEnd   = SOME arg,
				      curOperation = NONE,
		                      curPrecondition  = NONE,
		                      curPostcondition = NONE,
				      curAttribute = NONE,
				      curArgument  = NONE  }
    in 
	List.map env_from_argument aends
    end

(** 
 * compute the base cartridge specific lists.
 * The base cartridge supports the following lists:
 * classifier_list iterates over all classifiers of the model,
 * attribute_list iterates over all attributes of the current
 * classifier, operation_list iterates over all operations of the
 * current classifier, argument_list iterates over all arguments of
 * the current operation
 *)
fun foreach "classifier_list" env = foreach_classifier env
  | foreach "attribute_list" env  = foreach_attribute env 
  | foreach "operation_list" env  = foreach_operation env
  | foreach "argument_list"  env  = foreach_argument env
  | foreach "invariant_list"  env  = foreach_invariant env
  | foreach "precondition_list"  env  = foreach_precondition env
  | foreach "postcondition_list"  env  = foreach_postcondition env
  | foreach "nonprimitive_classifier_list" env = foreach_nonprimitive_classifier env
  | foreach "assocend_list"  env  = foreach_assocend env
  (* hier muss man das Environment noch etwas umpacken 
  | foreach listType env = map (pack env) 
			       (<SuperCartridge>.foreach name (unpack env))
   *)
  | foreach s _ = (Logger.error ("in Base_Cartridge.foreach: unknown list \""^s^"\".");
                   [])
		  
end
