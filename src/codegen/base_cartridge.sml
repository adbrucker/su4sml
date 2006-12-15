(*****************************************************************************
 *          su4sml - a SecureUML repository for SML              
 *                                                                            
 * simple.sml - a simple test file for the core repository
 * Copyright (C) 2005 Raphael Eidenbenz <eraphael@student.ethz.ch>
 *                                                                            
 * This file is part of su4sml.                                              
 *                                                                            
 * su4sml is free software; you can redistribute it and/or modify it under   
 * the terms of the GNU General Public License as published by the Free       
 * Software Foundation; either version 2 of the License, or (at your option)  
 * any later version.                                                         
 *                                                                            
 * su4sml is distributed in the hope that it will be useful, but WITHOUT ANY 
 * WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS 
 * FOR A PARTICULAR PURPOSE. See the GNU General Public License for more 
 * details.                                                              
 *                                                                            
 * You should have received a copy of the GNU General Public License along    
 * with this program; if not, write to the Free Software Foundation, Inc.,    
 * 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.                  
 ******************************************************************************)

(** 
 * This cartridge knows about the basic elements of UML class diagrams.
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
								 
(** returns the current operation *)
val curOperation: environment -> Rep.operation option

(** returns the current operation parameter *)
val curArgument  : environment -> (string * Rep_OclType.OclType) option
								  
end


structure Base_Cartridge : BASE_CARTRIDGE =
struct
(* translation functions *)
(* type translation table *)
 
fun  oclType2Native t        = Rep_OclType.string_of_OclType t
							   
fun visibility2Native XMI.public     = "public"
  |  visibility2Native XMI.private   = "private"
  |  visibility2Native XMI.protected = "protected"
  |  visibility2Native XMI.package   = "package"
									 
fun scope2Native XMI.ClassifierScope = "ClassifierScope"
  |  scope2Native XMI.InstanceScope  = "InstanceScope"
									  
									 
type Model = Rep.Classifier list
 
type environment = {model	     : Model,
					curClassifier: Rep_Core.Classifier option,
					curOperation : Rep_Core.operation option,
					curAttribute : Rep_Core.attribute option ,
					curArgument  : (string * Rep_OclType.OclType) option
								   }
				   
(* service functions for other cartridges to have access to the current
 * list items
 *)
fun getModel (env : environment) = #model env
fun curClassifier (env : environment) = (#curClassifier env)
fun curAttribute (env : environment) =  (#curAttribute env)
fun curOperation (env : environment) =  (#curOperation env)
fun curArgument (env : environment)  =  (#curArgument env)

fun curClassifier' (env : environment) = Option.valOf((#curClassifier env))
fun curAttribute' (env : environment) =  Option.valOf((#curAttribute env))
fun curOperation' (env : environment) =  Option.valOf((#curOperation env))
fun curArgument' (env : environment)  =  Option.valOf((#curArgument env))

fun initEnv model = { model = model,
					  curClassifier = NONE,
					  curOperation = NONE,
					  curAttribute = NONE,
					  curArgument  = NONE } : environment


(* FIX: check for NONEs in arguments environment *)
(** 
 * lookup base cartridge specific string-valued variables 
 * The base cartridge knows the following variables:
 * classifier_name, classifier_package, classifier_parent,
 * attribute_name, attribute_type, attribute_visibility,
 * attribute_scope, operation_name, operation_result_type,
 * operation_visibility, operation_scope, argument_name, argument_type  
 *)
fun lookup env "classifier_name"  	  = Rep_Core.short_name_of (curClassifier' env)
  | lookup env "classifier_package"   = (case (#curClassifier env) of 
                                               NONE  =>  Rep_OclType.string_of_path 
                                                             (Rep.package_of 
                                                                  (hd (#model env)))
                                             | SOME c => Rep_OclType.string_of_path 
                                                             (Rep.package_of 
                                                                  (curClassifier' env)))
  | lookup env "classifier_parent"     = Rep_Core.short_parent_name_of 
                                             (curClassifier' env)
  | lookup env "attribute_name"        = #name (curAttribute' env)
  | lookup env "attribute_type" 	   = oclType2Native (#attr_type 
                                                             (curAttribute' env))
  | lookup env "attribute_visibility"  = visibility2Native(#visibility 
                                                               (curAttribute' env))
  | lookup env "attribute_scope" 	   = scope2Native (#scope (curAttribute' env))
  | lookup env "operation_name" 	   = Rep.name_of_op (curOperation' env)
  | lookup env "operation_result_type" = oclType2Native (Rep.result_of_op 
                                                             (curOperation' env))
  | lookup env "operation_visibility"  = visibility2Native (#visibility 
                                                                (curOperation' env))
  | lookup env "operation_scope"	   = scope2Native (#scope (curOperation' env))
  | lookup env "argument_name" 	       = #1 (curArgument' env)
  | lookup env "argument_type" 		   = oclType2Native (#2 (curArgument' env))
  | lookup _ s = (Gcg_Helper.gcg_warning ("Couldn't lookup \""^s^
										  "\" in Base_Cartridge.lookup !"); s)


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
  | test env "isInterface"   = (case (#curClassifier env)  of 
									SOME (Rep.Interface{...}) => true 
								  | _                         => false)
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
  | test env "first_classifier" = (curClassifier' env = hd  (#model env))
  | test env "first_attribute" = (curAttribute'  env 
                                  = hd (Rep_Core.attributes_of (curClassifier' env)))
  | test env "first_operation" = (curOperation'  env 
                                  = hd (Rep_Core.operations_of (curClassifier' env)))
  | test env "first_argument"  = (curArgument'   env 
                                  = hd (Rep_Core.arguments_of_op (curOperation' env)))
  | test env "last_classifier" = (curClassifier' env = List.last (#model env))
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
  | test env  s = Gcg_Helper.gcg_error ("Couldn't evaluate predicate: "^
                                        s^" in base_cartridge.test")

		 
(* fun foreach_classifier: environment -> environment list *)
fun foreach_classifier (env : environment) 
  = let val cl = (#model env)
		fun env_from_classifier c = { model = (#model env),
									  curClassifier = SOME c,
									  curOperation = NONE,
									  curAttribute = NONE,
									  curArgument  = NONE }
	in 
		List.map env_from_classifier cl
	end
				  
fun foreach_attribute (env : environment) 
  = let val attrs = Rep_Core.attributes_of (curClassifier' env)
		fun env_from_attr a = { model = #model env,
								curClassifier = SOME (curClassifier' env),
								curOperation = NONE,
								curAttribute = SOME a,
								curArgument  = NONE	}
	in 
		List.map env_from_attr attrs
	end
			     
fun foreach_operation (env : environment) 
  = let val ops = Rep_Core.operations_of (curClassifier' env)
		fun env_from_op operation = { model = #model env,
									  curClassifier = SOME (curClassifier' env),
									  curOperation = SOME operation,
									  curAttribute = NONE,
									  curArgument  = NONE }
	in 
		List.map env_from_op ops
	end
		
fun foreach_argument (env : environment) 
  = let val args = Rep_Core.arguments_of_op (curOperation' env)
		fun env_from_argument arg = { model = #model env,
									  curClassifier = SOME (curClassifier' env),
									  curOperation = SOME (curOperation' env),
									  curAttribute = NONE,
									  curArgument  = SOME arg }
	in 
		List.map env_from_argument args
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
  (* hier muss man das Environment noch etwas umpacken 
  | foreach listType env = map (pack env) 
							   (<SuperCartridge>.foreach name (unpack env))
   *)
  | foreach s _ = Gcg_Helper.gcg_error ("Couldn't write foreach "^s^" ."^"\""^s^
										"\" not defined in base_cartridge.foreach ")
				  
end
