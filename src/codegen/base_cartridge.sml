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

structure Base_Cartridge : BASE_CARTRIDGE =
struct
(* translation functions *)
(* type translation table *)
 
 fun  oclType2Native t        = Rep_OclType.string_of_OclType t
 
 fun visibility2Native XMI.public = "public"
  |  visibility2Native XMI.private = "private"
  |  visibility2Native XMI.protected = "protected"
  |  visibility2Native XMI.package = "package"
  
 fun scope2Native XMI.ClassifierScope = "ClassifierScope"
  |  scope2Native XMI.InstanceScope = "InstanceScope"
  
 
 type environment = {model	     : Rep_SecureUML_ComponentUML.Model,
					 curClassifier: Rep_Core.Classifier option,
					 curOperation : Rep_Core.operation option,
					 curAttribute : Rep_Core.attribute option ,
					 curArgument  : (string * Rep_OclType.OclType) option
									}
		    
(* service functions for other cartridges to have access to the current
 * list items
 *)
fun model (env : environment) = #model env
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
			curArgument  = NONE
		       } : environment


(* FIX: check for NONEs in arguments environment *)
fun lookup (env : environment) "classifier_name"  	= Rep_Core.short_name_of (curClassifier' env)
  | lookup (env : environment) "classifier_package" = (case (#curClassifier env) of 
														  NONE  =>  Rep_OclType.string_of_path (Rep.package_of (hd (#1 (#model env))))
														| SOME c => Rep_OclType.string_of_path (Rep.package_of (curClassifier' env)))
  				  
  | lookup (env : environment) "classifier_parent" 	= Rep_Core.short_parent_name_of (curClassifier' env)
  | lookup (env : environment) "attribute_name" 	= #name (curAttribute' env)
  | lookup (env : environment) "attribute_type" 	= oclType2Native (#attr_type (curAttribute' env))
  | lookup (env : environment) "attribute_visibility"= visibility2Native(#visibility (curAttribute' env))
  | lookup (env : environment) "attribute_scope"	= scope2Native (#scope (curAttribute' env))
  | lookup (env : environment) "operation_name" 	= Rep.name_of_op (curOperation' env)
  | lookup (env : environment) "operation_result_type"= oclType2Native (Rep.result_of_op (curOperation' env))
  | lookup (env : environment) "operation_visibility"= visibility2Native (#visibility (curOperation' env))
  | lookup (env : environment) "operation_scope"	= scope2Native (#scope (curOperation' env))
  | lookup (env : environment) "argument_name" 	= #1 (curArgument' env)
  | lookup (env : environment) "argument_type" 	= oclType2Native (#2 (curArgument' env))
  | lookup _ s = (Gcg_Helper.gcg_warning ("Couldn't lookup \""^s^"\" in base_cartridge.lookup !"); s)


		 
fun evalCondition (env : environment) "isClass" 	  
		= (case (#curClassifier env)  of SOME (Rep.Class{...}) => true 
					  |    _ 	    => false)
  | evalCondition (env : environment) "isInterface"   
  		= (case (#curClassifier env)  of SOME (Rep.Interface{...}) => true 
						 |    _ 	  => false)
  | evalCondition (env : environment) "isEnumeration" 
  		= (case (#curClassifier env)  of SOME (Rep.Enumeration{...}) => true 
						 |    _ 	    => false)
  | evalCondition (env : environment) "isPrimitive"   
  		= (case (#curClassifier env)  of SOME (Rep.Primitive{...}) => true 
						 |    _ 	  => false)
  | evalCondition (env : environment) "hasParent"     
  		= let val parentName = 
  				Rep_OclType.string_of_path (Rep.parent_name_of (curClassifier' env))
  		  in
  		    (parentName <> "OclAny")
  		  end
  | evalCondition (env : environment) "first_classifier" = (curClassifier' env = hd (#1 (#model env)))
  | evalCondition (env : environment) "first_attribute"  = (curAttribute'  env = hd (Rep_Core.attributes_of (curClassifier' env)))
  | evalCondition (env : environment) "first_operation"  = (curOperation'  env = hd (Rep_Core.operations_of (curClassifier' env)))
  | evalCondition (env : environment) "first_argument"   = (curArgument'   env = hd (Rep_Core.arguments_of_op (curOperation' env)))
  | evalCondition (env : environment) "last_classifier"  = (curClassifier' env = List.last (#1 (#model env)))
  | evalCondition (env : environment) "last_attribute"   = (curAttribute' env = List.last (Rep_Core.attributes_of (curClassifier' env)))
  | evalCondition (env : environment) "last_operation"   = (curOperation' env  = List.last (Rep_Core.operations_of (curClassifier' env)))
  | evalCondition (env : environment) "last_argument"    = (curArgument' env  = List.last (Rep_Core.arguments_of_op (curOperation' env)))
  | evalCondition (env : environment) "attribute_isPublic" 	= ((#visibility (curAttribute' env)) = XMI.public)
  | evalCondition (env : environment) "attribute_isPrivate" = ((#visibility (curAttribute' env)) = XMI.private)
  | evalCondition (env : environment) "attribute_isProtected"=((#visibility (curAttribute' env)) = XMI.protected)
  | evalCondition (env : environment) "attribute_isPackage" = ((#visibility (curAttribute' env)) = XMI.package)
  | evalCondition (env : environment) "attribute_isStatic"	= ((#scope (curAttribute' env)) = XMI.ClassifierScope)
  | evalCondition (env : environment) "operation_isPublic" 	= ((#visibility (curOperation' env)) = XMI.public)
  | evalCondition (env : environment) "operation_isPrivate" = ((#visibility (curOperation' env)) = XMI.private)
  | evalCondition (env : environment) "operation_isProtected"=((#visibility (curOperation' env)) = XMI.protected)
  | evalCondition (env : environment) "operation_isPackage" = ((#visibility (curOperation' env)) = XMI.package)
  | evalCondition (env : environment) "operation_isStatic"  = ((#scope (curOperation' env)) = XMI.ClassifierScope)
  | evalCondition (env : environment)  s		
  		= Gcg_Helper.gcg_error ("Couldn't evaluate if-condition: "^s^" in base_cartridge.evalCondition")

		 
(* fun foreach_classifier: environment -> environment list *)
fun foreach_classifier (env : environment) 
			= let val cl = #1 (#model env);
				 fun env_from_classifier c = 
				 	{ model = (#model env),
					  curClassifier = SOME c,
					  curOperation = NONE,
					  curAttribute = NONE,
					  curArgument  = NONE
		       			}
			     in 
			       List.map env_from_classifier cl
			     end
			     
fun foreach_attribute (env : environment) 
			= let val attrs = Rep_Core.attributes_of (curClassifier' env);
				 fun env_from_attr a = 
				 	{ model = #model env,
					  curClassifier = SOME (curClassifier' env),
					  curOperation = NONE,
					  curAttribute = SOME a,
					  curArgument  = NONE
		       			}
			     in 
			       List.map env_from_attr attrs
			     end
			     
fun foreach_operation (env : environment) 
			= let val ops = Rep_Core.operations_of (curClassifier' env);
				 fun env_from_op operation = 
				 	{ model = #model env,
					  curClassifier = SOME (curClassifier' env),
					  curOperation = SOME operation,
					  curAttribute = NONE,
					  curArgument  = NONE
		       			}
			     in 
			       List.map env_from_op ops
			     end
fun foreach_argument (env : environment) 
			= let val args = Rep_Core.arguments_of_op (curOperation' env);
				 fun env_from_argument arg = 
				 	{ model = #model env,
					  curClassifier = SOME (curClassifier' env),
					  curOperation = SOME (curOperation' env),
					  curAttribute = NONE,
					  curArgument  = SOME arg
		       			}
			     in 
			       List.map env_from_argument args
			     end
			     
fun foreach "classifier_list" env = foreach_classifier env
 |  foreach "attribute_list" env  = foreach_attribute env 
 |  foreach "operation_list" env  = foreach_operation env
 |  foreach "argument_list"  env  = foreach_argument env
 (* hier muss man das Environment noch etwas umpacken 
 |  foreach listType env = map (pack env) (<SuperCartridge>.foreach name (unpack env))
  *)
 |  foreach s _ = Gcg_Helper.gcg_error ("Couldn't write foreach "^s^" ." ^
   				"\""^s^"\" not defined in base_cartridge.foreach ")

end
