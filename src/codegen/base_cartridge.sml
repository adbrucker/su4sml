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
 open Rep_OclType
 open Rep
 open Tpl_Parser
 open Rep_SecureUML_ComponentUML.Security
 open ComponentUML
 open Gcg_Helper
  (* type translation table *)
 
 fun  oclType2Native t        = Rep_OclType.string_of_OclType t
 
 fun visibility2Native public = "public"
  |  visibility2Native private = "private"
  |  visibility2Native protected = "protected"
  |  visibility2Native package = "package"
  
 fun scope2Native ClassifierScope = "ClassifierScope"
  |  scope2Native InstanceScope = "InstanceScope"
  
 
 type environment = {model	     : Rep_SecureUML_ComponentUML.Model,
 			curClassifier: Classifier,
 		    	curOperation : operation,
 		    	curAttribute : attribute,
 		    	curArgument  : string * OclType
		    }
		    
(* service functions for other cartridges to have access to the current
 * list items
 *)
fun model (env : environment) = #model env
fun curClassifier (env : environment) = #curClassifier env
fun curAttribute (env : environment) = #curAttribute env
fun curOperation (env : environment) = #curOperation env
fun curArgument (env : environment)  = #curArgument env
	

fun initEnv model = { model = model,
			curClassifier = emptyClassifier,
			curOperation = emptyOperation,
			curAttribute = emptyAttribute,
			curArgument  = emptyArgument
		       } : environment



fun lookup (env : environment) "classifier_name"  	= short_name_of (#curClassifier env)
  | lookup (env : environment) "classifier_package" = if ((#curClassifier env) = emptyClassifier) then (* not in foreach-loop yet *)
  				        Rep_OclType.string_of_path (package_of (hd (#1 (#model env))))
  				  else 
  				  	Rep_OclType.string_of_path (package_of (#curClassifier env))
  				  
  | lookup (env : environment) "classifier_parent" 	= short_parent_name_of (#curClassifier env)
  | lookup (env : environment) "attribute_name" 	= #name (#curAttribute env)
  | lookup (env : environment) "attribute_type" 	= oclType2Native (#attr_type (#curAttribute env))
  | lookup (env : environment) "attribute_visibility"= visibility2Native(#visibility (#curAttribute env))
  | lookup (env : environment) "attribute_scope"	= scope2Native (#scope (#curAttribute env))
  | lookup (env : environment) "operation_name" 	= name_of_op (#curOperation env)
  | lookup (env : environment) "operation_result_type"= oclType2Native (result_of_op (#curOperation env))
  | lookup (env : environment) "operation_visibility"= visibility2Native (#visibility (#curOperation env))
  | lookup (env : environment) "operation_scope"	= scope2Native (#scope (#curOperation env))
  | lookup (env : environment) "argument_name" 	= #1 (#curArgument env)
  | lookup (env : environment) "argument_type" 	= oclType2Native (#2 (#curArgument env))
  | lookup _ s = (gcg_warning ("Couldn't lookup \""^s^"\" in base_cartridge.lookup !"); s)


		 
fun evalCondition (env : environment) "isClass" 	  
		= (case (#curClassifier env)  of (Class{...}) => true 
					  |    _ 	    => false)
  | evalCondition (env : environment) "isInterface"   
  		= (case (#curClassifier env)  of (Interface{...}) => true 
						 |    _ 	  => false)
  | evalCondition (env : environment) "isEnumeration" 
  		= (case (#curClassifier env)  of (Enumeration{...}) => true 
						 |    _ 	    => false)
  | evalCondition (env : environment) "isPrimitive"   
  		= (case (#curClassifier env)  of (Primitive{...}) => true 
						 |    _ 	  => false)
  | evalCondition (env : environment) "hasParent"     
  		= let val parentName = 
  				Rep_OclType.string_of_path (parent_name_of (#curClassifier env))
  		  in
  		    (parentName <> "OclAny")
  		  end
  | evalCondition (env : environment) "first_classifier" = (#curClassifier env = hd (#1 (#model env)))
  | evalCondition (env : environment) "first_attribute"  = (#curAttribute  env = hd (attributes_of (#curClassifier env)))
  | evalCondition (env : environment) "first_operation"  = (#curOperation  env = hd (operations_of (#curClassifier env)))
  | evalCondition (env : environment) "first_argument"   = (#curArgument   env = hd (arguments_of_op (#curOperation env)))
  | evalCondition (env : environment) "last_classifier"  = (#curClassifier env = List.last (#1 (#model env)))
  | evalCondition (env : environment) "last_attribute"   = (#curAttribute env = List.last (attributes_of (#curClassifier env)))
  | evalCondition (env : environment) "last_operation"   = (#curOperation env  = List.last (operations_of (#curClassifier env)))
  | evalCondition (env : environment) "last_argument"    = (#curArgument env  = List.last (arguments_of_op (#curOperation env)))
  | evalCondition (env : environment) "attribute_isPublic" 	= ((#visibility (#curAttribute env)) = public)
  | evalCondition (env : environment) "attribute_isPrivate" = ((#visibility (#curAttribute env)) = private)
  | evalCondition (env : environment) "attribute_isProtected"=((#visibility (#curAttribute env)) = protected)
  | evalCondition (env : environment) "attribute_isPackage" = ((#visibility (#curAttribute env)) = package)
  | evalCondition (env : environment) "attribute_isStatic"	= ((#scope (#curAttribute env)) = ClassifierScope)
  | evalCondition (env : environment) "operation_isPublic" 	= ((#visibility (#curOperation env)) = public)
  | evalCondition (env : environment) "operation_isPrivate" = ((#visibility (#curOperation env)) = private)
  | evalCondition (env : environment) "operation_isProtected"=((#visibility (#curOperation env)) = protected)
  | evalCondition (env : environment) "operation_isPackage" = ((#visibility (#curOperation env)) = package)
  | evalCondition (env : environment) "operation_isStatic"  = ((#scope (#curOperation env)) = ClassifierScope)
  | evalCondition (env : environment)  s		
  		= gcg_error ("Couldn't evaluate if-condition: "^s^" in base_cartridge.evalCondition")

		 
(* fun foreach_classifier: environment -> environment list *)
fun foreach_classifier (env : environment) 
			= let val cl = #1 (#model env);
				 fun env_from_classifier c = 
				 	{ model = (#model env),
					  curClassifier = c,
					  curOperation = emptyOperation,
					  curAttribute = emptyAttribute,
					  curArgument  = emptyArgument
		       			}
			     in 
			       List.map env_from_classifier cl
			     end
			     
fun foreach_attribute (env : environment) 
			= let val attrs = attributes_of (#curClassifier env);
				 fun env_from_attr a = 
				 	{ model = #model env,
					  curClassifier = (#curClassifier env),
					  curOperation = emptyOperation,
					  curAttribute = a,
					  curArgument  = emptyArgument
		       			}
			     in 
			       List.map env_from_attr attrs
			     end
			     
fun foreach_operation (env : environment) 
			= let val ops = operations_of (#curClassifier env);
				 fun env_from_op operation = 
				 	{ model = #model env,
					  curClassifier = (#curClassifier env),
					  curOperation = operation,
					  curAttribute = emptyAttribute,
					  curArgument  = emptyArgument
		       			}
			     in 
			       List.map env_from_op ops
			     end
fun foreach_argument (env : environment) 
			= let val args = arguments_of_op (#curOperation env);
				 fun env_from_argument arg = 
				 	{ model = #model env,
					  curClassifier = (#curClassifier env),
					  curOperation = (#curOperation env),
					  curAttribute = emptyAttribute,
					  curArgument  = arg
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
 |  foreach s _ = gcg_error ("Couldn't write foreach "^s^" ." ^
   				"\""^s^"\" not defined in base_cartridge.foreach ")

end
