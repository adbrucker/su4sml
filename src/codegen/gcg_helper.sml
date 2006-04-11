(*****************************************************************************
 *          su4sml GCG - Generic Code Generator   
 *                                                                            
 * gcg_helper.sml - helper library for su4sml-gcg
 * Copyright (C) 2005 Raphael Eidenbenz <eraphael@student.ethz.ch>
 *                                                                            
 * This file is part of su4sml-gcg.                                              
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


 structure Gcg_Helper (* :
	 sig 
		 val emptyAction : ComponentUML.Action
		 val emptyArgument : string * OclType
		 val emptyAttribute : attribute
		 val emptyClassifier : Classifier
		 val emptyConstraint : Rep_OclTerm.OclTerm
		 val emptyOperation : operation
		 val emptyPermission : Permission
		 val emptyResource : Resource
		 val emptyRole : string
		 val emptyModel :  Rep_SecureUML_ComponentUML.Model
		 val action_to_string : ComponentUML.Action -> string
		 val isInPermission : ComponentUML.Action -> Permission -> bool
	 end*) =
 struct

(* open Rep 
 open Rep_OclType
 open Rep_OclTerm
 open Rep_SecureUML_ComponentUML.Security
 open ComponentUML 
 open XMI_DataTypes
*)
exception GCG_Error

fun gcg_error s = (print ("Error:"^s^"\n"); raise GCG_Error);

fun gcg_warning s = (print ("Warning: "^s^"\n"));

fun fieldSplit s d = String.fields (fn c => (c = d)) s

local
	fun endsWithEscape "" = false
	  | endsWithEscape s = (substring(s,size(s)-1,1) = "\\")
in
 fun joinEscapeSplitted d [] 	   = []
   | joinEscapeSplitted d [l]	   = [l]
   | joinEscapeSplitted d (h::s::t) = if endsWithEscape(h)
  				     then (substring(h,0,size(h)-1)^d^s)::t
  				     else h::(joinEscapeSplitted d (s::t))
end 

val curry = fn f => fn x => fn y => f (x, y)
val uncurry = fn f => fn (x, y) => f x y
 
 
(*
val emptyClassifier = (Primitive({ name=["",""],
	   			parent=NONE,
	   			operations=[],
	   			associationends=[],
	   			invariant=[],
	   			stereotypes=[],
	   			interfaces=[],
	   			thyname=NONE
	  			}));

val emptyOperation = ({name="",
	       		     precondition=[],
	       		     postcondition=[],
	       		     arguments=[],
			     result=DummyT,
			     isQuery=false,
			     visibility=private,
	                     scope=ClassifierScope 
	                    }: operation);

val emptyAttribute = ({name="",
     			attr_type=DummyT,
     			visibility=private,
     			scope=ClassifierScope,
     			init=NONE
			} : attribute);
			
val emptyArgument = ("",DummyT)

val emptyPermission = ({actions = [], 
			 constraints = [],
    			 name="", 
    			 roles= []
    			 } : Permission)
val emptyRole = ""
val emptyConstraint = (Literal("",DummyT))
val emptyResource = (ComponentUML.EntityMethod emptyOperation : Resource)
val emptyAction = SimpleAction("", emptyResource)

val emptyModel = (nil, {config_type = "",
	       		permissions = nil,
	       		subjects = nil,
	       		roles = nil,
	       		sa = nil}):Rep_SecureUML_ComponentUML.Model
*)

fun isSuffix  [] _ = true
 |  isSuffix  _ [] = false
 |  isSuffix (h1::t1) (h2::t2) = (h1=h2) andalso (isSuffix t1 t2)

(* fun resPath_of a = #2 (resource_of a) *)

 
(*
fun actionTypes_compatible _ "full_access"   = true
 |  actionTypes_compatible "read" "read"     = true
 |  actionTypes_compatible "update" "update" = true
 |  actionTypes_compatible _ _ = false
 *)
(* checks if a1 is part of a2 
fun is_contained_in a1 (a2 as (SimpleAction _)) = (a1 = a2)
 |  is_contained_in a1  a2 = let
 				val p1 = resPath_of a1
 				val p2 = resPath_of a2
 				val at1 = actionType_of a1
 				val at2 = actionType_of a2
 			     in
 			        (isSuffix p2 p1) andalso (actionTypes_compatible at1 at2)
 			     end *)

fun is_contained_in a1 a2 = (a1 = a2) orelse 
							List.exists (fn x=> x=true) ((List.map (is_contained_in a1) (ComponentUML.subordinated_actions a2))) 

fun isInPermission a (p:Rep_SecureUML_ComponentUML.Security.Permission) = List.exists (is_contained_in a) (#actions p)


(* fun resource_to_string (s,p) = "("^s^", "^(string_of_path p)^")"
fun action_to_string (SimpleAction (s,r)) = "SimpleAction("^s^", "^(resource_to_string r)^"))"
 |  action_to_string (CompositeAction (s,r)) = "CompositeAction("^s^", "^(resource_to_string r)^"))"
*)


fun assureDir file = let val dirList = rev (tl (rev (String.tokens (fn c => c = #"/") file)))

			 fun assert1 "" d = ((if (OS.FileSys.isDir d) then () else ())
			 			 	handle SysErr => (OS.FileSys.mkDir d) )
			 			   | assert1 prefix d = (if (OS.FileSys.isDir (prefix^"/"^d)) then () else ())
			 			 	handle SysErr => (OS.FileSys.mkDir (prefix^"/"^d))
			 

			 fun assertDList _ [] = ()
			   | assertDList prefix [d] = assert1 prefix d
			   | assertDList "" (h::t) = (assert1 "" h ; assertDList h t)
			   | assertDList prefix (h::t) = (assert1 prefix h; 
			   				  assertDList (prefix^"/"^h) t ) 
		     in
		     	assertDList "" dirList
		     end


end
