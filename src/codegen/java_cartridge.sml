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
 
 structure Java_Cartridge : CARTRIDGE =
 struct
 open Rep_OclType
 (* type translation table *)
 fun oclType2Native Integer  = "int"
   | oclType2Native Real     = "float"
   | oclType2Native String   = "string"
   | oclType2Native Boolean  = "bool"
   | oclType2Native OclVoid  = "void"
   | oclType2Native (Set(t)) = "java.util.TreeSet"
   | oclType2Native (Sequence(t)) = "java.util.TreeSet"
   | oclType2Native t        = Rep_OclType.string_of_OclType t
(* 	      
 	      | OclAny 
		     | Set of OclType | Sequence of OclType
		     | OrderedSet of OclType | Bag of OclType 
		     | Collection of OclType
		     | Classifier of Path | OclVoid | DummyT
*)

fun visibility2Native public = "public"
 |  visibility2Native private = "private"
 |  visibility2Native protected = "protected"
 |  visibility2Native package = "public"
 
fun scope2Native ClassifierScope = "static"
 |  scope2Native InstanceScope = ""

val template = "templates/java.tpl";

end