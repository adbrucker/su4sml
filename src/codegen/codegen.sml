(*****************************************************************************
 *          su4sml GCG - Generic Code Generator                          
 *                                                                            
 * codegen.sml - control file for su4sml-GCG
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

(*
OS.FileSys.chDir "../../../src";
*)

structure Codegen = struct 

structure Base_Gcg = GCG_Core (Base_Cartridge);
structure CSharp_Gcg = GCG_Core (CSharp_Cartridge(Base_Cartridge));
structure CSharpSecure_Gcg = GCG_Core (CSharp_Cartridge(SecureUML_Cartridge(Base_Cartridge)));
structure CSharp_NET1_Gcg = GCG_Core (CSharp_NET1_Cartridge(Base_Cartridge));
structure CSharpSecure_NET1_Gcg = GCG_Core (CSharp_NET1_Cartridge(SecureUML_Cartridge(Base_Cartridge)));
(*
structure Java_Gcg = GCG_Core (Java_Cartridge(Base_Cartridge));
structure JavaSecure_Gcg = GCG_Core (Java_Cartridge(SecureUML_Cartridge(Base_Cartridge)));
*)

fun generate xmi_file "base" = 
	Base_Gcg.generate ( Rep_SecureUML_ComponentUML.readXMI xmi_file) "templates/base.tpl"
 |  generate xmi_file "c#"   = 
 	CSharp_Gcg.generate ( Rep_SecureUML_ComponentUML.readXMI xmi_file) "templates/C#.tpl"
 |  generate xmi_file "c#_secure" = 
 	CSharpSecure_Gcg.generate ( Rep_SecureUML_ComponentUML.readXMI xmi_file) "templates/C#_SecureUML.tpl"
 |  generate xmi_file "c#_net1"   = 
 	CSharp_NET1_Gcg.generate ( Rep_SecureUML_ComponentUML.readXMI xmi_file) "templates/C#.tpl"
 |  generate xmi_file "c#_secure_net1" = 
 	CSharpSecure_NET1_Gcg.generate ( Rep_SecureUML_ComponentUML.readXMI xmi_file) "templates/C#_SecureUML.tpl"
 (*
 |  generate "java" = Java_Gcg.generate model "templates/java.tpl"
 |  generate "java_secure" = JavaSecure_Gcg.generate model "templates/java_SecureUML.tpl"
 *)
 |  generate _ s = print ("target language unknown : "^s^"\n"^
 			"usage: generate <xmi_file> \"base\" | \"c#\" | \"c#_secure\" | \"c#_net1\" | \"c#_secure_net1\"\n")
 			

fun main (_,[xmi_file,lang]) = generate xmi_file lang
  | main _ = print ("usage: codegen <xmi_file> <language>\n"^
  		    "\tlanguage = \"base\" | \"c#\" | \"c#_secure\" | \"c#_net1\" | \"c#_secure_net1\"\n")

end


val _ = Codegen.main(CommandLine.name(),CommandLine.arguments())