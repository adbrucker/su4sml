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

 structure Base_Gcg = GCG_Core (Base_Cartridge)

 structure CSharp_Gcg = GCG_Core (CSharp_Cartridge(Base_Cartridge))

 structure CSharpSecure_Gcg 
  = GCG_Core (CSharp_Cartridge( ComponentUML_Cartridge(Base_Cartridge))) 

structure CSharp_NET1_Gcg 
  = GCG_Core (CSharp_NET1_Cartridge(Base_Cartridge))

structure CSharpSecure_NET1_Gcg 
  = GCG_Core (CSharp_NET1_Cartridge(ComponentUML_Cartridge(Base_Cartridge)))

structure SecureUML_Base_Gcg 
  = GCG_Core (ComponentUML_Cartridge(Base_Cartridge))

structure CSharpSM_Gcg = GCG_Core (CSSM_Cartridge(CSharp_Cartridge(Base_Cartridge)))

structure Java_Gcg = GCG_Core (Java_Cartridge(Base_Cartridge))

structure Junit_Gcg = GCG_Core (Junit_Cartridge(Java_Cartridge(Base_Cartridge)))

structure SecureMova_Gcg = GCG_Core (ComponentUML_Cartridge(Base_Cartridge))

(*
structure JavaSecure_Gcg = GCG_Core (Java_Cartridge(SecureUML_Cartridge(Base_Cartridge)));
*)

fun generate xmi_file "base" = 
	Base_Gcg.generate ( RepParser.readFile xmi_file) "templates/base.tpl"
 |  generate xmi_file "c#"   = 
 	CSharp_Gcg.generate ( RepParser.readFile xmi_file) "templates/C#.tpl"
 |  generate xmi_file "c#_secure" = 
 	CSharpSecure_Gcg.generate ( RepParser.readFile xmi_file) "templates/C#_SecureUML.tpl"
 |  generate xmi_file "c#_net1"   = 
 	CSharp_NET1_Gcg.generate ( RepParser.readFile xmi_file) "templates/C#.tpl"
 |  generate xmi_file "c#_secure_net1" = 
 	CSharpSecure_NET1_Gcg.generate ( RepParser.readFile xmi_file) "templates/C#_SecureUML.tpl"
 |  generate xmi_file "c#sm" = 
    CSharpSM_Gcg.generate (RepParser.readFile xmi_file) "templates/C#_SM.tpl"
 |  generate xmi_file "java" = 
    Java_Gcg.generate (RepParser.readFile xmi_file) "templates/java.tpl"
 |  generate xmi_file "junit" = 
    Junit_Gcg.generate (RepParser.readFile xmi_file) "templates/junit.tpl"
 (*
 |  generate "java_secure" = JavaSecure_Gcg.generate model "templates/java_SecureUML.tpl"
 *)
(* | generate xmi_file "maude" = 
   Base_Gcg.generate ( RepParser.readFile xmi_file) "templates/maude.tpl"
 | generate xmi_file "maude_secure" = 
   SecureUML_Base_Gcg.generate ( Rep_SecureUML_ComponentUML.readXMI xmi_file) "templates/maude.tpl" *)
 | generate xmi_file "securemova" = 
   SecureMova_Gcg.generate (RepParser.transformXMI (XmiParser.readFile xmi_file)) 
                           "templates/securemova.tpl"
 |  generate _ s = print ("target language unknown : "^s^"\n"^
 			"usage: generate <xmi_file> \"base\" | \"c#\" | \"c#_secure\" | \"c#_net1\" | \"c#_secure_net1\" | \"java\" | \"junit\"\n")
 			

fun main (_,[xmi_file,lang]) = (generate xmi_file lang ; OS.Process.success)
  | main _ = (print ("usage: codegen <xmi_file> <language>\n"^ 
  		    "\tlanguage = \"base\" | \"c#\" | \"c#sm\" | \"c#_secure\" | \"c#_net1\" | \"c#_secure_net1\" | \"java\" | \"junit\" | \"maude\" | \"maude_secure\" \n"); OS.Process.success)

end


val _ = Codegen.main(CommandLine.name(),CommandLine.arguments())
