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

structure Java_Ocl_Gcg = GCG_Core (Java_Cartridge(Base_Cartridge))

structure SecureMova_Gcg = GCG_Core (ComponentUML_Cartridge(Base_Cartridge))

(*
structure JavaSecure_Gcg = GCG_Core (Java_Cartridge(SecureUML_Cartridge(Base_Cartridge)));
*)

datatype language = base | cSharp | cSharpSecure | dotNet | dotNetSecure 
		  | cSharpSM | java | junit | javaocl | securemova 

fun generateFromModel model base      
    = Base_Gcg.generate model "templates/base.tpl"
  | generateFromModel model cSharp        
    = CSharp_Gcg.generate model "templates/C#.tpl"
  | generateFromModel model cSharpSecure 
    = CSharpSecure_Gcg.generate model "templates/C#_SecureUML.tpl"
  | generateFromModel model dotNet   
    = CSharp_NET1_Gcg.generate model "templates/C#.tpl"
  | generateFromModel model dotNetSecure 
    = CSharpSecure_NET1_Gcg.generate model "templates/C#_SecureUML.tpl"
  | generateFromModel model cSharpSM      
    = CSharpSM_Gcg.generate model "templates/C#_SM.tpl"
  | generateFromModel model java      
    = Java_Gcg.generate model "templates/java.tpl"
  | generateFromModel model junit     
    = Junit_Gcg.generate model "templates/junit.tpl"
  | generateFromModel model javaocl   
    = Java_Ocl_Gcg.generate model "templates/java_ocl.tpl"
  | generateFromModel model securemova 
    = SecureMova_Gcg.generate model "templates/securemova.tpl"

fun generate xmi_file "base" 
    = generateFromModel ( RepParser.readFile xmi_file) base
  |  generate xmi_file "c#" 
    = generateFromModel ( RepParser.readFile xmi_file) cSharp
  |  generate xmi_file "c#_secure"  
    = generateFromModel ( RepParser.readFile xmi_file) cSharpSecure
  |  generate xmi_file "c#_net1"    
    = generateFromModel ( RepParser.readFile xmi_file) dotNet
  |  generate xmi_file "c#_secure_net1" 
    = generateFromModel ( RepParser.readFile xmi_file) dotNetSecure
  |  generate xmi_file "c#sm"  
    = generateFromModel (RepParser.readFile xmi_file) cSharpSM
  |  generate xmi_file "java"  
    = generateFromModel (RepParser.readFile xmi_file) java
  |  generate xmi_file "junit" 
    = generateFromModel (RepParser.readFile xmi_file) junit
  |  generate xmi_file "javaocl" 
    = generateFromModel (RepParser.readFile xmi_file) javaocl
 (*
 |  generate "java_secure" = JavaSecure_Gcg.generate model "templates/java_SecureUML.tpl"
 *)
(* | generate xmi_file "maude" = 
   Base_Gcg.generate ( RepParser.readFile xmi_file) "templates/maude.tpl"
 | generate xmi_file "maude_secure" = 
   SecureUML_Base_Gcg.generate ( Rep_SecureUML_ComponentUML.readXMI xmi_file) "templates/maude.tpl" *)
 | generate xmi_file "securemova" 
   = generateFromModel (RepParser.transformXMI (XmiParser.readFile xmi_file)) securemova
 |  generate _ s = print ("target language unknown : "^s^"\n"^
 			"usage: generate <xmi_file> \"base\" | \"c#\" | \"c#_secure\" | \"c#_net1\" | \"c#_secure_net1\" | \"java\" | \"junit\"\n")
 			

fun main (_,[xmi_file,lang]) = (generate xmi_file lang ; OS.Process.success)
  | main _ = (print ("usage: codegen <xmi_file> <language>\n"^
  		    "\tlanguage = \"base\" | \"c#\" | \"c#sm\" | \"c#_secure\" | \"c#_net1\" | \"c#_secure_net1\" | \"java\" | \"junit\" | \"maude\" | \"maude_secure\" | \"javaocl\"\n"); OS.Process.success)

end

val _ = Codegen.main(CommandLine.name(),CommandLine.arguments())
