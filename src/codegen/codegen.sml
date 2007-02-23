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

(* FIXME: instead of the next two functions, one could put the   *)
(* information into the cartridge_list. That way, one would have *)
(* to change 2 places less when adding a new cartridge.          *)
val parse_language =
 fn "base"           => base
  | "c#"             => cSharp
  | "c#_secure"      => cSharpSecure
  | "c#_net1"        => dotNet
  | "c#_secure_net1" => dotNetSecure
  | "c#sm"           => cSharpSM
  | "java"           => java
  | "junit"          => junit
  | "javaocl"        => javaocl
  | "securemova"     => securemova

val language_name =
 fn base         => "base"
  | cSharp       => "c#"
  | cSharpSecure => "c#_secure"
  | dotNet       => "c#_net1"
  | dotNetSecure => "c#_secure_net1"
  | cSharpSM     => "c#sm"
  | java         => "java"
  | junit        => "junit"
  | javaocl      => "javaocl"
  | securemova   => "securemova"
     


(* maybe this should also hav a "description" field? *)
type cartridge = {name     : language,
                  generator: Rep.Model -> string -> unit, 
                  parser   : string -> Rep.Model,
                  template : string}

(* maybe these should be declared by the individual cartridges and simply concatenated here? *) 
val cartridge_list = [ {name      = base,
                        generator = Base_Gcg.generate, 
                        parser    = RepParser.readFile, 
                        template  = "base.tpl"},
                       {name      = cSharp,
                        generator = CSharp_Gcg.generate,
                        parser    = RepParser.readFile,
                        template  = "C#.tpl"},
                       {name      = cSharpSecure,
                        generator = CSharpSecure_Gcg.generate,
                        parser    = RepParser.readFile,
                        template  = "C#_SecureUML.tpl"},
                       {name      = dotNet,
                        generator = CSharp_NET1_Gcg.generate,
                        parser    = RepParser.readFile,
                        template  = "C#.pl"},
                       {name      = dotNetSecure,
                        generator = CSharpSecure_NET1_Gcg.generate,
                        parser    = RepParser.readFile,
                        template  = "C#_SecureUML.tpl"},
                       {name      = cSharpSM,
                        generator = CSharpSM_Gcg.generate,
                        parser    = RepParser.readFile,
                        template  = "C#_SM.tpl"},
                       {name      = java,
                        generator = Java_Gcg.generate,
                        parser    = RepParser.readFile,
                        template  = "java.tpl"},
                       {name      = junit,
                        generator = Junit_Gcg.generate,
                        parser    = RepParser.readFile,
                        template  = "junit.tpl"},
                       {name      = javaocl,
                        generator = Java_Ocl_Gcg.generate,
                        parser    = RepParser.readFile,
                        template  = "java_ocl.tpl"},
                       {name      = securemova,
                        generator = SecureMova_Gcg.generate,
                        parser    = RepParser.transformXMI o XmiParser.readFile,
                        template  = "securemova.tpl"}]


val supported_languages = map (language_name o #name) cartridge_list 

val string_of_languages = String.concatWith " | " supported_languages

fun is_supported lang = ListEq.includes supported_languages lang

fun cartridge_of lang = Option.valOf (List.find (fn c => #name c = lang) cartridge_list)

                               
fun generateFromModel model lang =
    let val cart      = cartridge_of lang 
        val gen       = #generator (cartridge_of lang)
        val template  = "templates/"^(#template cart)
    in
        gen model template
    end

    
fun generate xmi_file lang = generateFromModel ((#parser (cartridge_of lang)) xmi_file) lang

fun print_usage () = print ("usage: codegen <xmi_file> <language>\n"^
  		            "\tlanguage = "^string_of_languages ^"\n")

fun main (_,[xmi_file,lang])          = ((if is_supported lang 
                                          then generate xmi_file (parse_language lang) 
                                          else print_usage ());
                                         OS.Process.success)
  (*  | main (_,[xmi_file,lang,template]) = (generate_with_template ; OS.Process.success) *)
  | main _                            = (print_usage(); OS.Process.success)
                                        
end

val _ = Codegen.main(CommandLine.name(),CommandLine.arguments())
