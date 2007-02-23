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


(* maybe this should also hav a "description" field? *)
type cartridge = {lang        : string, (* identifier (for input)         *)
                  name        : string, (* short description (for output) *)
                  description : string, (* long description               *)
                  recommended : bool,   (* presented to the end-user?     *)
                  generator   : Rep.Model -> string -> unit, 
                  parser      : string -> Rep.Model,
                  template    : string}

(* maybe these should be declared by the individual cartridges and simply concatenated here? *) 
(* FIXME: add long descriptions *)
val cartridge_list = [ {lang        = "base",
                        name        = "Base Cartridge",
                        description = "",
                        recommended = false,
                        generator   = Base_Gcg.generate, 
                        parser      = RepParser.readFile, 
                        template    = "base.tpl"},
                       {lang        = "c#",
                        name        = "C# Cartridge",
                        description = "",
                        recommended = true,
                        generator   = CSharp_Gcg.generate,
                        parser      = RepParser.readFile,
                        template    = "C#.tpl"},
                       {lang        = "c#_secure",
                        name        = "C# SecureUML Cartridge",
                        description = "",
                        recommended = true,
                        generator   = CSharpSecure_Gcg.generate,
                        parser      = RepParser.readFile,
                        template    = "C#_SecureUML.tpl"},
                       {lang        = "c#_net1",
                        name        = "C# .NET1 Cartridge",
                        description = "",
                        recommended = true,
                        generator   = CSharp_NET1_Gcg.generate,
                        parser      = RepParser.readFile,
                        template    = "C#.pl"},
                       {lang        = "c#_net1_secure",
                        name        = "C# .NET1 SecureUML Cartridge",
                        description = "",
                        recommended = true,
                        generator   = CSharpSecure_NET1_Gcg.generate,
                        parser      = RepParser.readFile,
                        template    = "C#_SecureUML.tpl"},
                       {lang        = "c#_sm",
                        name        = "C# StateMachine Cartridge",
                        description = "",
                        recommended = true,
                        generator   = CSharpSM_Gcg.generate,
                        parser      = RepParser.readFile,
                        template    = "C#_SM.tpl"},
                       {lang        = "java",
                        name        = "Java Cartridge",
                        description = "",
                        recommended = true,
                        generator   = Java_Gcg.generate,
                        parser      = RepParser.readFile,
                        template    = "java.tpl"},
                       {lang        = "junit",
                        name        = "Junit Cartridge",
                        description = "",
                        recommended = true,
                        generator   = Junit_Gcg.generate,
                        parser      = RepParser.readFile,
                        template    = "junit.tpl"},
                       {lang        = "java_ocl",
                        name        = "Java OCL Cartridge",
                        description = "",
                        recommended = true,
                        generator   = Java_Ocl_Gcg.generate,
                        parser      = RepParser.readFile,
                        template    = "java_ocl.tpl"},
                       {lang        = "securemova",
                        name        = "SecureMOVA Cartridge",
                        description = "",
                        recommended = false,
                        generator   = SecureMova_Gcg.generate,
                        parser      = RepParser.transformXMI o XmiParser.readFile,
                        template    = "securemova.tpl"}]:cartridge list


val supported_languages   = map #lang cartridge_list 
val recommended_languages = map #lang (List.filter #recommended cartridge_list)
val string_of_languages   = String.concatWith " | " recommended_languages

fun is_supported lang = ListEq.includes supported_languages lang

fun cartridge_of lang = Option.valOf (List.find (fn c => #lang c = lang) cartridge_list)

fun generateWithTemplate xmi_file lang template =
    let val cart  = cartridge_of lang 
        val gen   = #generator cart
        val model = (#parser cart) xmi_file
    in
        gen model template
    end
                               

fun genFromModelWithCart model (cart:cartridge) =
    let val gen = #generator cart
        val template =  "templates/"^(#template cart)
    in 
        gen model template
    end

fun generateWithCart xmi_file (cart:cartridge) = 
    genFromModelWithCart ((#parser cart) xmi_file) cart

fun generateFromModel model    lang = genFromModelWithCart model    (cartridge_of lang)
fun generate          xmi_file lang = generateWithCart     xmi_file (cartridge_of lang)

fun print_usage () = print ("usage: codegen <xmi_file> <language>\n"^
  		            "\tlanguage = "^string_of_languages ^"\n")

fun main (_,[xmi_file,lang])          = ((if   is_supported lang 
                                          then generate xmi_file lang
                                          else print_usage ());
                                         OS.Process.success)
  | main (_,[xmi_file,lang,template]) = ((if   is_supported lang 
                                          then generateWithTemplate xmi_file lang template
                                          else print_usage ());
                                         OS.Process.success) 
  | main _                            = (print_usage(); OS.Process.success)
                                        
end

val _ = Codegen.main(CommandLine.name(),CommandLine.arguments())
