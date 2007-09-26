(*****************************************************************************
 * su4sml --- a SML repository for managing (Secure)UML/OCL models
 *             http://projects.brucker.ch/su4sml/
 *                                                                            
 * codegen.sml ---
 * This file is part of su4sml.
 *
 * Copyright (c) 2005-2007, ETH Zurich, Switzerland
 *
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are
 * met:
 *
 *     * Redistributions of source code must retain the above copyright
 *       notice, this list of conditions and the following disclaimer.
 *
 *     * Redistributions in binary form must reproduce the above
 *       copyright notice, this list of conditions and the following
 *       disclaimer in the documentation and/or other materials provided
 *       with the distribution.
 *
 *     * Neither the name of the copyright holders nor the names of its
 *       contributors may be used to endorse or promote products derived
 *       from this software without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 * "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
 * LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
 * A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
 * OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
 * SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
 * LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
 * DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
 * THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
 * OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 ******************************************************************************)
(* $Id$ *)

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

structure Use_Gcg = GCG_Core (Use_Cartridge(Base_Cartridge))

structure SecureMova_Gcg = GCG_Core (ComponentUML_Cartridge(Base_Cartridge))

 structure Maven_POM_Gcg = GCG_Core (Maven_POM_Cartridge(Base_Cartridge))

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
val supported_cartridges = [ 
    (* Base Cartridge *)
    {lang        = "base",
     name        = "Base Cartridge",
     description = "",
     recommended = false,
     generator   = Base_Gcg.generate, 
     parser      = RepParser.readFile, 
     template    = "base.tpl"},
    (* C# Cartridge *)
    {lang        = "c#",
     name        = "C# Cartridge",
     description = "",
     recommended = true,
     generator   = CSharp_Gcg.generate,
     parser      = RepParser.readFile,
     template    = "C#.tpl"},
    (* C# SecureUML Cartridge *)
    {lang        = "c#_secure",
     name        = "C# SecureUML Cartridge",
     description = "",
     recommended = true,
     generator   = CSharpSecure_Gcg.generate,
     parser      = RepParser.readFile,
     template    = "C#_SecureUML.tpl"},
    (* C# .NET1 Cartridge *)
    {lang        = "c#_net1",
     name        = "C# .NET1 Cartridge",
     description = "",
     recommended = true,
     generator   = CSharp_NET1_Gcg.generate,
     parser      = RepParser.readFile,
     template    = "C#.pl"},
    (* C# .NET1 SecureUML Cartridge *)
    {lang        = "c#_net1_secure",
     name        = "C# .NET1 SecureUML Cartridge",
     description = "",
     recommended = true,
     generator   = CSharpSecure_NET1_Gcg.generate,
     parser      = RepParser.readFile,
     template    = "C#_SecureUML.tpl"},
    (* C# StateMachine Cartridge *)
    {lang        = "c#_sm",
     name        = "C# StateMachine Cartridge",
     description = "",
     recommended = true,
     generator   = CSharpSM_Gcg.generate,
     parser      = RepParser.readFile,
     template    = "C#_SM.tpl"},
    (* Java Cartridge *)
    {lang        = "java",
     name        = "Java Cartridge",
     description = "",
     recommended = true,
     generator   = Java_Gcg.generate,
     parser      = RepParser.readFile,
     template    = "java.tpl"},
    (* Junit Cartridge *)
    {lang        = "junit",
     name        = "Junit Cartridge",
     description = "",
     recommended = true,
     generator   = Junit_Gcg.generate,
     parser      = RepParser.readFile,
     template    = "junit.tpl"},
    (* Java OCL Cartridge *)
    {lang        = "java_ocl",
     name        = "Java OCL Cartridge",
     description = "",
     recommended = true,
     generator   = Java_Ocl_Gcg.generate,
     parser      = RepParser.readFile,
     template    = "java_ocl.tpl"},
    (* USE (UML Specification Environment) Cartridge *)
    {lang        = "USE",
     name        = "USE Cartridge",
     description = "",
     recommended = true,
     generator   = Use_Gcg.generate,
     parser      = RepParser.readFile,
     template    = "use.tpl"},
    (* SecureMOVA Cartridge *)
    {lang        = "securemova",
     name        = "SecureMOVA Cartridge",
     description = "",
     recommended = false,
     generator   = SecureMova_Gcg.generate,
     parser      = RepParser.transformXMI_ext o XmiParser.readFile,
     template    = "securemova.tpl"}]:cartridge list


val recommended_cartridges = List.filter #recommended supported_cartridges 

val supported_languages    = map #lang supported_cartridges 
val recommended_languages  = map #lang recommended_cartridges

fun is_supported lang      = ListEq.includes supported_languages   lang
fun is_recommended lang    = ListEq.includes recommended_languages lang

                            

fun cartridge_of lang = Option.valOf (List.find (fn c => #lang c = lang) supported_cartridges)

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

fun print_usage () = let val language_names      = map #name recommended_cartridges
                         val string_of_languages = String.concatWith " | " recommended_languages
                         val desc_of_languages   = map (fn (l,n) => l^":\t"^n)
                                                       (ListPair.zip (recommended_languages,
                                                                      language_names))
                     in 
                         print ("usage: codegen <xmi_file> <language>\n"^
  		                "where <language> = "^string_of_languages ^"\n\n"^
                                "supported languges are:\n"^
                                String.concatWith "\n" desc_of_languages^"\n")
                     end

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
