(*****************************************************************************
 * su4sml --- an SML repository for managing (Secure)UML/OCL models
 *             http://projects.brucker.ch/su4sml/
 *                                                                            
 * test-suite.sml --- a regression test-suite for su4sml
 * This file is part of su4sml.
 *
 * Copyright (c) 2006, 2007 ETH Zurich, Switzerland
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

signature TESTSUITE = sig
  val runTest : unit -> unit list
end

structure testsuite:TESTSUITE = struct

open Rep_OclType
open Rep_OclTerm
open Rep_Core

type result = {
     attributes     : bool,
     multiplicities : bool,
     aclass         : bool,
     nary          : bool,
     qualifiers     : bool
}

type testcase = {
     name  : string,
     model : Rep.Model,
     result : result
}

exception TestSuiteException of string

val initResult = {
     attributes     = false,
     multiplicities = false,
     aclass         = false,
     nary           = false,
     qualifiers     = false
}:result

(*

 Class{name=Classifier ["Default","Class"],
	     parent=NONE,
	     attributes=[],
	     operations=[],
	     associations=[],
	     invariant=[],
	     stereotypes=[],
	     interfaces=[],
	     thyname=NONE,
	     visibility=XMI.public,
       activity_graphs=[]
	 }

 {name=["Default","Association"],
  aends=[{name=["Default","Association","role"],
          aend_type=Classifier ["Default","Class"],
          multiplicity=[],
          ordered=false,
          visibility=XMI.public,
          init=NONE},
         {name=["Default","Association","role"],
          aend_type=Classifier ["Default","Class"],
          multiplicity=[],
          ordered=false,
          visibility=XMI.public,
          init=NONE}],
  qualifiers=[],
  aclass=NONE}

*)

val testcases = [
    {name = "Binary Association w/o Multiplicities",
     model = ([Class{name=Classifier ["Default","ClassA"],
	                   parent=NONE,
	                   attributes=[],
	                   operations=[],
	                   associations=[["Default","Association"]],
	                   invariant=[],
	                   stereotypes=[],
	                   interfaces=[],
	                   thyname=NONE,
	                   visibility=XMI_DataTypes.public,
                     activity_graphs=[]},
               Class{name=Classifier ["Default","ClassB"],
	                   parent=NONE,
	                   attributes=[],
	                   operations=[],
	                   associations=[["Default","Association"]],
	                   invariant=[],
	                   stereotypes=[],
	                   interfaces=[],
	                   thyname=NONE,
	                   visibility=XMI.public,
                     activity_graphs=[]}],
              [{name=["Default","Association"],
                aends=[{name=["Default","Association","roleA"],
                        aend_type=Classifier ["Default","ClassA"],
                        multiplicity=[],
                        ordered=false,
                        visibility=XMI.public,
                        init=NONE},
                       {name=["Default","Association","roleB"],
                        aend_type=Classifier ["Default","ClassB"],
                        multiplicity=[],
                        ordered=false,
                        visibility=XMI.public,
                        init=NONE}],
                qualifiers=[],
                aclass=NONE}]),
     result = initResult
    }:testcase,
    {
     name = "Binary Association",
     model = ([Class{name=Classifier ["Default","ClassA"],
	                   parent=NONE,
	                   attributes=[],
	                   operations=[],
	                   associations=[["Default","Association"]],
	                   invariant=[],
	                   stereotypes=[],
	                   interfaces=[],
	                   thyname=NONE,
	                   visibility=XMI_DataTypes.public,
                     activity_graphs=[]},
               Class{name=Classifier ["Default","ClassB"],
	                   parent=NONE,
	                   attributes=[],
	                   operations=[],
	                   associations=[["Default","Association"]],
	                   invariant=[],
	                   stereotypes=[],
	                   interfaces=[],
	                   thyname=NONE,
	                   visibility=XMI.public,
                     activity_graphs=[]}],
              [{name=["Default","Association"],
                aends=[{name=["Default","Association","roleA"],
                        aend_type=Classifier ["Default","ClassA"],
                        multiplicity=[(0,2)],
                        ordered=false,
                        visibility=XMI.public,
                        init=NONE},
                       {name=["Default","Association","roleB"],
                        aend_type=Classifier ["Default","ClassB"],
                        multiplicity=[(3,4)],
                        ordered=false,
                        visibility=XMI.public,
                        init=NONE}],
                qualifiers=[],
                aclass=NONE}]),
     result = initResult
    }:testcase,
    {name = "Association Class",
     model = ([Class{name=Classifier ["Default","ClassA"],
	                   parent=NONE,
	                   attributes=[],
	                   operations=[],
	                   associations=[["Default","Association"]],
	                   invariant=[],
	                   stereotypes=[],
	                   interfaces=[],
	                   thyname=NONE,
	                   visibility=XMI_DataTypes.public,
                     activity_graphs=[]},
               Class{name=Classifier ["Default","ClassB"],
	                   parent=NONE,
	                   attributes=[],
	                   operations=[],
	                   associations=[["Default","Association"]],
	                   invariant=[],
	                   stereotypes=[],
	                   interfaces=[],
	                   thyname=NONE,
	                   visibility=XMI.public,
                     activity_graphs=[]},
               AssociationClass{name=Classifier ["Default","AClass"],
	                              parent=NONE,
	                              attributes=[],
	                              operations=[],
                                associations=[],
	                              association=["Default","Association"],
	                              invariant=[],
	                              stereotypes=[],
	                              interfaces=[],
	                              thyname=NONE,
	                              visibility=XMI.public,
                                activity_graphs=[]}],
              [{name=["Default","Association"],
                aends=[{name=["Default","Association","roleA"],
                        aend_type=Classifier ["Default","ClassA"],
                        multiplicity=[(1,1)],
                        ordered=false,
                        visibility=XMI.public,
                        init=NONE},
                       {name=["Default","Association","roleB"],
                        aend_type=Classifier ["Default","ClassB"],
                        multiplicity=[(2,2)],
                        ordered=false,
                        visibility=XMI.public,
                        init=NONE}],
                qualifiers=[],
                aclass=SOME ["Default","AClass"]}]),
     result = initResult
    }:testcase ,
    {name = "N-ary Association",
     model = ([Class{name=Classifier ["Default","ClassA"],
	                   parent=NONE,
	                   attributes=[],
	                   operations=[],
	                   associations=[["Default","Association"]],
	                   invariant=[],
	                   stereotypes=[],
	                   interfaces=[],
	                   thyname=NONE,
	                   visibility=XMI_DataTypes.public,
                     activity_graphs=[]},
               Class{name=Classifier ["Default","ClassB"],
	                   parent=NONE,
	                   attributes=[],
	                   operations=[],
	                   associations=[["Default","Association"]],
	                   invariant=[],
	                   stereotypes=[],
	                   interfaces=[],
	                   thyname=NONE,
	                   visibility=XMI.public,
                     activity_graphs=[]},
               Class{name=Classifier ["Default","ClassC"],
	                   parent=NONE,
	                   attributes=[],
	                   operations=[],
	                   associations=[["Default","Association"]],
	                   invariant=[],
	                   stereotypes=[],
	                   interfaces=[],
	                   thyname=NONE,
	                   visibility=XMI.public,
                     activity_graphs=[]}],
              [{name=["Default","Association"],
                aends=[{name=["Default","Association","roleA"],
                        aend_type=Classifier ["Default","ClassA"],
                        multiplicity=[(1,1)],
                        ordered=false,
                        visibility=XMI.public,
                        init=NONE},
                       {name=["Default","Association","roleB"],
                        aend_type=Classifier ["Default","ClassB"],
                        multiplicity=[(2,2)],
                        ordered=false,
                        visibility=XMI.public,
                        init=NONE},
                       {name=["Default","Association","roleC"],
                        aend_type=Classifier ["Default","ClassC"],
                        multiplicity=[(3,3)],
                        ordered=false,
                        visibility=XMI.public,
                        init=NONE}],
                qualifiers=[],
                aclass=NONE}]),
     result = initResult
    }:testcase,
    {
     name = "Qualifier",
     model = ([Class{name=Classifier ["Default","ClassA"],
	                   parent=NONE,
	                   attributes=[],
	                   operations=[],
	                   associations=[["Default","Association"]],
	                   invariant=[],
	                   stereotypes=[],
	                   interfaces=[],
	                   thyname=NONE,
	                   visibility=XMI_DataTypes.public,
                     activity_graphs=[]},
               Class{name=Classifier ["Default","ClassB"],
	                   parent=NONE,
	                   attributes=[],
	                   operations=[],
	                   associations=[["Default","Association"]],
	                   invariant=[],
	                   stereotypes=[],
	                   interfaces=[],
	                   thyname=NONE,
	                   visibility=XMI.public,
                     activity_graphs=[]}],
              [{name=["Default","Association"],
                aends=[{name=["Default","Association","roleA"],
                        aend_type=Classifier ["Default","ClassA"],
                        multiplicity=[(0,2)],
                        ordered=false,
                        visibility=XMI.public,
                        init=NONE},
                       {name=["Default","Association","roleB"],
                        aend_type=Classifier ["Default","ClassB"],
                        multiplicity=[(3,4)],
                        ordered=false,
                        visibility=XMI.public,
                        init=NONE}],
                qualifiers=[("roleA",[{name="Number",
                                    attr_type=Rep_OclType.Integer,
                                    visibility=XMI.public,
                                    scope=XMI.ClassifierScope,
                                    stereotypes=[],
                                    init=NONE},
                                   {name="Date",
                                    attr_type=Rep_OclType.Integer,
                                    visibility=XMI.public,
                                    scope=XMI.ClassifierScope,
                                    stereotypes=[],
                                    init=NONE}])],
                aclass=NONE}]),
     result = initResult
    }:testcase(*,  
    {
     name = "Mixed",
     model = ([],[]),
     result = initResult
    }:testcase
*)
]

fun test (tc:testcase) = 
    let 
      val _         = print "### Removing Qualifiers ###\n"
	    val model = Rep_Transform.transformQualifiers (#model tc)
	       (* handle _ => ([],[]) *)
	    val remQuali = model <> ([],[])

      val _         = print "### Removing N-ary Associations ###\n" 
	    val model = Rep_Transform.transformNAryAssociations model
	       (*  handle _ => ([],[]) *)
      val remNary = model <> ([],[])

      val _         = print "### Removing Association Classes ###\n"     
	    val model = Rep_Transform.transformAssociationClasses model
	        (* handle _ => ([],[])  *)
	    val remAclass = model <> ([],[])
			                
	    val _         = print "### Removing Binary Multiplicities ###\n"
	    val model   = Rep_Transform.transformMultiplicities model
  (*        handle _ => ([],[])  *)
	    val remMult = model <> ([],[])
			                
	    val _         = print "### Generating Attributes ###\n"
	    val model     = Rep_Core.normalize_ext model
(*	        handle _ => ([],[])*)
	    val genAttr = model <> ([],[])
                    
                       
    in 
	    {name = #name tc,
	     model = #model tc,
	     result = {attributes     = genAttr,
                 multiplicities = remMult,
                 aclass         = remAclass,
                 nary           = remNary,
                 qualifiers     = remQuali
                }:result
      }:testcase
    end

fun printResult (tc:testcase) = 
    let
	fun printBool b = if b then "passed" else "FAILED"
	val _   = print ("\n *** "^(#name tc)^" ***\n")
	val res = (#result tc) 
	val _   = print ("   attributes:     "^(printBool (#attributes res))^"\n")
	val _   = print ("   multiplicities: "^(printBool(#multiplicities res))^"\n")
	val _   = print ("   aclasses:       "^(printBool (#aclass res))^"\n")
	val _   = print ("   naries:         "^(printBool (#nary res))^"\n")
	val _   = print ("   qualifiers:     "^(printBool (#qualifiers res))^"\n\n")
	val _   = print ("   ==> overall:    "^(printBool ((#attributes res) 
							                                    andalso (#multiplicities res)
							                                    andalso (#aclass res) 
							                                    andalso (#nary res) 
							                                    andalso (#qualifiers res)))^
                   "\n")
    in
	    ()
    end

(* val _ = (Ext_Library.log_level := 1);(); *)

fun runTest () = map printResult  (map test testcases)

end
  
val _ = testsuite.runTest()
