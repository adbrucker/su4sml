(*****************************************************************************
 * su4sml --- a SML repository for managing (Secure)UML/OCL models
 *             http://projects.brucker.ch/su4sml/
 *                                                                            
 * simple.sml --- a simple test file for the core repository
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

open Rep;
open Rep_OclType;
open XMI_DataTypes;
open Rep_SecureUML_ComponentUML.Security;
open ComponentUML;

val A = Class({name=["simple","A"],
	       parent=NONE,
	       stereotypes=[],
	       attributes=[({	name="i",
     				attr_type=Integer,
     				visibility=private,
     				scope=InstanceScope,
     				init=NONE
     				} : attribute),
	       			({name="r",
     				attr_type=Real,
     				visibility=public,
     				scope=InstanceScope,
     				init=NONE
     				} : attribute),
     				({name="attribB",
				attr_type=Classifier(["simple","B"]),
				visibility=public,
     				scope=InstanceScope,
				init=NONE
     				} : attribute)
			   ] ,
	       associationends=[({name="B",
				 aend_type=Classifier(["simple","B"]),
				 multiplicity=[(1,5)],
				 ordered=false,
				 visibility=public,
		       		 init=NONE
			       } : associationend)],
	       operations=[({name="main",
	       		     precondition=[],
	       		     postcondition=[],
	       		     arguments=[("p",Integer)],
			     result=OclVoid,
			     isQuery=true,
			     visibility=public,
	                     scope=ClassifierScope 
	                    }: operation)],
	       interfaces=[], 
	       invariant=[],
	       activity_graphs=[],
	       thyname=NONE
	      })

val B = Class({name=["simple","B"],
	       parent=NONE,
	       stereotypes=[],
	       attributes=[({	name="j",
     				attr_type=Integer,
     				visibility=private,
     				scope=InstanceScope,
     				init=NONE
     				} : attribute),
     				({name="attribA",
				attr_type=Classifier(["simple","A"]),
				visibility=public,
     				scope=InstanceScope,
				init=NONE
     				} : attribute)
			   ] ,
	       associationends=[({name="B",
				 aend_type=Classifier(["simple","B"]),
				 multiplicity=[(~1,~1)],
				 ordered=false,
				 visibility=public,
		       		 init=NONE
			       }:associationend)],
	       operations=[],
	       interfaces=[], 
	       invariant=[],
	       activity_graphs=[],
	       thyname=NONE
	      })

val C = Class({name=["simple","C"],
	       parent=SOME(["simple","A"]),
	       stereotypes=[],
	       attributes=[({	name="sl",
     				attr_type=Sequence(String),
     				visibility=public,
     				scope=InstanceScope,
     				init=NONE
     				} : attribute),
     			   ({	name="slset",
				attr_type=Set(String),
				visibility=public,
				scope=InstanceScope,
				init=NONE
     				} : attribute)
     			  ],
	       associationends=[],
	       operations=[],
	       interfaces=[], 
	       invariant=[],
	       activity_graphs=[],
	       thyname=NONE
	      })

val D = Class({name=["simple","D"],
	       parent=SOME(["simple","A"]),
	       stereotypes=[],
	       attributes=[({	name="r",
     				attr_type=Real,
     				visibility=private,
     				scope=InstanceScope,
     				init=NONE
     				} : attribute)],
	       associationends=[],
	       operations=[],
	       interfaces=[], 
	       invariant=[],
	       activity_graphs=[],
	       thyname=NONE
	      })

val E = Class({name=["simple","E"],
	       parent=SOME(["simple","B"]),
	       stereotypes=[],
	       attributes=[({	name="r",
     				attr_type=Real,
     				visibility=package,
     				scope=InstanceScope,
     				init=NONE
     				} : attribute)],
	       associationends=[],
	       operations=[],
	       interfaces=[], 
	       invariant=[],
	       activity_graphs=[],
	       thyname=NONE
	      })

val cl = [A,B,C,D,E]

val perms =[{name="FullAccessClassABC",
	     roles=["Supervisor","Admin"],
	     constraints=[]: Rep_OclTerm.OclTerm list,
	     actions= [ (CompositeAction ("full_access",("Entity",["simple","A"]))),
	     		(CompositeAction ("full_access",("Entity",["simple","B"]))),
	     		(CompositeAction ("full_access",("Entity",["simple","C"])))]: Design.Action list 
	     },
	     {name="ReadRealProperties",
	     	     roles=["Supervisor","Admin","Raphi"],
	     	     constraints=[]: Rep_OclTerm.OclTerm list,
	     	     actions= [ (SimpleAction ("read",("EntityAttribute",["simple","A","r"]))),
	     	     		(SimpleAction ("read",("EntityAttribute",["simple","D","r"]))),
	     	     		(SimpleAction ("read",("EntityAttribute",["simple","E","r"])))]: Design.Action list 
	     },
	     {name="CreateDeleteClassABC",
	     	     roles=["Supervisor","Admin"],
	     	     constraints=[]: Rep_OclTerm.OclTerm list,
	     	     actions= [ (SimpleAction ("create",("Entity",["simple","A"]))),
	     	     		(SimpleAction ("create",("Entity",["simple","B"]))),
	     	     		(SimpleAction ("create",("Entity",["simple","C"]))),
	     	     		(SimpleAction ("delete",("Entity",["simple","A"]))),
	     	     		(SimpleAction ("delete",("Entity",["simple","B"]))),
	     	     		(SimpleAction ("delete",("Entity",["simple","C"])))]: Design.Action list 
	     }
	   ]

val model = (cl, {config_type = "SecureUML",
	       		permissions = perms,
	       		subjects = nil,
	       		roles = nil,
	       		sa = nil}):Rep_SecureUML_ComponentUML.Model 

