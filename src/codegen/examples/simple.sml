(*****************************************************************************
 *          su4sml - a SecureUML repository for SML              
 *                                                                            
 * simple.sml - a simple test file for the core repository
 * Copyright (C) 2003-2005 Achim D. Brucker <brucker@inf.ethz.ch>
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

