(*****************************************************************************
 *          su4sml - a SecureUML repository for SML              
 *                                                                            
 * ocl_library.sml - 
 * Copyright (C) 2006  Achim D. Brucker <brucker@inf.ethz.ch>   
 *                     Jürgen Doser <doserj@inf.ethz.ch>
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

(** A Package consisting of the OCL Library. *)
signature OCL_LIBRARY=
sig
  val oclLib : Rep_Core.Classifier list
  val OclLibPackage : string
end

structure OclLibrary : OCL_LIBRARY=
struct
open Rep_Core
open Rep_OclType

val OclLibPackage="oclLib"
		  
val oclLib =
    [Template
	 {
	  parameter = TemplateParameter "T",
          classifier = Class
			   {
			    interfaces=[],attributes=[],
			    associationends=[],
			    activity_graphs=[],
			    invariant=[],
			    name=["Sequence(T)"],
			    operations=[
			    {
			     arguments=[("object",Classifier [OclLibPackage,"T"])],
			     isQuery=true,
			     name="count",
			     postcondition=[],
			     precondition=[],
			     result=Classifier [OclLibPackage,"Integer"],
			     scope=XMI.InstanceScope,
			     visibility=XMI.public
			    },
			    {
			      arguments=[],isQuery=true,name="notEmpty",postcondition=[],
			      precondition=[],result=Classifier [OclLibPackage,"Boolean"],
			      scope=XMI.InstanceScope,visibility=XMI.public
			     },
			    {
			      arguments=[],isQuery=true,name="size",postcondition=[],
			      precondition=[],result=Classifier [OclLibPackage,"Integer"],
			      scope=XMI.InstanceScope,visibility=XMI.public
			     },
			    {
			      arguments=[],isQuery=true,name="isEmpty",postcondition=[],
			      precondition=[],result=Classifier [OclLibPackage,"Boolean"],
			      scope=XMI.InstanceScope,visibility=XMI.public
			     },
			    {
			     arguments=[("s",Classifier [OclLibPackage,"Sequence(T)"])],
			     isQuery=true,
			     name="=",
			     postcondition=[],
			     precondition=[],
			     result=Classifier [OclLibPackage,"Boolean"],
			     scope=XMI.InstanceScope,
			     visibility=XMI.public
			    },
			    {
			     arguments=[("s",Classifier [OclLibPackage,"Sequence(T)"])],
			     isQuery=true,
			     name="==",
			     postcondition=[],
			     precondition=[],
			     result=Classifier [OclLibPackage,"Boolean"],
			     scope=XMI.InstanceScope,
			     visibility=XMI.public
			    },
			    {
			     arguments=[("s",Classifier [OclLibPackage,"Sequence(T)"])],
			     isQuery=true,
			     name="~=",
			     postcondition=[],
			     precondition=[],
			     result=Classifier [OclLibPackage,"Boolean"],
			     scope=XMI.InstanceScope,
			     visibility=XMI.public
			    },
			    {
			     arguments=[("s",Classifier [OclLibPackage,"Sequence(T)"])],
			     isQuery=true,
			     name="~==",
			     postcondition=[],
			     precondition=[],
			     result=Classifier [OclLibPackage,"Boolean"],
			     scope=XMI.InstanceScope,
			     visibility=XMI.public
			    },
			    {
			     arguments=[("s",Classifier [OclLibPackage,"Sequence(T)"])],
			     isQuery=true,
			     name="~=~",
			     postcondition=[],
			     precondition=[],
			     result=Classifier [OclLibPackage,"Boolean"],
			     scope=XMI.InstanceScope,
			     visibility=XMI.public
			    },
			    {
			     arguments=[("s",Classifier [OclLibPackage,"Sequence(T)"])],
			     isQuery=true,
			     name="~==~",
			     postcondition=[],
			     precondition=[],
			     result=Classifier [OclLibPackage,"Boolean"],
			     scope=XMI.InstanceScope,
			     visibility=XMI.public
			    },
			    {
			     arguments=[("s",Classifier [OclLibPackage,"Sequence(T)"])],
			     isQuery=true,
			     name="union",
			     postcondition=[],
			     precondition=[],
			     result=Classifier [OclLibPackage,"Sequence(T)"],
			     scope=XMI.InstanceScope,
			     visibility=XMI.public
			    },
			    {
			     arguments=[],
			     isQuery=true,
			     name="flatten",
			     postcondition=[],
			     precondition=[],
			     result=Classifier [OclLibPackage,"Sequence(T)"],
			     scope=XMI.InstanceScope,
			     visibility=XMI.public
			    },
			    {
			     arguments=[("object", Classifier [OclLibPackage,"T"])],
			     isQuery=true,
			     name="append",
			     postcondition=[],
			     precondition=[],
			     result=Classifier [OclLibPackage,"Sequence(T)"],
			     scope=XMI.InstanceScope,
			     visibility=XMI.public
			    },
			    {
			     arguments=[("object",Classifier [OclLibPackage,"T"])],
			     isQuery=true,
			     name="prepend",
			     postcondition=[],
			     precondition=[],
			     result=Classifier [OclLibPackage,"Sequence(T)"],
			     scope=XMI.InstanceScope,
			     visibility=XMI.public
			    },
			    {
			     arguments=[("index",Classifier [OclLibPackage,"Integer"]),
					("object",Classifier [OclLibPackage,"T"])],
			     isQuery=true,name="insertAt",
			     postcondition=[],
			     precondition=[],
			     result=Classifier [OclLibPackage,"Sequence(T)"],
			     scope=XMI.InstanceScope,
			     visibility=XMI.public
			    },
			    {
			     arguments=[("lower",Classifier [OclLibPackage,"Integer"]),
					("upper",Classifier [OclLibPackage,"Integer"])],
			     isQuery=true,
			     name="subSequence",postcondition=[],
			     precondition=[],
			     result=Classifier [OclLibPackage,"Sequence(T)"],
			     scope=XMI.InstanceScope,
			     visibility=XMI.public
			    },
			    {
			     arguments=[("i",Classifier [OclLibPackage,"Integer"])],
			     isQuery=true,
			     name="at",
			     postcondition=[],
			     precondition=[],
			     result=Classifier [OclLibPackage,"T"],
			     scope=XMI.InstanceScope,
			     visibility=XMI.public
			    },
			    {
			     arguments=[("obj",Classifier [OclLibPackage,"T"])],
			     isQuery=true,
			     name="indexOf",
			     postcondition=[],
			     precondition=[],
			     result=Classifier [OclLibPackage,"Integer"],
			     scope=XMI.InstanceScope,
			     visibility=XMI.public
			    },
			    {
			     arguments=[],
			     isQuery=true,
			     name="first",
			     postcondition=[],
			     precondition=[],
			     result=Classifier [OclLibPackage,"T"],
			     scope=XMI.InstanceScope,
			     visibility=XMI.public
			    },
			    {
			     arguments=[],
			     isQuery=true,
			     name="last",
			     postcondition=[],
			     precondition=[],
			     result=Classifier [OclLibPackage,"T"],
			     scope=XMI.InstanceScope,visibility=XMI.public
			    },
			    {
			     arguments=[("object",Classifier [OclLibPackage,"T"])],
			     isQuery=true,
			     name="including",
			     postcondition=[],
			     precondition=[],
			     result=Classifier [OclLibPackage,"Sequence(T)"],
			     scope=XMI.InstanceScope,
			     visibility=XMI.public
			    },
			    {
			     arguments=[("object",Classifier [OclLibPackage,"T"])],
			     isQuery=true,
			     name="excluding",
			     postcondition=[],
			     precondition=[],
			     result=Classifier [OclLibPackage,"Sequence(T)"],
			     scope=XMI.InstanceScope,
			     visibility=XMI.public
			    },
			    {
			     arguments=[],
			     isQuery=true,
			     name="asSet",
			     postcondition=[],
			     precondition=[],
			     result=Classifier [OclLibPackage,"Set(T)"],
			     scope=XMI.InstanceScope,
			     visibility=XMI.public
			    },
			    {
			     arguments=[],
			     isQuery=true,
			     name="asSequence",
			     postcondition=[],
			     precondition=[],
			     result=Classifier [OclLibPackage,"Sequence(T)"],
			     scope=XMI.InstanceScope,
			     visibility=XMI.public
			    },
			    {
			     arguments=[],
			     isQuery=true,
			     name="asBag",
			     postcondition=[],
			     precondition=[],
			     result=Classifier [OclLibPackage,"Bag(T)"],
			     scope=XMI.InstanceScope,
			     visibility=XMI.public
			    }],
			    parent=NONE,
			    stereotypes=[],thyname=NONE
			   }: Rep_Core.Classifier
	 },
     Template
	 {
	  parameter = TemplateParameter "T",
          classifier = Class
			   {
			    attributes=[],
			    associationends=[],
			    activity_graphs=[],
			    interfaces=[],
			    invariant=[],
			    name=["Bag(T)"],
			    operations=[
			    {
			     arguments=[("bag",Classifier [OclLibPackage,"Bag(T)"])],
			     isQuery=true,name="=",postcondition=[],precondition=[],
			     result=Classifier [OclLibPackage,"Boolean"],scope=XMI.InstanceScope,
			     visibility=XMI.public
			    },
			    {
			      arguments=[],isQuery=true,name="size",postcondition=[],
			      precondition=[],result=Classifier [OclLibPackage,"Integer"],
			      scope=XMI.InstanceScope,visibility=XMI.public
			     },
			    {
			      arguments=[],isQuery=true,name="notEmpty",postcondition=[],
			      precondition=[],result=Classifier [OclLibPackage,"Boolean"],
			      scope=XMI.InstanceScope,visibility=XMI.public
			     },
			    {
			      arguments=[],isQuery=true,name="isEmpty",postcondition=[],
			      precondition=[],result=Classifier [OclLibPackage,"Boolean"],
			      scope=XMI.InstanceScope,visibility=XMI.public
			     },
			    {
			     arguments=[("bag",Classifier [OclLibPackage,"Bag(T)"])],
			     isQuery=true,name="==",postcondition=[],precondition=[],
			     result=Classifier [OclLibPackage,"Boolean"],scope=XMI.InstanceScope,
			     visibility=XMI.public
			    },
			    {
			     arguments=[("bag",Classifier [OclLibPackage,"Bag(T)"])],
			     isQuery=true,name="~=",postcondition=[],precondition=[],
			     result=Classifier [OclLibPackage,"Boolean"],scope=XMI.InstanceScope,
			     visibility=XMI.public
			    },
			    {
			     arguments=[("bag",Classifier [OclLibPackage,"Bag(T)"])],
			     isQuery=true,name="~==",postcondition=[],precondition=[],
			     result=Classifier [OclLibPackage,"Boolean"],scope=XMI.InstanceScope,
			     visibility=XMI.public
			    },
			    {
			     arguments=[("bag",Classifier [OclLibPackage,"Bag(T)"])],
			     isQuery=true,name="~=~",postcondition=[],precondition=[],
			     result=Classifier [OclLibPackage,"Boolean"],scope=XMI.InstanceScope,
			     visibility=XMI.public
			    },
			    {
			     arguments=[("bag",Classifier [OclLibPackage,"Bag(T)"])],
			     isQuery=true,name="~==~",postcondition=[],precondition=[],
			     result=Classifier [OclLibPackage,"Boolean"],scope=XMI.InstanceScope,
			     visibility=XMI.public
			    },
			    {
			     arguments=[("bag",Classifier [OclLibPackage,"Bag(T)"])],
			     isQuery=true,name="union",postcondition=[],precondition=[],
			     result=Classifier [OclLibPackage,"Bag(T)"],
			     scope=XMI.InstanceScope,visibility=XMI.public
			    },
			    {
			     arguments=[("bag",Classifier [OclLibPackage,"Bag(T)"])],
			     isQuery=true,name="intersection",postcondition=[],
			     precondition=[],result=Classifier [OclLibPackage,"Bag(T)"],
			     scope=XMI.InstanceScope,visibility=XMI.public
			    },
			    {
			     arguments=[("set",Classifier [OclLibPackage,"Set(T)"])],
			     isQuery=true,name="intersection",postcondition=[],
			     precondition=[],result=Classifier [OclLibPackage,"Set(T)"],
			     scope=XMI.InstanceScope,visibility=XMI.public
			    },
			    {
			     arguments=[("object",Classifier [OclLibPackage,"T"])],
			     isQuery=true,name="including",postcondition=[],
			     precondition=[],result=Classifier [OclLibPackage,"Bag(T)"],
			     scope=XMI.InstanceScope,visibility=XMI.public
			    },
			    {
			     arguments=[("object",Classifier [OclLibPackage,"T"])],
			     isQuery=true,name="excluding",postcondition=[],
			     precondition=[],result=Classifier [OclLibPackage,"Bag(T)"],
			     scope=XMI.InstanceScope,visibility=XMI.public
			    },
			    {
			     arguments=[("object",Classifier [OclLibPackage,"T"])],
			     isQuery=true,name="count",postcondition=[],precondition=[],
			     result=Classifier [OclLibPackage,"Integer"],scope=XMI.InstanceScope,
			     visibility=XMI.public
			    },
			    {
			     arguments=[],isQuery=true,name="flatten",postcondition=[],
			     precondition=[],result=Classifier [OclLibPackage,"Bag(T)"],
			     scope=XMI.InstanceScope,visibility=XMI.public
			    },
			    {
			     arguments=[],isQuery=true,name="asSet",postcondition=[],
			     precondition=[],result=Classifier [OclLibPackage,"Set(T)"],
			     scope=XMI.InstanceScope,visibility=XMI.public
			    },
			    {
			     arguments=[],isQuery=true,name="asSequence",
			     postcondition=[],precondition=[],
			     result=Classifier [OclLibPackage,"Sequence(T)"],
			     scope=XMI.InstanceScope,visibility=XMI.public
			    },
			    {
			     arguments=[],isQuery=true,name="asBag",postcondition=[],
			     precondition=[],result=Classifier [OclLibPackage,"Bag(T)"],
			     scope=XMI.InstanceScope,visibility=XMI.public}],parent=NONE,
			    stereotypes=[],thyname=NONE
			   }: Rep_Core.Classifier
	 },
     Template
	 {
	  parameter = TemplateParameter "T",
          classifier =  Class
			    {
			     attributes=[],
			     associationends=[],
			     activity_graphs=[],
			     interfaces=[],invariant=[],name=["Set(T)"],
			     operations=[
			     {
			      arguments=[("s",Classifier [OclLibPackage,"Set(T)"])],
			      isQuery=true,name="union",postcondition=[],precondition=[],
			      result=Classifier [OclLibPackage,"Set(T)"],
			      scope=XMI.InstanceScope,visibility=XMI.public
			     },
			     {
			      arguments=[],isQuery=true,name="notEmpty",postcondition=[],
			      precondition=[],result=Classifier [OclLibPackage,"Boolean"],
			      scope=XMI.InstanceScope,visibility=XMI.public
			     },
			     {
			      arguments=[],isQuery=true,name="size",postcondition=[],
			      precondition=[],result=Classifier [OclLibPackage,"Integer"],
			      scope=XMI.InstanceScope,visibility=XMI.public
			     },
			     {
			      arguments=[("bag",Classifier [OclLibPackage,"Bag(T)"])],
			      isQuery=true,name="union",postcondition=[],precondition=[],
			      result=Classifier [OclLibPackage,"Bag(T)"],
			      scope=XMI.InstanceScope,visibility=XMI.public
			     },
			     {
			      arguments=[("s",Classifier [OclLibPackage,"Set(T)"])],
			      isQuery=true,name="=",postcondition=[],precondition=[],
			      result=Classifier [OclLibPackage,"Boolean"],scope=XMI.InstanceScope,
			      visibility=XMI.public
			     },
			     {
			      arguments=[("s",Classifier [OclLibPackage,"Set(T)"])],
			      isQuery=true,name="==",postcondition=[],precondition=[],
			      result=Classifier [OclLibPackage,"Boolean"],scope=XMI.InstanceScope,
			      visibility=XMI.public
			     },
{
			      arguments=[],isQuery=true,name="isEmpty",postcondition=[],
			      precondition=[],result=Classifier [OclLibPackage,"Boolean"],
			      scope=XMI.InstanceScope,visibility=XMI.public
			     },
			     {
			      arguments=[("s",Classifier [OclLibPackage,"Set(T)"])],
			      isQuery=true,name="~=",postcondition=[],precondition=[],
			      result=Classifier [OclLibPackage,"Boolean"],scope=XMI.InstanceScope,
			      visibility=XMI.public
			     },
			     {
			      arguments=[("s",Classifier [OclLibPackage,"Set(T)"])],
			      isQuery=true,name="~==",postcondition=[],precondition=[],
			      result=Classifier [OclLibPackage,"Boolean"],scope=XMI.InstanceScope,
			      visibility=XMI.public
			     },
			     {
			      arguments=[("s",Classifier [OclLibPackage,"Set(T)"])],
			      isQuery=true,name="~=~",postcondition=[],precondition=[],
			      result=Classifier [OclLibPackage,"Boolean"],scope=XMI.InstanceScope,
			      visibility=XMI.public
			     },

			     {
			      arguments=[("s",Classifier [OclLibPackage,"Set(T)"])],
			      isQuery=true,name="~==~",postcondition=[],precondition=[],
			      result=Classifier [OclLibPackage,"Boolean"],scope=XMI.InstanceScope,
			      visibility=XMI.public
			     },
			     {
			      arguments=[("s",Classifier [OclLibPackage,"Set(T)"])],
			      isQuery=true,name="intersection",postcondition=[],
			      precondition=[],result=Classifier [OclLibPackage,"Set(T)"],
			      scope=XMI.InstanceScope,visibility=XMI.public
			     },
			     {
			      arguments=[("bag",Classifier [OclLibPackage,"Bag(T)"])],
			      isQuery=true,name="intersection",postcondition=[],
			      precondition=[],result=Classifier [OclLibPackage,"Set(T)"],
			      scope=XMI.InstanceScope,visibility=XMI.public
			     },
			     {
			      arguments=[("s",Classifier [OclLibPackage,"Set(T)"])],
			      isQuery=true,name="-",postcondition=[],precondition=[],
			      result=Classifier [OclLibPackage,"Set(T)"],
			      scope=XMI.InstanceScope,visibility=XMI.public
			     },
			     {
			      arguments=[("object",Classifier [OclLibPackage,"T"])],
			      isQuery=true,name="including",postcondition=[],
			      precondition=[],result=Classifier [OclLibPackage,"Set(T)"],
			      scope=XMI.InstanceScope,visibility=XMI.public
			     },
			     {
			      arguments=[("object",Classifier [OclLibPackage,"T"])],
			      isQuery=true,name="excluding",postcondition=[],
			      precondition=[],result=Classifier [OclLibPackage,"Set(T)"],
			      scope=XMI.InstanceScope,visibility=XMI.public
			     },
			     {
			      arguments=[("s",Classifier [OclLibPackage,"Set(T)"])],
			      isQuery=true,name="symmetricDifference",postcondition=[],
			      precondition=[],result=Classifier [OclLibPackage,"Set(T)"],
			      scope=XMI.InstanceScope,visibility=XMI.public
			     },
			     {
			      arguments=[("object",Classifier [OclLibPackage,"T"])],
			      isQuery=true,name="count",postcondition=[],precondition=[],
			      result=Classifier [OclLibPackage,"Integer"],scope=XMI.InstanceScope,
			      visibility=XMI.public
			     },
			     {
			      arguments=[],isQuery=true,name="flatten",postcondition=[],
			      precondition=[],result=Classifier [OclLibPackage,"Set(T)"],
			      scope=XMI.InstanceScope,visibility=XMI.public
			     },
			     {
			      arguments=[],isQuery=true,name="asSet",postcondition=[],
			      precondition=[],result=Classifier [OclLibPackage,"Set(T)"],
			      scope=XMI.InstanceScope,visibility=XMI.public
			     },
			     {
			      arguments=[],isQuery=true,name="asSequence",
			      postcondition=[],precondition=[],
			      result=Classifier [OclLibPackage,"Sequence(T)"],
			      scope=XMI.InstanceScope,visibility=XMI.public
			     },

			     {
			      arguments=[],isQuery=true,name="asBag",postcondition=[],
			      precondition=[],result=Classifier [OclLibPackage,"Bag(T)"],
			      scope=XMI.InstanceScope,visibility=XMI.public
			     },

			     {
			      arguments=[("set",Classifier [OclLibPackage,"Set(T)"])],
			      isQuery=true,name="union",postcondition=[],precondition=[],
			      result=Classifier [OclLibPackage,"Bag(T)"],
			      scope=XMI.InstanceScope,visibility=XMI.public}],parent=NONE,
			     stereotypes=[],thyname=NONE
			    }: Rep_Core.Classifier
	 },
     Template
	 {
	  parameter = TemplateParameter "T",
          classifier =  Class
			    {
			     attributes=[],
			     associationends=[],
			     activity_graphs=[],	  
			     interfaces=[],invariant=[],name=["Collection(T)"],
			     operations=[
			     {
			      arguments=[],isQuery=true,name="size",postcondition=[],
			      precondition=[],result=Classifier [OclLibPackage,"Integer"],
			      scope=XMI.InstanceScope,visibility=XMI.public
			     },
			     {
			      arguments=[],isQuery=true,name="modifiedOnly",
			      postcondition=[],precondition=[],
			      result=Classifier [OclLibPackage,"Boolean"],scope=XMI.InstanceScope,
			      visibility=XMI.public
			     },
			     {
			      arguments=[("object",Classifier [OclLibPackage,"T"])],
			      isQuery=true,name="includes",postcondition=[],
			      precondition=[],result=Classifier [OclLibPackage,"Boolean"],
			      scope=XMI.InstanceScope,visibility=XMI.public
			     },
			     {
			      arguments=[("object",Classifier [OclLibPackage,"T"])],
			      isQuery=true,name="excludes",postcondition=[],
			      precondition=[],result=Classifier [OclLibPackage,"Boolean"],
			      scope=XMI.InstanceScope,visibility=XMI.public
			     },
			     {
			      arguments=[("object",Classifier [OclLibPackage,"T"])],
			      isQuery=true,name="count",postcondition=[],precondition=[],
			      result=Classifier [OclLibPackage,"Integer"],scope=XMI.InstanceScope,
			      visibility=XMI.public
			     },
			     {
			      arguments=[("c",Classifier [OclLibPackage,"Collection(T)"])],
			      isQuery=true,name="includesAll",postcondition=[],
			      precondition=[],result=Classifier [OclLibPackage,"Boolean"],
			      scope=XMI.InstanceScope,visibility=XMI.public
			     },
			     {
			      arguments=[("c",Classifier [OclLibPackage,"Collection(T)"])],
			      isQuery=true,name="excludesAll",postcondition=[],
			      precondition=[],result=Classifier [OclLibPackage,"Boolean"],
			      scope=XMI.InstanceScope,visibility=XMI.public
			     },
			     {
			      arguments=[],isQuery=true,name="isEmpty",postcondition=[],
			      precondition=[],result=Classifier [OclLibPackage,"Boolean"],
			      scope=XMI.InstanceScope,visibility=XMI.public
			     },
			     {
			      arguments=[],isQuery=true,name="notEmpty",postcondition=[],
			      precondition=[],result=Classifier [OclLibPackage,"Boolean"],
			      scope=XMI.InstanceScope,visibility=XMI.public
			     },
			     {
			      arguments=[],isQuery=true,name="sum",postcondition=[],
			      precondition=[],result=Classifier [OclLibPackage,"T"],
			      scope=XMI.InstanceScope,visibility=XMI.public
			     },
			     {
			      arguments=[("c2",
					  Classifier [OclLibPackage,"Collection(T)"])],
			      isQuery=true,name="product",postcondition=[],
			      precondition=[],result=Classifier [OclLibPackage,"Set(T)"],
			      scope=XMI.InstanceScope,visibility=XMI.public}],parent=NONE,
			     stereotypes=[],thyname=NONE
			    }: Rep_Core.Classifier
	 },
     Class
	 {
	  attributes=[],
	  associationends=[],
	  activity_graphs=[],	  
	  interfaces=[],invariant=[],name=["OclVoid"],operations=[],
	  parent=NONE,stereotypes=[],thyname=NONE
	 },

     Primitive
	 {
	  associationends=[],interfaces=[],invariant=[],name=[OclLibPackage,"OclAny"],
	  operations=[
	  {
	   arguments=[("object",Classifier [OclLibPackage,"OclAny"])],
           isQuery=true,name="=",postcondition=[],precondition=[],
           result=Classifier [OclLibPackage,"Boolean"],scope=XMI.InstanceScope,
           visibility=XMI.public
	  },
          {
	   arguments=[("object",Classifier [OclLibPackage,"OclAny"])],
           isQuery=true,name="==",postcondition=[],precondition=[],
           result=Classifier [OclLibPackage,"Boolean"],scope=XMI.InstanceScope,
           visibility=XMI.public
	  },
          {
	   arguments=[("object",Classifier [OclLibPackage,"OclAny"])],
           isQuery=true,name="~=",postcondition=[],precondition=[],
           result=Classifier [OclLibPackage,"Boolean"],scope=XMI.InstanceScope,
           visibility=XMI.public
	  },
          {
	   arguments=[("object",Classifier [OclLibPackage,"OclAny"])],
           isQuery=true,name="~==",postcondition=[],precondition=[],
           result=Classifier [OclLibPackage,"Boolean"],scope=XMI.InstanceScope,
           visibility=XMI.public
	  },
          {
	   arguments=[("object",Classifier [OclLibPackage,"OclAny"])],
           isQuery=true,name="~=~",postcondition=[],precondition=[],
           result=Classifier [OclLibPackage,"Boolean"],scope=XMI.InstanceScope,
           visibility=XMI.public
	  },
          {
	   arguments=[("object",Classifier [OclLibPackage,"OclAny"])],
           isQuery=true,name="~==~",postcondition=[],precondition=[],
           result=Classifier [OclLibPackage,"Boolean"],scope=XMI.InstanceScope,
           visibility=XMI.public
	  },
          {
	   arguments=[("object",Classifier [OclLibPackage,"OclAny"])],
           isQuery=true,name="<>",postcondition=[],precondition=[],
           result=Classifier [OclLibPackage,"Boolean"],scope=XMI.InstanceScope,
           visibility=XMI.public
	  },
          {
	   arguments=[],isQuery=true,name="oclIsNew",postcondition=[],
           precondition=[],result=Classifier [OclLibPackage,"Boolean"],
           scope=XMI.InstanceScope,visibility=XMI.public
	  },
          {
	   arguments=[],isQuery=true,name="oclIsUndefined",
           postcondition=[],precondition=[],
           result=Classifier [OclLibPackage,"Boolean"],scope=XMI.InstanceScope,
           visibility=XMI.public
	  },
          {
	   arguments=[],isQuery=true,name="asSet",postcondition=[],
           precondition=[],result=Classifier [OclLibPackage,"Set(OclAny)"],
           scope=XMI.InstanceScope,visibility=XMI.public
	  },
          {
	   arguments=[],isQuery=true,name="atPre",postcondition=[],
           precondition=[],result=Classifier [OclLibPackage,"OclAny"],
           scope=XMI.InstanceScope,visibility=XMI.public
	  },
          {
	   arguments=[("statename",Classifier ["OclState"])],
           isQuery=true,name="oclIsInState",postcondition=[],
           precondition=[],result=Classifier [OclLibPackage,"Boolean"],
           scope=XMI.InstanceScope,visibility=XMI.public
	  },
          {
	   arguments=[],isQuery=true,name="allInstances",
           postcondition=[],precondition=[],
           result=Classifier [OclLibPackage,"Set(OclAny)"],
           scope=XMI.ClassifierScope,visibility=XMI.public}],parent=NONE,
	  stereotypes=[],thyname=NONE
	 },
     Primitive
	 {
	  associationends=[],interfaces=[],invariant=[],name=[OclLibPackage,"String"],
	  operations=[
	  {
	   arguments=[],isQuery=true,name="size",postcondition=[],
           precondition=[],result=Classifier [OclLibPackage,"Integer"],
           scope=XMI.InstanceScope,visibility=XMI.public
	  },
          {
	   arguments=[("s",Classifier [OclLibPackage,"String"])],
           isQuery=true,name="concat",postcondition=[],
           precondition=[],result=Classifier [OclLibPackage,"String"],
           scope=XMI.InstanceScope,visibility=XMI.public
	  },
          {
	   arguments=[("lower",Classifier [OclLibPackage,"Integer"]),
		      ("upper",Classifier [OclLibPackage,"Integer"])],
           isQuery=true,name="substring",postcondition=[],
           precondition=[],result=Classifier [OclLibPackage,"String"],
           scope=XMI.InstanceScope,visibility=XMI.public
	  },
          {
	   arguments=[],isQuery=true,name="toInteger",
           postcondition=[],precondition=[],
           result=Classifier [OclLibPackage,"Integer"],scope=XMI.InstanceScope,
           visibility=XMI.public
	  },
          {
	   arguments=[],isQuery=true,name="toReal",postcondition=[],
           precondition=[],result=Classifier [OclLibPackage,"Real"],
           scope=XMI.InstanceScope,visibility=XMI.public
	  },
          {
	   arguments=[],isQuery=true,name="toUpper",postcondition=[],
           precondition=[],result=Classifier [OclLibPackage,"String"],
           scope=XMI.InstanceScope,visibility=XMI.public
	  },
          {
	   arguments=[],isQuery=true,name="toLower",postcondition=[],
           precondition=[],result=Classifier [OclLibPackage,"String"],
           scope=XMI.InstanceScope,visibility=XMI.public}],parent=NONE,
	  stereotypes=[],thyname=NONE
	 },
     Primitive
	 {
	  associationends=[],interfaces=[],invariant=[],name=[OclLibPackage,"Boolean"],
	  operations=[
	  {
	   arguments=[("b",Classifier [OclLibPackage,"Boolean"])],
           isQuery=true,name="or",postcondition=[],precondition=[],
           result=Classifier [OclLibPackage,"Boolean"],scope=XMI.InstanceScope,
           visibility=XMI.public
	  },
          {
	   arguments=[("b",Classifier [OclLibPackage,"Boolean"])],
           isQuery=true,name="xor",postcondition=[],precondition=[],
           result=Classifier [OclLibPackage,"Boolean"],scope=XMI.InstanceScope,
           visibility=XMI.public
	  },
          {
	   arguments=[("b",Classifier [OclLibPackage,"Boolean"])],
           isQuery=true,name="and",postcondition=[],precondition=[],
           result=Classifier [OclLibPackage,"Boolean"],scope=XMI.InstanceScope,
           visibility=XMI.public
	  },
          {
	   arguments=[("b",Classifier [OclLibPackage,"Boolean"])],
           isQuery=true,name="implies",postcondition=[],
           precondition=[],result=Classifier [OclLibPackage,"Boolean"],
           scope=XMI.InstanceScope,visibility=XMI.public
	  },
          {
	   arguments=[],isQuery=true,name="not",postcondition=[],
           precondition=[],result=Classifier [OclLibPackage,"Boolean"],
           scope=XMI.InstanceScope,visibility=XMI.public
	  },
          {
	   arguments=[("b",Classifier [OclLibPackage,"Boolean"])],
           isQuery=true,name="sor",postcondition=[],precondition=[],
           result=Classifier [OclLibPackage,"Boolean"],scope=XMI.InstanceScope,
           visibility=XMI.public
	  },
          {
	   arguments=[("b",Classifier [OclLibPackage,"Boolean"])],
           isQuery=true,name="sxor",postcondition=[],precondition=[],
           result=Classifier [OclLibPackage,"Boolean"],scope=XMI.InstanceScope,
           visibility=XMI.public
	  },
          {
	   arguments=[("b",Classifier [OclLibPackage,"Boolean"])],
           isQuery=true,name="sand",postcondition=[],precondition=[],
           result=Classifier [OclLibPackage,"Boolean"],scope=XMI.InstanceScope,
           visibility=XMI.public
	  },
          {
	   arguments=[("b",Classifier [OclLibPackage,"Boolean"])],
           isQuery=true,name="simplies",postcondition=[],
           precondition=[],result=Classifier [OclLibPackage,"Boolean"],
           scope=XMI.InstanceScope,visibility=XMI.public
	  },
          {
	   arguments=[("b",Classifier [OclLibPackage,"Boolean"])],
           isQuery=true,name="implies1",postcondition=[],
           precondition=[],result=Classifier [OclLibPackage,"Boolean"],
           scope=XMI.InstanceScope,visibility=XMI.public
	  },
          {
	   arguments=[("b",Classifier [OclLibPackage,"Boolean"])],
           isQuery=true,name="implies2",postcondition=[],
           precondition=[],result=Classifier [OclLibPackage,"Boolean"],
           scope=XMI.InstanceScope,visibility=XMI.public}],parent=NONE,
	  stereotypes=[],thyname=NONE
	 },
     Primitive
	 {
	  associationends=[],interfaces=[],invariant=[],name=[OclLibPackage,"Integer"],
	  operations=[
	  {
	   arguments=[("i",Classifier [OclLibPackage,"Integer"])],
           isQuery=true,name="+",postcondition=[],precondition=[],
           result=Classifier [OclLibPackage,"Integer"],scope=XMI.InstanceScope,
           visibility=XMI.public
	  },
          {
	   arguments=[("i",Classifier [OclLibPackage,"Integer"])],
           isQuery=true,name="-",postcondition=[],precondition=[],
           result=Classifier [OclLibPackage,"Integer"],scope=XMI.InstanceScope,
           visibility=XMI.public
	  },
          {
	   arguments=[("i",Classifier [OclLibPackage,"Integer"])],
           isQuery=true,name="*",postcondition=[],precondition=[],
           result=Classifier [OclLibPackage,"Integer"],scope=XMI.InstanceScope,
           visibility=XMI.public
	  },
          {
	   arguments=[],isQuery=true,name="-",postcondition=[],
           precondition=[],result=Classifier [OclLibPackage,"Integer"],
           scope=XMI.InstanceScope,visibility=XMI.public
	  },
          {
	   arguments=[("i",Classifier [OclLibPackage,"Integer"])],
           isQuery=true,name="/",postcondition=[],precondition=[],
           result=Classifier [OclLibPackage,"Real"],scope=XMI.InstanceScope,
           visibility=XMI.public
	  },
          {
	   arguments=[("i",Classifier [OclLibPackage,"Integer"])],
           isQuery=true,name="div",postcondition=[],precondition=[],
           result=Classifier [OclLibPackage,"Integer"],scope=XMI.InstanceScope,
           visibility=XMI.public
	  },
          {
	   arguments=[],isQuery=true,name="abs",postcondition=[],
           precondition=[],result=Classifier [OclLibPackage,"Integer"],
           scope=XMI.InstanceScope,visibility=XMI.public
	  },
          {
	   arguments=[("i",Classifier [OclLibPackage,"Integer"])],
           isQuery=true,name="mod",postcondition=[],precondition=[],
           result=Classifier [OclLibPackage,"Integer"],scope=XMI.InstanceScope,
           visibility=XMI.public
	  },
          {
	   arguments=[("i",Classifier [OclLibPackage,"Integer"])],
           isQuery=true,name="max",postcondition=[],precondition=[],
           result=Classifier [OclLibPackage,"Integer"],scope=XMI.InstanceScope,
           visibility=XMI.public
	  },
          {
	   arguments=[("i",Classifier [OclLibPackage,"Integer"])],
           isQuery=true,name="min",postcondition=[],precondition=[],
           result=Classifier [OclLibPackage,"Integer"],scope=XMI.InstanceScope,
           visibility=XMI.public}],parent=NONE,stereotypes=[],
	  thyname=NONE
	 },
     Primitive
	 {
	  associationends=[],interfaces=[],invariant=[],name=[OclLibPackage,"Real"],
	  operations=[
	  {
	   arguments=[("r",Classifier [OclLibPackage,"Real"])],
           isQuery=true,name="+",postcondition=[],precondition=[],
           result=Classifier [OclLibPackage,"Real"],scope=XMI.InstanceScope,
           visibility=XMI.public
	  },
          {
	   arguments=[("r",Classifier [OclLibPackage,"Real"])],
           isQuery=true,name="-",postcondition=[],precondition=[],
           result=Classifier [OclLibPackage,"Real"],scope=XMI.InstanceScope,
           visibility=XMI.public
	  },
          {
	   arguments=[("r",Classifier [OclLibPackage,"Real"])],
           isQuery=true,name="*",postcondition=[],precondition=[],
           result=Classifier [OclLibPackage,"Real"],scope=XMI.InstanceScope,
           visibility=XMI.public
	  },
          {
	   arguments=[],isQuery=true,name="-",postcondition=[],
           precondition=[],result=Classifier [OclLibPackage,"Real"],
           scope=XMI.InstanceScope,visibility=XMI.public
	  },
          {
	   arguments=[("r",Classifier [OclLibPackage,"Real"])],
           isQuery=true,name="/",postcondition=[],precondition=[],
           result=Classifier [OclLibPackage,"Real"],scope=XMI.InstanceScope,
           visibility=XMI.public
	  },
          {
	   arguments=[],isQuery=true,name="abs",postcondition=[],
           precondition=[],result=Classifier [OclLibPackage,"Real"],
           scope=XMI.InstanceScope,visibility=XMI.public
	  },
          {
	   arguments=[],isQuery=true,name="floor",postcondition=[],
           precondition=[],result=Classifier [OclLibPackage,"Integer"],
           scope=XMI.InstanceScope,visibility=XMI.public
	  },
          {
	   arguments=[],isQuery=true,name="round",postcondition=[],
           precondition=[],result=Classifier [OclLibPackage,"Integer"],
           scope=XMI.InstanceScope,visibility=XMI.public
	  },
          {
	   arguments=[("r",Classifier [OclLibPackage,"Real"])],
           isQuery=true,name="max",postcondition=[],precondition=[],
           result=Classifier [OclLibPackage,"Real"],scope=XMI.InstanceScope,
           visibility=XMI.public
	  },
          {
	   arguments=[("r",Classifier [OclLibPackage,"Real"])],
           isQuery=true,name="min",postcondition=[],precondition=[],
           result=Classifier [OclLibPackage,"Real"],scope=XMI.InstanceScope,
           visibility=XMI.public
	  },
          {
	   arguments=[("r",Classifier [OclLibPackage,"Real"])],
           isQuery=true,name="<",postcondition=[],precondition=[],
           result=Classifier [OclLibPackage,"Boolean"],scope=XMI.InstanceScope,
           visibility=XMI.public
	  },
          {
	   arguments=[("r",Classifier [OclLibPackage,"Real"])],
           isQuery=true,name=">",postcondition=[],precondition=[],
           result=Classifier [OclLibPackage,"Boolean"],scope=XMI.InstanceScope,
           visibility=XMI.public
	  },
          {
	   arguments=[("r",Classifier [OclLibPackage,"Real"])],
           isQuery=true,name="<=",postcondition=[],precondition=[],
           result=Classifier [OclLibPackage,"Boolean"],scope=XMI.InstanceScope,
           visibility=XMI.public
	  },
          {
	   arguments=[("r",Classifier [OclLibPackage,"Real"])],
           isQuery=true,name=">=",postcondition=[],precondition=[],
           result=Classifier [OclLibPackage,"Boolean"],scope=XMI.InstanceScope,
           visibility=XMI.public}],parent=NONE,stereotypes=[],
	  thyname=NONE
	 }
    ] : Rep_Core.Classifier list

end
