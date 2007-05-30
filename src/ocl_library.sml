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
  val is_oclLib : Rep_Core.Classifier -> bool
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
			    name=Sequence (TemplateParameter "T"),
			    operations=[
			    {
			     arguments=[("object",TemplateParameter "T")],
			     isQuery=true,
			     name="count",
			     postcondition=[],
			     precondition=[],
			     result=Integer,
			     scope=XMI.InstanceScope,
			     visibility=XMI.public
			    },
			    {
			     arguments=[],
			     isQuery=true,
			     name="notEmpty",
			     postcondition=[],
			     precondition=[],
			     result=Boolean,
			     scope=XMI.InstanceScope,
			     visibility=XMI.public
			    },
			    {
			     arguments=[],
			     isQuery=true,
			     name="size",
			     postcondition=[],
			     precondition=[],
			     result=Integer,
			     scope=XMI.InstanceScope,
			     visibility=XMI.public
			    },
			    {
			     arguments=[],
			     isQuery=true,
			     name="isEmpty",
			     postcondition=[],
			     precondition=[],
			     result=Boolean,
			     scope=XMI.InstanceScope,
			     visibility=XMI.public
			    },
			    {
			     arguments=[("s",Sequence(TemplateParameter "T"))],
			     isQuery=true,
			     name="=",
			     postcondition=[],
			     precondition=[],
			     result=Boolean,
			     scope=XMI.InstanceScope,
			     visibility=XMI.public
			    },
			    {
			     arguments=[("s",Sequence(TemplateParameter "T"))],
			     isQuery=true,
			     name="==",
			     postcondition=[],
			     precondition=[],
			     result=Boolean,
			     scope=XMI.InstanceScope,
			     visibility=XMI.public
			    },
			    {
			     arguments=[("s",Sequence(TemplateParameter "T"))],
			     isQuery=true,
			     name="~=",
			     postcondition=[],
			     precondition=[],
			     result=Boolean,
			     scope=XMI.InstanceScope,
			     visibility=XMI.public
			    },
			    {
			     arguments=[("s",Sequence(TemplateParameter "T"))],
			     isQuery=true,
			     name="~==",
			     postcondition=[],
			     precondition=[],
			     result=Boolean,
			     scope=XMI.InstanceScope,
			     visibility=XMI.public
			    },
			    {
			     arguments=[("s",Sequence(TemplateParameter "T"))],
			     isQuery=true,
			     name="~=~",
			     postcondition=[],
			     precondition=[],
			     result=Boolean,
			     scope=XMI.InstanceScope,
			     visibility=XMI.public
			    },
			    {
			     arguments=[("s",Sequence(TemplateParameter "T"))],
			     isQuery=true,
			     name="~==~",
			     postcondition=[],
			     precondition=[],
			     result=Boolean,
			     scope=XMI.InstanceScope,
			     visibility=XMI.public
			    },
			    {
			     arguments=[("s",Sequence(TemplateParameter "T"))],
			     isQuery=true,
			     name="union",
			     postcondition=[],
			     precondition=[],
			     result=Sequence(TemplateParameter "T"),
			     scope=XMI.InstanceScope,
			     visibility=XMI.public
			    },
			    {
			     arguments=[],
			     isQuery=true,
			     name="flatten",
			     postcondition=[],
			     precondition=[],
			     result=Sequence(TemplateParameter "T"),
			     scope=XMI.InstanceScope,
			     visibility=XMI.public
			    },
			    {
			     arguments=[("object", TemplateParameter "T")],
			     isQuery=true,
			     name="append",
			     postcondition=[],
			     precondition=[],
			     result=Sequence(TemplateParameter "T"),
			     scope=XMI.InstanceScope,
			     visibility=XMI.public
			    },
			    {
			     arguments=[("object",TemplateParameter "T")],
			     isQuery=true,
			     name="prepend",
			     postcondition=[],
			     precondition=[],
			     result=Sequence(TemplateParameter "T"),
			     scope=XMI.InstanceScope,
			     visibility=XMI.public
			    },
			    {
			     arguments=[("index",Integer),
					("object",TemplateParameter "T")],
			     isQuery=true,name="insertAt",
			     postcondition=[],
			     precondition=[],
			     result=Sequence(TemplateParameter "T"),
			     scope=XMI.InstanceScope,
			     visibility=XMI.public
			    },
			    {
			     arguments=[("lower",Integer),
					("upper",Integer)],
			     isQuery=true,
			     name="subSequence",postcondition=[],
			     precondition=[],
			     result=Sequence(TemplateParameter "T"),
			     scope=XMI.InstanceScope,
			     visibility=XMI.public
			    },
			    {
			     arguments=[("i",Integer)],
			     isQuery=true,
			     name="at",
			     postcondition=[],
			     precondition=[],
			     result=TemplateParameter "T",
			     scope=XMI.InstanceScope,
			     visibility=XMI.public
			    },
			    {
			     arguments=[("obj",TemplateParameter "T")],
			     isQuery=true,
			     name="indexOf",
			     postcondition=[],
			     precondition=[],
			     result=Integer,
			     scope=XMI.InstanceScope,
			     visibility=XMI.public
			    },
			    {
			     arguments=[],
			     isQuery=true,
			     name="first",
			     postcondition=[],
			     precondition=[],
			     result=TemplateParameter "T",
			     scope=XMI.InstanceScope,
			     visibility=XMI.public
			    },
			    {
			     arguments=[],
			     isQuery=true,
			     name="last",
			     postcondition=[],
			     precondition=[],
			     result=TemplateParameter "T",
			     scope=XMI.InstanceScope,
			     visibility=XMI.public
			    },
			    {
			     arguments=[("object",TemplateParameter "T")],
			     isQuery=true,
			     name="including",
			     postcondition=[],
			     precondition=[],
			     result=Sequence(TemplateParameter "T"),
			     scope=XMI.InstanceScope,
			     visibility=XMI.public
			    },
			    {
			     arguments=[("object",TemplateParameter "T")],
			     isQuery=true,
			     name="excluding",
			     postcondition=[],
			     precondition=[],
			     result=Sequence(TemplateParameter "T"),
			     scope=XMI.InstanceScope,
			     visibility=XMI.public
			    },
			    {
			     arguments=[],
			     isQuery=true,
			     name="asSet",
			     postcondition=[],
			     precondition=[],
			     result=Set(TemplateParameter "T"),
			     scope=XMI.InstanceScope,
			     visibility=XMI.public
			    },
			    {
			     arguments=[],
			     isQuery=true,
			     name="asSequence",
			     postcondition=[],
			     precondition=[],
			     result=Sequence(TemplateParameter "T"),
			     scope=XMI.InstanceScope,
			     visibility=XMI.public
			    },
			    {
			     arguments=[],
			     isQuery=true,
			     name="asBag",
			     postcondition=[],
			     precondition=[],
			     result=Bag(TemplateParameter "T"),
			     scope=XMI.InstanceScope,
			     visibility=XMI.public
			    }],
			    parent= NONE,
			    stereotypes=[],
			    thyname=NONE
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
			    name=Bag (TemplateParameter "T"),
			    operations=[
			    {
			     arguments=[("bag",Bag(TemplateParameter "T"))],
			     isQuery=true,name="=",
			     postcondition=[],
			     precondition=[],
			     result=Boolean,
			     scope=XMI.InstanceScope,
			     visibility=XMI.public
			    },
			    {
			      arguments=[],
			      isQuery=true,
			      name="size",
			      postcondition=[],
			      precondition=[],
			      result=Integer,
			      scope=XMI.InstanceScope,
			      visibility=XMI.public
			     },
			    {
			      arguments=[],
			      isQuery=true,
			      name="notEmpty",
			      postcondition=[],
			      precondition=[],
			      result=Boolean,
			      scope=XMI.InstanceScope,
			      visibility=XMI.public
			     },
			    {
			      arguments=[],
			      isQuery=true,
			      name="isEmpty",
			      postcondition=[],
			      precondition=[],
			      result=Boolean,
			      scope=XMI.InstanceScope,
			      visibility=XMI.public
			     },
			    {
			     arguments=[("bag",Bag(TemplateParameter "T"))],
			     isQuery=true,
			     name="==",
			     postcondition=[],
			     precondition=[],
			     result=Boolean,
			     scope=XMI.InstanceScope,
			     visibility=XMI.public
			    },
			    {
			     arguments=[("bag",Bag(TemplateParameter "T"))],
			     isQuery=true,
			     name="~=",
			     postcondition=[],
			     precondition=[],
			     result=Boolean,
			     scope=XMI.InstanceScope,
			     visibility=XMI.public
			    },
			    {
			     arguments=[("bag",Bag(TemplateParameter "T"))],
			     isQuery=true,
			     name="~==",
			     postcondition=[],
			     precondition=[],
			     result=Boolean,
			     scope=XMI.InstanceScope,
			     visibility=XMI.public
			    },
			    {
			     arguments=[("bag",Bag(TemplateParameter "T"))],
			     isQuery=true,
			     name="~=~",
			     postcondition=[],
			     precondition=[],
			     result=Boolean,
			     scope=XMI.InstanceScope,
			     visibility=XMI.public
			    },
			    {
			     arguments=[("bag",Bag(TemplateParameter "T"))],
			     isQuery=true,
			     name="~==~",
			     postcondition=[],
			     precondition=[],
			     result=Boolean,
			     scope=XMI.InstanceScope,
			     visibility=XMI.public
			    },
			    {
			     arguments=[("bag",Bag(TemplateParameter "T"))],
			     isQuery=true,
			     name="union",
			     postcondition=[],
			     precondition=[],
			     result=Bag(TemplateParameter "T"),
			     scope=XMI.InstanceScope,
			     visibility=XMI.public
			    },
			    {
			     arguments=[("bag",Bag(TemplateParameter "T"))],
			     isQuery=true,
			     name="intersection",
			     postcondition=[],
			     precondition=[],
			     result=Bag(TemplateParameter "T"),
			     scope=XMI.InstanceScope,
			     visibility=XMI.public
			    },
			    {
			     arguments=[("set",Set(TemplateParameter "T"))],
			     isQuery=true,
			     name="intersection",
			     postcondition=[],
			     precondition=[],
			     result=Set(TemplateParameter "T"),
			     scope=XMI.InstanceScope,
			     visibility=XMI.public
			    },
			    {
			     arguments=[("object",TemplateParameter "T")],
			     isQuery=true,
			     name="including",
			     postcondition=[],
			     precondition=[],
			     result=Bag(TemplateParameter "T"),
			     scope=XMI.InstanceScope,
			     visibility=XMI.public
			    },
			    {
			     arguments=[("object",TemplateParameter "T")],
			     isQuery=true,
			     name="excluding",
			     postcondition=[],
			     precondition=[],
			     result=Bag(TemplateParameter "T"),
			     scope=XMI.InstanceScope,
			     visibility=XMI.public
			    },
			    {
			     arguments=[("object",TemplateParameter "T")],
			     isQuery=true,
			     name="count",
			     postcondition=[],
			     precondition=[],
			     result=Integer,
			     scope=XMI.InstanceScope,
			     visibility=XMI.public
			    },
			    {
			     arguments=[],
			     isQuery=true,
			     name="flatten",
			     postcondition=[],
			     precondition=[],
			     result=Bag(TemplateParameter "T"),
			     scope=XMI.InstanceScope,
			     visibility=XMI.public
			    },
			    {
			     arguments=[],
			     isQuery=true,
			     name="asSet",
			     postcondition=[],
			     precondition=[],
			     result=Set(TemplateParameter "T"),
			     scope=XMI.InstanceScope,
			     visibility=XMI.public
			    },
			    {
			     arguments=[],
			     isQuery=true,
			     name="asSequence",
			     postcondition=[],
			     precondition=[],
			     result=Sequence(TemplateParameter "T"),
			     scope=XMI.InstanceScope,
			     visibility=XMI.public
			    },
			    {
			     arguments=[],
			     isQuery=true,
			     name="asBag",
			     postcondition=[],
			     precondition=[],
			     result=Bag(TemplateParameter "T"),
			     scope=XMI.InstanceScope,
			     visibility=XMI.public}],
			    parent=NONE,
			    stereotypes=[],
			    thyname=NONE
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
			     interfaces=[],invariant=[],
			     name=Set (TemplateParameter "T"),
			     operations=[
			     {
			      arguments=[("s",Set(TemplateParameter "T"))],
			      isQuery=true,
			      name="union",
			      postcondition=[],
			      precondition=[],
			      result=Set(TemplateParameter "T"),
			      scope=XMI.InstanceScope,
			      visibility=XMI.public
			     },
			     {
			      arguments=[],
			      isQuery=true,
			      name="notEmpty",
			      postcondition=[],
			      precondition=[],
			      result=Boolean,
			      scope=XMI.InstanceScope,
			      visibility=XMI.public
			     },
			     {
			      arguments=[],
			      isQuery=true,
			      name="size",
			      postcondition=[],
			      precondition=[],
			      result=Integer,
			      scope=XMI.InstanceScope,
			      visibility=XMI.public
			     },
			     {
			      arguments=[("bag",Bag(TemplateParameter "T"))],
			      isQuery=true,
			      name="union",
			      postcondition=[],
			      precondition=[],
			      result=Bag(TemplateParameter "T"),
			      scope=XMI.InstanceScope,
			      visibility=XMI.public
			     },
			     {
			      arguments=[("s",Set(TemplateParameter "T"))],
			      isQuery=true,
			      name="=",
			      postcondition=[],
			      precondition=[],
			      result=Boolean,
			      scope=XMI.InstanceScope,
			      visibility=XMI.public
			     },
			     {
			      arguments=[("s",Set(TemplateParameter "T"))],
			      isQuery=true,
			      name="==",
			      postcondition=[],
			      precondition=[],
			      result=Boolean,
			      scope=XMI.InstanceScope,
			      visibility=XMI.public
			     },
{
			      arguments=[],
			      isQuery=true,
			      name="isEmpty",
			      postcondition=[],
			      precondition=[],
			      result=Boolean,
			      scope=XMI.InstanceScope,
			      visibility=XMI.public
			     },
			     {
			      arguments=[("s",Set(TemplateParameter "T"))],
			      isQuery=true,
			      name="~=",
			      postcondition=[],
			      precondition=[],
			      result=Boolean,
			      scope=XMI.InstanceScope,
			      visibility=XMI.public
			     },
			     {
			      arguments=[("s",Set(TemplateParameter "T"))],
			      isQuery=true,
			      name="~==",
			      postcondition=[],
			      precondition=[],
			      result=Boolean,
			      scope=XMI.InstanceScope,
			      visibility=XMI.public
			     },
			     {
			      arguments=[("s",Set(TemplateParameter "T"))],
			      isQuery=true,
			      name="~=~",
			      postcondition=[],
			      precondition=[],
			      result=Boolean,
			      scope=XMI.InstanceScope,
			      visibility=XMI.public
			     },
			     {
			      arguments=[("s",Set(TemplateParameter "T"))],
			      isQuery=true,
			      name="~==~",
			      postcondition=[],
			      precondition=[],
			      result=Boolean,
			      scope=XMI.InstanceScope,
			      visibility=XMI.public
			     },
			     {
			      arguments=[("s",Set(TemplateParameter "T"))],
			      isQuery=true,
			      name="intersection",
			      postcondition=[],
			      precondition=[],
			      result=Set(TemplateParameter "T"),
			      scope=XMI.InstanceScope,
			      visibility=XMI.public
			     },
			     {
			      arguments=[("bag",Bag(TemplateParameter "T"))],
			      isQuery=true,
			      name="intersection",
			      postcondition=[],
			      precondition=[],
			      result=Set(TemplateParameter "T"),
			      scope=XMI.InstanceScope,
			      visibility=XMI.public
			     },
			     {
			      arguments=[("s",Set(TemplateParameter "T"))],
			      isQuery=true,
			      name="-",
			      postcondition=[],
			      precondition=[],
			      result=Set(TemplateParameter "T"),
			      scope=XMI.InstanceScope,
			      visibility=XMI.public
			     },
			     {
			      arguments=[("object",TemplateParameter "T")],
			      isQuery=true,
			      name="including",
			      postcondition=[],
			      precondition=[],
			      result=Set(TemplateParameter "T"),
			      scope=XMI.InstanceScope,
			      visibility=XMI.public
			     },
			     {
			      arguments=[("object",TemplateParameter "T")],
			      isQuery=true,
			      name="excluding",
			      postcondition=[],
			      precondition=[],
			      result=Set(TemplateParameter "T"),
			      scope=XMI.InstanceScope,
			      visibility=XMI.public
			     },
			     {
			      arguments=[("s",Set(TemplateParameter "T"))],
			      isQuery=true,
			      name="symmetricDifference",
			      postcondition=[],
			      precondition=[],
			      result=Set(TemplateParameter "T"),
			      scope=XMI.InstanceScope,
			      visibility=XMI.public
			     },
			     {
			      arguments=[("object",TemplateParameter "T")],
			      isQuery=true,
			      name="count",
			      postcondition=[],
			      precondition=[],
			      result=Integer,
			      scope=XMI.InstanceScope,
			      visibility=XMI.public
			     },
			     {
			      arguments=[],
			      isQuery=true,
			      name="flatten",
			      postcondition=[],
			      precondition=[],
			      result=Set(TemplateParameter "T"),
			      scope=XMI.InstanceScope,
			      visibility=XMI.public
			     },
			     {
			      arguments=[],
			      isQuery=true,
			      name="asSet",
			      postcondition=[],
			      precondition=[],
			      result=Set(TemplateParameter "T"),
			      scope=XMI.InstanceScope,
			      visibility=XMI.public
			     },
			     {
			      arguments=[],
			      isQuery=true,
			      name="asSequence",
			      postcondition=[],
			      precondition=[],
			      result=Sequence(TemplateParameter "T"),
			      scope=XMI.InstanceScope,
			      visibility=XMI.public
			     },

			     {
			      arguments=[],
			      isQuery=true,
			      name="asBag",
			      postcondition=[],
			      precondition=[],
			      result=Bag(TemplateParameter "T"),
			      scope=XMI.InstanceScope,
			      visibility=XMI.public
			     },

			     {
			      arguments=[("set",Set(TemplateParameter "T"))],
			      isQuery=true,
			      name="union",
			      postcondition=[],
			      precondition=[],
			      result=Bag(TemplateParameter "T"),
			      scope=XMI.InstanceScope,
			      visibility=XMI.public}],
			     parent=NONE,
			     stereotypes=[],
			     thyname=NONE
			    }: Rep_Core.Classifier
	 },Template
	 {
	  parameter = TemplateParameter "T",
          classifier =  Class
			    {
			     attributes=[],
			     associationends=[],
			     activity_graphs=[],
			     interfaces=[],
			     invariant=[],
			     name=OrderedSet (TemplateParameter "T"),
			     operations=[
			     {
			      arguments=[("s",OrderedSet(TemplateParameter "T"))],
			      isQuery=true,
			      name="union",
			      postcondition=[],
			      precondition=[],
			      result=OrderedSet(TemplateParameter "T"),
			      scope=XMI.InstanceScope,
			      visibility=XMI.public
			     },
			     {
			      arguments=[],
			      isQuery=true,
			      name="notEmpty",
			      postcondition=[],
			      precondition=[],
			      result=Boolean,
			      scope=XMI.InstanceScope,
			      visibility=XMI.public
			     },
			     {
			      arguments=[],
			      isQuery=true,
			      name="size",
			      postcondition=[],
			      precondition=[],
			      result=Integer,
			      scope=XMI.InstanceScope,
			      visibility=XMI.public
			     },
			     {
			      arguments=[("bag",Bag(TemplateParameter "T"))],
			      isQuery=true,
			      name="union",
			      postcondition=[],
			      precondition=[],
			      result=Bag(TemplateParameter "T"),
			      scope=XMI.InstanceScope,
			      visibility=XMI.public
			     },
			     {
			      arguments=[("s",OrderedSet(TemplateParameter "T"))],
			      isQuery=true,name="=",
			      postcondition=[],
			      precondition=[],
			      result=Boolean,
			      scope=XMI.InstanceScope,
			      visibility=XMI.public
			     },
			     {
			      arguments=[("s",OrderedSet(TemplateParameter "T"))],
			      isQuery=true,
			      name="==",
			      postcondition=[],
			      precondition=[],
			      result=Boolean,
			      scope=XMI.InstanceScope,
			      visibility=XMI.public
			     },
{
			      arguments=[],
			      isQuery=true,
			      name="isEmpty",
			      postcondition=[],
			      precondition=[],
			      result=Boolean,
			      scope=XMI.InstanceScope,
			      visibility=XMI.public
			     },
			     {
			      arguments=[("s",OrderedSet(TemplateParameter "T"))],
			      isQuery=true,name="~=",
			      postcondition=[],
			      precondition=[],
			      result=Boolean,
			      scope=XMI.InstanceScope,
			      visibility=XMI.public
			     },
			     {
			      arguments=[("s",OrderedSet(TemplateParameter "T"))],
			      isQuery=true,
			      name="~==",
			      postcondition=[],
			      precondition=[],
			      result=Boolean,
			      scope=XMI.InstanceScope,
			      visibility=XMI.public
			     },
			     {
			      arguments=[("s",OrderedSet(TemplateParameter "T"))],
			      isQuery=true,
			      name="~=~",
			      postcondition=[],
			      precondition=[],
			      result=Boolean,
			      scope=XMI.InstanceScope,
			      visibility=XMI.public
			     },

			     {
			      arguments=[("s",OrderedSet(TemplateParameter "T"))],
			      isQuery=true,
			      name="~==~",
			      postcondition=[],
			      precondition=[],
			      result=Boolean,
			      scope=XMI.InstanceScope,
			      visibility=XMI.public
			     },
			     {
			      arguments=[("s",OrderedSet(TemplateParameter "T"))],
			      isQuery=true,
			      name="intersection",
			      postcondition=[],
			      precondition=[],
			      result=OrderedSet(TemplateParameter "T"),
			      scope=XMI.InstanceScope,
			      visibility=XMI.public
			     },
			     {
			      arguments=[("bag",Bag(TemplateParameter "T"))],
			      isQuery=true,
			      name="intersection",
			      postcondition=[],
			      precondition=[],
			      result=OrderedSet(TemplateParameter "T"),
			      scope=XMI.InstanceScope,
			      visibility=XMI.public
			     },
			     {
			      arguments=[("s",OrderedSet(TemplateParameter "T"))],
			      isQuery=true,
			      name="-",
			      postcondition=[],
			      precondition=[],
			      result=OrderedSet(TemplateParameter "T"),
			      scope=XMI.InstanceScope,
			      visibility=XMI.public
			     },
			     {
			      arguments=[("object",TemplateParameter "T")],
			      isQuery=true,name="including",postcondition=[],
			      precondition=[],result=Set(TemplateParameter "T"),
			      scope=XMI.InstanceScope,visibility=XMI.public
			     },
			     {
			      arguments=[("object",TemplateParameter "T")],
			      isQuery=true,name="excluding",postcondition=[],
			      precondition=[],result=Set(TemplateParameter "T"),
			      scope=XMI.InstanceScope,visibility=XMI.public
			     },
			     {
			      arguments=[("s",OrderedSet(TemplateParameter "T"))],
			      isQuery=true,name="symmetricDifference",postcondition=[],
			      precondition=[],result=Set(TemplateParameter "T"),
			      scope=XMI.InstanceScope,visibility=XMI.public
			     },
			     {
			      arguments=[("object",TemplateParameter "T")],
			      isQuery=true,name="count",postcondition=[],precondition=[],
			      result=Integer,scope=XMI.InstanceScope,
			      visibility=XMI.public
			     },
			     {
			      arguments=[],isQuery=true,name="flatten",postcondition=[],
			      precondition=[],result=Set(TemplateParameter "T"),
			      scope=XMI.InstanceScope,visibility=XMI.public
			     },
			     {
			      arguments=[],isQuery=true,name="asSet",postcondition=[],
			      precondition=[],result=Set(TemplateParameter "T"),
			      scope=XMI.InstanceScope,visibility=XMI.public
			     },
			     {
			      arguments=[],isQuery=true,name="asSequence",
			      postcondition=[],precondition=[],
			      result=Sequence(TemplateParameter "T"),
			      scope=XMI.InstanceScope,visibility=XMI.public
			     },

			     {
			      arguments=[],isQuery=true,name="asBag",postcondition=[],
			      precondition=[],result=Bag(TemplateParameter "T"),
			      scope=XMI.InstanceScope,visibility=XMI.public
			     },

			     {
			      arguments=[("oset",OrderedSet(TemplateParameter "T"))],
			      isQuery=true,name="union",postcondition=[],precondition=[],
			      result=Bag(TemplateParameter "T"),
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
			     interfaces=[],invariant=[],name=Collection(TemplateParameter "T"),
			     operations=[
			     {
			      arguments=[],isQuery=true,name="size",postcondition=[],
			      precondition=[],result=Integer,
			      scope=XMI.InstanceScope,visibility=XMI.public
			     },
			     {
			      arguments=[],isQuery=true,name="modifiedOnly",
			      postcondition=[],precondition=[],
			      result=Boolean,scope=XMI.InstanceScope,
			      visibility=XMI.public
			     },
			     {
			      arguments=[("object",TemplateParameter "T")],
			      isQuery=true,name="includes",postcondition=[],
			      precondition=[],result=Boolean,
			      scope=XMI.InstanceScope,visibility=XMI.public
			     },
			     {
			      arguments=[("object",TemplateParameter "T")],
			      isQuery=true,name="excludes",postcondition=[],
			      precondition=[],result=Boolean,
			      scope=XMI.InstanceScope,visibility=XMI.public
			     },
			     {
			      arguments=[("object",TemplateParameter "T")],
			      isQuery=true,name="count",postcondition=[],precondition=[],
			      result=Integer,scope=XMI.InstanceScope,
			      visibility=XMI.public
			     },
			     {
			      arguments=[("c",Collection(TemplateParameter "T"))],
			      isQuery=true,name="includesAll",postcondition=[],
			      precondition=[],result=Boolean,
			      scope=XMI.InstanceScope,visibility=XMI.public
			     },
			     {
			      arguments=[("c",Collection(TemplateParameter "T"))],
			      isQuery=true,name="excludesAll",postcondition=[],
			      precondition=[],result=Boolean,
			      scope=XMI.InstanceScope,visibility=XMI.public
			     },
			     {
			      arguments=[],isQuery=true,name="isEmpty",postcondition=[],
			      precondition=[],result=Boolean,
			      scope=XMI.InstanceScope,visibility=XMI.public
			     },
			     {
			      arguments=[],isQuery=true,name="notEmpty",postcondition=[],
			      precondition=[],result=Boolean,
			      scope=XMI.InstanceScope,visibility=XMI.public
			     },
			     {
			      arguments=[],isQuery=true,name="sum",postcondition=[],
			      precondition=[],result=Integer,
			      scope=XMI.InstanceScope,visibility=XMI.public
			     },
			     {
			      arguments=[("c2",
					  Collection(TemplateParameter "T"))],
			      isQuery=true,name="product",postcondition=[],
			      precondition=[],result=Set(TemplateParameter "T"),
			      scope=XMI.InstanceScope,visibility=XMI.public}],parent=NONE,
			     stereotypes=[],thyname=NONE
			    }: Rep_Core.Classifier
	 },
     Class
	 {
	  attributes=[],
	  associationends=[],
	  activity_graphs=[],	  
	  interfaces=[],
	  invariant=[],
	  name = OclVoid,
	  operations=[],
	  parent= SOME OclAny,
	  stereotypes=[],
	  thyname=NONE
	 },

     Primitive
	 {
	  associationends=[],interfaces=[],invariant=[],name=OclAny,
	  operations=[
	  {
	   arguments=[("object",OclAny)],
           isQuery=true,name="=",postcondition=[],precondition=[],
           result=Boolean,scope=XMI.InstanceScope,
           visibility=XMI.public
	  },
          {
	   arguments=[("object",OclAny)],
           isQuery=true,name="==",postcondition=[],precondition=[],
           result=Boolean,scope=XMI.InstanceScope,
           visibility=XMI.public
	  },
          {
	   arguments=[("object",OclAny)],
           isQuery=true,name="~=",postcondition=[],precondition=[],
           result=Boolean,scope=XMI.InstanceScope,
           visibility=XMI.public
	  },
          {
	   arguments=[("object",OclAny)],
           isQuery=true,name="~==",postcondition=[],precondition=[],
           result=Boolean,scope=XMI.InstanceScope,
           visibility=XMI.public
	  },
          {
	   arguments=[("object",OclAny)],
           isQuery=true,name="~=~",postcondition=[],precondition=[],
           result=Boolean,scope=XMI.InstanceScope,
           visibility=XMI.public
	  },
          {
	   arguments=[("object",OclAny)],
           isQuery=true,name="~==~",postcondition=[],precondition=[],
           result=Boolean,scope=XMI.InstanceScope,
           visibility=XMI.public
	  },
          {
	   arguments=[("object",OclAny)],
           isQuery=true,name="<>",postcondition=[],precondition=[],
           result=Boolean,scope=XMI.InstanceScope,
           visibility=XMI.public
	  },
          {
	   arguments=[],isQuery=true,name="oclIsNew",postcondition=[],
           precondition=[],result=Boolean,
           scope=XMI.InstanceScope,visibility=XMI.public
	  },
          {
	   arguments=[],isQuery=true,name="oclIsUndefined",
           postcondition=[],precondition=[],
           result=Boolean,scope=XMI.InstanceScope,
           visibility=XMI.public
	  },
          {
	   arguments=[],isQuery=true,name="asSet",postcondition=[],
           precondition=[],result=Set(TemplateParameter "T"),
           scope=XMI.InstanceScope,visibility=XMI.public
	  },
          {
	   arguments=[],isQuery=true,name="atPre",postcondition=[],
           precondition=[],result=OclAny,
           scope=XMI.InstanceScope,visibility=XMI.public
	  },
          {
	   arguments=[("statename",Classifier ["OclState"])],
           isQuery=true,name="oclIsInState",postcondition=[],
           precondition=[],result=Boolean,
           scope=XMI.InstanceScope,visibility=XMI.public
	  },
          {
	   arguments=[],isQuery=true,name="allInstances",
           postcondition=[],precondition=[],
           result=Set(OclAny),
           scope=XMI.ClassifierScope,visibility=XMI.public}],parent=NONE,
	  stereotypes=[],thyname=NONE
	 },
     Primitive
	 {
	  associationends=[],interfaces=[],invariant=[],name=String,
	  operations=[
	  {
	   arguments=[],isQuery=true,name="size",postcondition=[],
           precondition=[],result=Integer,
           scope=XMI.InstanceScope,visibility=XMI.public
	  },
          {
	   arguments=[("s",String)],
           isQuery=true,name="concat",postcondition=[],
           precondition=[],result=String,
           scope=XMI.InstanceScope,visibility=XMI.public
	  },
          {
	   arguments=[("lower",Integer),
		      ("upper",Integer)],
           isQuery=true,name="substring",postcondition=[],
           precondition=[],result=String,
           scope=XMI.InstanceScope,visibility=XMI.public
	  },
          {
	   arguments=[],isQuery=true,name="toInteger",
           postcondition=[],precondition=[],
           result=Integer,scope=XMI.InstanceScope,
           visibility=XMI.public
	  },
          {
	   arguments=[],isQuery=true,name="toReal",postcondition=[],
           precondition=[],result=Real,
           scope=XMI.InstanceScope,visibility=XMI.public
	  },
          {
	   arguments=[],isQuery=true,name="toUpper",postcondition=[],
           precondition=[],result=String,
           scope=XMI.InstanceScope,visibility=XMI.public
	  },
          {
	   arguments=[],isQuery=true,name="toLower",postcondition=[],
           precondition=[],result=String,
           scope=XMI.InstanceScope,visibility=XMI.public}],
	  parent= SOME OclAny,
	  stereotypes=[],
	  thyname=NONE
	 },
     Primitive
	 {
	  associationends=[],interfaces=[],invariant=[],name=Boolean,
	  operations=[
	  {
	   arguments=[("b",Boolean)],
           isQuery=true,name="or",postcondition=[],precondition=[],
           result=Boolean,scope=XMI.InstanceScope,
           visibility=XMI.public
	  },
          {
	   arguments=[("b",Boolean)],
           isQuery=true,name="xor",postcondition=[],precondition=[],
           result=Boolean,scope=XMI.InstanceScope,
           visibility=XMI.public
	  },
          {
	   arguments=[("b",Boolean)],
           isQuery=true,name="and",postcondition=[],precondition=[],
           result=Boolean,scope=XMI.InstanceScope,
           visibility=XMI.public
	  },
          {
	   arguments=[("b",Boolean)],
           isQuery=true,name="implies",postcondition=[],
           precondition=[],result=Boolean,
           scope=XMI.InstanceScope,visibility=XMI.public
	  },
          {
	   arguments=[],isQuery=true,name="not",postcondition=[],
           precondition=[],result=Boolean,
           scope=XMI.InstanceScope,visibility=XMI.public
	  },
          {
	   arguments=[("b",Boolean)],
           isQuery=true,name="sor",postcondition=[],precondition=[],
           result=Boolean,scope=XMI.InstanceScope,
           visibility=XMI.public
	  },
          {
	   arguments=[("b",Boolean)],
           isQuery=true,name="sxor",postcondition=[],precondition=[],
           result=Boolean,scope=XMI.InstanceScope,
           visibility=XMI.public
	  },
          {
	   arguments=[("b",Boolean)],
           isQuery=true,name="sand",postcondition=[],precondition=[],
           result=Boolean,scope=XMI.InstanceScope,
           visibility=XMI.public
	  },
          {
	   arguments=[("b",Boolean)],
           isQuery=true,name="simplies",postcondition=[],
           precondition=[],result=Boolean,
           scope=XMI.InstanceScope,visibility=XMI.public
	  },
          {
	   arguments=[("b",Boolean)],
           isQuery=true,name="implies1",postcondition=[],
           precondition=[],result=Boolean,
           scope=XMI.InstanceScope,visibility=XMI.public
	  },
          {
	   arguments=[("b",Boolean)],
           isQuery=true,name="implies2",postcondition=[],
           precondition=[],result=Boolean,
           scope=XMI.InstanceScope,visibility=XMI.public}],
	  parent= SOME OclAny,
	  stereotypes=[],
	  thyname=NONE
	 },
     Primitive
	 {
	  associationends=[],interfaces=[],invariant=[],name=Integer,
	  operations=[
	  {
	   arguments=[("i",Integer)],
           isQuery=true,name="+",postcondition=[],precondition=[],
           result=Integer,scope=XMI.InstanceScope,
           visibility=XMI.public
	  },
          {
	   arguments=[("i",Integer)],
           isQuery=true,name="-",postcondition=[],precondition=[],
           result=Integer,scope=XMI.InstanceScope,
           visibility=XMI.public
	  },
          {
	   arguments=[("i",Integer)],
           isQuery=true,name="*",postcondition=[],precondition=[],
           result=Integer,scope=XMI.InstanceScope,
           visibility=XMI.public
	  },
          {
	   arguments=[],isQuery=true,name="-",postcondition=[],
           precondition=[],result=Integer,
           scope=XMI.InstanceScope,visibility=XMI.public
	  },
          {
	   arguments=[("i",Integer)],
           isQuery=true,name="/",postcondition=[],precondition=[],
           result=Real,scope=XMI.InstanceScope,
           visibility=XMI.public
	  },
          {
	   arguments=[("i",Integer)],
           isQuery=true,name="div",postcondition=[],precondition=[],
           result=Integer,scope=XMI.InstanceScope,
           visibility=XMI.public
	  },
          {
	   arguments=[],isQuery=true,name="abs",postcondition=[],
           precondition=[],result=Integer,
           scope=XMI.InstanceScope,visibility=XMI.public
	  },
          {
	   arguments=[("i",Integer)],
           isQuery=true,name="mod",postcondition=[],precondition=[],
           result=Integer,scope=XMI.InstanceScope,
           visibility=XMI.public
	  },
          {
	   arguments=[("i",Integer)],
           isQuery=true,name="max",postcondition=[],precondition=[],
           result=Integer,scope=XMI.InstanceScope,
           visibility=XMI.public
	  },
          {
	   arguments=[("i",Integer)],
           isQuery=true,name="min",postcondition=[],precondition=[],
           result=Integer,scope=XMI.InstanceScope,
           visibility=XMI.public}],
	  parent= SOME Real,
	  stereotypes=[],
	  thyname=NONE
	 },
     Primitive
	 {
	  associationends=[],interfaces=[],invariant=[],name=Real,
	  operations=[
	  {
	   arguments=[("r",Real)],
           isQuery=true,name="+",postcondition=[],precondition=[],
           result=Real,scope=XMI.InstanceScope,
           visibility=XMI.public
	  },
          {
	   arguments=[("r",Real)],
           isQuery=true,name="-",postcondition=[],precondition=[],
           result=Real,scope=XMI.InstanceScope,
           visibility=XMI.public
	  },
          {
	   arguments=[("r",Real)],
           isQuery=true,name="*",postcondition=[],precondition=[],
           result=Real,scope=XMI.InstanceScope,
           visibility=XMI.public
	  },
          {
	   arguments=[],isQuery=true,name="-",postcondition=[],
           precondition=[],result=Real,
           scope=XMI.InstanceScope,visibility=XMI.public
	  },
          {
	   arguments=[("r",Real)],
           isQuery=true,name="/",postcondition=[],precondition=[],
           result=Real,scope=XMI.InstanceScope,
           visibility=XMI.public
	  },
          {
	   arguments=[],isQuery=true,name="abs",postcondition=[],
           precondition=[],result=Real,
           scope=XMI.InstanceScope,visibility=XMI.public
	  },
          {
	   arguments=[],isQuery=true,name="floor",postcondition=[],
           precondition=[],result=Integer,
           scope=XMI.InstanceScope,visibility=XMI.public
	  },
          {
	   arguments=[],isQuery=true,name="round",postcondition=[],
           precondition=[],result=Integer,
           scope=XMI.InstanceScope,visibility=XMI.public
	  },
          {
	   arguments=[("r",Real)],
           isQuery=true,name="max",postcondition=[],precondition=[],
           result=Real,scope=XMI.InstanceScope,
           visibility=XMI.public
	  },
          {
	   arguments=[("r",Real)],
           isQuery=true,name="min",postcondition=[],precondition=[],
           result=Real,scope=XMI.InstanceScope,
           visibility=XMI.public
	  },
          {
	   arguments=[("r",Real)],
           isQuery=true,name="<",postcondition=[],precondition=[],
           result=Boolean,scope=XMI.InstanceScope,
           visibility=XMI.public
	  },
          {
	   arguments=[("r",Real)],
           isQuery=true,name=">",postcondition=[],precondition=[],
           result=Boolean,scope=XMI.InstanceScope,
           visibility=XMI.public
	  },
          {
	   arguments=[("r",Real)],
           isQuery=true,name="<=",postcondition=[],precondition=[],
           result=Boolean,scope=XMI.InstanceScope,
           visibility=XMI.public
	  },
          {
	   arguments=[("r",Real)],
           isQuery=true,name=">=",postcondition=[],precondition=[],
           result=Boolean,scope=XMI.InstanceScope,
           visibility=XMI.public}],
	  parent= SOME OclAny,
	  stereotypes=[],
	  thyname=NONE
	 }
    ] : Rep_Core.Classifier list

fun is_oclLib c = let
    fun listin _ []  = false
      | listin e (x::xs) = if e=x then true else listin e xs
in
    listin c oclLib 
end
end
