(*****************************************************************************
 *          su4sml - a SecureUML repository for SML              
 *                                                                            
 * rep_su2holocl.sml - A SecureUML to UML/OCL model transformation
 * Copyright (C) 2001-2006  Achim D. Brucker <brucker@inf.ethz.ch>   
 *                          Jürgen Doser <doserj@inf.ethz.ch>
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

signature SECUREUML2HOLOCL =
sig
    val transform : Rep_SecureUML_ComponentUML.Model -> Rep.Model 
end

structure SecureUML2HolOcl:SECUREUML2HOLOCL= struct 
open Rep_Core
open XMI_DataTypes
open Rep_OclTerm

fun transform_postcond sc expr = expr (* FIXME: substitute operation, attribute, *)
(* and associationend calls with @pre variants *)

fun transform_postconds sc {name, precondition, postcondition, arguments, result,
                             isQuery, scope, visibility} = 
    { name=name,
      precondition=precondition,
      postcondition=map (transform_postcond sc) postcondition,
      arguments=arguments,
      result=result,
      isQuery=isQuery,
      scope=scope,
      visibility=visibility
    }

fun create_getter c {name,attr_type,visibility,scope,stereotypes,init} =
    { name="get"^name, (* FIX: capitalize first letter of name *)
      precondition=nil,
      (* post: result=self.att *)
      postcondition=[(SOME ("generated_getter_for_"^name),
                      OperationCall ( Variable ("result",attr_type), attr_type,
                                      ["oclLib","OclAny","="],
                                      [(AttributeCall ((Variable ("self", Classifier (name_of c))), Classifier (name_of c),
                                                       (name_of c)@[name],
                                                       attr_type),attr_type)],
                                      Boolean ))], 
      arguments=nil,
      result=attr_type,
      isQuery=true,
      scope=scope,
      visibility=public
    }

fun create_setter c {name,attr_type,visibility,scope,stereotypes,init} =
    { name="set"^name, (* FIX: capitalize first letter of name *)
      precondition=nil,
      (* post: self.att=arg *)
      (* FIX: and self.att->modifiedOnly() *)
      postcondition=[(SOME ("generated_setter_for_"^name),
                      OperationCall (AttributeCall ((Variable ("self", Classifier (name_of c))), Classifier (name_of c),
                                                    (name_of c)@[name],
                                                    attr_type), attr_type,
                                     ["oclLib","OclAny","="],
                                     [(Variable ("arg",attr_type),attr_type)],
                                     Boolean))], 
      arguments=[("arg",attr_type)],
      result=OclVoid,
      isQuery=false,
      scope=scope,
      visibility=public
    }

fun create_secured {name, precondition, postcondition, arguments, result,
                    isQuery, scope, visibility} =
    { name=name^"_sec",
      precondition=precondition,
      postcondition=postcondition, (* FIX: substitute attribute and operation calls *)
      arguments=arguments,
      result=result,
      isQuery=isQuery,
      scope=scope,
      visibility=public
    }
      

fun add_operation_to_classifier oper (Class {name, parent, attributes, 
                                             operations, associationends, 
                                             invariant, stereotypes, 
                                             interfaces, thyname, activity_graphs})
  = Class {name=name, parent=parent, attributes=attributes, 
           operations=oper::operations, 
           associationends=associationends, invariant=invariant, 
           stereotypes=stereotypes, interfaces=interfaces, 
           thyname=thyname, activity_graphs=activity_graphs}
    

fun add_operations sc (c as (Class {name, parent, attributes, operations, 
                                    associationends, invariant, stereotypes, 
                                    interfaces, thyname, activity_graphs})) = 
    let val constructor = {name="new",
                           precondition=nil,
                           (* FIX: result.oclIsNew() and result->modiefiedOnly() *)
                           postcondition=nil,
                           arguments=nil,
                           result=Classifier (name_of c),
                           isQuery=false,
                           scope=ClassifierScope,
                           visibility=public}
        val destructor  = {name="delete",
                           precondition=nil,
                           (* FIX: self.oclIsUndefined() and self@pre->modifiedOnly()*)
                           postcondition=nil,
                           arguments=nil,
                           result=OclVoid,
                           isQuery=false,
                           scope=InstanceScope,
                           visibility=public}
        val getters = map (create_getter c) attributes
        val setters = map (create_setter c) attributes
        val secured_ops = map create_secured operations
        val generated_ops = constructor::destructor::getters @ setters @ secured_ops  
        val access_controlled_ops = map (transform_postconds sc) generated_ops
    in 
        List.foldl (fn (oper,x) => add_operation_to_classifier oper x) c
                   access_controlled_ops 
    end
                       
(* transforming the model consists of generating new oeprations with appropriate *)
(* postconditions. *)
fun transform_model sc cl = map (add_operations sc) cl
                                
                                
val static_auth_env = [
   Class
     {activity_graphs=[],
      associationends=[{aend_type=Classifier
                                    ["AuthorizationEnvironment","Principal"],
                        init=NONE,
                        multiplicity=[(1,1)],
                        name="principal",
                        ordered=false,
                        visibility=public}],
      attributes=[],
      interfaces=[],
      invariant=[],
      name=["AuthorizationEnvironment","Context"],
      operations=[],
      parent=NONE,
      stereotypes=[],
      thyname=NONE},
   Class
     {activity_graphs=[],
      associationends=[{aend_type=Classifier
                                      ["AuthorizationEnvironment","Role"],
                        init=NONE,multiplicity=[(0,~1)],
                        name="roles",
                        ordered=false,
                        visibility=public},
                       {aend_type=Classifier
                                      ["AuthorizationEnvironment","Principal"],
                        init=NONE,
                        multiplicity=[(0,~1)],
                        name="principal",
                        ordered=false,
                        visibility=public}],
      attributes=[{attr_type=String,
                   init=NONE,name="name",
                   scope=InstanceScope,
                   stereotypes=[],
                   visibility=public}],
      interfaces=[],
      invariant=[],
      name=["AuthorizationEnvironment","Identity"],
      operations=[],
      parent=NONE,
      stereotypes=[],
      thyname=NONE},
   Class
       {activity_graphs=[],
        associationends=[{aend_type=Classifier
                                        ["AuthorizationEnvironment","Identity"],
                          init=NONE,multiplicity=[(0,~1)],
                          name="identity",
                          ordered=false,
                          visibility=public}],
        attributes=[{attr_type=String,
                     init=NONE,name="name",
                     scope=InstanceScope,
                     stereotypes=[],
                     visibility=public}],
        interfaces=[],
        invariant=[],
        name=["AuthorizationEnvironment","Role"],
        operations=[{arguments=[("s",String)],
                     isQuery=false,
                     name="getRoleByName",
                     postcondition=[],
                     precondition=[],
                     result=Classifier ["AuthorizationEnvironment","Role"],
                     scope=ClassifierScope,
                     visibility=public}],
        parent=NONE,
        stereotypes=[],
        thyname=NONE},
   Class
       {activity_graphs=[],
        associationends=[{aend_type=Classifier
                                    ["AuthorizationEnvironment","Identity"],
                          init=NONE,
                          multiplicity=[(1,1)],
                          name="identity",
                          ordered=false,
                          visibility=public},
                         {aend_type=Classifier
                                        ["AuthorizationEnvironment","Context"],
                          init=NONE,
                          multiplicity=[(0,~1)],
                          name="context",
                          ordered=false,
                          visibility=public}],
        attributes=[],
        interfaces=[],
        invariant=[],
        name=["AuthorizationEnvironment","Principal"],
        operations=[{arguments=[("s",String)],
                     isQuery=false,
                     name="isInRole",
                     postcondition=[],
                     precondition=[],
                     result=Boolean,
                     scope=InstanceScope,
                     visibility=public}],
        parent=NONE,
        stereotypes=[],
        thyname=NONE}]


fun add_role_hierarchy sc cl = cl (* FIXME: Role.allInstances().name =Bag{...} *)
(* FIXME: self.roles.name->includes(r1) implies *)
(*        self.roles.name->includes(r2) *)

fun transform (cl,sc) =
    let
        val transformed_model = transform_model sc cl
        val auth_env          = add_role_hierarchy sc (map normalize static_auth_env)
    in
         transformed_model @ auth_env
    end
    
end 
