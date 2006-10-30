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


structure SecureUML2HolOcl:sig 
    val transform : Rep_SecureUML_ComponentUML.Model -> Rep.Model 
end = struct 
open Rep_Core
open XMI_DataTypes
open Rep_OclTerm


(** capitalize the string s.
 * Returns the given string with the first letter changed to upper case
 * Should be moved to a helper library?
 *)
fun capitalize s = let val slist = String.explode s
                   in 
                       String.implode (Char.toUpper (List.hd slist)::List.tl slist)
                   end
(* gives the type of an OCL expression *)
fun type_of (Literal (_,t))                = t
  | type_of (CollectionLiteral (_,t))      = t
  | type_of (If(_,_,_,_,_,_,t))            = t
  | type_of (AssociationEndCall(_,_,_,t))  = t
  | type_of (AttributeCall(_,_,_,t))       = t
  | type_of (OperationCall(_,_,_,_,t))     = t
  | type_of (OperationWithType(_,_,_,_,t)) = t
  | type_of (Variable(_,t))                = t
  | type_of (Let(_,_,_,_,_,t))             = t
  | type_of (Iterate(_,_,_,_,_,_,_,_,t))   = t
  | type_of (Iterator(_,_,_,_,_,_,t))      = t

fun atpre exp = OperationCall (exp, type_of exp,
                               ["oclLib","OclAny","atPre"],
                               nil,
                               type_of exp)

fun deep_atpre (t as Literal _) = t
  | deep_atpre (t as CollectionLiteral _) = t
  | deep_atpre (t as If (cond,cond_type,then_term,then_type,
                                 else_term,else_type,result_type) )
    = If (deep_atpre cond, cond_type,
          deep_atpre then_term, then_type,
          deep_atpre else_term, else_type,
          result_type)
  | deep_atpre (t as AssociationEndCall (source,source_type, 
                                         path, result_type))
    = atpre (AssociationEndCall (deep_atpre source, 
                                 source_type, path, result_type))
  | deep_atpre (t as AttributeCall (source,source_type,path,result_type))
    = atpre (AttributeCall (deep_atpre source, source_type,
                            path, result_type))
  | deep_atpre (t as OperationCall (source, source_type, path, args, result_type)) 
    = atpre (OperationCall (deep_atpre source, source_type, path,
                            ListPair.zip (map (deep_atpre o #1) args, map #2 args),
                            result_type))
  | deep_atpre (t as OperationWithType (source, source_type, param, param_type,
                                        result_type))
    (* FIXME: do we have to wrap it with atPre? *)
    = OperationWithType (deep_atpre source, source_type, param, param_type,
                         result_type)
  | deep_atpre (t as Variable _) = t
  | deep_atpre (t as Let (var,var_type,rhs,rhs_type,in_term,in_type)) 
    = Let (var,var_type,
           deep_atpre rhs, rhs_type,
           deep_atpre in_term, in_type)
  | deep_atpre (t as Iterate (vars,acc,acc_type,acc_init,source,source_type,
                              body, body_type, result_type)) 
    = Iterate (vars, acc, acc_type, deep_atpre acc_init,
               deep_atpre source, source_type,
               deep_atpre body, body_type,
               result_type)
  | deep_atpre (t as Iterator (name, vars, source, source_type,
                               body, body_type, result_type)) 
    = Iterator (name, vars, deep_atpre source, source_type,
               deep_atpre body, body_type, result_type)



(* FIXME: substitute operation, attribute, *)
(* and associationend calls with @pre variants *)
fun transform_postcond sc (s,t) = (s,deep_atpre t)
  
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




(** replace all operation calls in the given expression with calls to 
 * the "secured" variant. 
 *)
fun replace_opcalls (t as Literal _) = t
  | replace_opcalls (t as CollectionLiteral _) = t
  | replace_opcalls (t as If (cond,ctype,then_term,then_type,
                              else_term,else_type,result_type)) = 
    If (replace_opcalls cond,      ctype,
        replace_opcalls then_term, then_type,
        replace_opcalls else_term, else_type,
        result_type)
  | replace_opcalls (t as AssociationEndCall (source,source_type,path,result_type)) =
    AssociationEndCall (replace_opcalls source, source_type,
                        path, result_type)
  | replace_opcalls (t as AttributeCall (source,source_type,path,result_type)) =
    AttributeCall (replace_opcalls source, source_type, path, result_type)
  | replace_opcalls (t as OperationCall (source,source_type,path,args,result_type))=
    OperationCall (replace_opcalls source, source_type,
                   let val oper = List.hd (List.rev path)
                       val pth  = List.tl (List.rev path)
                   in 
                       List.rev (oper^"_sec"::pth)
                   end,
                   ListPair.zip (map (replace_opcalls o #1) args,
                             map #2 args),
                   result_type)
  | replace_opcalls (t as OperationWithType (source,source_type,
                                             param,param_type,result_type)) =
    OperationWithType (replace_opcalls source, source_type,
                       param, param_type, result_type)
  | replace_opcalls (t as Variable _) = t
  | replace_opcalls (t as Let (var,var_type,rhs,rhs_type,in_term,in_type)) =
    Let (var,var_type,
         replace_opcalls rhs, rhs_type,
         replace_opcalls in_term, in_type)
  | replace_opcalls (t as Iterate (vars,acc,acc_type,acc_init,source,source_type, 
                                   body, body_type, result_type)) =
    Iterate (vars,
             acc,acc_type, replace_opcalls acc_init,
             replace_opcalls source, source_type,
             replace_opcalls body, body_type,
             result_type)
  | replace_opcalls (t as Iterator (iter,vars,source,source_type,body,body_type,
                                   result_type))=
    Iterator (iter,vars,
              replace_opcalls source, source_type,
              replace_opcalls body, body_type,
              result_type)

(** replace all attribute calls in the given expression with calls to the 
 * corresponding getter operation.
 *)
fun replace_attcalls (t as Literal _) = t
  | replace_attcalls (t as CollectionLiteral _) = t
  | replace_attcalls (t as If (cond,ctype,then_term,then_type,
                              else_term,else_type,result_type)) = 
    If (replace_attcalls cond,      ctype,
        replace_attcalls then_term, then_type,
        replace_attcalls else_term, else_type,
        result_type)
  | replace_attcalls (t as AssociationEndCall (source,source_type,path,result_type)) =
    AssociationEndCall (replace_attcalls source, source_type,
                        path, result_type)
  | replace_attcalls (t as AttributeCall (source,source_type,path,result_type)) =
    OperationCall (replace_attcalls source, source_type, 
                   let val att = List.hd (List.rev path)
                       val pth = List.tl (List.rev path)
                   in 
                       List.rev ("get"^(capitalize att)::pth)
                   end, 
                   nil, (* the getter has no arguments *)
                   result_type)
  | replace_attcalls (t as OperationCall (source,source_type,path,args,result_type))=
    OperationCall (replace_attcalls source, source_type,
                   path,args, result_type)
  | replace_attcalls (t as OperationWithType (source,source_type,
                                             param,param_type,result_type)) =
    OperationWithType (replace_attcalls source, source_type,
                       param, param_type, result_type)
  | replace_attcalls (t as Variable _) = t
  | replace_attcalls (t as Let (var,var_type,rhs,rhs_type,in_term,in_type)) =
    Let (var,var_type,
         replace_attcalls rhs, rhs_type,
         replace_attcalls in_term, in_type)
  | replace_attcalls (t as Iterate (vars,acc,acc_type,acc_init,source,source_type, 
                                   body, body_type, result_type)) =
    Iterate (vars,
             acc,acc_type, replace_attcalls acc_init,
             replace_attcalls source, source_type,
             replace_attcalls body, body_type,
             result_type)
  | replace_attcalls (t as Iterator (iter,vars,source,source_type,body,body_type,
                                   result_type))=
    Iterator (iter,vars,
              replace_attcalls source, source_type,
              replace_attcalls body, body_type,
              result_type)

(** creates a getter function for the given attribute.
 * The name of the function is get<Attributename>, and it has 
 * a postcondition of result=self.att
 * should be moved to Rep_Core?
 *)
fun create_getter c {name,attr_type,visibility,scope,stereotypes,init} =
    { name="get"^(capitalize name),
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

(** creates a setter function for the given attribute.
 * The name of the function is set<Attributename>, and it has 
 * a postcondition of self.att=arg and self.att->modifiedOnly()
 * Should be moved to rep_core?
 *)
fun create_setter c {name,attr_type,visibility,scope,stereotypes,init} =
    { name="set"^(capitalize name),
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

(** creates a "secured" version of the given operation.
 * The main change: in the postcondition, attribute calls are replaces with 
 * calls to appropriate getter functions, and operation calls with calls 
 * to corresponding "secured" operations.
 *)
fun create_secured {name, precondition, postcondition, arguments, result,
                    isQuery, scope, visibility} =
    { name=name^"_sec",
      precondition=precondition,
      postcondition=map (fn (name,t) => (name,replace_attcalls (replace_opcalls t))) 
                        postcondition, 
      arguments=arguments,
      result=result,
      isQuery=isQuery,
      scope=scope,
      visibility=public
    }
      

(** adds an operation to a classifier.
 * Should be moved to Rep_Core?
 *)
fun add_operation_to_classifier oper (Class {name, parent, attributes, 
                                             operations, associationends, 
                                             invariant, stereotypes, 
                                             interfaces, thyname, activity_graphs})
  = Class {name=name, parent=parent, attributes=attributes, 
           operations=oper::operations, 
           associationends=associationends, invariant=invariant, 
           stereotypes=stereotypes, interfaces=interfaces, 
           thyname=thyname, activity_graphs=activity_graphs}
  | add_operation_to_classifier oper (Interface {name, parents, operations,  
                                                 invariant, stereotypes,  thyname})
    = Interface {name=name, parents=parents, operations=oper::operations,
                invariant=invariant, stereotypes=stereotypes, thyname=thyname}
  | add_operation_to_classifier oper (Enumeration {name, parent, operations,
                                                   literals, invariant, stereotypes,
                                                   interfaces, thyname})
    = Enumeration{name=name, parent=parent, operations=oper::operations, 
                  literals=literals, invariant=invariant, stereotypes=stereotypes,
                  interfaces=interfaces, thyname=thyname}
  | add_operation_to_classifier oper (Primitive {name, parent, operations, 
                                                 associationends, invariant, 
                                                 stereotypes, interfaces, thyname})
    = Primitive{name=name, parent=parent, operations=oper::operations, 
                associationends=associationends, invariant=invariant, 
                stereotypes=stereotypes, interfaces=interfaces, thyname=thyname}
  | add_operation_to_classifier oper (Template {parameter, classifier}) 
    = Template { parameter=parameter, 
                 classifier=add_operation_to_classifier oper classifier
               }

fun add_operations sc c = 
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
        val getters = map (create_getter c) (attributes_of c)
        val setters = map (create_setter c) (attributes_of c)
        val secured_ops = map create_secured (operations_of c)
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

(* FIXME: Role.allInstances().name = Bag{...} *)
(* FIXME: self.roles.name->includes(r1) implies *)
(*        self.roles.name->includes(r2) *)
fun add_role_hierarchy sc cl = cl 

fun transform (cl,sc) =
    let
        val transformed_model = transform_model sc cl
        val auth_env          = add_role_hierarchy sc (map normalize static_auth_env)
    in
         transformed_model @ auth_env
    end
    
end 
