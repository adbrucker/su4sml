(*****************************************************************************
 * su4sml --- a SML repository for managing (Secure)UML/OCL models
 *             http://projects.brucker.ch/su4sml/
 *                                                                            
 * rep_su2holocl.sml --- a SecureUML to UML/OCL model transformation
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

structure SecureUML2HolOcl:sig 
    val transform : Rep_SecureUML_ComponentUML.Model -> Rep.Model 
end = struct 
open library
open Rep_Core
open XMI_DataTypes
open Rep_OclTerm
open Rep_OclHelper
open Rep_SecureUML_ComponentUML
open StringHandling     

(* can this be expressed more combinatorially? *)
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
  | deep_atpre (t as Variable _) 
    (* FIXME: we probably want to wrap this, to get self@pre? *)
    = t
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



(* FIXME: find appropriate name for this function *)  
fun transform_postconds {name, precondition, postcondition, body,arguments, result,
                         isQuery, scope, visibility} = 
    { name=name,
      precondition=precondition,
      postcondition=map (fn (s,t) => (s,deep_atpre t)) postcondition,
      arguments=arguments,
      result=result,
      body=body,
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
      body=nil,
      (* post: result=self.att *)
      postcondition=[(SOME ("generated_getter_for_"^name),
                      ocl_eq (result attr_type) 
                             (ocl_attcall (self (Classifier (name_of c)))
                                          ((name_of c)@[name])
                                          attr_type))], 
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
    let val self_att = ocl_attcall (self (Classifier (name_of c))) 
                                   ((name_of c)@[name])
                                   attr_type
    in 
        { name="set"^(capitalize name),
          precondition=nil,
	  body=nil,
          postcondition=[(SOME ("generated_setter_for_"^name),
                          ocl_and (ocl_eq self_att
                                          (Variable ("arg", attr_type)))
                                  (ocl_modifiedOnly (ocl_set [self_att] attr_type)))
                        ],
          arguments=[("arg",attr_type)],
          result=OclVoid,
          isQuery=false,
          scope=scope,
          visibility=public
        }
    end
    
(** creates a "secured" version of the given operation.
 * The main change: in the postcondition, attribute calls are replaced with 
 * calls to appropriate getter functions, and operation calls with calls 
 * to corresponding "secured" operations.
 *)
fun create_secured {name, body,precondition, postcondition, arguments, result,
                    isQuery, scope, visibility} =
    { name=name^"_sec",
      precondition=precondition,
      postcondition=map (fn (name,t) => (name,replace_attcalls (replace_opcalls t))) 
                        postcondition, 
      arguments=arguments,
      result=result,
      body=body,
      isQuery=isQuery,
      scope=scope,
      visibility=public
    }



(** The design model transformation for a single class. 
 * generates constructors, destructors, setters, getters, and "secured" operations. 
 *)
fun add_operations c = 
    let val self_type = Classifier (name_of c) 
        val res = result (Classifier (name_of c))
        val constructor = {name="new",
                           precondition=nil,
                           (* post: result.oclIsNew() and result->modiefiedOnly() *)
                           postcondition=[(SOME "generated_constructor",
                                           ocl_and (ocl_isNew (result self_type))
                                                   (ocl_modifiedOnly (ocl_set [res] (self_type))))
                                          ],
			   body = [],
                           arguments=nil,
                           result=Classifier (name_of c),
                           isQuery=false,
                           scope=ClassifierScope,
                           visibility=public}
        val destructor  = {name="delete",
                           precondition=nil,
			   body=nil,
                           (* post: self.oclIsUndefined() and self@pre->modifiedOnly() *)
                           postcondition=[(SOME "generated_destructor",
                                           ocl_and (ocl_isUndefined (self self_type))
                                                   (ocl_modifiedOnly 
                                                        (ocl_set [atpre (self self_type)] 
                                                                 self_type)))
                                         ],
                           arguments=nil,
                           result=OclVoid,
                           isQuery=false,
                           scope=InstanceScope,
                           visibility=public}
        val getters = map (create_getter c) (attributes_of c)
        val setters = map (create_setter c) (attributes_of c)
        val sec_ops = map create_secured (operations_of c)
        val generated_ops = [constructor,destructor]@getters@setters@sec_ops  
    in 
        List.foldl (uncurry addOperation) c generated_ops 
    end
                       

(* billk_tag: associationend -> path + associations *)
val identity_role_association =
    {name=["AuthorizationEnvironment","IdentityRoleAssociation"],
     aends=[{name=["AuthorizationEnvironment","Association","identity"],
	     aend_type=Classifier ["AuthorizationEnvironment","Identity"],
	     init=NONE,
	     multiplicity=[(0,~1)],
	     ordered=false,
	     visibility=public},
	    {name=["AuthorizationEnvironment","Association","roles"],
	     aend_type=Classifier ["AuthorizationEnvironment","Role"],
	     init=NONE,multiplicity=[(0,~1)],
	     ordered=false,
	     visibility=public}
	   ],
     aclass=NONE}
    
val identity_principal_association =
    {name=["AuthorizationEnvironment","IdentityPrincipalAssociation"],
     aends=[{name=["AuthorizationEnvironment","Association","identity"],
	     aend_type=Classifier ["AuthorizationEnvironment","Identity"],
	     init=NONE,
	     multiplicity=[(1,1)],
	     ordered=false,
	     visibility=public},
	    {name=["AuthorizationEnvironment","Association","principal"],
	     aend_type=Classifier ["AuthorizationEnvironment","Principal"],
	     init=NONE,multiplicity=[(0,~1)],
	     ordered=false,
	     visibility=public}
	   ],
     aclass=NONE}
    
val context_principal_association =
    {name=["AuthorizationEnvironment","ContextPrincipalAssociation"],
     aends=[{name=["AuthorizationEnvironment","ContextPrincipalAssociation","principal"],
	     aend_type=Classifier ["AuthorizationEnvironment","Principal"],
	     init=NONE,
	     multiplicity=[(1,1)],
	     ordered=false,
	     visibility=public},
	    {name=["AuthorizationEnvironment","ContextPrincipalAssociation","context"],
	     aend_type=Classifier ["AuthorizationEnvironment","Context"],
	     init=NONE,
	     multiplicity=[(0,~1)],
	     ordered=false,
	     visibility=public}
	   ],
     aclass=NONE}

val role =    
    Class {activity_graphs=[],
(*           associationends=[{aend_type=Classifier
                                           ["AuthorizationEnvironment","Identity"],
                             init=NONE,multiplicity=[(0,~1)],
                             name="identity",
                             ordered=false,
                             visibility=public}],
 *)        associations=[["AuthorizationEnvironment","IdentityRoleAssociation"]],
           attributes=[{attr_type=String,
                        init=NONE,name="name",
                        scope=InstanceScope,
                        stereotypes=[],
                        visibility=public}],
           interfaces=[],
           invariant=[],
           name=Classifier ["AuthorizationEnvironment","Role"],
           operations=[{arguments=[("s",String)],
                        isQuery=false,
                        name="getRoleByName",
                        postcondition=[],
                        precondition=[],
                        body=[],
                        result=Classifier ["AuthorizationEnvironment","Role"],
                        scope=ClassifierScope,
                        visibility=public}],
           parent=NONE,
           stereotypes=[],
           thyname=NONE}
    
val identity =  
    Class { activity_graphs=[],
(*            associations=[{aend_type=Classifier
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
*)	    associations= [["AuthorizationEnvironment","IdentityRoleAssociation"],
			   ["AuthorizationEnvironment","IdentityPrincipalAssociation"]
			  ],
            attributes=[{attr_type=String,
                         init=NONE,name="name",
                         scope=InstanceScope,
                         stereotypes=[],
                         visibility=public}],
            interfaces=[],
            invariant=[],
            name=Classifier ["AuthorizationEnvironment","Identity"],
            operations=[],
            parent=NONE,
            stereotypes=[],
            thyname=NONE
          }
    
val static_auth_env = [
    Class { activity_graphs=[],
(*            associations=[{aend_type=Classifier
                                            ["AuthorizationEnvironment","Principal"],
                              init=NONE,
                              multiplicity=[(1,1)],
                              name="principal",
                              ordered=false,
                              visibility=public}],
 *)         associations=[["AuthorizationEnvironment","ContextPrincipalAssociation"]],
            attributes=[],
            interfaces=[],
            invariant=[],
            name=Classifier ["AuthorizationEnvironment","Context"],
            operations=[],
            parent=NONE,
            stereotypes=[],
            thyname=NONE},
    Class
        { activity_graphs=[],
(*          associations=[{aend_type=Classifier
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
*)	  associations=[["AuthorizationEnvironment","IdentityPrincipalAssociation"],
			["AuthorizationEnvironment","ContextPrincipalAssociation"]
		       ],
          attributes=[],
          interfaces=[],
          invariant=[],
          name=Classifier ["AuthorizationEnvironment","Principal"],
          operations=[{arguments=[("s",String)],
                       isQuery=false,
                       name="isInRole",
                       postcondition=[],
                       precondition=[],
                       body=[],
                     result=Boolean,
                       scope=InstanceScope,
                       visibility=public}],
          parent=NONE,
          stereotypes=[],
          thyname=NONE}]
                      

(** defines the role hierarchy. *)
(* FIXME: context Identity inv: self.roles.name->includes(r1) implies *)
(*        self.roles.name->includes(r2) *)
fun define_role_hierarchy (sc:Security.Configuration) = 
    let val identity_name = ["AuthorizationEnvironment","Identity"] 
        val identity_type = Classifier identity_name
        val role_name     = ["AuthorizationEnvironment","Role"]
        val role_type     = Classifier role_name
        val self_roles    = 
            ocl_aendcall (self identity_type) 
                         ["AuthorizationEnvironment" , "Identity", "roles"] 
                         (Bag (Classifier ["AuthorizationEnvironment","Role"])) 
        val self_roles_name = ocl_collect self_roles "anonIterVar_0" 
                                          (ocl_attcall (Variable ("anonIterVar_0", 
                                                                  role_type))
                                                       ["AuthorizationEnvironment","Role","name"]
                                                       String)
        fun is_in_role x =  ocl_includes self_roles_name x
        fun invariant_for_role_inheritance (sub,super) = 
            ocl_implies (is_in_role (Literal (sub,String)))
                        (is_in_role (Literal (super,String)))
    in
        List.foldl (fn (rh,ident) => addInvariant
                                         (SOME "role_hierarchy", 
                                          invariant_for_role_inheritance rh) ident)
                   identity
                   (#rh sc)
    end
                               
(** defines the list or roles in the model. 
 * context Role inv: Role.allInstances().name = Bag{...}
 *)
fun define_roles sc = 
    let val roles = Security.all_roles sc
        val role_collection = map (fn x =>CollectionItem(Literal (x,String),String)) 
                                  roles 
        val role_type = Classifier ["AuthorizationEnvironment", "Role"] 
        val inv = ocl_eq (ocl_collect (ocl_allInstances (self role_type))
                                      "anonIterVar_0"
                                      (ocl_attcall (Variable ("anonIterVar_0",role_type))
                                                   ["AuthorizationEnvironment","Role","name"]
                                                   String))
                         (CollectionLiteral (role_collection, Bag String))
    in 
        addInvariant (SOME "list_of_roles",inv) role
    end
                               
(** transform the postconditions to also include the authorization constraints. *)
(* FIXME: implement this *)
fun create_sec_postconds sc c = c


fun transform (model:Rep.Model,sc) =
    let
        val transformed_design_model = (map add_operations (#1 model),#2 model)
        val transformed_model = create_sec_postconds sc transformed_design_model 
        val auth_env          = map (normalize (#2 transformed_model)) (define_roles sc::define_role_hierarchy sc::static_auth_env) 
    in
         ((#1 transformed_model) @ auth_env,identity_role_association::identity_principal_association::
				       context_principal_association::(#2 transformed_model))
    end
    
end 
