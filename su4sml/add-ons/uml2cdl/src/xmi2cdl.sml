(*****************************************************************************
 * uml2cdl --- a converter from UML models to WS-CDL. part of su4sml
 *             http://projects.brucker.ch/su4sml/
 *                                                                            
 * xmi2cdl.sml --- 
 * This file is part of uml2cdl.
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

(**
 * functions for parsing xmi into cdl types.
 * TODO: fix exception handling
 *)
structure Xmi2Cdl :
sig
    val transformXMI : XMI.XmiContent -> CDL.tPackage
    val readXMI      : string -> CDL.tPackage
    (* generic exception if something is wrong *)
    exception IllFormed of string 
end  = 
struct
open Xmi_IDTable
exception IllFormed of string

fun filterClassifiersByStereotype t st list = 
    List.filter (fn XMI.Class x            => List.exists (fn y => find_stereotype t y = st) (#stereotype x)
		  | XMI.AssociationClass x => List.exists (fn y => find_stereotype t y = st) (#stereotype x)
		  | _ => false ) list 
    
fun filterInformationType  t = filterClassifiersByStereotype t "CDL.InformationType"
fun filterRoleType         t = filterClassifiersByStereotype t "CDL.RoleType"
fun filterRelationshipType t = filterClassifiersByStereotype t "CDL.RelationshipType"
fun filterParticipantType  t = filterClassifiersByStereotype t "CDL.ParticipantType"
fun filterChannelType      t = filterClassifiersByStereotype t "CDL.ChannelType"
fun filterChoreography     t = filterClassifiersByStereotype t "CDL.Choreography"
fun filterToken            t = filterClassifiersByStereotype t "CDL.Token"


fun mkBehavior (XMI.Interface c) = { name = #name c,
				     (* FIX: how to specify the WSDL interface? *)
				     interface = NONE }
    handle IllFormed msg => raise IllFormed ("Error in mkBehavior: "^msg^"\n") 


(** 
 * transform a class with stereotype <<CDL.Token>> into a CDL.tToken.
 * The information type of the token is given by the attribute with  
 * the name information type                                         
 *)
fun mkToken t (XMI.Class c) = 
    { name = #name c,
      informationType = getOpt(Option.map (XMI.classifier_name_of o (find_classifier t) o #type_id) 
					  (List.find (fn (x:XMI.Attribute) => #name x = "informationType") 
						     (#attributes c)),"dummyType")} 
    handle IllFormed msg => raise IllFormed ("Error in mkToken: "^msg^"\n") 

(** 
 * transform a class with Stereotype <<CDL.RoleType>> into a CDL.tRoleType.
 * the list of behaviors of the roleType is given by the interfaces it
 * realizes 
 *)
fun mkRoleType t (XMI.Class c) = 
    { name= #name c,
      behavior =  map (mkBehavior o (find_classifier t) o 
		       #supplier o (find_dependency t))
		      (#clientDependency c)
		      }
    handle IllFormed msg => raise IllFormed ("Error in mkRoleType: "^msg^"\n") 
                                  
(** 
 * transform an association class with stereotype <<CDL.RelationshipType>>     
 * into a  CDL.tRelationshipType. The roles participating in this relationship 
 * are given by the classes participating in the association                   
 *)
fun mkRelationshipType t (XMI.AssociationClass c) = 
    let val (fst_aend::snd_aend::aends) = #connection c
	val fst_name = XMI.classifier_name_of (find_classifier t (#participant_id fst_aend)) 
	val snd_name = XMI.classifier_name_of (find_classifier t (#participant_id snd_aend)) 
    in
	{name = #name c,
	 role = ({ref_type=fst_name,
		  behavior = NONE},
		 {ref_type=snd_name,
		  behavior = NONE})
	 }
    end
    handle IllFormed msg => raise IllFormed ("Error in mkRelationshipType: "^msg^"\n") 


val mkRelationshipRef = XMI.classifier_name_of    

(** 
 * transform a class with Stereotype <<CDL.ParticipantType>> into a 
 * CDL.tParticipantType. The roles that identify this participant   
 * type are given by composite aggregations                         
 *)
fun mkParticipantType t (XMI.Class c) = 
    let val connected_ids = map #participant_id (find_aends t (#xmiid c))
	val connected_names = map (XMI.classifier_name_of o (find_classifier t)) 
				  connected_ids
    in 
	{ name = #name c,
	  role = map (fn x => {ref_type=x}) connected_names  }
    end
    handle IllFormed msg => raise IllFormed ("Error in mkParticipantType: "^msg^"\n") 


(** 
 * transform a class with stereotype <<CDL.ChannelType>> into a     
 * CDL.tChannelType.  the role type is specified by an association, 
 * the attributes "usage" and "action" are specified by stereotypes 
 * <<CDL.action>> and <<CDL.usage>>. The reference token is given by
 * the type of the attribute with name "reference"                  
 *)
fun mkChannelType t (class as XMI.Class c) = 
    let val connected_ids = map #participant_id (find_aends t (#xmiid c))
	val connected_names = map (XMI.classifier_name_of o (find_classifier t)) 
				  connected_ids
    in 
	{name = #name c,
	 usage = (Option.map CDL.tUsageFromString 
		      (class_taggedvalue_of t "CDL.usage" class)),
	 action = (Option.map CDL.tActionFromString 
		       (class_taggedvalue_of t "CDL.action" class)),
	 passing = nil, (* FIX *)
	 role = {ref_type = hd connected_names,
		 behavior = NONE}, (* FIX *)
	 reference = {token = getOpt(Option.map (XMI.classifier_name_of o 
						 (find_classifier t) o #type_id) 
						(List.find (fn (x:XMI.Attribute) => 
							       #name x = "reference") 
							   (#attributes c)),
						"dummyToken")},
	 identity = NONE (* FIX *)}
    end
    handle IllFormed msg => raise IllFormed ("Error in mkChannelType: "^msg^"\n") 
	
(** 
 * transform an attribute of a classifier into a CDL.tVariable.
 * the information type, resp. channel type, is given by the tagged
 * value "CDL.InformationType", resp. "CDL.ChannelType".  The role
 * types are given by the tagged value "CDL.roleTypes". FIX: the
 * attributes "mutable" and "silent" should be taken from the
 * changeability resp. visibility of the attribute.  FIX: the
 * attribute "free" should be taken from the tagged value CDL.free    
 *)
fun mkVariable t (a:XMI.Attribute) = 
    let val cls       = (find_classifier t) (#type_id a)
	val st        = map (find_stereotype t) (XMI.classifier_stereotype_of cls)
	val type_name = XMI.classifier_name_of cls
	val rt        = attribute_taggedvalue_of t "CDL.roleTypes" a
    in  
	{ name = #name a,
	  informationType = if List.exists (fn x => x="CDL.InformationType") st
			    then SOME type_name
			    else NONE, 
	  channelType = if List.exists (fn x => x="CDL.ChannelType") st
			then SOME type_name
			else NONE, (* FIX: make informationType and channelType mutually exclusive *)
	  mutable = NONE, (* FIX: use Changeability *)
	  free = NONE, (* FIX: use tagged value CDL.free *)
	  silent = NONE, (* FIX: use VisibilityKind? *)
	  roleTypes = rt
	  }
    end
    handle IllFormed msg => raise IllFormed ("Error in mkVariable: "^msg^"\n") 

(** 
 * transform the given action state with stereotype <<CDL.send>>,      
 * the subsequent objectflow state, and the subsubsequent action state 
 * with stereotype <<CDL.receive>> into an exchange                    
 *)
fun mkExchange t ((send_state,objflow_state,receive_state):XMI.StateVertex*XMI.StateVertex*XMI.StateVertex) =
    let val obj_name= XMI.state_name_of objflow_state
	val obj_type= find_classifierInState_classifier t (XMI.state_type_of objflow_state)
	val send_state_st = map (find_stereotype t) 
				(XMI.state_stereotype_of send_state)
    in
	{ name = "dummyExchange",
	  informationType = (Option.map XMI.classifier_name_of 
					(Option.filter (classifier_has_stereotype t "CDL.InformationType") obj_type)),
	  channelType = (Option.map XMI.classifier_name_of 
				    (Option.filter (classifier_has_stereotype t "CDL.ChannelType") obj_type)),
	  action = if List.exists (fn x => x="CDL.request") send_state_st
		   then CDL.request
		   else if List.exists (fn x => x="CDL.respond") send_state_st 
		   then CDL.respond
		   else getOpt(Option.map CDL.tActionFromString 
					  (state_taggedvalue_of t "CDL.action" 
								send_state),
					  CDL.request),
	  send = {variable= SOME ("cdl:getVariable('"^obj_name^"','','')"),
		  (* state_taggedvalue_of t "CDL.variable" send_state, *)
		  recordReference = nil,
		  causeException = Option.mapPartial Bool.fromString (state_taggedvalue_of t "CDL.causeException" send_state)},
	  receive = {variable= SOME ("cdl:getVariable('"^obj_name^"','','')"),
		     (* state_taggedvalue_of t "CDL.variable" receive_state, *)
		     recordReference = nil,
		     causeException = Option.mapPartial Bool.fromString (state_taggedvalue_of t "CDL.causeException" send_state)}
	  }
    end
    handle IllFormed msg => raise IllFormed ("Error in mkExchange: "^msg^"\n") 

fun entry_action_body (XMI.ActionState {entry,...}) =
    case entry
     of SOME (XMI.mk_Procedure {body,...}) => body
(* fun entry_action_body _ = "" *)

(** 
 * transform a sequence of object flows into a CDL.tInteraction. 
 * the participating roles are specified by the partition (swimlane)
 * of the source and target of the object flow
 * the channel variable used is specified by the tagged value
 * "CDL.channelVariable" of the first object flow
 * the operation is specified by the tagged value "CDL.operation" 
 *)
fun mkInteraction t (act:CDL.tActivity list) (st:XMI.StateVertex)=
    let val stereo  = map (find_stereotype t) (XMI.state_stereotype_of st)
	val receive = (hd o (successor_states_of t) o 
		       hd o (successor_states_of t)) st
	val send_partition = getOpt(state_taggedvalue_of t "partition" st,
				    "unspecified")
	val receive_partition = getOpt(state_taggedvalue_of t "partition" receive,
				       "unspecified")
	val relationship = getOpt(state_taggedvalue_of t "CDL.relationshipType" st,
				  "unspecified")
	val (from,to)  = if List.exists (fn x => x = "CDL.respond") stereo
			 then (receive_partition,send_partition)
			 else (send_partition,receive_partition)
	fun find_successive_exch_states rtype send = 
	    let val objflow = hd (successor_states_of t send)
		val receive = hd (successor_states_of t objflow)
		val next    = hd (successor_states_of t receive)
		val stereo  = map (find_stereotype t) (XMI.state_stereotype_of next)
	    in  (* check whether there is another exchange following   *)
		(* this one as part of the same interaction.           *)
		(* FIX: also check whether the roletypes are the same. *)
		(* * if not, we have to start a new interaction        *)
		if (List.exists (fn x => x = "CDL.send" orelse
					 x = "CDL.request" orelse
					 x = "CDL.respond") stereo) 
		   andalso getOpt(state_taggedvalue_of t "CDL.relationshipType" next,
				  "unspecified") = rtype
		then let val (sts,nxt) = find_successive_exch_states rtype next
		     in ((send,objflow,receive)::sts,nxt)
		     end
		else ([(send,objflow,receive)],next)
	    end
	val (exch_states,next) = find_successive_exch_states relationship st
	val state_name = XMI.state_name_of st
	val interaction_name = if state_name = "" then
				   entry_action_body st
			       else state_name
	val operation = getOpt(state_taggedvalue_of t "CDL.operation" st,"")
	val op_name = if operation = "" then interaction_name
		      else operation
			  
    in 
	((CDL.interaction {name = interaction_name,
			   channelVariable = getOpt(state_taggedvalue_of 
							t "CDL.channelVariable" st,
						    "unspecified"),
			   operation = op_name,
			   align = (Option.mapPartial 
					Bool.fromString 
					(state_taggedvalue_of t "CDL.align" st)),
			   initiate = (Option.mapPartial 
					   Bool.fromString 
					   (state_taggedvalue_of t "CDL.initiate" 
								 st)),
			   participate = {relationshipType = relationship,
					  fromRole = from,
					  toRole = to},
			   exchange = map (mkExchange t) exch_states,
			   timeout = NONE, (* FIX *)
			   record = nil (* FIX *)
			   })::act,next)
    end
    handle IllFormed msg => raise IllFormed ("Error in mkInteraction: "^msg^"\n") 

(** 
 * transform a procedure into an CDL.tDescription 
 * the description type is take from the language of the procedure,
 * the content is taken from the body of the procedure.
 *)
fun mkDescription (XMI.mk_Procedure p) =
    {description_type = CDL.tDescriptionTypeFromString (#language p),
     content = #body p}
    handle IllFormed msg => raise IllFormed ("Error in mkDescription: "^msg^"\n") 


(** 
 * transform an Action State with stereotype <<CDL.silent>> into a CDL silent    
 * action. if the entry action of the action state has language "documentation", 
 * "reference", or "semantics", the body of the expression will be translated    
 * into a corresponding description element                                      
 *)
fun mkSilentAction t (act:CDL.tActivity list) (st:XMI.StateVertex) = 
    let val next = hd (successor_states_of t st)
	fun isDoc (XMI.mk_Procedure({language="documentation",...})) = true
	  | isDoc (XMI.mk_Procedure({language="reference",...})) = true
	  | isDoc (XMI.mk_Procedure({language="semantics",...})) = true
	  | isDoc _ = true
	val doc = Option.mapPartial (Option.filter isDoc) (XMI.state_entry_of st) 
    in  
	((CDL.silentAction {roleType = state_taggedvalue_of t "partition" st,
			    description = Option.map mkDescription doc})::act,
	 next) 
    end
    handle IllFormed msg => raise IllFormed ("Error in mkSilentAction: "^msg^"\n") 
 
(** 
 * transform any other Action State into a CDL.noAction.
 * FIX: only states withwith Stereotype <<CDL.noAction>> should be transformed. 
 * All other action states should raise an error.
 *)
fun mkNoAction t (act:CDL.tActivity list) (st:XMI.StateVertex) =
    let val next = hd (successor_states_of t st)
    in
	((CDL.noAction {roleType = state_taggedvalue_of t "partition" st })::act,
	 next)
    end
    handle IllFormed msg => raise IllFormed ("Error in mkNoAction: "^msg^"\n") 

fun mkAssign t (act:CDL.tActivity list) (st:XMI.StateVertex) =
    let val next = hd (successor_states_of t st)
    in
	((CDL.assign { copy = [{name = "undefined", (* FIX *)
                                causeException = SOME false, (* FIX *)
                                source = {variable = "cdl:getVariable('"^
                                                     (Option.valOf(state_taggedvalue_of t "CDL.source" st))^
                                                     "','','')",
                                          expression = "" },
                                target = {variable = "cdl.getVariable('"^
                                                     (Option.valOf(state_taggedvalue_of t "CDL.target" st))^
                                                     "','','')"}
                              }],
                       roleType = Option.valOf(state_taggedvalue_of t "partition" st )})::act,
	 next)
    end
    handle IllFormed msg => raise IllFormed ("Error in mkAssign: "^msg^"\n") 
    

(** 
 * transforms the activitygraph starting from the given state into a   
 * list of CDL.tActivity's.  All functions called from here return a   
 * list of CDL.activitys and the next state in the activity graph      
 * if at the end (when the final state is reached), the activity list  
 * contains more than one element, the list is wrapped into a sequence 
 *)
fun mkActivity t (act:CDL.tActivity list) (st:XMI.StateVertex) =
    let val stereo = map (find_stereotype t) (XMI.state_stereotype_of st)
	fun wrap_up [x] = x
	  | wrap_up xs = CDL.sequence (rev xs)
	val (acts,next) = if List.exists (fn x => x = "CDL.send" orelse
						  x = "CDL.request" orelse
						  x = "CDL.respond") stereo
			  then mkInteraction t act st
			  else if List.exists (fn x => x = "CDL.silent") stereo 
			  then mkSilentAction t act st
			  else if List.exists (fn x => x = "CDL.assign") stereo 
			  then mkAssign t act st
			  else if XMI.state_is_fork st 
			  then mkParallel t act st
			  (* FIX: there are other types of course *)
			  else mkNoAction t act st
    in 
	case next of XMI.FinalState _                   => (wrap_up acts, next)
		   | XMI.PseudoState{kind=XMI.join,...} => (wrap_up acts, next)
		   | _  => mkActivity t acts next
    end
    handle IllFormed msg => raise IllFormed ("Error in mkActivity: "^msg^"\n") 
(**
 * transform the parallel action states following this fork state into
 * a CDL.parallel activity  
 *)
and mkParallel t (act:CDL.tActivity list) (st:XMI.StateVertex) = 
    let val (acts,join::states) = ListPair.unzip (map (mkActivity t []) 
					       (successor_states_of t st))
    in 
        (((CDL.parallel acts)::act),hd (successor_states_of t join))
    end
    handle IllFormed msg => raise IllFormed ("Error in mkParallel: "^msg^"\n") 

(** transforms a class with stereotype <<CDL.Choreography>> into a       
 * CDL.tchoreography. The attributes complete, isolation, root, and     
 * coordination are specified by the tagged values CDL.complete,        
 * CDL.isolation, CDL.root, and CDL.coordination. The relationships     
 * are specified by associations to classes with stereotype             
 * <<CDL.RelationshipType>>. VariableDefinitions are given by the       
 * attributes of the class. Enclosed choreographie definitions are      
 * specified by composite aggregations to classes with stereotype       
 * <<CDL.Choreography>>. The activity performed by this choreography    
 * is specified by the activity graph connected to this class. The      
 * ExceptionBlock and FinalizerBlocks are specified by assocations with 
 * stereotype <<CDL.exceptionBlock>> resp. <<CDL.finalizerBlock>> to    
 * classes with stereotype <<CDL.WorkUnit>>                             *)
fun mkChoreography t (class as XMI.Class c) = 
    let val activity_graph = hd (find_activity_graph_of t (#xmiid c))
			handle Empty => raise IllFormed "Choreography does not have an activity diagram."
	val initial_state = initial_state_of t activity_graph
	val first_state = hd (successor_states_of t initial_state)
		handle Empty => raise IllFormed "Choreography activity seems to be empty."
	val connected_classifiers = map ((find_classifier t) o #participant_id)
					(find_aends t (#xmiid c))
	val connected_reltypes = 
	    List.filter (fn x => classifier_has_stereotype 
				     t "CDL.RelationshipType" x) 
			connected_classifiers
    in
	CDL.choreography {name = #name c,
			  complete = class_taggedvalue_of t "CDL.complete" class,
			  isolation = (Option.mapPartial 
					   Bool.fromString 
					   (class_taggedvalue_of t "CDL.isolation" 
								 class)),
			  root = (Option.mapPartial 
				      Bool.fromString 
				      (class_taggedvalue_of t "CDL.root" class)),
			  coordination = Option.mapPartial 
					     Bool.fromString 
					     (class_taggedvalue_of 
						  t "CDL.coordination" class),
			  relationship = map mkRelationshipRef connected_reltypes,
			  variableDefinitions = (map (mkVariable t) 
						     (#attributes c)),
			  choreography = [], (* FIX: use composite aggregations *)
			  activity =  #1 (mkActivity t nil first_state),
			  exceptionBlock = [], (* FIX: use association with stereotype exceptionBlock *)
			  finalizerBlock = [] (* FIX: use association with stereotype finalizerBlock *)}
    end
    handle IllFormed msg => raise IllFormed ("Error in mkChoreography: "^msg^"\n") 



(* fill as needed... *)
val type_mapping = [("string",    "xsd:string"),
                    ("String",    "xsd:string"),
                    ("stringType","xsd:string"),
                    ("bool",      "xsd:boolean"),
                    ("uri",       "xsd:anyURI"),
                    ("uriType",   "xsd:anyURI"),
                    ("byte",      "xsd:byte")] 

fun classifier_name (XMI.Class {name,...}) = name
  | classifier_name (XMI.Primitive {name,...}) = name

fun is_predefined_type s = ListEq.includes (map #1 type_mapping) s 

fun map_type s = foldl (fn ((from,to),b) => if  b=from then to
                                            else b) s type_mapping

fun class2typename s = if is_predefined_type s then map_type s
                       else s^"Type"

fun mkXsdTypeFromAtt t (att:XMI.Attribute) =
    let val class_name = classifier_name (find_classifier t (#type_id att))
    in 
        XML_Schema.SimpleType {name = #name att,
                               typ = class2typename class_name
                              }
    end
    
fun mkXsdType t (XMI.Class c) =
    XML_Schema.ComplexType {name = (#name c)^"Type",
                            sequence = map (mkXsdTypeFromAtt t) (#attributes c)
                           }

(** 
 * transform a class with stereotype <<CDL.informationType>> into a
 * CDL information type. The type is given by the tagged value 
 * "CDL.type", the element type is given by the tagged value
 * "CDL.element".  The exception type may (optional) be given by the
 * tagged value "CDL.exceptionType" 
 * FIX: use attributes of class to generate an XML Schema complex type and use that. 
 *)
fun mkInformationType t (XMI.Class c) =
    {name = #name c,
     (* was: class_taggedvalue_of t "CDL.type" (XMI.Class c) *)
     information_type = SOME (class2typename (#name c)),
     element = class_taggedvalue_of t "CDL.element" (XMI.Class c),
     exceptionType = Option.mapPartial Bool.fromString (class_taggedvalue_of t "CDL.exceptionType" (XMI.Class c))}
  | mkInformationType t (XMI.Primitive c) =
    {name = #name c,
     (* was: class_taggedvalue_of t "CDL.type" (XMI.Class c) *)
     information_type = SOME (class2typename (#name c)),
     element=class_taggedvalue_of t "CDL.element" (XMI.Primitive c),
     exceptionType = Option.mapPartial Bool.fromString (class_taggedvalue_of t "CDL.exceptionType" (XMI.Primitive c))}
    handle IllFormed msg => raise IllFormed ("Error in mkInformationType: "^msg^"\n") 

(**
 * transform a UML package (model) into a CDL package.
 * author, version and targetNamespace of the package may be					    
 * specififed by the tagged values "CDL.author", "CDL.version", 
 * and "CDL.targetNamespace"
 *)
fun mkPackage t (XMI.Package p) =
    {name = #name p,
     author = getOpt(package_taggedvalue_of t "CDL.author" (XMI.Package p),
		     "unspecified author"),
     version = getOpt(package_taggedvalue_of t "CDL.version" (XMI.Package p),
		      "unspecified version"),
     targetNamespace = getOpt(package_taggedvalue_of 
				  t "CDL.targetNamespace" (XMI.Package p),
			      "http://unspecified.namespace/"),
     schema =  { attributeFormDefault = "qualified",
                 elementFormDefault = "qualified",
                 targetNamespace = getOpt(package_taggedvalue_of 
				  t "CDL.targetNamespace" (XMI.Package p),
			      "http://unspecified.namespace/"),
                 definitions = map (mkXsdType t) 
                                   (List.filter (not o is_predefined_type o classifier_name) 
                                                (filterInformationType t (#classifiers p)))
               },
     informationType = map (mkInformationType t)
			   (filterInformationType t (#classifiers p)),
     token = map (mkToken t) (filterToken t (#classifiers p)),
     tokenLocator = nil, (* FIX *)
     roleType = map (mkRoleType t)
			   (filterRoleType t (#classifiers p)),
     relationshipType = map (mkRelationshipType t)
			   (filterRelationshipType t (#classifiers p)),
     participantType = map (mkParticipantType t)
			   (filterParticipantType t (#classifiers p)),
     channelType = map (mkChannelType t)
			   (filterChannelType t (#classifiers p)),
     choreography = map (mkChoreography t)
			   (filterChoreography t (#classifiers p))
     }:CDL.tPackage
    handle IllFormed msg => raise IllFormed ("Error in mkPackage: "^msg^"\n") 

(**
 * transform the first package (i.e., the top-level model) of an
 * xmi-file into a CDL package.  
 *)
fun transformXMI ({classifiers,constraints,packages,
		   stereotypes,variable_declarations,state_machines, activity_graphs}) = 
    let val (xmiid_table: (string,HashTableEntry) HashTable.hash_table) =
	    HashTable.mkTable (HashString.hashString, (op =)) (101, Option)
	(* for some reasons, there are model elements outside of the top-level *) 
	(* model the xmi-file. So we have to handle them here seperately:      *)
	val _ = map (insert_classifier xmiid_table nil) classifiers
	val _ = map (insert_constraint xmiid_table) constraints
	val _ = map (insert_stereotype xmiid_table) stereotypes
	val _ = map (insert_variable_dec xmiid_table) variable_declarations
	(* "hd packages" is supposed to be the first model in the xmi-file *)
	val model = hd packages
		handle Empty => raise IllFormed "no model found"
	val _ = insert_model xmiid_table model
	val _ = map (insert_activity_graph xmiid_table) activity_graphs
	val _ = transform_associations xmiid_table model
    in 
	mkPackage xmiid_table model (* FIX: use first package with *)
                                    (* stereotype <<CDL.package>>  *)
    end
    (* handle XmlTree.IllFormed msg =>  error ("Warning: "^msg^"\n") *)
    handle IllFormed msg => error ("Warning: "^msg^"\n") 
	     (* | _ => error "" *)
(** 
 * read an xmi-file and transform to CDL.
 *)
fun readXMI f = (transformXMI o XmiParser.readFile) f

end
