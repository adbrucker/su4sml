(*****************************************************************************
 * uml2cdl --- a converter from UML models to WS-CDL. part of su4sml
 *             http://projects.brucker.ch/su4sml/
 *                                                                            
 * cdl2xml.sml --- 
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
 * functions for serializing cdl types to xml trees.
 *)
structure Cdl2Xml = 
struct
open CDL
fun filter_opt_args opt = map (fn (x,y) => (x,valOf y)) 
			      (List.filter (fn (x,y) => Option.isSome y) opt)

fun option_to_list (SOME s) = [s]
  | option_to_list NONE   = nil

(* FIX: empty for now *)
fun tTokenLocator2Xml        it = XmlTree.Node (("tokenLocator",[]),[])
fun tPassing2Xml             it = XmlTree.Node (("passing",[]),[])
fun tIdentity2Xml            it = XmlTree.Node (("identity",[]),[])
fun tException2Xml           it = XmlTree.Node (("exceptionBlock",[]),[])
fun tFinalizer2Xml           it = XmlTree.Node (("finalizerBlock",[]),[])
fun tRecord2Xml              it = XmlTree.Node (("record",[]),[])
fun tFinalizerReference2Xml  it = XmlTree.Node (("finalizerReference",[]),[]) 
fun tTimeout2Xml             it = XmlTree.Node (("timeout",[]),[]) 

fun tSourceVariableRef2Xml   (it:tSourceVariableRef) = XmlTree.Node (("source",[("variable",#variable it)]),[])
fun tVariableRef2Xml  s      (it:tVariableRef) = XmlTree.Node ((s,[("variable",#variable it)]),[])


fun tCopy2Xml                (it:tCopy) = XmlTree.Node (("copy",[("name",#name it)]),
                                                [tSourceVariableRef2Xml (#source it),
                                                 tVariableRef2Xml "target" (#target it)])


fun tBind2Xml it = XmlTree.Node (("bind",[]),[]) 

fun tDescription2Xml (it:CDL.tDescription) =
    XmlTree.Node (("description",[("type",CDL.tDescriptionType2String (#description_type it))]), [XmlTree.Text (#content it)])

fun tParticipate2Xml (it:CDL.tParticipate) = 
    XmlTree.Node (("participate",[("relationshipType",#relationshipType it),
				  ("fromRoleTypeRef",#fromRole it),
				  ("toRoleTypeRef",#toRole it)]),[])


fun tRelationshipRef2Xml it = XmlTree.Node (("relationship",[("type",it)]),[])

fun tVariableRecordRef2Xml s (it:CDL.tVariableRecordRef) =
    let val opt_args = filter_opt_args [("variable",#variable it),
					("causeException",Option.map Bool.toString (#causeException it))]
    in XmlTree.Node ((s,opt_args),[])
    end


fun tExchange2Xml (it:CDL.tExchange) =
    let val opt_args = filter_opt_args [("informationType",#informationType it),
					("channelType",#channelType it)]
    in 
	XmlTree.Node (("exchange",[("name",#name it),
				   ("action",CDL.tAction2String (#action it))]
				  @opt_args),
		      [tVariableRecordRef2Xml "send" (#send it),
		       tVariableRecordRef2Xml "receive" (#receive it)])
    end


(* FIX: handle optional attribute roleTypes *)
fun tVariable2Xml (it:CDL.tVariable) =
    let val opt_args = filter_opt_args [("informationType",#informationType it),
					("channelType",#channelType it),
					("mutable",Option.map Bool.toString (#mutable it)),
					("free",Option.map Bool.toString (#free it)),
					("silent",Option.map Bool.toString (#silent it)),
					("roleTypes", #roleTypes it)]
    in 
	XmlTree.Node (("variable",[("name",#name it)]@opt_args),[])
    end

fun tVariableDefinitions2Xml it = XmlTree.Node (("variableDefinitions",[]),
						map tVariable2Xml it)


fun tActivity2Xml (CDL.sequence it) = XmlTree.Node (("sequence",[]),
						    map tActivity2Xml it)
  | tActivity2Xml (CDL.parallel it) = XmlTree.Node (("parallel",[]),
						    map tActivity2Xml it)
  | tActivity2Xml (CDL.choice   it) = XmlTree.Node (("choice",[]),
						    map tActivity2Xml it)
  | tActivity2Xml (CDL.workunit it) = tWorkunit2Xml it
  | tActivity2Xml (CDL.interaction it) = 
    let val opt_args = filter_opt_args 
			   [("align",Option.map Bool.toString (#align it)),
			    ("initiate",Option.map Bool.toString (#initiate it))]
    in 	
	XmlTree.Node (("interaction",[("name",#name it),
				      ("channelVariable",#channelVariable it),
				      ("operation",#operation it)]@opt_args),
		      List.concat [[tParticipate2Xml (#participate it)],
				   map tExchange2Xml (#exchange it),
				   map tTimeout2Xml (option_to_list (#timeout it)),
				   map tRecord2Xml (#record it)])
    end
  | tActivity2Xml (CDL.perform it) =
    let val opt_args = filter_opt_args [("choreographyInstanceId",
					 #choreographyInstanceId it)]
    in 
	XmlTree.Node (("perform",[("choreographyName",#choreographyName it)]@opt_args),
		      List.concat [map tBind2Xml (#bind it),
				   map tChoreography2Xml (option_to_list (#choreography it))])
    end
  | tActivity2Xml (CDL.assign it) =
    XmlTree.Node (("assign",[("roleType",#roleType it)]),
		  map tCopy2Xml (#copy it))
  | tActivity2Xml (CDL.silentAction it) = 
    let val opt_args = filter_opt_args [("roleType",#roleType it)]
    in 
	XmlTree.Node (("silentAction",opt_args), map tDescription2Xml 
						     (option_to_list (#description it)))
    end
  | tActivity2Xml (CDL.noAction it) = 
    let val opt_args = filter_opt_args [("roleType",#roleType it)]
    in 
	XmlTree.Node (("noAction",opt_args),[])
    end
  | tActivity2Xml (CDL.finalize it) =
    XmlTree.Node (("finalize",[("name",#name it)]),
		   map tFinalizerReference2Xml (#finalizerReference it))
and tWorkunit2Xml (it:CDL.tWorkunit) = 
    let val opt_args = filter_opt_args [("guard",#guard it),
					("repeat", #repeat it),
					("block", Option.map Bool.toString (#block it))]
    in 
	XmlTree.Node (("workunit",[("name",#name it)]@opt_args),
		      map tActivity2Xml (#activity it))
    end
and tChoreography2Xml ((CDL.choreography it):CDL.tChoreography) = 
    let val opt_args = filter_opt_args [("complete",#complete it),
					("isolation",Option.map Bool.toString (#isolation it)),
					("root",Option.map Bool.toString (#root it)),
					("coordination",Option.map Bool.toString (#coordination it))]
    in 
	XmlTree.Node (("choreography",[("name",#name it)]@opt_args),
		      List.concat [map tRelationshipRef2Xml (#relationship it),
				   [tVariableDefinitions2Xml (#variableDefinitions it)],
				   map tChoreography2Xml (#choreography it),
				   [tActivity2Xml (#activity it)],
				   map tException2Xml (#exceptionBlock it),
				   map tFinalizer2Xml (#finalizerBlock it)])
    end

fun tTokenReference2Xml (it:CDL.tTokenReference) = 
    XmlTree.Node (("token",[("name",it)]),[])

fun tReference2Xml (it:CDL.tReference) = 
    XmlTree.Node (("reference",[]), [tTokenReference2Xml (#token it)])

fun tRoleRef32Xml (it:CDL.tRoleRef3) = 
    let val opt_args = filter_opt_args [("behavior",#behavior it)]
    in
	XmlTree.Node (("roleType",[("typeRef",#ref_type it)]@opt_args),[])
    end

fun tChannelType2Xml (it:CDL.tChannelType) = 
    let val opt_args = filter_opt_args 
			   [("usage",Option.map CDL.tUsage2String (#usage it)),
			    ("action", Option.map CDL.tAction2String (#action it))]
    in 
	XmlTree.Node (("channelType",[("name",#name it)]),
		      List.concat [map tPassing2Xml (#passing it),
				   [tRoleRef32Xml (#role it)],
				   [tReference2Xml (#reference it)],
				   map tIdentity2Xml (option_to_list (#identity it))])
    end

fun tBehavior2Xml (it:CDL.tBehavior) = 
    let val opt_args = filter_opt_args [("interface",#interface it)]
    in 
	XmlTree.Node (("behavior",[("name",#name it)]),[])
    end

fun tRoleRef22Xml (it:CDL.tRoleRef2) =
    XmlTree.Node (("roleType",[("typeRef",#ref_type it)]),[])

fun tParticipantType2Xml (it:CDL.tParticipantType) = 
    XmlTree.Node (("participantType",[("name",#name it)]),
		  map tRoleRef22Xml (#role it))

fun tRoleRef2Xml (it:CDL.tRoleRef) =
    XmlTree.Node (("roleType",[("typeRef",#ref_type it)]),[])

fun tRelationshipType2Xml (it:CDL.tRelationshipType) =
    XmlTree.Node (("relationshipType",[("name",#name it)]),
		  [tRoleRef2Xml (#1 (#role it)),
		   tRoleRef2Xml (#2 (#role it))])

fun tToken2Xml (it:CDL.tToken) = 
    XmlTree.Node (("token",[("name",#name it),
			    ("informationType",#informationType it)]),
		  [])


fun tRoleType2XML (it:CDL.tRoleType) = 
    XmlTree.Node (("roleType",[("name",#name it)]),
		   map tBehavior2Xml (#behavior it))


fun tInformationType2Xml (it:CDL.tInformationType) = 
    let val opt_args = filter_opt_args [("type",#information_type it),
					("element",#element it),
					("exceptionType",Option.map Bool.toString (#exceptionType it))]
    in 
	XmlTree.Node (("informationType",[("name",#name it)]@opt_args),
		       [])
    end
	

fun tPackage2Xml (package:CDL.tPackage) =
    XmlTree.Node (("package",[("name",#name package),
			                  ("author",#author package),
			                  ("version",#version package),
			                  ("targetNamespace",#targetNamespace package),
			                  ("xmlns","http://www.w3.org/2005/10/cdl"),
			                  ("xmlns:xsi","http://www.w3.org/2001/XMLSchema-instance"),
                              (* SAP seems to use these ones also: *)
                              ("xmlns:cdl2bpelns","http://cdl2bpel.cecka.sap.com"),
                              ("xmlns:choreons","http://unspecified.namespace"), (* ??? *)
                              ("xmlns:xsd","http://www.w3.org/2001/XMLSchema")
                  ]),
		  List.concat [[XML_Schema2Xml.xsdSchema2Xml (#schema package)],
                               map tInformationType2Xml  (#informationType package),
			       map tToken2Xml            (#token package),
			       map tTokenLocator2Xml     (#tokenLocator package),
			       map tRoleType2XML         (#roleType package),
			       map tRelationshipType2Xml (#relationshipType package),
			       map tParticipantType2Xml  (#participantType package),
			       map tChannelType2Xml      (#channelType package),
			       map tChoreography2Xml     (#choreography package)])

			       


(* just for testing purposes: 
val exampleChoreography = ({name="WP35_CDL",author="Juergen Doser",version="0.01",
			    targetNamespace="http://eu.trustcom/...",
			    informationType=[{name="dummy",
					      information_type=SOME "dummyT",
					      element=NONE,
					      exceptionType=NONE}],
			    token=[{name="customerRef",informationType="xsi:uri"},
				   {name="designerRef",informationType="xsi:uri"},
				   {name="analysisRef",informationType="xsi:uri"},
				   {name="manufacturabilityRef",
				    informationType="xsi:uri"}],
			    tokenLocator=[],
			    roleType=[{name="BPEL_Engine",
				       behavior=[{name="BPEL_Engine",
						  interface=NONE}]},
				      {name="Customer_Negotiation",
				       behavior=[{name="Customer_Negotiation",
						  interface=NONE}]},
				      {name="Design",
				       behavior=[{name="Design",interface=NONE}]},
				      {name="Analysis",
				       behavior=[{name="Analysis",interface=NONE}]},
				      {name="Manufacturability_Evaluation",
				       behavior=[{name="Manufacturability_Evaluation",
						  interface=NONE}]}],
			    relationshipType=[{name="BPEL_Design",
					       role=({ref_type="BPEL_Engine",
						      behavior=NONE},
						     {ref_type="Design",
						      behavior=NONE})},
					      {name="BPEL_Customer",
					       role=({ref_type="BPEL_Engine",
						      behavior=NONE},
						     {ref_type="Customer_Negotiation",
						      behavior=NONE})},
					      {name="Design_Analysis",
					       role=({ref_type="Analysis",
						      behavior=NONE},
						     {ref_type="Design",
						      behavior=NONE})},
					      {name="Design_Manufacturability",
					       role=({ref_type="Design",
						      behavior=NONE},
						     {ref_type="Manufacturability_Evaluation",behavior=NONE})}],
			    participantType=[],
			    channelType=[{name="CustomerChannel",usage=NONE,
					  action=NONE,passing=[],
					  role=[{ref_type="Customer_Negotiation",
						 behavior=NONE}],
					  reference=[{token={name="customerRef"}}],
					  identity=nil},
					 {name="DesignChannel",usage=NONE,
					  action=NONE,passing=[],
					  role=[{ref_type="Design",
						 behavior=NONE}],
					  reference=[{token={name="designRef"}}],
					  identity=nil},
					 {name="AnalysisChannel",usage=NONE,
					  action=NONE,passing=[],
					  role=[{ref_type="Analysis",
						 behavior=NONE}],
					  reference=[{token={name="analysisRef"}}],
					  identity=nil},
					 {name="ManufacturabilityChannel",usage=NONE,
					  action=NONE,passing=[],
					  role=[{ref_type="Manufacturability_Evaluation",
						 behavior=NONE}],
					  reference=[{token={name="manufacturabilityRef"}}],
					  identity=nil}],
			    choreography=[CDL.choreography {name="Negotiation",
					   complete=NONE,isolation=NONE,
					   root=NONE,coordination=NONE,
					   relationship=[],
					   variableDefinitions=[],
					   choreography=[],
					   activity=[],
					   exceptionBlock=[],
					   finalizerBlock=[]
					   }]		 
			    })

val test = WriteXmlTree.writeFile "test.cdl" (tPackage2Xml exampleChoreography) *)
end
