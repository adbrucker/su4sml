
(** 
 * SML types corresponding to the WS-CDL xml schema.
 * also includes some helper functions.
 *)
structure CDL =
struct
open XML_Schema

(**
 * The CDL description type.
 *
 *<simpleType name="tDescriptionType">
 *   <restriction base="string">
 *     <enumeration value="documentation"/>
 *     <enumeration value="reference"/>
 *     <enumeration value="semantics"/>
 *   </restriction>
 * </simpleType>
 *
 *)
datatype tDescriptionType = documentation | reference | semantics

fun tDescriptionType2String documentation = "documentation"
  | tDescriptionType2String reference     = "reference"
  | tDescriptionType2String semantics     = "semantics"

fun tDescriptionTypeFromString "documentation" = documentation
  | tDescriptionTypeFromString "reference"     = reference
  | tDescriptionTypeFromString "semantics"     = semantics
  | tDescriptionTypeFromString ""              = documentation  

(* Basically, each type below that extends tExtensibleElements  *)
(* should have an additional record field: "description: {_type: tDescriptionType,
							  content: string }" *)
(* The content of the description is "free-form XML" *)

type tDescription = { description_type: tDescriptionType,
		      content: string }
(*
 <simpleType name="tBoolean-expr">
    <restriction base="string"/>
  </simpleType>
  <simpleType name="tXPath-expr">
    <restriction base="string"/>
  </simpleType>
*)

type tBoolean_expr = string
type tXPath_expr   = string

(*
  <simpleType name="tWhenType">
    <restriction base="string">
      <enumeration value="before"/>
      <enumeration value="after"/>
      <enumeration value="timeout"/>
    </restriction>
  </simpleType>
*)

datatype tWhenType = when_before | after | timeout

(*
  <simpleType name="tUsage">
    <restriction base="string">
      <enumeration value="once"/>
      <enumeration value="unlimited"/>
    </restriction>
  </simpleType>
*)

datatype tUsage = once | unlimited

fun tUsage2String once = "once"
  | tUsage2String unlimited = "unlimited"

fun tUsageFromString "once" = once
  | tUsageFromString "unlimited" = unlimited
 

(*
  <simpleType name="tAction2">
    <restriction base="string">
      <enumeration value="request"/>
      <enumeration value="respond"/>
    </restriction>
  </simpleType>
*)

datatype tAction2 = request | respond (* conflicts with tAction! *)

(*
  <simpleType name="tAction">
    <restriction base="string">
      <enumeration value="request-respond"/>
      <enumeration value="request"/>
      <enumeration value="respond"/>
    </restriction>
  </simpleType>
*)

datatype tAction = request | respond | request_respond (* conflicts with tAction2 *)

fun tAction2String request = "request"
  | tAction2String respond = "respond"
  | tAction2String request_respond = "request-respond"

fun tActionFromString "request" = request
  | tActionFromString "respond" = respond
  | tActionFromString "request-respond" = request_respond

(*
  <complexType name="tFinalizerReference">
    <complexContent>
      <extension base="cdl:tExtensibleElements">
        <attribute name="choreographyName" type="NCName" use="required"/>
        <attribute name="choreographyInstanceId" type="cdl:tXPath-expr"
                                                      use="optional"/>
        <attribute name="finalizerName" type="NCName" use="optional"/>
      </extension>
    </complexContent>
  </complexType>
*)

type tFinalizerReference = {choreographyName: NCName,
			    choreographyInstanceId: tXPath_expr option,
			    finalizerName: NCName}
			   

(*
  <complexType name="tVariableRef">
    <complexContent>
      <extension base="cdl:tExtensibleElements">
        <attribute name="variable" type="cdl:tXPath-expr" 
                use="required"/>
      </extension>
    </complexContent>
  </complexType>
*)

type tVariableRef = {variable: tXPath_expr}

(*
   <complexType name="tSourceVariableRef">
    <complexContent>
      <extension base="cdl:tExtensibleElements">
        <attribute name="variable" type="cdl:tXPath-expr" 
                use="optional"/>
        <attribute name="expression" type="cdl:tXPath-expr" 
                use="optional"/>
      </extension>
    </complexContent>
  </complexType>
*)

type tSourceVariableRef = {variable: tXPath_expr,
			               expression: tXPath_expr}


(*
  <complexType name="tCopy">
    <complexContent>
      <extension base="cdl:tExtensibleElements">
        <sequence>
          <element name="source" type="cdl:tSourceVariableRef"/>
          <element name="target" type="cdl:tVariableRef"/>
        </sequence>
        <attribute name="name" type="NCName" use="required"/>
        <attribute name="causeException" type="boolean" 
                use="optional" default="false"/>
      </extension>
    </complexContent>
  </complexType>
*)

type tCopy = {name: NCName,
	      causeException: bool option,
	      source: tSourceVariableRef ,
	      target: tVariableRef }
(*
  <complexType name="tRecord">
    <complexContent>
      <extension base="cdl:tExtensibleElements">
        <sequence>
          <element name="source" type="cdl:tSourceVariableRef"/>
          <element name="target" type="cdl:tVariableRef"/>
        </sequence>
        <attribute name="name" type="NCName" use="required"/>
        <attribute name="causeException" type="boolean" use="optional" default="false"/>
        <attribute name="when" type="cdl:tWhenType" use="required"/>
      </extension>
    </complexContent>
  </complexType>
*)

type tRecord = {name: NCName,
		causeException: bool option,
		when: tWhenType}

(*
  <complexType name="tVariableRecordRef">
    <complexContent>
      <extension base="cdl:tExtensibleElements">
        <attribute name="variable" type="cdl:tXPath-expr" 
                use="optional"/>
        <attribute name="recordReference" use="optional">
          <simpleType>
             <list itemType="NCName"/>
          </simpleType>
        </attribute>
        <attribute name="causeException" type="boolean" 
                use="optional" default="false"/>
      </extension>
    </complexContent>
  </complexType>
*)

type tVariableRecordRef = {variable: tXPath_expr option,
			   recordReference: NCName list,
			   causeException: bool option}

(*
  <complexType name="tExchange">
    <complexContent>
      <extension base="cdl:tExtensibleElements">
        <sequence>
          <element name="send" type="cdl:tVariableRecordRef"/>
          <element name="receive" type="cdl:tVariableRecordRef"/>
        </sequence>
        <attribute name="name" type="NCName" use="required"/>
        <attribute name="informationType" type="QName" 
                 use="optional"/>
        <attribute name="channelType" type="QName" 
                 use="optional"/>
        <attribute name="action" type="cdl:tAction2" use="required"/>
      </extension>
    </complexContent>
  </complexType>
*)

type tExchange = {name: NCName,
		  informationType: QName option,
		  channelType: QName option,
		  action: tAction,
		  send: tVariableRecordRef,
		  receive: tVariableRecordRef}
(*
 </complexType>
  <complexType name="tParticipate">
    <complexContent>
      <extension base="cdl:tExtensibleElements">
        <attribute name="relationshipType" type="QName" use="required"/>
        <attribute name="fromRole" type="QName" use="required"/>
        <attribute name="toRole" type="QName" use="required"/>
      </extension>
    </complexContent>
  </complexType>
*)

type tParticipate = {relationshipType: QName,
		     fromRole: QName,
		     toRole: QName}

(* 
  <complexType name="tTimeout">
    <complexContent>
      <extension base="cdl:tExtensibleElements">
        <attribute name="time-to-complete" type="cdl:tXPath-expr" use="required"/>
        <attribute name="fromRoleRecordReference" use="optional">
          <simpleType>
             <list itemType="NCName"/>
          </simpleType>
        </attribute>
        <attribute name="toRoleRecordReference" use="optional">
          <simpleType>
             <list itemType="NCName"/>
          </simpleType>
        </attribute>
      </extension>
    </complexContent>
  </complexType>
*)

type tTimeout = {time_to_complete: tXPath_expr,
		 fromRoleRecordReference: NCName list option, (* hmpf *)
		 toRoleRecordReference: NCName list option (* hmpf *)
					}

(*
  <complexType name="tBindVariable">
    <complexContent>
      <extension base="cdl:tExtensibleElements">
        <attribute name="variable" type="cdl:tXPath-expr" 
                 use="required"/>
        <attribute name="role" type="QName" use="required"/>
      </extension>
    </complexContent>
  </complexType>
*)

type tBindVariable = {variable: tXPath_expr,
		      role: QName}

(*
  <complexType name="tBind">
    <complexContent>
      <extension base="cdl:tExtensibleElements">
        <sequence>
          <element name="this" type="cdl:tBindVariable"/>
          <element name="free" type="cdl:tBindVariable"/>
        </sequence>
         <attribute name="name" type="NCName" use="required"/>
      </extension>
    </complexContent>
  </complexType>
*)

type tBind = {name:NCName,
	      this: tBindVariable,
	      free: tBindVariable}

(*
  <complexType name="tVariable">
    <complexContent>
      <extension base="cdl:tExtensibleElements">
        <attribute name="name" type="NCName" use="required"/>
        <attribute name="informationType" type="QName" 
                 use="optional"/>
        <attribute name="channelType" type="QName" use="optional"/>
        <attribute name="mutable" type="boolean" use="optional"
                 default="true"/>
        <attribute name="free" type="boolean" use="optional" 
                 default="false"/>
        <attribute name="silent" type="boolean" use="optional"
                 default="false"/>
        <attribute name="roleTypes" use="optional">
           <simpleType>
              <list itemType="QName"/>
           </simpleType>
        </attribute>
      </extension>
    </complexContent>
  </complexType>
*)

type tVariable = {name: NCName,
		  informationType: QName option,
		  channelType: QName option,
		  mutable: bool option,
		  free: bool option,
		  silent: bool option,
		  roleTypes: QName option}

(*
  <complexType name="tRelationshipRef">
    <complexContent>
      <extension base="cdl:tExtensibleElements">
        <attribute name="type" type="QName" use="required"/>
      </extension>
    </complexContent>
  </complexType>
*)

type tRelationshipRef = QName 


(*
  <complexType name="tVariableDefinitions">
    <complexContent>
      <extension base="cdl:tExtensibleElements">
        <sequence>
          <element name="variable" type="cdl:tVariable"
                  maxOccurs="unbounded"/>
        </sequence>
      </extension>
    </complexContent>
  </complexType>
*)

type tVariableDefinitions = tVariable list

(*
  <group name="activity">
    <choice>
      <element name="sequence" type="cdl:tSequence"/>
      <element name="parallel" type="cdl:tParallel"/>
      <element name="choice" type="cdl:tChoice"/>
      <element name="workunit" type="cdl:tWorkunit"/>
      <element name="interaction" type="cdl:tInteraction"/>
      <element name="perform" type="cdl:tPerform"/>
      <element name="assign" type="cdl:tAssign"/>
      <element name="silentAction" type="cdl:tSilentAction"/>
      <element name="noAction" type="cdl:tNoAction"/>
      <element name="finalize" type="cdl:tFinalize"/>
    </choice>
  </group>

  <complexType name="tSequence">
    <complexContent>
      <extension base="cdl:tExtensibleElements">
        <sequence>
          <group ref="cdl:activity" maxOccurs="unbounded"/>
        </sequence>
      </extension>
    </complexContent>
  </complexType>
  <complexType name="tParallel">
    <complexContent>
      <extension base="cdl:tExtensibleElements">
        <sequence>
          <group ref="cdl:activity" maxOccurs="unbounded"/>
        </sequence>
      </extension>
    </complexContent>
  </complexType>
  <complexType name="tChoice">
    <complexContent>
      <extension base="cdl:tExtensibleElements">
        <sequence>
          <group ref="cdl:activity" maxOccurs="unbounded"/>
        </sequence>
      </extension>
    </complexContent>
  </complexType>
  <complexType name="tWorkunit">
    <complexContent>
      <extension base="cdl:tExtensibleElements">
        <sequence>
          <group ref="cdl:activity"/>
        </sequence>
        <attribute name="name" type="NCName" use="required"/>
        <attribute name="guard" type="cdl:tBoolean-expr" 
                 use="optional"/>
        <attribute name="repeat" type="cdl:tBoolean-expr" 
                 use="optional"/>
        <attribute name="block" type="boolean" 
                 use="optional" default="false"/>
      </extension>
    </complexContent>
  </complexType>
  <complexType name="tPerform">
    <complexContent>
      <extension base="cdl:tExtensibleElements">
        <sequence>
          <element name="bind" type="cdl:tBind" 
                  minOccurs="0" maxOccurs="unbounded"/>
          <element name="choreography" type="cdl:tChoreography"
                   minOccurs="0" maxOccurs="1"/>
        </sequence>
        <attribute name="choreographyName" type="QName" use="required"/>
        <attribute name="choreographyInstanceId" type="cdl:tXPath-expr" use="optional"/>
      </extension>
    </complexContent>
  </complexType>

  <complexType name="tInteraction">
    <complexContent>
      <extension base="cdl:tExtensibleElements">
        <sequence>
          <element name="participate" type="cdl:tParticipate"/>
          <element name="exchange" type="cdl:tExchange" minOccurs="0"
                  maxOccurs="unbounded"/>
          <element name="timeout" type="cdl:tTimeout" minOccurs="0"
                  maxOccurs="1"/>
          <element name="record" type="cdl:tRecord" minOccurs="0" 
                  maxOccurs="unbounded"/>
        </sequence>
        <attribute name="name" type="NCName" use="required"/>
        <attribute name="channelVariable" type="QName" 
                 use="required"/>
        <attribute name="operation" type="NCName" use="required"/>
        <attribute name="align" type="boolean" use="optional" 
                 default="false"/>
        <attribute name="initiate" type="boolean" 
                 use="optional" default="false"/>
      </extension>
    </complexContent>
  </complexType>
  <complexType name="tAssign">
    <complexContent>
      <extension base="cdl:tExtensibleElements">
        <sequence>
      <element name="copy" type="cdl:tCopy"
               maxOccurs="unbounded"/>
        </sequence>
        <attribute name="roleType" type="QName" use="required"/>
      </extension>
    </complexContent>
  </complexType>
  <complexType name="tSilentAction">
    <complexContent>
      <extension base="cdl:tExtensibleElements">
        <attribute name="roleType" type="QName" use="optional"/>
      </extension>
    </complexContent>
  </complexType>
  <complexType name="tNoAction">
    <complexContent>
      <extension base="cdl:tExtensibleElements">
        <attribute name="roleType" type="QName" use="optional"/>
      </extension>
    </complexContent>
  </complexType>
  <complexType name="tFinalize">
    <complexContent>
      <extension base="cdl:tExtensibleElements">
        <sequence>
          <element name="finalizerReference" type="cdl:tFinalizerReference"
                                maxOccurs="unbounded"/>
        </sequence>
        <attribute name="name" type="NCName" use="required"/>
      </extension>
    </complexContent>
  </complexType>

(*
  <complexType name="tChoreography">
    <complexContent>
      <extension base="cdl:tExtensibleElements">
        <sequence>
          <element name="relationship" type="cdl:tRelationshipRef" 
                  maxOccurs="unbounded"/>
          <element name="variableDefinitions"
                  type="cdl:tVariableDefinitions" minOccurs="0"/>
          <element name="choreography" type="cdl:tChoreography"
                   minOccurs="0" maxOccurs="unbounded"/>
          <group ref="cdl:activity"/> (* what is this??? *)
          <element name="exceptionBlock" type="cdl:tException"
                  minOccurs="0"/>
          <element name="finalizerBlock" type="cdl:tFinalizer"
                      minOccurs="0" maxOccurs="unbounded"/>
        </sequence>
        <attribute name="name" type="NCName" use="required"/>
        <attribute name="complete" type="cdl:tBoolean-expr" 
                     use="optional"/>
        <attribute name="isolation" type="boolean" 
                     use="optional" default="false"/>
        <attribute name="root" type="boolean" use="optional" 
                     default="false"/>
        <attribute name="coordination" type="boolean" use="optional" 
                     default="false"/>
      </extension>
    </complexContent>
  </complexType>
*)

(*
  <complexType name="tFinalizer">
    <complexContent>
      <extension base="cdl:tExtensibleElements">
        <sequence>
          <element name="workunit" type="cdl:tWorkunit"/>
        </sequence>
        <attribute name="name" type="NCName" use="required"/>
      </extension>
    </complexContent>
  </complexType>
*)


(*
  <complexType name="tException">
    <complexContent>
      <extension base="cdl:tExtensibleElements">
        <sequence>
          <element name="workunit" type="cdl:tWorkunit"
                  maxOccurs="unbounded"/>
        </sequence>
        <attribute name="name" type="NCName" use="required"/>
      </extension>
    </complexContent>
  </complexType>
*)

*)

(* mutually recursive datatypes are quite a hassle... *)
datatype tActivity = sequence of tActivity list
		   | parallel of tActivity list
		   | choice   of tActivity list
		   | workunit of tWorkunit
		   | interaction of {name: NCName,
				     channelVariable: QName,
				     operation: NCName,
				     align: bool option,
				     initiate: bool option,
				     participate: tParticipate,
				     exchange: tExchange list,
				     timeout: tTimeout option,
				     record: tRecord list
					     }
		   | perform of {choreographyName: QName,
				 choreographyInstanceId: tXPath_expr option,
				 bind: tBind list,
				 choreography: tChoreography option}
		   | assign of {copy: tCopy list,
				 roleType: QName}
	           | silentAction of {roleType: QName option,
				      description: tDescription option}
		   | noAction of {roleType: QName option}
		   | finalize of {name: NCName,
				  finalizerReference: tFinalizerReference list}
and tChoreography = choreography of {name: NCName,
				     complete: tBoolean_expr option,
				     isolation: bool option,
				     root: bool option,
				     coordination: bool option,
				     relationship: tRelationshipRef list,
				     variableDefinitions: tVariableDefinitions,
				     choreography: tChoreography list,
				     activity: tActivity,
				     exceptionBlock: tException list,
				     finalizerBlock: tFinalizer list
						     }
and tFinalizer = tfinalizer of {name: NCName,
		   workunit: tWorkunit list
			     }
and tException = texception of {name: NCName,
		  workunit: tWorkunit list
			    }
withtype tWorkunit = {name: NCName,
		      guard: tBoolean_expr option,
		      repeat: tBoolean_expr option,
		      block: bool option,
		      activity: tActivity list}




(*
  <complexType name="tTokenReference">
    <complexContent>
      <extension base="cdl:tExtensibleElements">
        <attribute name="name" type="QName" use="required"/>
      </extension>
    </complexContent>
  </complexType>
*)

type tTokenReference = QName  (* this can probably be merged 
                                        with the type tReference... *)


(*
  <complexType name="tIdentity">
    <complexContent>
      <extension base="cdl:tExtensibleElements">
        <sequence>
          <element name="token" type="cdl:tTokenReference" 
                  minOccurs="1" maxOccurs="unbounded"/>
        </sequence>
      </extension>
    </complexContent>
  </complexType>
*)

type tIdentity = {token: tTokenReference list}

(*
  <complexType name="tReference">
    <complexContent>
      <extension base="cdl:tExtensibleElements">
        <sequence>
          <element name="token" type="cdl:tTokenReference"
                      minOccurs="1" maxOccurs="1"/>
        </sequence>
      </extension>
    </complexContent>
  </complexType>
*)

type tReference = {token: tTokenReference}

(*
  <complexType name="tPassing">
    <complexContent>
      <extension base="cdl:tExtensibleElements">
        <attribute name="channel" type="QName" use="required"/>
        <attribute name="action" type="cdl:tAction" use="optional" 
                 default="request"/>
        <attribute name="new" type="boolean" use="optional"
                 default="false"/>
      </extension>
    </complexContent>
  </complexType>
*)

type tPassing = {channel: QName,
		 action: tAction option,
		 new: bool option}

(*
  <complexType name="tRoleRef3">
    <complexContent>
      <extension base="cdl:tExtensibleElements">
        <attribute name="type" type="QName" use="required"/>
        <attribute name="behavior" type="NCName" use="optional"/>
      </extension>
    </complexContent>
  </complexType>
*)

type tRoleRef3 = {ref_type: QName,
		  behavior: NCName option }

(*
  <complexType name="tChannelType">
    <complexContent>
      <extension base="cdl:tExtensibleElements">
        <sequence>
          <element name="passing" type="cdl:tPassing" minOccurs="0"
                  maxOccurs="unbounded"/>
          <element name="role" type="cdl:tRoleRef3"/>
          <element name="reference" type="cdl:tReference"/>
          <element name="identity" type="cdl:tIdentity" minOccurs="0" 
                  maxOccurs="1"/>
        </sequence>
        <attribute name="name" type="NCName" use="required"/>
        <attribute name="usage" type="cdl:tUsage" use="optional" 
                     default="unlimited"/>
        <attribute name="action" type="cdl:tAction" use="optional"
                     default="request"/>
      </extension>
    </complexContent>
  </complexType>
*)

type tChannelType = {name: NCName,
		     usage: tUsage option,
		     action: tAction option,
		     passing: tPassing list,
		     role: tRoleRef3,
		     reference: tReference,
		     identity: tIdentity option}

(*
  <complexType name="tRoleRef2">
    <complexContent>
      <extension base="cdl:tExtensibleElements">
        <attribute name="type" type="QName" use="required"/>
      </extension>
    </complexContent>
  </complexType>
*)

type tRoleRef2 = {ref_type: QName}

(* 
  <complexType name="tParticipantType">
    <complexContent>
      <extension base="cdl:tExtensibleElements">
        <sequence>
          <element name="role" type="cdl:tRoleRef2" 
                  maxOccurs="unbounded"/>
        </sequence>
        <attribute name="name" type="NCName" use="required"/>
      </extension>
    </complexContent>
  </complexType>
*)

type tParticipantType = {name: NCName,
			 role: tRoleRef2 list (* hmpf... *)}

(*
  <complexType name="tRoleRef">
    <complexContent>
      <extension base="cdl:tExtensibleElements">
        <attribute name="type" type="QName" use="required"/>
        <attribute name="behavior" use="optional">
          <simpleType>
             <list itemType="NCName"/>
          </simpleType>
        </attribute>
      </extension>
    </complexContent>
  </complexType>
*)

type tRoleRef = {ref_type: QName,
		 behavior: NCName list option (* hmpf... *)
			   }

(* 
  <complexType name="tRelationshipType">
    <complexContent>
      <extension base="cdl:tExtensibleElements">
        <sequence>
          <element name="role" type="cdl:tRoleRef" minOccurs="2"
                  maxOccurs="2"/>
        </sequence>
        <attribute name="name" type="NCName" use="required"/>
      </extension>
    </complexContent>
  </complexType>
*)

type tRelationshipType = {name: NCName,
			  role: tRoleRef * tRoleRef
				}

(*
  <complexType name="tBehavior">
    <complexContent>
      <extension base="cdl:tExtensibleElements">
        <attribute name="name" type="NCName" use="required"/>
        <attribute name="interface" type="QName" use="optional"/>
      </extension>
    </complexContent>
  </complexType>
*)

type tBehavior = {name: NCName,
		  interface: QName option
			     }

(*
  <complexType name="tRoleType">
    <complexContent>
      <extension base="cdl:tExtensibleElements">
        <sequence>
          <element name="behavior" type="cdl:tBehavior"
                  maxOccurs="unbounded"/>
        </sequence>
        <attribute name="name" type="NCName" use="required"/>
      </extension>
    </complexContent>
  </complexType>
*)

type tRoleType = {name: NCName,
		  behavior: tBehavior list
			     }

(*
  <complexType name="tTokenLocator">
    <complexContent>
      <extension base="cdl:tExtensibleElements">
        <attribute name="tokenName" type="QName" use="required"/>
        <attribute name="informationType" type="QName"
                 use="required"/>
        <attribute name="part" type="NCName" use="optional" />
        <attribute name="query" type="cdl:tXPath-expr" 
                 use="required"/>
      </extension>
    </complexContent>
  </complexType>
*)

type tTokenLocator = {tokenName: QName,
		      informationType: QName,
		      part: NCName option,
		      query: tXPath_expr
			     }

(*
  <complexType name="tInformationType">
    <complexContent>
      <extension base="cdl:tExtensibleElements">
        <attribute name="name" type="NCName" use="required"/>
        <attribute name="type" type="QName" use="optional"/>
        <attribute name="element" type="QName" use="optional"/>
        <attribute name="exceptionType" type="boolean" use="optional"    
                   default="false" />
      </extension>
    </complexContent>
  </complexType>
*)

type tInformationType = {name: NCName,
			 information_type: QName option,
			 element: QName option,
			 exceptionType: bool option
					}
     
(*
  <complexType name="tToken">
    <complexContent>
      <extension base="cdl:tExtensibleElements">
        <attribute name="name" type="NCName" use="required"/>
        <attribute name="informationType" type="QName"
                 use="required"/>
      </extension>
    </complexContent>
  </complexType>
*)
type tToken = { name: NCName,
		informationType: QName
		} 


(*
 <complexType name="tPackage">
   <complexContent>
      <extension base="cdl:tExtensibleElements">
        <sequence>
          <element name="informationType" type="cdl:tInformationType" 
                  minOccurs="0" maxOccurs="unbounded"/>
          <element name="token" type="cdl:tToken" minOccurs="0"
                  maxOccurs="unbounded"/>
          <element name="tokenLocator" type="cdl:tTokenLocator" 
                  minOccurs="0" maxOccurs="unbounded"/>
          <element name="roleType" type="cdl:tRoleType" minOccurs="0"
                  maxOccurs="unbounded"/>
          <element name="relationshipType" type="cdl:tRelationshipType" 
                  minOccurs="0" maxOccurs="unbounded"/>
          <element name="participantType" type="cdl:tParticipantType" 
                  minOccurs="0" maxOccurs="unbounded"/>
          <element name="channelType" type="cdl:tChannelType"
                  minOccurs="0" maxOccurs="unbounded"/>
          <element name="choreography" type="cdl:tChoreography" 
                  minOccurs="0" maxOccurs="unbounded"/> 
        </sequence> 
        <attribute name="name" type="NCName" use="required"/>
        <attribute name="author" type="string" use="optional"/>
        <attribute name="version" type="string" use="optional"/>
        <attribute name="targetNamespace" type="anyURI" 
                 use="required"/>
      </extension>
    </complexContent>
  </complexType> *)


type tPackage = {name: NCName, 
		 author: string,
		 version: string,
		 targetNamespace: anyURI,
                 schema: xsdSchema,
		 informationType: tInformationType list,
		 token: tToken list,
		 tokenLocator: tTokenLocator list,
		 roleType: tRoleType list,
		 relationshipType: tRelationshipType list,
		 participantType: tParticipantType list,
		 channelType: tChannelType list,
		 choreography: tChoreography list
			       }
end
