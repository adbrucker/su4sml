structure SM_Helper : sig 
    val alwaysTrigger  : Rep.Event
    val alwaysGuard    : Rep.Guard
    val emptyEvent     : Rep.Event
    val emptyGuard     : Rep.Guard
    val emptyState     : Rep.StateVertex
    val emptyTransition: Rep.Transition
    val lastGuard      : Rep.Guard
    val emptyEffect    : Rep.Procedure
end =
struct
open Rep
open Rep_OclType
open Rep_OclTerm

(* open Rep_SecureUML_ComponentUML.Security*)
open ComponentUML
open XMI_DataTypes


val emptyEvent = CallEvent(["","","EMPTY"],[])
val emptyGuard = OperationCall(Variable("",DummyT),DummyT,["EMPTY"],[],DummyT):Guard
val emptyState = (Rep_StateMachine.State_SimpleState({ state_id="",
				      outgoing=[],
				      incoming=[],
				      name=""}))

val emptyTransition = {effect=NONE,
		       guard=NONE,
		       source="",
		       target="",
		       trans_id="",
		       trigger=NONE}
                      
val lastGuard = OperationCall(Variable("self",DummyT),DummyT,["else"],[],Boolean):Guard
val alwaysTrigger = CallEvent(["auto","auto","auto"],[])
val alwaysGuard = OperationCall(Variable("self",DummyT), DummyT, ["alwaysG"],[],Boolean):Guard
val emptyEffect = {proc_id="",
		   language="",
		   body="",
		   expression=""}
end
