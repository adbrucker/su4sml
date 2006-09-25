structure SM_Helper =
struct
open Rep
open Rep_OclType
open Rep_OclTerm
open Rep_SecureUML_ComponentUML.Security
open ComponentUML
open XMI_DataTypes

val alwaysTrigger = CallEvent(["auto","auto","auto"],[])
val alwaysGuard = OperationCall(Variable("self",DummyT), DummyT, ["alwaysG"],[],Boolean):Guard

val emptyEvent = CallEvent(["","","EMPTY"],[])
val emptyGuard = OperationCall(Variable("",DummyT),DummyT,["EMPTY"],[],DummyT):Guard
val emptyState = (State_SimpleState({ state_id="",
				      outgoing=[],
				      incoming=[],
				      name=""}))

val emptyTransition = (T_mk({effect=NONE,
			    guard=NONE,
			    source="",
			    target="",
			    trans_id="",
			    trigger=NONE}))

val lastGuard = OperationCall(Variable("self",DummyT),DummyT,["else"],[],Boolean):Guard
val alwaysTrigger = CallEvent(["auto","auto","auto"],[])
val alwaysGuard = OperationCall(Variable("self",DummyT), DummyT, ["alwaysG"],[],Boolean):Guard
val emptyEffect = Proc_mk{proc_id="",
			  language="",
			  body="",
			  expression=""}
end
