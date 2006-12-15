(*****************************************************************************************)
(* 		   su4sml - State Machine generator (SMG)				 *)
(* 			    based upon GCG						 *)
(* 											 *)
(* stateMachine.sml - implementation of the Statechart->StateMachine transformer.	 *)
(* 											 *)
(* 	    Copyright (C) 2005 by Rolf Simon Adelsberger (RSA)				 *)
(* 			<rsa@student.ethz.ch>						 *)
(* 											 *)
(* This file is part of the StateMachine cartridge for su4sml 				 *)
(*                                                                            		 *)
(* su4sml is free software; you can redistribute it and/or modify it under   		 *)
(* the terms of the GNU General Public License as published by the Free       		 *)
(* Software Foundation; either version 2 of the License, or (at your option)  		 *)
(* any later version.                                                         		 *)
(*                                                                            		 *)
(* su4sml is distributed in the hope that it will be useful, but WITHOUT ANY 		 *)
(* WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS 		 *)
(* FOR A PARTICULAR PURPOSE. See the GNU General Public License for more 		 *)
(* details.                                                              		 *)
(*                                                                            		 *)
(* You should have received a copy of the GNU General Public License along    		 *)
(* with this program; if not, write to the Free Software Foundation, Inc.,    		 *)
(* 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.				 *)
(*****************************************************************************************)

(*use "../../me/sources/types.sml";
use "../../me/sources/helperFunctions.sml";
use "../../me/sources/stringHandling.sml";


use "SM_helper.sml";
*)

structure StateMachine =
struct

open Rep
open Rep_OclType
open Rep_OclTerm
open Rep_StateMachine
open Rep_SecureUML_ComponentUML.Security
open SM_Helper
open StateMachineTypes

(********************************************)
(* Generic types handling functions:	    *)
(* ---------------------------------	    *)
(********************************************)

(*simulate SET behaviour*)
(* ''a list -> ''a list *)
fun createDistinct([]) = []
  | createDistinct(h::t) = createDistinct((List.filter (fn x => not (x=h)) t))@[h]

(*return the initial state*)
exception MalformedStateMachine


(************************************)
(* STATE handling functions:	    *)
(* -------------------------	    *)
(************************************)

fun is_StartState(PseudoState{kind,...}) = (kind=XMI.initial)
  | is_StartState(_) = false

(* StateVertex -> StateVertex_Id *)
fun id_of_state(State_SimpleState{state_id,...}) = state_id
  | id_of_state(State_CompositeState{state_id,...}) = state_id
  | id_of_state(SimpleState_ActionState{state_id,...}) = state_id
  | id_of_state(SimpleState_ObjectflowState{state_id,...}) = state_id
  | id_of_state(State_FinalState{state_id,...}) = state_id
  | id_of_state(PseudoState{state_id,...}) = state_id
  | id_of_state(SyncState{state_id,...}) = state_id

fun name_of_state(State_SimpleState{name,...}) = name
  | name_of_state(State_CompositeState{name,...}) = name
  | name_of_state(SimpleState_ActionState{name,...}) = name
  | name_of_state(State_FinalState{...}) = "Final"
  | name_of_state(S as PseudoState{kind,...}) = case kind
						 of XMI.initial => "INIT"^id_of_state(S)
						  | XMI.junction => "ERROR"
						  | _ => "WRONG"

(*  StateVertex_Id * StateVertex list ref -> StateVertex *)
fun get_state_by_id(id:StateVertex_Id, SLp: StateVertex list ref) = let fun hasID i S = (id_of_state(S) = i)
									val filterf = hasID id
								    in
									hd (List.filter filterf (!SLp))
								    end

(*returns the list of subvertices *)
(* StateVertex -> StateVertex list *)
fun get_subvertex_list(State_CompositeState{subvertex,...}) = subvertex
  | get_subvertex_list(S:StateVertex) = []

(*return list of outgoing states' transition ids*)
(* StateVertex -> Transition_Id list *)
fun get_next_transitions_ID(State_SimpleState{outgoing,...}) = outgoing
  | get_next_transitions_ID(State_CompositeState{outgoing,...}) = outgoing
  | get_next_transitions_ID(SimpleState_ActionState{outgoing,...}) = outgoing
  | get_next_transitions_ID(SimpleState_ObjectflowState{outgoing,...}) = outgoing
  | get_next_transitions_ID(State_FinalState{...}) = [] 
  | get_next_transitions_ID(PseudoState{outgoing,...}) = outgoing
  | get_next_transitions_ID(SyncState{outgoing,...}) = outgoing

(* StateVertex -> Transition_Id list *)
fun get_prev_transitions_T(State_SimpleState{incoming,...}) = incoming
  | get_prev_transitions_T(State_CompositeState{incoming,...}) = incoming
  | get_prev_transitions_T(SimpleState_ActionState{incoming,...}) = incoming
  | get_prev_transitions_T(SimpleState_ObjectflowState{incoming,...}) = incoming
  | get_prev_transitions_T(State_FinalState{...}) = [] 
  | get_prev_transitions_T(PseudoState{incoming,...}) = incoming
  | get_prev_transitions_T(SyncState{incoming,...}) = incoming

(* StateVertex -> bool *)
fun isPseudo(PseudoState{kind,...}) = not(kind=XMI.initial)
  | isPseudo(_) = false

fun isInit(PseudoState{kind,...}) = kind=XMI.initial
  | isInit(_) = false

(* StateVertex -> bool *)
(* fun Final(State_FinalState x) = true *)
(*   | Final(_) = false *)
fun isFinal(State_FinalState{...}) = true
  | isFinal(_) = false


fun FinalState(SL:StateVertex list) = hd (List.filter (fn (State_FinalState x) => true
							| (_) => false) SL)

(* get list of subvertices *)
(* StateVertex -> StateVertex list *)
fun subvertices_of_state(State_CompositeState{subvertex,...}) = subvertex
  | subvertices_of_state(S:StateVertex) = []

(* StateVertex * SM_Trans list ref -> SM_Trans list *)
fun get_next_SM_Trans(S, TLp) = let fun filterF({source,...}:SM_Trans) = (source = id_of_state(S))
				in 
				    List.filter filterF (!TLp)
				end

(* StateVertex * SM_Trans list ref * 'a -> Event list *)
fun events_of_state(S:StateVertex, TLp, SLp) = let val TransitionOutList = get_next_SM_Trans(S,TLp)
						   fun TRIGGERS({triggers,...}:SM_Trans) = triggers
						   fun collectEvts([]) = []
						     | collectEvts(h::t) = TRIGGERS(h)::collectEvts(t)
					       in 
						   createDistinct(List.concat (collectEvts(TransitionOutList)))
					       end

(*****************************************)
(* Transition handling functions:	 *)
(* ------------------------------	 *)
(*****************************************)

(* Transition -> Transition_Id *)
fun id_of_trans(T_mk{trans_id,...}) = trans_id

(* Transition -> StateVertex_Id *)
fun source_of_trans(T_mk{source,...}) = source

(* Transition -> StateVertex_Id *)
fun target_of_trans(T_mk{target,...}) = target

(* Transition -> Guard option *)
fun guard_of_trans(T_mk{guard,...}) = guard

fun effect_of_trans(T_mk{effect,...}) = effect

(* Transition -> Event option *)
fun trigger_of_trans(T_mk{trigger,...}) = trigger


(***************************************)
(* SM_Trans handling functions:	       *)
(* ----------------------------	       *)
(***************************************)

(* SM_Trans -> StateVertex_Id *)
fun target_of_SM_Trans({target,...}:SM_Trans) = target

(* SM_Trans -> Guard list *)
fun guards_of_SM_Trans({guards,...}:SM_Trans) = guards

(* SM_Trans -> Event list *)
fun triggers_of_SM_Trans({triggers,...}:SM_Trans) = triggers

(* Transition_Id * StateVertex_Id * StateVertex_Id * Guard list * Event list * Action list -> SM_Trans *)
fun mk_SM_T(tid, s, t, ga, ta, ea) = {trans_id = tid,
				      source = s,
				      target = t,
				      guards = ga,
				      triggers = ta,
				      effects = ea}:SM_Trans


(************************************)
(* Event handling functions:	    *)
(* -------------------------	    *)
(************************************)

(* Event -> string *)
fun name_of_event(CallEvent([a,b,c],_):Event) = c
  | name_of_event(_) = "BLAEVENENT"

(* Event -> string *)
fun path_of_event(CallEvent([a,b,c],_)) = c
  | path_of_event(_) = ""




(*****************************************)
(* Classifier handling functions:	 *)
(* ------------------------------	 *)
(*****************************************)

(*returns the activity graphs (list) of the given Classifier --> this is a list of StateMachines*)
(* Classifier -> ActivityGraph list *)
fun ActGraph_of_classif(Class{activity_graphs,...}) = activity_graphs
  | ActGraph_of_classif(_) = []

(*get the list of transitions *)
(* ActivityGraph -> Transition list *)
fun transitions_of_SM(SM_mk{transition,...}:ActivityGraph) = transition

(*return top state of a state machine*)
(* StateMachine -> StateVertex *)
fun top_state_of(SM_mk{top,...}) = top

(*get all defined activity graphs in a list of Classifiers --> ActivityGraph list*)
(* Classifier list -> ActivityGraph list *)
fun ActGraph_of_classif_from_list(C:Classifier list) = let fun isFull l = (fn x => (List.length x > 0)) l
						in
						    List.concat (List.filter isFull (List.map ActGraph_of_classif C))
						end


fun name_of_classif(C as  Class c) = Rep.short_name_of C
  | name_of_classif(Primitive p) = "PRIMITIVE"
  | name_of_classif(_) = "XXX"

(*return list of subertices ot the given Classifier (Class, Primitive,...)*)
(* Classifier -> StateVertex list *)
fun states_of_classif(C:Classifier) = let fun soc(x) = let val SM:StateMachine list = ActGraph_of_classif(x)
							in 
							   if SM=[] then []
							   (*FIXME: SM can have more than one AG defined... (remove "hd")
								       SM can be another type than CompositeState...*)
							   else get_subvertex_list(top_state_of(hd SM))
						       end
					  val SL1 = soc(C)
					  fun dive([]) = []
					    | dive((State_CompositeState{name=n,
									 subvertex=a,
									 incoming=b,
									 isConcurrent=c,
									 outgoing=d,
									 state_id=f})::t) = a@[(State_CompositeState{name=n,
														     subvertex=a,
														     incoming=b,
														     isConcurrent=c,
														     outgoing=d,
														     state_id=f})]@dive(t)
					    | dive(h::t) = h::dive(t)
				      in 
					  dive(SL1)
				      end



(* Classifier -> Transition list *)
fun transitions_of_classif(C:Classifier) = let val AG:ActivityGraph list = ActGraph_of_classif(C)
					   in 
					       if AG = [] then []
					       else transitions_of_SM(hd AG)
					   end

fun events_of_classif(C:Classifier) = let val TL = transitions_of_classif(C)
					  val TrigL = List.map trigger_of_trans TL
				      in 
					  createDistinct (List.map Option.valOf (List.filter Option.isSome TrigL))
				      end






(*returns a string with every transition_id separated by a "," *)
(* Classifier -> string *)
fun get_names_of_trans(C:Classifier) = let val SM = ActGraph_of_classif C
					   val transitions_LL = List.map transitions_of_SM SM
					   fun concat_names s [] = s
					     | concat_names s (h::t) = concat_names (s^","^h) t
					   val ids = List.map id_of_trans (List.concat transitions_LL)
				       in 
					 concat_names "" ids
				       end




(*processes the list of given {Simple,Pseudo,...} states and creates a "sane" representation. *)
(* Transition_Id * Transition list ref -> Transition *)
fun get_trans_by_id(id:Transition_Id, TLp: Transition list ref) = let fun hasID i T = (id_of_trans(T) = i)
								      val filterf = hasID id
								      val TL = !TLp
								  in 
								      hd (List.filter filterf TL)
								  end


(*returns list of state having given kind. NOTE: this only applies for PseudoStates*)
(* PseudoStateVars * StateVertex list -> StateVertex list *)
fun get_state_by_kind(KIND:PseudoStateVars, SVL: StateVertex list) = let fun hasKind k (PseudoState{kind,...}) = (k = kind)
									   | hasKind k _ = false
									 val filterF = hasKind KIND
								     in 
									 List.filter filterF SVL
								     end

(* StateVertex list -> StateVertex *)
fun get_initial(SVL: StateVertex list) = let val IL = get_state_by_kind(XMI.initial,SVL)
					     val count = List.length IL
					 in 
					     if (count > 1) orelse (count = 0) then raise MalformedStateMachine
					     else hd IL
					 end

(* StateVertex list -> StateVertex list *)
fun get_PseudoStates(C:StateVertex list) = List.filter isPseudo C

(* StateVertex list -> StateVertex list *)
fun get_other_States(C:StateVertex list) = List.filter (fn x => (not (isPseudo x))) C




(* returns the list of state ids that are targets of the items in the transition list "outgoing" *)
(* StateVertex * Transition list ref -> Transition list *)
fun get_next_transitions(S:StateVertex, TransL: Transition list ref) = let fun filterf x y = get_trans_by_id(x, ref y)
									   val OUT_T = get_next_transitions_ID(S)
									   val funList = List.map filterf OUT_T
									   fun foreach([],R,T) = R
									     | foreach(h::t,R,T) = foreach(t,(h T)::R, T)
								       in 
									   foreach(funList,[],!TransL)
								       end

(* Transition_Id * Transition list -> StateVertex_Id *)
fun next_state_id(TransID: Transition_Id, TransList: Transition list) = let val TRANS = get_trans_by_id(TransID, ref TransList)
									in 
									    target_of_trans(TRANS)
									end

(* Transition * Transition list * StateVertex list -> GEPath *)
(* fun GE(T, TL, SL) = let val S = get_state_by_id(target_of_trans(T),ref SL) *)
(* 			val TRIGGER = trigger_of_trans(T) *)
(* 			val GUARD = guard_of_trans(T) *)
(* 			val NEXTL = get_next_transitions(S,ref TL) *)
(* 		    in  *)
(* 			if Pseudo(S) then GEBranch(GUARD,TRIGGER,List.map (fn X => GE(X,TL,SL)) NEXTL) *)
(* 			else GELeaf(GUARD,TRIGGER,target_of_trans(T)) *)
(* 		    end *)

(* fun traverseGE(GELeaf(one,two,three)) = [([one],[two],three)] *)
(*   | traverseGE(GEBranch(one,two,LIST)) = let val DEEPER = List.map traverseGE LIST *)
(* 					     val PRUNED = List.concat DEEPER *)
(* 					     fun ADD(L1,L2,S) = (one::L1,two::L2,S) *)
(* 					     val REALLY = List.map ADD PRUNED *)
(* 					 in  *)
(* 					     REALLY *)
(* 					 end *)

(*or, without the GEPath datastructure: *)

(* operation * Transition list -> bool *)
fun acts_as_trigger(O:operation,T: Transition list) = let fun collect_triggers(TL) = let val triggerList = List.map (fn T_mk{trigger,...} => trigger) TL
											 val triggerList_pure = List.filter Option.isSome triggerList
											 val EventList = List.map Option.valOf triggerList_pure
											 fun getPath(CallEvent(path,parameters)) = path
											   | getPath(_) = []: Path
											 fun extractPath(E:Event) = List.nth(getPath(E),2)
										     in 
											 List.map extractPath EventList
										     end
							  val TriggerID_list = collect_triggers(T)
						      in
							  List.exists (fn x => x = name_of_op(O)) TriggerID_list
						      end



(* operation list * Transition list -> operation list *)
fun get_triggers(O: operation list, T: Transition list) = let fun filterf([],Ls,Lr) = Lr
								| filterf(h::t, Ls, Lr) = if acts_as_trigger(h,Ls) then filterf(t,Ls,h::Lr)
											  else filterf(t,Ls,Lr)
							  in 
							     filterf(O,T,[]) 
							  end





(* Classifier -> string list *)
fun get_event_list(C:Classifier) = let val EL = events_of_classif(C)
				   in 
				       List.map name_of_event EL
				   end

(* returns the path (string list) of the operation call *)
(* OclTerm -> Path *)
fun path_of_operation(OperationCall(_,_,P,L,_)) = P
  | path_of_operation(_) = []

(* Guard -> Path *)
fun path_of_guard(G:Guard) = path_of_operation(G)


(* Guard -> string *)
fun ident_of_guard(G:Guard) =  let val sop = string_of_path(path_of_operation(G))
			       in 
				   case sop
				    of "else" => "elseG"
				     | _ => sop
			       end

(* (Guard option list * Event option list * 'a) list -> (string list * string list * 'a) list *)
fun processPL(L) = let fun processOptionL(LIST) = List.map (fn X => Option.valOf(X)) (List.filter Option.isSome LIST)
 		       fun processOCList(LIST) = List.concat (List.map path_of_guard LIST)
 		       fun processEVList(LIST) = List.map name_of_event LIST
 		       fun transform(a,b,c) = (processOCList(processOptionL(a)),processEVList(processOptionL(b)),c) 
		   in 
		       List.map transform L
		   end

(*Classifier -> bool *)
fun hasAG(C:Classifier) = let val AG = ActGraph_of_classif(C)
			  in 
			      not (AG = [])
			  end

(* OclTerm -> bool *)
fun is_else_Guard(G as OperationCall(Variable(_,_),_,[a,b,c],_,_)) = a = "else"
  | is_else_Guard(_) = false


(* StateVertex -> bool *)
fun isComposite(State_CompositeState{...}) = true
  | isComposite(_) = false

(*re-route all transitions pointing to a Composite State.
  Afther this there does _NOT_ exist a transition from any
  state to a Composite State anymore *)
(* Transition list * StateVertex list ref -> Transition list *)
fun correct_TransList([],_) = []
  | correct_TransList((head as T_mk{trans_id=tid,
				    source=src,
				    target=tgt,
				    guard=gd,
				    trigger=tr,
				    effect=eff})::tail,StateList) = let val S = get_state_by_id(target_of_trans(head),StateList)
								    in 
									case isComposite(S)
									 of true => let val subvertices = get_subvertex_list(S)
											(*val outList = get_next_transitions(S,TransList)*)
											val NextInit = get_initial(subvertices)
										    in 
											T_mk{trans_id=tid,
											     source=src,
											     target=id_of_state(NextInit),
											     guard=gd,
											     trigger=tr,
											     effect=eff}::correct_TransList(tail,StateList)
										    end
											
									  | false => head::correct_TransList(tail,StateList)
								    end

(* ''a * ''a list -> bool *)
fun ListMember(a,L) = List.exists (fn X => X=a) L


(* ''a * ''a list -> ''a list *)
fun removeFromList(a,[]) = []
  | removeFromList(a,h::t) = if a=h then removeFromList(a,t)
			     else h::removeFromList(a,t)

(*assumption: elements are unique --> set-like *)
(* ''a list * ''a list -> ''a list *)
fun ListDifference([],L2) = L2
  | ListDifference(L1,[]) = L1
  | ListDifference(h::t,L) = if ListMember(h,L) then ListDifference(t,removeFromList(h,L))
			     else h::ListDifference(t,removeFromList(h,L))
(* Transition -> bool *)
fun isTriggered(T_mk{trigger=SOME(x),...}) = true
  | isTriggered(_) = false

(* return the transitions of this composite state having a trigger defined *)
(* StateVertex * Transition list ref -> Transition list *)
fun triggered_OUT_Trans(State_CompositeState{outgoing,...}, TL) = let val Trans = List.map (fn X => get_trans_by_id(X,TL)) outgoing
								  in 
								      List.filter isTriggered Trans
								  end
  | triggered_OUT_Trans(_) = []

(* here we have to handle two cases:
	1) the super state's (composite state) transitions without a trigger
	2) the SS' transitions with a trigger
   In the case of 1) we only have to re-route these transitions such that they have as origin the inner finalState
   To tackle 2) we have to create for each inner state an additional transition to the the targets of those transitions.
   Of course the Events/Guards/Effects have to be preserved...
*)
(* StateVertex list * Transition list * Transition list ref -> Transition list *)
fun correctCompositeTransitions([],RES,_) = RES
  | correctCompositeTransitions((SCS as State_CompositeState{name,
							     state_id,
							     outgoing,
							     incoming,
							     subvertex,
							     isConcurrent})::tail,newTrans,allTrans) = let val SV = get_subvertex_list(SCS)
													   val subverticesID = List.map id_of_state SV
													   val subvertex_count = List.length(SV)
													   val triggered = triggered_OUT_Trans(SCS,allTrans)
													   val outlist = get_next_transitions(SCS,allTrans)
													   val untriggered = ListDifference(triggered,outlist)
													   val Final = FinalState(SV)
													   fun createIncList 0 = []
													     | createIncList n  = (createIncList (n-1))@[n]
													   fun handleUntriggered([],_) = []
													     | handleUntriggered(h::t,T_id) = T_mk{trans_id = "newUT_"^Int.toString(T_id),
																		   source = id_of_state(Final),
																		   target = target_of_trans(h),
																		   guard = guard_of_trans(h),
																		   trigger = trigger_of_trans(h),
																		   effect = effect_of_trans(h)}::handleUntriggered(t,(T_id+1))
													   fun createTrans T_id T S = T_mk{trans_id = T_id,
																	   source = S,
																	   target = target_of_trans(T),
																	   guard = guard_of_trans(T),
																	   trigger = trigger_of_trans(T),
																	   effect = effect_of_trans(T)}
													   fun applyList([],_) = []
													     | applyList(h::t,fList) = (List.map (fn X => X(h)) fList)@applyList(t,fList)
																       
													   fun handleTriggered([],_) = []
													     | handleTriggered(h::t,T_id) = let val idList = List.map (fn X => Int.toString(T_id)^"_"^Int.toString(X)) (createIncList subvertex_count)
																		val funList = (List.map (fn X => X(h)) (List.map createTrans idList))
																	    in
																		applyList(subverticesID,funList)@handleTriggered(t,(T_id+1))
																	    end
													   val newTriggered = handleTriggered(triggered,0)
													   val newUTriggered = handleUntriggered(untriggered,0)
												       in 
													   correctCompositeTransitions(tail,
																       newTrans@newTriggered@newUTriggered,
																       allTrans)
												       end
  | correctCompositeTransitions(h::t,nt,at) = correctCompositeTransitions(t,nt,at)

(* StateVertex list * Transition list -> StateVertex list *)
fun updateStates([],_) = []
  | updateStates(h::t,Transitions) = let val affectedOutTransitions = List.filter (fn X => source_of_trans(X) = id_of_state(h)) Transitions (* transitions that source from h *)
					 val affectedInTransitions = List.filter (fn X => target_of_trans(X) = id_of_state(h)) Transitions
					 val newOut = createDistinct((List.map id_of_trans affectedOutTransitions)@get_next_transitions_ID(h))
					 val newIn = createDistinct((List.map id_of_trans affectedInTransitions)@get_prev_transitions_T(h))
					 fun updateState(State_CompositeState{name=n,state_id=sid,outgoing=ol,incoming=il,subvertex=sv,isConcurrent=c}) = State_CompositeState{name=n,state_id=sid,outgoing=ol@newOut,incoming=il@newIn,subvertex=sv,isConcurrent=c}
 					   | updateState(State_SimpleState{name=n,state_id=sid,outgoing=ol,incoming=il}) = State_SimpleState{name=n,state_id=sid,outgoing=ol@newOut,incoming=il@newIn} 
					   | updateState(SimpleState_ActionState{name=n,state_id=sid,outgoing=ol,incoming=il,isDynamic=d}) = SimpleState_ActionState{name=n,state_id=sid,outgoing=ol@newOut,incoming=il@newIn,isDynamic=d}
					   | updateState(SimpleState_ObjectflowState{state_id=sid,outgoing=ol,incoming=il,isSynch=s,parameter=p,types=t}) = SimpleState_ObjectflowState{state_id=sid,outgoing=ol,incoming=il,isSynch=s,parameter=p,types=t}
					   | updateState(State_FinalState{state_id=sid,incoming=il}) = State_FinalState{state_id=sid,incoming=il@newIn}
					   | updateState(PseudoState{state_id=sid,kind=k,outgoing=ol,incoming=il}) = PseudoState{state_id=sid,kind=k,outgoing=ol@newOut,incoming=il@newIn}
					   | updateState(SyncState{state_id=sid,outgoing=ol,incoming=il}) = SyncState{state_id=sid,outgoing=ol@newOut,incoming=il@newIn}
				     in 
					 updateState(h)::updateStates(t,Transitions)
				     end


(* 
 T: Transition
 TL: Transition list ref (all)
 SL: (State * parent) list ref (all) 

for every transition T we calculate the propper transition list to every target state.
I.e. we "divide out" the CompositeStates and the PseudoStates.
*)  
fun CollectTGE(T, TL, SL) = let  val S = get_state_by_id(target_of_trans(T), SL)
				 val TRIGGER = trigger_of_trans(T)
				 val GUARD = guard_of_trans(T)
				 val EFFECT = effect_of_trans(T)
				 val NEXTL = get_next_transitions(S,TL)
				 fun ADD(L1,L2,L3,S) = (GUARD::L1,TRIGGER::L2,EFFECT::L3,S)
			    in 
				if isPseudo(S) orelse isInit(S) then List.map ADD (List.concat (List.map (fn X => CollectTGE(X,TL,SL)) NEXTL))
				else [([GUARD],[TRIGGER],[EFFECT],target_of_trans(T))]
			    end

(* add transitions of to the out-states of the 'parent' states inside a composite state *)
(* NOTE: has to be called with a list of states that does NOT contain pseudo states!!*)

(*  StateVertex list * Transition list * Transition list * StateVertex list -> SM_Trans list list *)
fun calculate_SM_Trans([],_,_,_) = []
  | calculate_SM_Trans((SCS as State_CompositeState{name,state_id,outgoing,incoming,subvertex,isConcurrent})::t,
		       Parent_OutList,
		       TransList,
		       StateList) = let val outlist = get_next_transitions(SCS, ref TransList)
				    in
					calculate_SM_Trans(subvertex,
							   Parent_OutList@outlist,
							   TransList,
							   StateList)@calculate_SM_Trans(t,
											 Parent_OutList,
											 TransList,
											 StateList)
				    end
  | calculate_SM_Trans(h::t,
		       Parent_OutList:Transition list,
		       TransList:Transition list,
		       StateList:StateVertex list) = let val outlist = get_next_transitions(h,ref TransList):Transition list
							 val TL = List.concat (List.map (fn X => CollectTGE(X,ref TransList,ref StateList)) outlist)
							 val UTriggeredParentT = List.concat (List.map (fn X => CollectTGE(X,ref TransList, ref StateList)) (List.filter (fn X => not(isTriggered(X))) Parent_OutList))
							 val TriggeredParentT = List.concat (List.map (fn X => CollectTGE(X,ref TransList, ref StateList)) (List.filter isTriggered Parent_OutList))
							 val ParentT = List.concat (List.map (fn X => CollectTGE(X,ref TransList, ref StateList)) Parent_OutList)
							 fun getValOfOptionList(LIST) = List.map (fn X =>Option.valOf(X)) (List.filter Option.isSome LIST)
							 fun Path2SM_Trans Source (Guards,Events,Effects,Target) = { trans_id = "nothing",
														     source = Source,
														     target = Target,
														     guards = Guards,
														     triggers = Events,
														     effects = Effects
														     }:SM_Trans
							 val PList = List.map (fn (a,b,c,d) => (getValOfOptionList(a),getValOfOptionList(b),getValOfOptionList(c),d)) TL
							 val TriggeredPPList = List.map (fn (a,b,c,d) => (getValOfOptionList(a),getValOfOptionList(b),getValOfOptionList(c),d)) TriggeredParentT
							 val PPList = List.map (fn (a,b,c,d) => (getValOfOptionList(a),getValOfOptionList(b),getValOfOptionList(c),d)) ParentT
							 val UTriggeredPPList = List.map (fn (a,b,c,d) => (getValOfOptionList(a),getValOfOptionList(b),getValOfOptionList(c),d)) UTriggeredParentT
							 val SM_TransL = if isFinal(h) then (List.map (Path2SM_Trans (id_of_state(h))) (PList@PPList))
									 else List.map (Path2SM_Trans (id_of_state(h))) (PList@TriggeredPPList)
						     in
							 SM_TransL::calculate_SM_Trans(t,Parent_OutList,TransList,StateList)
						     end

(* Classifier -> SM_Trans list *)
fun SM_Trans_of_classif(C:Classifier) = let val SL = states_of_classif(C)
					    val TL = correct_TransList(transitions_of_classif(C),ref SL)
					    val TL = TL@correctCompositeTransitions(SL,[],ref TL)
					    val SL = updateStates(SL,TL)
					    val NPS = List.filter (fn X => (not (isPseudo(X)))) SL
					    val rawTransList = createDistinct(List.concat(calculate_SM_Trans(NPS,[],TL,states_of_classif(C)))) 
					    fun addAutoTriggers([]) = []
					      | addAutoTriggers(h::t) = case triggers_of_SM_Trans(h)
									 of [] => mk_SM_T((#trans_id h), 
											  (#source h), 
											  (#target h), 
											  (#guards h), 
											  [alwaysTrigger],
											  (#effects h))::addAutoTriggers(t)
									  | _ => h::addAutoTriggers(t)
					    fun addAutoGuards([]) = []
					      | addAutoGuards(h::t) = case guards_of_SM_Trans(h)
								       of [] =>  mk_SM_T((#trans_id h),
											 (#source h), 
											 (#target h),
											 [alwaysGuard], 
											 (#triggers h),
											 (#effects h))::addAutoGuards(t)
									| _ => h::addAutoGuards(t)
					in 
					    addAutoGuards(addAutoTriggers(rawTransList))
					end


(* SM_Trans list * Guard -> SM_Trans list *)
(* Sorts the list L in such a way, that all SM_Transitions with guard G are at the end *)
fun sort_SM_TransL_withGAtEnd(L,G) = let fun filterG({guards,...}:SM_Trans) = List.exists (fn X => X=G) guards
					  val GL = List.filter filterG L
					  val nGL = List.filter (fn X => not(filterG(X))) L
				      in 
					  nGL@GL
				      end

(*FIXME - we have to discard the trigger-array and use a single trigger instead *)
fun next_SM_Trans_4EV(S, TLp,E) = let fun filterS({source,...}:SM_Trans) = (source=id_of_state(S))
				      fun filterE({triggers,...}:SM_Trans) = List.exists (fn X => X=E) triggers
				      in 
					  List.filter filterE (List.filter filterS (!TLp))
				      end
(* effects_of_SM_Trans = fn : SM_Trans -> Action list *)
fun effects_of_SM_Trans({effects,...}: SM_Trans) = effects

fun ident_of_effect(Proc_mk{proc_id,...}) = proc_id

fun is_TopState(S as State_CompositeState{...},C:Classifier) = let val top = top_state_of(hd (ActGraph_of_classif(C)))
								   val dbt = print(id_of_state(top))
							       in 
								   S = top
							       end
  | is_TopState(_,_) = false


fun realInit(C:Classifier) = let val TopState = top_state_of(hd(ActGraph_of_classif(C)))
				 val subvertices = get_subvertex_list(TopState)
				 val InitL = List.filter is_StartState subvertices
			     in 
				 case InitL
				  of [] => raise MalformedStateMachine
				   | [x] => x
				   | _ => raise MalformedStateMachine
			     end
(* Codegen.generate (Rep_SecureUML_ComponentUML.readXMI "examples/Chessboard_composite.xmi") "c#_sm_VIZ"; *)

end
