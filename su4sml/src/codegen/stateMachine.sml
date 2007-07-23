(*****************************************************************************
 * su4sml --- a SML repository for managing (Secure)UML/OCL models
 *             http://projects.brucker.ch/su4sml/
 *                                                                            
 * stateMachine.sml --- 
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

structure StateMachine =
struct

open Rep
open Rep_OclType
open Rep_OclTerm
open Rep_StateMachine
open SM_Helper
open StateMachineTypes
open ListEq
(********************************************)
(* Generic types handling functions:	    *)
(* ---------------------------------	    *)
(********************************************)


(*return the initial state*)
exception MalformedStateMachine

(* TODO: check which functions here are really used... *)

(************************************)
(* STATE handling functions:	    *)
(* -------------------------	    *)
(************************************)


(*  StateVertex_Id * StateVertex list ref -> StateVertex *)
fun get_state_by_id (id:StateVertex_Id, SLp: StateVertex list ref) = 
    hd (List.filter (fn x => id_of_state x = id) (!SLp))

fun FinalState (SL:StateVertex list) = hd (List.filter isFinal SL)
                                       
                                       
(* StateVertex * SM_Trans list ref -> SM_Trans list *)
fun get_next_SM_Trans (s:StateVertex) TLp = 
    List.filter (fn (x:SM_Trans) => #source x = id_of_state s) (!TLp)

(* StateVertex * SM_Trans list ref * 'a -> Event list *)
fun events_of_state (S:StateVertex, TLp, SLp) = 
    (makeDistinct o List.concat o map #triggers o get_next_SM_Trans S) TLp
    

(***************************************)
(* SM_Trans handling functions:	       *)
(* ----------------------------	       *)
(***************************************)

(* SM_Trans -> StateVertex_Id *)
fun target_of_SM_Trans ({target,...}:SM_Trans) = target

(* SM_Trans -> Guard list *)
fun guards_of_SM_Trans ({guards,...}:SM_Trans) = guards

(* SM_Trans -> Event list *)
fun triggers_of_SM_Trans ({triggers,...}:SM_Trans) = triggers

(* Transition_Id * StateVertex_Id * StateVertex_Id * Guard list * Event list * Action list -> SM_Trans *)
fun mk_SM_T (tid, s, t, ga, ta, ea) = {trans_id = tid,
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


(*get all defined activity graphs in a list of Classifiers --> ActivityGraph list*)
(* Classifier list -> ActivityGraph list *)
fun ActGraph_of_classif_from_list (C:Classifier list) = 
    List.concat (map activity_graphs_of C)

fun name_of_classif (C as  Class c) = Rep.short_name_of C
  | name_of_classif (Primitive p) = "PRIMITIVE"
  | name_of_classif (_) = "XXX"

(*return list of subertices ot the given Classifier (Class, Primitive,...)*)
(* Classifier -> StateVertex list *)
fun states_of_classif (C:Classifier) = 
    let fun soc x = 
            let val SM:StateMachine list = activity_graphs_of x
	    in 
		if SM=[] then []
		(*FIXME: SM can have more than one AG defined... (remove "hd")
			    SM can be another type than CompositeState...*)
		else subvertices_of (#top (hd SM))
	    end
	fun dive [] = []
	  | dive ((State_CompositeState{name=n,
				        subvertex=a,
				        incoming=b,
				        isConcurrent=c,
				        outgoing=d,
				        state_id=f})::t) = 
            (* FIXME: don't we have to recurse here into a? *)
            a@[(State_CompositeState{name=n,
				     subvertex=a,
				     incoming=b,
				     isConcurrent=c,
				     outgoing=d,
				     state_id=f})]@dive(t)
	  | dive (h::t) = h::dive(t)
    in 
	dive (soc C)
    end



(* Classifier -> Transition list *)
fun transitions_of_classif (C:Classifier) = 
    let val AG:ActivityGraph list = activity_graphs_of C
    in 
	if AG = [] then []
	else #transition (hd AG)
    end

fun events_of_classif (C:Classifier) = 
    makeDistinct (List.mapPartial #trigger (transitions_of_classif C))

(*returns a string with every transition_id separated by a "," *)
(* Classifier -> string *)
fun get_names_of_trans (C:Classifier) = 
    let val ids = map #trans_id (List.concat (map #transition (activity_graphs_of C)))
    in 
	String.concatWith "," ids
    end
    



(*processes the list of given {Simple,Pseudo,...} states and creates a "sane" representation. *)
(* Transition_Id * Transition list ref -> Transition *)
fun get_trans_by_id (id:Transition_Id, TLp: Transition list ref) = 
    hd (List.filter (fn x => #trans_id x = id) (!TLp))
        

(*returns list of state having given kind. NOTE: this only applies for PseudoStates*)
(* PseudoStateVars * StateVertex list -> StateVertex list *)
fun get_state_by_kind (KIND:PseudoStateVars, SVL: StateVertex list) = 
    let fun hasKind k (PseudoState{kind,...}) = (k = kind)
	  | hasKind k _ = false
    in 
	List.filter (hasKind KIND) SVL
    end
    
(* StateVertex list -> StateVertex *)
fun get_initial (SVL: StateVertex list) = case List.find isInit SVL
                                           of SOME x => x
                                            | _      => raise MalformedStateMachine

(* StateVertex list -> StateVertex list *)
fun get_PseudoStates (C:StateVertex list) = List.filter isPseudo C

(* StateVertex list -> StateVertex list *)
fun get_other_States (C:StateVertex list) = List.filter (fn x => (not (isPseudo x))) C




(* returns the list of state ids that are targets of the items in the transition list "outgoing" *)
(* StateVertex * Transition list ref -> Transition list *)
(* FIXME: there must be some simpler way... *)
fun get_next_transitions (s:StateVertex, TransL: Transition list ref) = 
    let fun filterf x y = get_trans_by_id (x, ref y)
        val funList = List.map filterf (outgoing_of s)
    in 
        foldl (fn (f,x) => f (!TransL) :: x ) [] funList
    end

(* Transition_Id * Transition list -> StateVertex_Id *)
fun next_state_id (TransID: Transition_Id, TransList: Transition list) = 
    #target (get_trans_by_id (TransID, ref TransList))
    

(* operation * Transition list -> bool *)
fun acts_as_trigger (O:operation) (T:Transition list) = 
    let val EventList                            = List.mapPartial #trigger T
	fun getPath (CallEvent(path,parameters)) = path
	  | getPath (_)                          = []: Path
	fun extractPath (E:Event)                = List.nth (getPath E, 2)
    in
	List.exists (fn x => x = name_of_op O) (map extractPath EventList)
    end


				  

(* returns the path (string list) of the operation call *)
(* OclTerm -> Path *)
fun path_of_operation (OperationCall(_,_,P,L,_)) = P
  | path_of_operation (_) = []

(* Guard -> Path *)
fun path_of_guard (G:Guard) = path_of_operation(G)


(* Guard -> string *)
fun ident_of_guard (G:Guard) =  let val sop = string_of_path(path_of_operation(G))
			        in 
				    case sop
				     of "else" => "elseG"
				      | _ => sop
			        end
(*
(* (Guard option list * Event option list * 'a) list -> (string list * string list * 'a) list *)
fun processPL L = 
    let fun processOptionL (LIST) = List.mapPartial (fn x => x) LIST
 	fun processOCList  (LIST) = List.concat (List.map path_of_guard LIST)
 	fun processEVList  (LIST) = List.map name_of_event LIST
 	fun transform     (a,b,c) = (processOCList (processOptionL a),
                                     processEVList (processOptionL b),
                                     c) 
    in 
	List.map transform L
    end
*)
(*Classifier -> bool *)
fun hasAG (C:Classifier) = not ((activity_graphs_of C) = [])

(* OclTerm -> bool *)
fun is_else_Guard (G as OperationCall(Variable(_,_),_,[a,b,c],_,_)) = a = "else"
  | is_else_Guard (_)                                               = false


(*re-route all transitions pointing to a Composite State.
           Afther this there does _NOT_ exist a transition from any
           state to a Composite State anymore *)
(* Transition list * StateVertex list ref -> Transition list *)
fun correct_TransList([],_) = []
  | correct_TransList((head as {trans_id=tid, source=src, target=tgt, guard=gd,
				    trigger=tr, effect=eff})::tail,StateList) = 
    let val S = get_state_by_id(#target(head),StateList)
    in 
	case isComposite(S)
	 of true => let val subvertices = subvertices_of(S)
			(*val outList = get_next_transitions(S,TransList)*)
			val NextInit = get_initial(subvertices)
		    in 
			{trans_id=tid,
			 source=src,
			 target=id_of_state(NextInit),
			 guard=gd,
			 trigger=tr,
			 effect=eff}::correct_TransList(tail,StateList)
		    end
		    
	  | false => head::correct_TransList(tail,StateList)
    end
    
                 

(* ''a * ''a list -> ''a list *)
fun removeFromList(a,[]) = []
  | removeFromList(a,h::t) = if a=h then removeFromList(a,t)
			     else h::removeFromList(a,t)

(*assumption: elements are unique --> set-like *)
(* ''a list * ''a list -> ''a list *)
(* FIXME: find out the intendes semantics and move to ListEq *)
fun ListDifference([],L2) = L2
  | ListDifference(L1,[]) = L1
  | ListDifference(h::t,L) = if ListEq.includes L h  then ListDifference(t,removeFromList(h,L))
			     else h::ListDifference(t,removeFromList(h,L))

(* return the transitions of this composite state having a trigger defined *)
(* StateVertex * Transition list ref -> Transition list *)
fun triggered_OUT_Trans(State_CompositeState{outgoing,...}, TL) = 
    let val Trans = List.map (fn X => get_trans_by_id(X,TL)) outgoing
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
  | correctCompositeTransitions((SCS as State_CompositeState{name,state_id,outgoing,incoming,
                                         		     subvertex,isConcurrent})::tail,
                                newTrans, allTrans) = 
    let val SV = subvertices_of(SCS)
	val subverticesID = List.map id_of_state SV
	val subvertex_count = List.length(SV)
	val triggered = triggered_OUT_Trans(SCS,allTrans)
	val outlist = get_next_transitions(SCS,allTrans)
	val untriggered = ListDifference(triggered,outlist)
	val Final = FinalState(SV)
	fun createIncList 0 = []
	  | createIncList n  = (createIncList (n-1))@[n]
	fun handleUntriggered ([]:Transition list,_) = []
	  | handleUntriggered (h::t,T_id) = {trans_id = "newUT_"^Int.toString(T_id),
					     source = id_of_state(Final),
					     target = #target(h),
					     guard = #guard(h),
					     trigger = #trigger(h),
					     effect = #effect(h)}::handleUntriggered(t,(T_id+1))
	fun createTrans T_id (T:Transition) S = {trans_id = T_id,
				                 source = S,
				                 target = #target(T),
				                 guard = #guard(T),
				                 trigger = #trigger(T),
				                 effect = #effect(T)}
	fun applyList([],_) = []
	  | applyList(h::t,fList) = (List.map (fn X => X(h)) fList)@applyList(t,fList)
				    
	fun handleTriggered([],_) = []
	  | handleTriggered(h::t,T_id) = 
            let val idList = List.map (fn X => Int.toString(T_id)^"_"^Int.toString(X)) 
                                      (createIncList subvertex_count)
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
fun updateStates([],_:Transition list) = []
  | updateStates(h::t,Transitions) = 
    let val affectedOutTransitions = List.filter (fn X => #source X = id_of_state h) 
                                                 Transitions (* transitions that source from h *)
	val affectedInTransitions = List.filter (fn X => #target(X) = id_of_state(h)) 
                                                Transitions
	val newOut = makeDistinct((List.map #trans_id affectedOutTransitions)@outgoing_of(h))
	val newIn = makeDistinct((List.map #trans_id affectedInTransitions)@incoming_of(h))

        fun updateState s = add_incoming (add_outgoing s newOut) newIn
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
fun CollectTGE(T, TL, SL) = 
    let val S = get_state_by_id(#target(T), SL)
	val TRIGGER = #trigger(T)
	val GUARD = #guard(T)
	val EFFECT = #effect(T)
	val NEXTL = get_next_transitions(S,TL)
	fun ADD(L1,L2,L3,S) = (GUARD::L1,TRIGGER::L2,EFFECT::L3,S)
    in 
	if isPseudo(S) orelse isInit(S) 
        then List.map ADD (List.concat (List.map (fn X => CollectTGE(X,TL,SL)) NEXTL))
	else [([GUARD],[TRIGGER],[EFFECT],#target(T))]
    end

(* add transitions of to the out-states of the 'parent' states inside a composite state *)
(* NOTE: has to be called with a list of states that does NOT contain pseudo states!!*)

(*  StateVertex list * Transition list * Transition list * StateVertex list -> SM_Trans list list *)
fun calculate_SM_Trans([],_,_,_) = []
  | calculate_SM_Trans((SCS as State_CompositeState{name,state_id,outgoing,incoming,
                                                    subvertex,isConcurrent})::t,
		       Parent_OutList, TransList, StateList) = 
    let val outlist = get_next_transitions(SCS, ref TransList)
    in
	calculate_SM_Trans(subvertex,
			   Parent_OutList@outlist,
			   TransList,
			   StateList)@calculate_SM_Trans(t, Parent_OutList, TransList, StateList)
    end
  | calculate_SM_Trans(h::t, Parent_OutList:Transition list, TransList:Transition list,
		       StateList:StateVertex list) = 
    let val outlist = get_next_transitions(h,ref TransList):Transition list
	val TL = List.concat (List.map (fn X => CollectTGE(X,ref TransList,ref StateList)) outlist)
	val UTriggeredParentT = 
            List.concat (List.map (fn X => CollectTGE(X,ref TransList, ref StateList)) 
                                  (List.filter (fn X => not(isTriggered(X))) Parent_OutList))
	val TriggeredParentT = 
            List.concat (List.map (fn X => CollectTGE(X,ref TransList, ref StateList)) 
                                  (List.filter isTriggered Parent_OutList))
	val ParentT = List.concat (List.map (fn X => CollectTGE(X,ref TransList, ref StateList)) 
                                            Parent_OutList)
	fun getValOfOptionList(LIST) = List.map (fn X =>Option.valOf(X)) 
                                                (List.filter Option.isSome LIST)
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
fun SM_Trans_of_classif (C:Classifier) = 
    let val SL = states_of_classif(C)
	val TL = correct_TransList(transitions_of_classif(C),ref SL)
	val TL = TL@correctCompositeTransitions(SL,[],ref TL)
	val SL = updateStates(SL,TL)
	val NPS = List.filter (fn X => (not (isPseudo(X)))) SL
	val rawTransList = makeDistinct (List.concat(calculate_SM_Trans(NPS,[],TL,states_of_classif(C)))) 
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
fun sort_SM_TransL_withGAtEnd(L,G) = 
    let fun filterG({guards,...}:SM_Trans) = List.exists (fn X => X=G) guards
	val GL = List.filter filterG L
	val nGL = List.filter (fn X => not(filterG(X))) L
    in 
	nGL@GL
    end

(*FIXME - we have to discard the trigger-array and use a single trigger instead *)
fun next_SM_Trans_4EV(S, TLp,E) = 
    let fun filterS({source,...}:SM_Trans) = (source=id_of_state(S))
	fun filterE({triggers,...}:SM_Trans) = List.exists (fn X => X=E) triggers
    in 
	List.filter filterE (List.filter filterS (!TLp))
    end
(* effects_of_SM_Trans = fn : SM_Trans -> Action list *)
fun effects_of_SM_Trans({effects,...}: SM_Trans) = effects

(* fun ident_of_effect ({proc_id,...}:Procedure) = proc_id*)

fun is_TopState(S as State_CompositeState{...},C:Classifier) = 
    let val top = #top (hd (activity_graphs_of C))
	val dbt = print(id_of_state(top))
    in 
	S = top
    end
  | is_TopState(_,_) = false


fun realInit (c:Classifier) = (get_initial o subvertices_of o #top o hd o activity_graphs_of) c
     

end
