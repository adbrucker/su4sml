(*****************************************************************************
 * su4sml --- an SML repository for managing (Secure)UML/OCL models
 *             http://projects.brucker.ch/su4sml/
 *                                                                            
 * c#sm_cartridge.sml --- 
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

functor CSSM_Cartridge(SuperCart : BASE_CARTRIDGE) : CARTRIDGE =
 struct


 open Rep_OclType
 open Rep_StateMachine
 open Gcg_Helper
 (* open Rep_SecureUML_ComponentUML.Security*)
 open ComponentUML
 open SM_Helper
 open StateMachineTypes
open StringHandling
open StateMachine 

 val emptySM_Trans = { trans_id="",
		       source = "",
		       target = "",
		       guards = [],
		       triggers = [],
		       effects = []
		       }:SM_Trans

 type environment = {
		      curState : StateVertex,
		      allTransitions: SM_Trans list,
		      curTransition : (SM_Trans*int),
		      curEvent: Event,
		      curGuard: (Guard*int),
   		      curEffect: Procedure,
 		      extension : SuperCart.environment 
		    }
 

 fun initEnv model = {
		       curState = emptyState,
		       allTransitions = [],
		       curTransition = (emptySM_Trans,0),
		       curEvent = emptyEvent,
		       curGuard = (emptyGuard,0),
  		       curEffect = emptyEffect,
 		       extension = SuperCart.initEnv model 
		      } : environment

(* unpack : environment -> SuperCart.environment *)
fun unpack (env:environment) = #extension env

(* pack : environment -> SuperCart.environment -> environment *)
fun pack (env: environment) (new_env : SuperCart.environment) = {
								 curState = #curState env,
								 allTransitions = #allTransitions env,
								 curTransition = #curTransition env,
								 curEvent = #curEvent env,
								 curGuard = #curGuard env,
 								 curEffect = #curEffect env,
								 extension=new_env 
								 }
                           
(* fun getModel env = SuperCart.getModel (unpack env) *)

(*
 * lookup  environment -> string -> string			
 * might override some lookup entries of the base cartridge 
 *)
fun lookup (env : environment) "state_name" = toUpper(name_of_state(#curState env))
  | lookup (env : environment) "state_ident" = id_of_state(#curState env)
  | lookup (env : environment) "final_state_name" = toUpper(id_of_state(FinalState(states_of_classif(Option.valOf(SuperCart.curClassifier (unpack env))))))
  | lookup (env : environment) "transition_target" = target_of_SM_Trans(#1(#curTransition env))
  | lookup (env : environment) "guard_ident" = ident_of_guard(#1(#curGuard env))
  | lookup (env : environment) "event_name" = toUpper(name_of_event(#curEvent env))
  | lookup (env : environment) "cur_event_id" = toUpper(name_of_event(#curEvent env))
  | lookup (env : environment) "effect_ident" = #proc_id (#curEffect env)
  | lookup (env : environment) "trigger_name" = name_of_event(#curEvent env)
  | lookup (env : environment) "real_init" = id_of_state(realInit(Option.valOf(SuperCart.curClassifier (unpack env))))
  | lookup (env : environment) s =  SuperCart.lookup (unpack env) s


fun evalCondition (env : environment) "hasAG" = hasAG(Option.valOf(SuperCart.curClassifier (unpack env)))
  | evalCondition (env : environment) "isTrigger" = let val Transitions = transitions_of_classif(Option.valOf(SuperCart.curClassifier (unpack env)))
							val oper = Option.valOf(SuperCart.curOperation (unpack env))
						    in 
							acts_as_trigger oper Transitions
						    end
  | evalCondition (env : environment) "isLastGuard" = (#2(#curGuard env)) = 0
  | evalCondition (env : environment) "isLastTrans" = (#2(#curTransition env)) = 0
(*  | evalCondition (env : environment) "isStart" = is_StartState(#curState env)*)
  (* pass unknown condition types to Superior Cartridge *)
  | evalCondition (env : environment) s = SuperCart.test (unpack env) s


val test = evalCondition

fun foreach_event(env: environment) = let val eventList = events_of_classif(Option.valOf(SuperCart.curClassifier (unpack env)))
					  fun env_from_ev X = { 
							       curState = #curState env,
							       allTransitions = #allTransitions env,
							       curTransition = (emptySM_Trans,0),
							       curEvent = X,
							       curGuard = (emptyGuard,0),
							       curEffect = emptyEffect,
							       extension = #extension env
							       }
				      in 
					  List.map env_from_ev eventList
				      end

fun foreach_events_of_state(env: environment) = let val TL = (#allTransitions env)
						    val SL = states_of_classif(Option.valOf(SuperCart.curClassifier (unpack env)))
						    val EVTList = events_of_state((#curState env), ref TL, ref SL)
						    fun env_from_EoS evt =
							{
							 curEvent = evt,
							 curGuard = (emptyGuard,0),
							 curState = #curState env,
							 allTransitions = #allTransitions env,
							 curTransition = #curTransition env,
							 curEffect = emptyEffect,
							 extension = #extension env
							 }
						in
						    List.map env_from_EoS EVTList
						end

fun foreach_state(env: environment) = let val stateList = states_of_classif(Option.valOf(SuperCart.curClassifier (unpack env)))
					  val realStates = List.filter (fn X => not(isPseudo(X))) stateList
					  fun env_from_state X = { 
								  curState = X,		
								  allTransitions = #allTransitions env,
								  curTransition = (emptySM_Trans,0),
								  curEvent = #curEvent env,
								  curGuard = (emptyGuard,0),
								  curEffect = emptyEffect,
								  extension = #extension env
								  }
				      in 
					  List.map env_from_state realStates
				      end

fun foreach_classifier (env : environment) 
			= let val envL = SuperCart.foreach "classifier_list" (unpack env)
			      fun env_from_classifier e = 
				  { 
				   curState = emptyState,
				   allTransitions = SM_Trans_of_classif(Option.valOf(SuperCart.curClassifier(e))),(* NOTE: here the SM_Trans are calculated *)
				   curTransition = (emptySM_Trans,0),
				   curEvent = (#curEvent env),
				   curGuard = (emptyGuard,0),
				   curEffect = emptyEffect,
				   extension = e
		       		   }
			  in 
			      List.map env_from_classifier envL
			  end
			     

fun foreach_transition(env: environment) = let val TransL = next_SM_Trans_4EV((#curState env),ref (#allTransitions env), (#curEvent env))
					       val LEN = List.length(TransL)
					       fun env_from_TL T = {
								    curState = #curState env,
								    allTransitions = #allTransitions env,
								    curTransition = T,
								    curEvent = #curEvent env,
								    curGuard = (emptyGuard,0),
								    curEffect = emptyEffect,
								    extension = #extension env
								    }
					       fun transform([],_) = []
						 | transform(h::t,n) = (h,n)::transform(t,(n-1))
					   in 
					       List.map env_from_TL (transform((sort_SM_TransL_withGAtEnd(TransL,lastGuard)),(LEN-1)))
					   end

fun foreach_guard(env: environment) = let val GL = guards_of_SM_Trans(#1(#curTransition env))
					  fun env_from_GL G = {
							       curState = #curState env,
							       allTransitions = #allTransitions env,
							       curTransition = #curTransition env,
							       curEvent = #curEvent env,
							       curGuard = G,
							       curEffect = emptyEffect,
							       extension = #extension env
							       }
					  val LEN = List.length(GL)
					  fun transform([],_) = []
					    | transform(h::t,n) = (h,n)::transform(t,(n-1))
				      in
					  List.map env_from_GL (transform(GL,(LEN-1)))
				      end

fun all_guards(env: environment) = let val AGL = makeDistinct(List.concat (List.map guards_of_SM_Trans (#allTransitions env)))
				       fun env_from_GL G = {
							    curState = #curState env,
							    allTransitions = #allTransitions env,
							    curTransition = #curTransition env,
							    curEvent = #curEvent env,
							    curGuard = G,
							    curEffect = emptyEffect,
							    extension = #extension env
							    }
				       val LEN =  List.length(AGL)
				       fun transform([],_) = []
					 | transform(h::t,n) = (h,n)::transform(t,(n-1))
				   in 
				       List.map env_from_GL (transform(AGL,(LEN-1)))
				   end

fun foreach_effect(env: environment) = let val EffL = effects_of_SM_Trans(#1(#curTransition env))
					   fun env_from_EffL E = {
								  curState = #curState env,
								  allTransitions = #allTransitions env,
								  curTransition = #curTransition env,
								  curEvent = #curEvent env,
								  curEffect = E,
								  curGuard = #curGuard env,
								  extension = #extension env
								  }
				       in 
					   List.map env_from_EffL EffL
				       end




fun foreach "event_list" env       = foreach_event env
  | foreach "state_list" env       = foreach_state env
  | foreach "transition_list" env  = foreach_transition env
  | foreach "guard_of_trans_list" env       = foreach_guard env
  | foreach "guard_list" env       = all_guards env
  | foreach "effect_list" env      = foreach_effect env  
  | foreach "events_of_state" env  = foreach_events_of_state env
  | foreach (LT as "classifier_list") env = ListPair.map (uncurry pack) ((foreach_classifier env), (SuperCart.foreach LT (unpack env)))
  | foreach L (env:environment)    = map (pack env) (SuperCart.foreach L (unpack env))
end
