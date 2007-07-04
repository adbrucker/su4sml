@//////////////////////////////////////////////////////////////////////////////
@// su4sml --- a SML repository for managing (Secure)UML/OCL models
@//             http://projects.brucker.ch/su4sml/
@//                                                                            
@// C#_SM.tpl --- 
@// This file is part of su4sml.
@//
@// Copyright (c) 2005-2007, ETH Zurich, Switzerland
@//
@// All rights reserved.
@//
@// Redistribution and use in source and binary forms, with or without
@// modification, are permitted provided that the following conditions are
@// met:
@//
@//     * Redistributions of source code must retain the above copyright
@//       notice, this list of conditions and the following disclaimer.
@//
@//     * Redistributions in binary form must reproduce the above
@//       copyright notice, this list of conditions and the following
@//       disclaimer in the documentation and/or other materials provided
@//       with the distribution.
@//
@//     * Neither the name of the copyright holders nor the names of its
@//       contributors may be used to endorse or promote products derived
@//       from this software without specific prior written permission.
@//
@// THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
@// "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
@// LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
@// A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
@// OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
@// SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
@// LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
@// DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
@// THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
@// (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
@// OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
@///////////////////////////////////////////////////////////////////////////////
@// $Id$

@openfile generated/csharp/StateMachine_$classifier_package$.cs
// generated by su4sml GCG - Generic Code Generator
@foreach classifier_list 
@if hasAG
@nl
@nl using System;
@nl using System.Collections;
@nl
@nl namespace $classifier_package$ 
@nl {
//State Class
	@nl @tab public class State
	@nl @tab {
	@nl @tab@tab public string name;
	@nl @tab@tab public Hashtable transitions; //this is indexed by the event IDs.
	@nl
	@nl @tab@tab public State(string n)
	@nl @tab@tab {
	@nl @tab@tab@tab transitions = new Hashtable();
	@nl @tab@tab@tab name = n;
	@nl @tab@tab }
	@nl @tab }
	@nl
	@nl
	@nl @tab public delegate bool EventF();
	@nl @tab public delegate bool Guard();
	@nl @tab public delegate bool Effect();
// Transition Class
	@nl @tab public class Transition
	@nl @tab {
	@nl @tab@tab public State target;
	@nl @tab@tab public Guard[] guards;
	@nl @tab@tab public Effect[] effects;
	@nl 
	@nl @tab@tab public Transition(State t, Guard[] ga, Effect[] e)
	@nl @tab@tab {
	@nl @tab@tab@tab target = t;
	@nl @tab@tab@tab guards = ga;
	@nl @tab@tab@tab effects = e;
	@nl @tab@tab }
	@nl @tab }
	@nl
// State machine class
	@nl @tab public class StateMachine
	@nl @tab {
	@nl @tab@tab public enum EVENTS
	@nl @tab@tab {
	@nl @tab@tab@tab  ALWAYS = -1,
		@foreach event_list
			@nl @tab@tab@tab $event_name$,		
		@end
	@nl @tab@tab@tab  NUM_EV
	@nl @tab@tab };
	@nl @tab@tab public State CUR_STATE;
	@nl 	
	@nl @tab@tab State START;
		@foreach state_list
			@nl @tab@tab State $state_ident$;
		@end
	@nl @tab@tab State END;
	@nl
	@nl 
	@nl @tab@tab private void init_states()
	@nl @tab@tab {
	@nl @tab@tab@tab START = new State("Initial");
		@foreach state_list
			@nl @tab@tab@tab $state_ident$ = new State("$state_ident$");
		@end
	@nl @tab@tab@tab END = new State("Final");
	@nl @tab@tab }
	@nl
	@nl @tab@tab public StateMachine()
	@nl @tab@tab {
	@nl @tab@tab init_states();
		@foreach state_list
			@foreach events_of_state
				 @nl
				 @nl @tab@tab $state_ident$.transitions.Add(EVENTS.$cur_event_id$,new Transition[]{
				 @foreach transition_list
				 	@nl @tab@tab@tab@tab
					new Transition($transition_target$,
					    @nl @tab@tab@tab@tab@tab new Guard[]{
						@foreach guard_of_trans_list
						     new Guard($guard_ident$)
						     @if isLastGuard
							 } @//closing guard array
						     @else
							, @//between guards
						     @end
						@end 
					    , @//comma between guards and Effects
					    @nl @tab@tab@tab@tab@tab new Effect[] {				
						@foreach effect_list
							 new Effect($effect_ident$),
						@end 
						})
						@if isLastTrans
							}); @//endof transition array						
						@else
							, @//separate transitions
						@end
				@end
			@end @//endof events_of_state
			@nl
			@nl
		@end @//endof state_list
	@nl @tab@tab CUR_STATE = START = $real_init$;
	@nl @tab@tab END = $final_state_name$;
	@nl @tab@tab }
	@nl
	@nl @tab@tab //these three guards always exist
	@nl @tab@tab public bool alwaysG(){return true;}
	@nl @tab@tab public bool elseG(){return true;}
	@nl @tab@tab public bool noneR(){return true;}
	@nl
	@nl
	@nl @tab@tab private bool checkState(EVENTS E_ID)
	@nl @tab@tab {
	@nl @tab@tab@tab return (CUR_STATE.transitions.ContainsKey(E_ID));
	@nl @tab@tab }
	@nl
	@nl		
	@foreach event_list
		@nl @tab@tab public void $trigger_name$()
		@nl @tab@tab {
		@nl @tab@tab@tab EVENTS E_ID = EVENTS.$event_name$;
		@nl @tab@tab@tab if(checkState(E_ID)) {
		@nl @tab@tab@tab@tab Console.WriteLine("Checkstate ok...\n");
		@nl @tab@tab@tab@tab Transition[] trans = (Transition[]) CUR_STATE.transitions[E_ID];
		@nl @tab@tab@tab@tab foreach(Transition t in trans){
		@nl @tab@tab@tab@tab@tab foreach(Guard g in t.guards){
		@nl @tab@tab@tab@tab@tab@tab if(g()){
		@nl @tab@tab@tab@tab@tab@tab@tab Console.WriteLine("Guard \"{0}\" in $event_name$ succeeded...\n",g.Method.ToString());
		@//@nl @tab@tab@tab@tab@tab@tab@tab CUR_STATE = t.target;
		@//@nl @tab@tab@tab@tab@tab@tab@tab return;
		@nl @tab@tab@tab@tab@tab@tab } else {
		@nl @tab@tab@tab@tab@tab@tab@tab Console.WriteLine("Guard \"{0}\" in $event_name$ didn't hold...\n",g.Method.ToString());
		@nl @tab@tab@tab@tab@tab@tab@tab break;
		@nl @tab@tab@tab@tab@tab@tab }
		@nl @tab@tab@tab@tab@tab@tab //if we arrived here, all guards did hold -->
		@nl @tab@tab@tab@tab@tab@tab CUR_STATE = t.target;
		@nl @tab@tab@tab@tab@tab@tab //fire all effects
		@nl @tab@tab@tab@tab@tab foreach(Effect e in t.effects) {
		@nl @tab@tab@tab@tab@tab@tab e.eval();
		@nl @tab@tab@tab@tab@tab }
		@nl @tab@tab@tab@tab@tab@tab //go back
		@nl @tab@tab@tab@tab@tab@tab return;
		@nl @tab@tab@tab@tab@tab }
		@nl @tab@tab@tab@tab }
		@nl @tab@tab@tab } else {
		Console.WriteLine("Current state \"{0}\" does not accept Event \"{1}\"", CUR_STATE.name,E_ID.ToString());
		}
		@nl @tab@tab }
		@nl
	@end
		@nl @tab@tab private void auto() {
		@nl @tab@tab@tab if(checkState(EVENTS.AUTO)){
		@nl @tab@tab@tab@tab Transitions[] trans = (Transition[]) CUR_STATE.transitions[EVENTS.AUTO];
		@nl @tab@tab@tab@tab foreach(Transition t in trans) {
		@nl @tab@tab@tab@tab@tab foreach(Guard g in t.guards) {
		@nl @tab@tab@tab@tab@tab if(!g()) {
		@nl @tab@tab@tab@tab@tab@tab break; // break out of the iteration over the guards
		@nl @tab@tab@tab@tab@tab }
		@nl @tab@tab@tab@tab@tab CUR_STATE = t.target;
		@nl @tab@tab@tab@tab@tab } 
		@nl @tab@tab@tab@tab foreach(Effect e in t.effects) {
		@nl @tab@tab@tab@tab@tab e.eval();
		@nl @tab@tab@tab@tab }
		@nl @tab@tab@tab@tab return;
		@nl @tab@tab@tab }
		@nl @tab@tab }
		@nl @tab@tab else {
		@nl @tab@tab@tab return;
		@nl @tab@tab }
		@nl @tab }	
	@nl @tab@tab } @//endof state machine
@nl} // End
@end
@end
