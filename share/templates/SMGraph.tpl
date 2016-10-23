@//////////////////////////////////////////////////////////////////////////////
@// su4sml --- an SML repository for managing (Secure)UML/OCL models
@//             http://projects.brucker.ch/su4sml/
@//                                                                            
@// SMGraph.tpl --- 
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

@//Template for the visualization

@openfile generated/csharp/SMGraph.java

@nl package SMG;
@nl
@nl import java.awt.Color;
@nl import java.awt.Container;
@nl import java.awt.Dimension;
@nl import java.awt.GridBagConstraints;
@nl import java.awt.GridBagLayout;
@nl import java.awt.Point;
@nl 
@nl import javax.swing.JButton;
@nl import javax.swing.JCheckBox;
@nl import javax.swing.JFrame;
@nl import javax.swing.JPanel;
@nl
@nl
@nl public class SMGraph extends JFrame
@nl {
@nl @tab private static drawPanel dPanel;
@nl @tab private CheckboxListener CBListener = new CheckboxListener();
@nl @tab private StateMachine SM = new StateMachine();
@nl @tab JFrame guardsF;
@nl @tab GridBagConstraints verticalC = new GridBagConstraints();
@nl		
@nl @tab public static void main(String[] args) 
@nl @tab {
@nl @tab @tab JFrame.setDefaultLookAndFeelDecorated(true);
@nl @tab @tab new SMGraph();
@nl @tab }
@nl
@nl @tab public State initGraph()
@nl @tab {
@nl @tab guardsF = new JFrame("Guards");
@nl @tab JPanel guardsPanel = new JPanel();
@nl @tab guardsPanel.setLayout(new GridBagLayout());
@nl @tab CBListener.setDrawPanel(dPanel);
@nl /*<dynamic_part I>*/
@foreach classifier_list
@if hasAG
	@foreach state_list
		@nl @tab State $state_ident$ = new State("$state_name$");
	@end
	@foreach guard_list
		@nl @tab Guard $guard_ident$ = new Guard("$guard_ident$");
	@end
	@foreach state_list
		@foreach events_of_state
			@foreach transition_list
				@nl @tab $state_ident$.addTransition("$cur_event_id$",$transition_target$,new Guard[]{
					@//@nl @tab@tab@tab@tab@tab@tab@tab
					@foreach guard_of_trans_list
						$guard_ident$
						@if isLastGuard
							}
						@else
							,
						@end
					@end
				);
			@end
		@end @// events_of_state
	@end @//state_list
@nl
@foreach guard_list
	@nl @tab JCheckBox G_$guard_ident$ = new JCheckBox("$guard_ident$");
	@nl @tab $guard_ident$.setCB(G_$guard_ident$);
	@nl @tab G_$guard_ident$.addActionListener(CBListener);
	@nl @tab guardsPanel.add(G_$guard_ident$,verticalC);
@end
	@if hasAG
		@nl
		@nl @tab $real_init$.activate();
		@nl @tab SM.INIT = $real_init$;
	@end
@end
@end
@nl /*</dynamic_part I>*/

@nl @tab SM.init();
@nl @tab Container guardsP = guardsF.getContentPane();
@nl @tab guardsP.add(guardsPanel);		
@nl @tab guardsF.setSize(100,40*guardsPanel.getComponentCount());
@nl @tab guardsF.setLocation(new Point(100,100));
@nl @tab guardsF.setVisible( true );
@nl @tab return SM.INIT;
@nl @tab }
	
@nl @tab public SMGraph()
@nl @tab {
@nl @tab@tab verticalC.gridx = 1;
@nl @tab@tab verticalC.fill = GridBagConstraints.BOTH;
@nl		
@nl @tab@tab dPanel = new drawPanel();
@nl
@nl @tab@tab SM.setDrawArea(dPanel);
@nl @tab@tab State StartNode;
@nl @tab@tab StartNode = initGraph();
@nl
@nl @tab@tab JFrame frame = new JFrame("SMGraph");
@nl		
@nl @tab@tab frame.setDefaultCloseOperation( JFrame.EXIT_ON_CLOSE );
@nl @tab@tab dPanel.setOpaque( true );
@nl @tab@tab frame.setContentPane( dPanel);
@nl @tab@tab frame.setBackground(Color.white);
@nl @tab@tab dPanel.setSize(800,600);
@nl @tab@tab frame.setSize(new Dimension(800,600));
@nl @tab@tab frame.validate();
@nl @tab@tab frame.setVisible(true);
@nl
@nl @tab@tab JFrame buttonF = new JFrame("Events");
@nl @tab@tab Container buttonP = buttonF.getContentPane();
@nl @tab@tab buttonP.setLayout( new GridBagLayout());
@nl @tab@tab dPanel.addGraph(StartNode);
@nl /*<dynamic part II> */
@foreach classifier_list
	@foreach event_list
		@nl @tab@tab JButton b_$event_name$ = new JButton("$event_name$");
		@nl @tab@tab buttonP.add(b_$event_name$,verticalC);
		@nl @tab@tab b_$event_name$.setActionCommand("$event_name$");
		@nl @tab@tab b_$event_name$.addActionListener(SM);
	@end
@end
@nl /* </dynamic par II> */
@nl /* generic part */
@nl @tab@tab JButton b_AUTO = new JButton("AUTO");
@nl @tab@tab buttonP.add(b_AUTO,verticalC);
@nl @tab@tab b_AUTO.setActionCommand("AUTO");
@nl @tab@tab b_AUTO.addActionListener(SM);
@nl @tab@tab buttonF.setSize(150,50*buttonP.getComponentCount());
@nl @tab@tab buttonF.setVisible(true);
@nl @tab}

@nl}		

