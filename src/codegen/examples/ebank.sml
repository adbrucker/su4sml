(*****************************************************************************
 * su4sml --- a SML repository for managing (Secure)UML/OCL models
 *             http://projects.brucker.ch/su4sml/
 *                                                                            
 * ebank.sml --- a simple test file for the core repository
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

open Rep_Core;
open Rep_OclTerm;

  val ebank = [Class
     {activity_graphs=[],associationends=[],
      attributes=[("source",Set (Classifier ["eBank","Account"])),
                  ("destination",Set (Classifier ["eBank","Account"])),
                  ("amount",Integer),("date",Classifier ["eBank","Date"])],
      interfaces=[],
      invariant=[],
      name=["eBank","Transaction"],
      operations=[{arguments=[],isQuery=false,name="makeTransfer",
                   postcondition=[],precondition=[],result=Boolean}],
      parent=NONE,stereotypes=[],thyname=NONE},
   Class
     {activity_graphs=[],associationends=[],
      attributes=[("t1",Set (Classifier ["eBank","Transaction"])),
                  ("t2",Set (Classifier ["eBank","Transaction"])),
                  ("owner",Set (Classifier ["eBank","Customer"]))],
      interfaces=[],
      invariant=[],
      name=["eBank","Account"],
      operations=[{arguments=[("amount",Integer)],isQuery=false,
                   name="makeDeposit",postcondition=[],precondition=[],
                   result=Boolean},
                  {arguments=[("amount",Integer)],isQuery=false,
                   name="makeWithdrawal",postcondition=[],precondition=[],
                   result=Boolean},
                  {arguments=[],isQuery=false,name="getBalance",
                   postcondition=[],precondition=[],result=Integer},
                  {arguments=[],isQuery=false,name="getCurrency",
                   postcondition=[],precondition=[],result=String}],
      parent=NONE,stereotypes=[],thyname=NONE},
   Class
     {activity_graphs=[],associationends=[],
      attributes=[("accounts",Set (Classifier ["eBank","Account"])),
                  ("name",String),("address",String),("gender",Boolean),
                  ("title",String)],interfaces=[],
      invariant=[],name=["eBank","Customer"],operations=[],
      parent=NONE,stereotypes=[],thyname=NONE},
   Class
     {activity_graphs=[],associationends=[],
      attributes=[("checks",Set (Classifier ["eBank","Check"])),
                  ("checkAccount",Set (Classifier ["eBank","CreditAccount"]))],
      interfaces=[],
      invariant=[],name=["eBank","Checkbook"],operations=[],
      parent=NONE,stereotypes=[],thyname=NONE},
   Class
     {activity_graphs=[],associationends=[],
      attributes=[("checkbook",Set (Classifier ["eBank","Checkbook"]))],
      interfaces=[],
      invariant=[],
      name=["eBank","Check"],
      operations=[{arguments=[],isQuery=false,name="newOperation",
                   postcondition=[],precondition=[],
                   result=Classifier ["eBank","void"]}],parent=NONE,
      stereotypes=[],thyname=NONE},
   Class
     {activity_graphs=[],associationends=[],
      attributes=[("accountNumber",String),("currency",String),
                  ("balance",Integer)],interfaces=[],invariant=[],
      name=["eBank","BankAccount"],
      operations=[{arguments=[],isQuery=false,name="getBalance",
                   postcondition=[],precondition=[],result=Integer},
                  {arguments=[("amount",Integer)],isQuery=false,
                   name="makeDeposit",postcondition=[],precondition=[],
                   result=Boolean},
                  {arguments=[("amount",Integer)],isQuery=false,
                   name="makeWithdrawal",postcondition=[],precondition=[],
                   result=Boolean}],parent=SOME ["eBank","Account"],
      stereotypes=[],thyname=NONE},
   Class
     {activity_graphs=[],associationends=[],
      attributes=[("tradingCurrency",String),("price",Integer)],interfaces=[],
      invariant=[],name=["eBank","CurrencyTradingAccount"],
      operations=[{arguments=[("amount",Integer)],isQuery=false,name="buy",
                   postcondition=[],precondition=[],result=Boolean},
                  {arguments=[("amount",Integer)],isQuery=false,name="sell",
                   postcondition=[],precondition=[],result=Boolean},
                  {arguments=[],isQuery=false,name="getBalance",
                   postcondition=[],precondition=[],result=Integer},
                  {arguments=[],isQuery=false,name="getNormBalance",
                   postcondition=[],precondition=[],result=Boolean}],
      parent=SOME ["eBank","BankAccount"],stereotypes=[],thyname=NONE},
   Class
     {activity_graphs=[],associationends=[],attributes=[],interfaces=[],
      invariant=[],name=["eBank","Date"],operations=[],parent=NONE,
      stereotypes=[],thyname=NONE},
   Class
     {activity_graphs=[],associationends=[],
      attributes=[("book",Set (Classifier ["eBank","Checkbook"])),
                  ("creditLimit",Integer),("maxAmount",Integer)],
      interfaces=[],
      invariant=[],name=["eBank","CreditAccount"],
      operations=[{arguments=[("amount",Integer)],isQuery=false,
                   name="makeDeposit",postcondition=[],precondition=[],
                   result=Boolean},
                  {arguments=[("amount",Integer)],isQuery=false,
                   name="makeWithdrawal",postcondition=[],precondition=[],
                   result=Boolean}],parent=SOME ["eBank","BankAccount"],
      stereotypes=[],thyname=NONE},
   Primitive
     {associationends=[],interfaces=[],invariant=[],name=["eBank","void"],
      operations=[],parent=NONE,stereotypes=[],thyname=NONE}]



val cl = ebank 
