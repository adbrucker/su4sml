(*****************************************************************************
 *          su4sml - a SecureUML repository for SML              
 *                                                                            
 * ebank.sml - a simple test file for the core repository
 * Copyright (C) 2003-2005 Achim D. Brucker <brucker@inf.ethz.ch>
 *                                                                            
 * This file is part of su4sml.                                              
 *                                                                            
 * su4sml is free software; you can redistribute it and/or modify it under   
 * the terms of the GNU General Public License as published by the Free       
 * Software Foundation; either version 2 of the License, or (at your option)  
 * any later version.                                                         
 *                                                                            
 * su4sml is distributed in the hope that it will be useful, but WITHOUT ANY 
 * WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS 
 * FOR A PARTICULAR PURPOSE. See the GNU General Public License for more 
 * details.                                                              
 *                                                                            
 * You should have received a copy of the GNU General Public License along    
 * with this program; if not, write to the Free Software Foundation, Inc.,    
 * 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.                  
 ******************************************************************************)

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
