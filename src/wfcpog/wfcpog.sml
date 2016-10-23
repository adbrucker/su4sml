(*****************************************************************************
 * su4sml --- an SML repository for managing (Secure)UML/OCL models
 *             http://projects.brucker.ch/su4sml/
 *                                                                            
 * wfcpog.sml ---
 * This file is part of su4sml.
 *
 * Copyright (c) 2008-2009 Achim D. Brucker, Germany
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
(* $Id: wfcpog.sml 7273 2008-02-18 07:18:05Z brucker $ *)
signature WFCPOG =
sig
					  
							   
  type wfpo_id = string

  datatype wf_or_po = WFC of wfpo -> Rep.Model -> bool
		    | POG of wfpo -> Rep.Model -> (Rep_OclType.Path * Rep_OclTerm.OclTerm) list
       and 
       wfpo = WFPO of {
       identifier      : wfpo_id,               (* identifier                     *) 
       name            : string,                 (* short description (for output) *)
       description     : string,                 (* long description               *)
       recommended     : bool,                   (* presented to the end-user?     *)
       depends         : wfpo_id list,          (* wfcpos that must be applied    *)
       recommends      : wfpo_id list,          (* wfcpos that should be applied  *)
       apply           : wf_or_po,               (* apply this wf check or po gen. *) 
       data            : Object.T Datatab.table  (* arbitrary data, mainly used 
						      for configuration data       *)
       }
	      
   
  val get_data : wfpo -> Object.T Datatab.table 
  val up_data  : Object.T Datatab.table  -> wfpo -> wfpo
	      
  val is_wfc : wfpo -> bool
  val is_pog : wfpo -> bool
  val apply_of : wfpo -> wf_or_po
  val id_of  : wfpo -> wfpo_id
  val name_of : wfpo -> string

  exception WFC_FailedMessage of string
  exception WFC_FailedException of (wfpo * string)
  exception WFCPOG_Exception of string

end



structure WFCPOG:WFCPOG =
struct

type wfpo_id = string

datatype wf_or_po = WFC of wfpo -> Rep.Model -> bool
		  | POG of wfpo -> Rep.Model -> (Rep_OclType.Path * Rep_OclTerm.OclTerm) list
and wfpo = WFPO of {
     identifier      : wfpo_id, (* identifier                     *) 
     name            : string,   (* short description (for output) *)
     description     : string,   (* long description               *)
     recommended     : bool,     (* presented to the end-user?     *)
     depends         : wfpo_id list, 
     recommends      : wfpo_id list, 
     apply           : wf_or_po,
     data            : Object.T Datatab.table 
 }

exception WFC_FailedMessage of string
exception WFC_FailedException of (wfpo * string)
exception WFCPOG_Exception of string

fun get_data (WFPO w) = #data w
fun up_data  data' (WFPO{identifier=identifier,name=name,description=description,
		     recommended=recommended, depends=depends,recommends=remommends,apply=apply,
		     data=data})
    = (WFPO{identifier=identifier,name=name,description=description,
	recommended=recommended, depends=depends,recommends=remommends,apply=apply,
	data=data'})

fun is_wfc (WFPO {apply=apply,...}) = (case apply of 
					 WFC _ => true
				       | _     => false )

fun is_pog (WFPO {apply=apply,...}) = (case apply of 
					 POG _ => true
				       | _     => false )


fun apply_of (WFPO {apply=apply,...}) = apply


fun id_of (WFPO{identifier=identifier,...}) = identifier

fun name_of (WFPO{name=name,...}) = name


end


signature WFCPOG_DATA_ARGS =
sig
  type T
  val empty: T
  val copy: T -> T
  val extend: T -> T
end;
	  
signature WFCPOG_DATA =
sig
  type T
  val get: WFCPOG.wfpo -> T
  val put: T -> WFCPOG.wfpo -> WFCPOG.wfpo
  val map: (T -> T) -> WFCPOG.wfpo -> WFCPOG.wfpo 
end;


signature WFCPOG_PRIVATE_DATA =
sig
  type serial
  exception error of string
  val declare : Object.T
                -> (Object.T -> Object.T) -> (Object.T -> Object.T) -> serial
  val get : serial -> (Object.T -> 'a) -> WFCPOG.wfpo -> 'a
  val put : serial -> ('a -> Object.T) -> 'a -> WFCPOG.wfpo -> WFCPOG.wfpo
end

structure WFCPOG_Data:WFCPOG_PRIVATE_DATA = 
struct

exception error of string

type kind =
 {empty:  Object.T,
  copy:   Object.T -> Object.T,
  extend: Object.T -> Object.T
 };
     
val kinds = ref (Datatab.empty: kind Datatab.table)

fun invoke f k =
    (case Datatab.lookup (! kinds) k of
       SOME kind => f kind
     | NONE => raise error "Invalid wfpo data identifier");
fun invoke_empty k   = invoke ((fn x => fn _ => x) o #empty) k ();
val invoke_copy      = invoke #copy;
val invoke_extend    = invoke #extend;

type serial = int;
local 
  val count = ref (0: int)
  fun inc i = (i := ! i + (1: int); ! i);
in 
   fun serial () =  inc count 
end

fun change r f = r := f (! r);

fun declare empty copy extend =
    let
    fun CRITICAL a  = a ()
    val k = serial ();
    val kind = {empty = empty, copy = copy, extend = extend};
    val _ = CRITICAL (fn () => change kinds (Datatab.update (k, kind)));
  in k end;

fun get k dest wfpo = 
    dest ((case Datatab.lookup (WFCPOG.get_data wfpo) k of
	     SOME x => x
	   | NONE => invoke_copy k (invoke_empty k)  ))   (*adhoc value*)
    
fun put k mk x = 
    let
      fun modify_wfpo f x = WFCPOG.up_data (f (WFCPOG.get_data x)) x
    in
      modify_wfpo (Datatab.update (k, mk x))
    end
end;

functor WFCPOG_DataFun(Data: WFCPOG_DATA_ARGS) : WFCPOG_DATA =
struct

structure WFCPOG_Data = WFCPOG_Data;

type T = Data.T;
exception Data of T;

val kind = WFCPOG_Data.declare
  (Data Data.empty)
  (fn Data x => Data (Data.copy x))
  (fn Data x => Data (Data.extend x))
  ;

val get = WFCPOG_Data.get kind (fn Data x => x);
val put = WFCPOG_Data.put kind Data;

fun map f wfpo = put (f (get wfpo)) wfpo;

end;
