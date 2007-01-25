(*****************************************************************************
 *          su4sml - a SecureUML repository for SML              
 *                                                                            
 * rep_secure.sml - repository structure for uml models with security 
 *                  specifications
 * Copyright (C) 2005 Achim D. Brucker <brucker@inf.ethz.ch>   
 *                    Juergen Doser    <doserj@inf.ethz.ch>
 *                    Burkhart Wolff   <bwolff@inf.ethz.ch>
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


(** a repository for uml models equipped with a security language *)
signature REP_SECUREUML =
sig
	
	(** the security language used. *)
    structure Security : SECUREUML
						 
	(** 
	 * a "secure repository" model consist of a repository model 
	 * plus a security configuration.
	 *)
    type Model = Rep_Core.Classifier list * Security.Configuration
				 
	(** *) 
    val readXMI: string -> Model

    val test: (string * string list) -> OS.Process.status
			   
end

functor Rep_SecureUML(structure Security : SECUREUML) : REP_SECUREUML  =
struct

    structure Security = Security

    type Model = Rep_Core.Classifier list * Security.Configuration

    val readXMI = Security.parse o RepParser.readFile

    fun test (_,filename::_) = (Rep2String.printList (#1 (readXMI filename)); OS.Process.success)
end

structure Rep_SecureUML_ComponentUML 
  = Rep_SecureUML(structure Security = SecureUML(structure Design=ComponentUML))
			

(* structure Rep_SecureUML_ControllerUML
  = Rep_Secure(structure Security = SecureUML(structure Design=ControllerUML))*)
