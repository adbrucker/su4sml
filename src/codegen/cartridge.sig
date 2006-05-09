(*****************************************************************************
 *          su4sml - a SecureUML repository for SML              
 *                                                                            
 * cartridge.sig - the minimal signature every cartridge has to implement
 * Copyright (C) 2005 Raphael Eidenbenz <eraphael@student.ethz.ch>
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

(** the minimal signature every code-generator cartridge has to implement. *)
signature CARTRIDGE =
sig
	
	(** 
	 * the environment in which template-file statements are to be evaluated. 
	 * Ususally this will contain lists of model elements and
	 * "pointers" to the "current" elements
	 *)
	type environment

	(** 
	 * The particular model from which model element information is
	 * taken. 
	 * This can be cartridge specific.
	 *)
	type Model
		 
	(** 
	 * returns the model information as it is part of the current
	 * environment. 
	 *) 
	val getModel : environment -> Model

	(** initialze the environment by parsing the given classifier list *)
	val initEnv : Rep.Model ->  environment
													   
	(** look up string-valued variables in the environment by name. *)
	val lookup : environment -> string -> string

	(** evaluate boolean-valued predicates in the environment by name. *)
	val test : environment -> string -> bool

	(** 
	 * return a list of environment, where the "current" element
	 * iterates over a given list. 
	 *)
	val foreach : string ->  environment -> environment list
end
