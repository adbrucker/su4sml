(*****************************************************************************
 *          su4sml - a SecureUML repository for SML              
 *                                                                            
 * base_cartridge.sig - an extended signature of CARTRIDGE specific 
 *			for the base cartridge 
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

(** 
 * This cartridge knows about the basic elements of UML class diagrams.
 * The elements are classifiers, attributes, and operations with their
 * parameters in terms of the Rep interface
 *) 
signature BASE_CARTRIDGE =
sig
	include CARTRIDGE
	
(** returns the current classifier. *)
val curClassifier: environment -> Rep.Classifier option
								  
(** returns the current attribute *)
val curAttribute: environment -> Rep.attribute option
								 
(** returns the current operation *)
val curOperation: environment -> Rep.operation option

(** returns the current operation parameter *)
val curArgument  : environment -> (string * Rep_OclType.OclType) option
								  
end
