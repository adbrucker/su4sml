(*****************************************************************************
 *          su4sml - a SecureUML repository for SML              
 *                                                                            
 * secureuml_cartridge.sig - an extended signature of CARTRIDGE specific 
 *			for the SecureUML cartridge 
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

signature SECUREUML_CARTRIDGE =
sig

(* from CARTRIDGE *)
 type environment
 val initEnv : Rep_SecureUML_ComponentUML.Model ->  environment

 val lookup : environment -> string -> string
 val evalCondition : environment -> string -> bool
 val foreach : string ->  environment -> environment list

(* specific for SECUREUML_CARTRIDGE *)
 val curPermissionSet: environment -> Rep_SecureUML_ComponentUML.Security.Permission list
 val curPermission : environment -> Rep_SecureUML_ComponentUML.Security.Permission
 val curRole : environment -> string
 val curConstraint : environment -> Rep_OclTerm.OclTerm
 
end
