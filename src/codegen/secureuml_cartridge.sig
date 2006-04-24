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
 include BASE_CARTRIDGE
(* specific for SECUREUML_CARTRIDGE *)
 val curPermissionSet: environment -> Rep_SecureUML_ComponentUML.Security.Permission list option
 val curPermission : environment -> Rep_SecureUML_ComponentUML.Security.Permission option
 val curRole : environment -> string option
 val curConstraint : environment -> Rep_OclTerm.OclTerm option 

val isInPermission :  Rep_SecureUML_ComponentUML.Security.Design.Action -> Rep_SecureUML_ComponentUML.Security.Permission -> bool
 structure Security:SECURITY_LANGUAGE
end
