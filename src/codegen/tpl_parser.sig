(*****************************************************************************
 *          su4sml GCG - Generic Code Generator                            
 *                                                                            
 * tpl_parser.sig - template parser of a su4sml-gcg template
 * Copyright (C) 2005 Raphael Eidenbenz <eraphael@student.ethz.ch>
 *                                                                            
 * This file is part of su4sml-gcg.                                              
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

signature TPL_PARSER = 
sig

datatype TemplateTree
  = ElseNode of TemplateTree list
  | EvalLeaf of TemplateTree list
  | ForEachNode of string * TemplateTree list
  | IfNode of string * TemplateTree list
  | OpenFileLeaf of string
  | OpenFileIfNotExistsLeaf of string
  | RootNode of TemplateTree list
  | TextLeaf of string

val printTTree  	: TemplateTree -> unit
val parse 	  	: string -> TemplateTree

end
