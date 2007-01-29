(*****************************************************************************
 *          su4sml - a SecureUML repository for SML              
 *                                                                            
 * xmltree.sml - datastructure for xml files
 * Copyright (C) 2005  Achim D. Brucker <brucker@inf.ethz.ch>
 *                     Jürgen Doser <doserj@inf.ethz.ch>
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

structure XmlTree : sig
    type Attribute
    type Tag = string * Attribute list
               
    datatype Tree = Node of Tag * Tree list
                  | Text of string
                            
    val text              : Tree -> string
    val attributes        : Tree -> Attribute list
    val tagname           : Tree -> string
    val children          : Tree -> Tree list
    val node_children     : Tree -> Tree list
    val text_children     : Tree -> Tree list
    val optional_value_of : string -> Attribute list -> string option
    val value_of          : string -> Attribute list -> string
    val has_attribute     : string -> Tree -> bool         
    exception IllFormed of string
end = struct 
open library
infix 1 |>
exception IllFormed = Fail
                       
(** A name-value pair. *)
type Attribute = (string * string) 

(** Tags consist of element names, and a list of attribute name-value pairs. *)
type Tag = string * Attribute list

datatype Tree = Node of Tag * Tree list
              | Text of string

val filter_nodes = List.filter (fn Node x => true
								 | _      => false)

val filter_text  = List.filter (fn Text x => true
								 | _      => false)
		   
fun text (Text s) = s
  | text _        = raise IllFormed "in XmlTree.text: argument is a Node element"
			    
fun attributes (Node ((elem,atts),trees)) = atts
  | attributes _ = raise IllFormed "in attributes_of: argument is a Text-Node"

fun children   (Node ((elem,atts),trees)) = trees
  | children   _ = raise IllFormed "in XmlTree.children: argument is a Text-Node"

fun node_children (Node ((elem,atts),trees)) = filter_nodes trees
  | node_children   _ = raise IllFormed "in XmlTree.node_children: argument is a Text-Node"

fun text_children (Node ((elem,atts),trees)) = filter_text trees
  | text_children _ = raise IllFormed "in XmlTree.text_children: argument is a Text-Node"

fun tagname    (Node ((elem,atts),trees)) = elem
  | tagname    (Text _) = ""

fun optional_value_of string atts = Option.map #2 (List.find (fn (x,_) => x = string) atts)


fun has_attribute string tree = Option.isSome (optional_value_of string (attributes tree))


fun value_of string atts = valOf (optional_value_of string atts)
    handle Option => raise IllFormed ("in value_of: did not find attribute "^string)

end
