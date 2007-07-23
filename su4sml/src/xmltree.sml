(*****************************************************************************
 * su4sml --- a SML repository for managing (Secure)UML/OCL models
 *             http://projects.brucker.ch/su4sml/
 *                                                                            
 * xmltree.sml --- datastructure for xml files
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

(** datatypes and functions for XML trees. *)
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
end = struct 
open library
infix 1 |>
                       
(** A name-value pair. *)
type Attribute = (string * string) 

(** Tags consist of element names, and a list of attribute name-value pairs. *)
type Tag = string * Attribute list

(** A Node in an XML tree is either a tag with substrees, or plain text. *)
datatype Tree = Node of Tag * Tree list
              | Text of string

val filter_nodes = List.filter (fn Node x => true
				 | _      => false)
                   
val filter_text  = List.filter (fn Text x => true
				 | _      => false)

fun tagname    (Node ((elem,atts),trees)) = elem
  | tagname    (Text _) = ""

fun text (Text s) = s
  | text x        = error ("in XmlTree.text: argument is a Node element (<"^tagname x^">).")
			  
fun attributes (Node ((elem,atts),trees)) = atts
  | attributes _ = error "in attributes_of: argument is a Text-Node"

fun children   (Node ((elem,atts),trees)) = trees
  | children   _ = error "in XmlTree.children: argument is a Text-Node"

fun node_children (Node ((elem,atts),trees)) = filter_nodes trees
  | node_children   _ = error "in XmlTree.node_children: argument is a Text-Node"

fun text_children (Node ((elem,atts),trees)) = filter_text trees
  | text_children _ = error "in XmlTree.text_children: argument is a Text-Node"

fun optional_value_of string atts = Option.map #2 (List.find (fn (x,_) => x = string) atts)


fun has_attribute string tree = Option.isSome (optional_value_of string (attributes tree))


fun value_of string atts = valOf (optional_value_of string atts)
    handle Option => error ("in XmlTree.value_of: argument has no attribute "^string)
                     
end
