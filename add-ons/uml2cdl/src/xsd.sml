(*****************************************************************************
 * uml2cdl --- a converter from UML models to WS-CDL. part of su4sml
 *             http://projects.brucker.ch/su4sml/
 *                                                                            
 * xsd.sml --- 
 * This file is part of uml2cdl.
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
(** very simple structure defining basix xml schema types. *)
structure XML_Schema = 
struct 
(* for simplicity: *)

(** 
 * XML namespace NCName. 
 * the basic grammar is: 
 * NCName ::= (Letter | '_') (Letter | Digit | '.' | '-' | '_')* 
 *)
type NCName = string

(** 
 * XML namespace QName, i.e., a NCName with optionally a namespace prefix. 
 * the basic grammar is: QName ::= (NCName ':')? NCName    
 *)
type QName  = string 

type anyURI = string (* a URI like "http://www.example.com/file.html#id5"*)


type xsdElement = { name  : string,
                    typ   : string }

type xsdSequence = xsdElement list


datatype xsdType = SimpleType of xsdElement
                 | ComplexType of xsdComplexType
withtype xsdComplexType = { name : string,
                            sequence : xsdType list
                          }

type xsdSchema = { attributeFormDefault: string,
                   elementFormDefault:   string,
                   targetNamespace:      string,
                   definitions:          xsdType list
                 }

end


structure XML_Schema2Xml =
struct
open XML_Schema


fun xsdType2Xml (ComplexType ct) = 
    XmlTree.Node (("xsd:complexType",[("name", #name ct)
                  ]),
                  [
                   XmlTree.Node (("xsd:sequence",[]),
                                 map xsdType2Xml (#sequence ct)
                                )
                  ]
                 )
  | xsdType2Xml (SimpleType st) = 
    XmlTree.Node (("xsd:element",[("name",#name st),("type",#typ st)]),[])

fun xsdSchema2Xml (schema:xsdSchema) = 
    XmlTree.Node (("xsd:schema",[("attributeFormDefault",#attributeFormDefault schema),
			         ("elementFormDefault", #elementFormDefault schema),
			         ("targetNamespace", #targetNamespace schema)
                  ]),
                  map xsdType2Xml (#definitions schema))



end
