##############################################################################
# su4sml --- a SML repository for managing (Secure)UML/OCL models
#             http://projects.brucker.ch/su4sml/
#                                                                            
# Makefile --- 
# This file is part of su4sml.
#
# Copyright (c) 2005-2007, ETH Zurich, Switzerland
#
# All rights reserved.
#
# Redistribution and use in source and binary forms, with or without
# modification, are permitted provided that the following conditions are
# met:
#
#     * Redistributions of source code must retain the above copyright
#       notice, this list of conditions and the following disclaimer.
#
#     * Redistributions in binary form must reproduce the above
#       copyright notice, this list of conditions and the following
#       disclaimer in the documentation and/or other materials provided
#       with the distribution.
#
#     * Neither the name of the copyright holders nor the names of its
#       contributors may be used to endorse or promote products derived
#       from this software without specific prior written permission.
#
# THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
# "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
# LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
# A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
# OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
# SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
# LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
# DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
# THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
# (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
# OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
##############################################################################
# $Id: AVL_test.thy 40058 2007-06-29 21:40:20Z brucker $

DATE=$(shell date)
BOTTOM="generated on $(DATE)"

.PHONY: doc apidoc tags

doc: 
	smldoc --directory=doc --linksource --showsummary \
	       --overview=doc/overview.html \
	       --bottom=$(BOTTOM)\
	       -c utf8 \
	       --linksource\
	       --listsubmodule\
	       --windowtitle="SU4SML_Documentation ($(DATE))" \
		`find src -name "*.sml" -or -name "*.sig"`

apidoc: 
	smldoc --windowtitle="SU4SML API Documentation" --hidebysig --directory=doc/api --linksource --showsummary --overview=doc/overview.html src/su4sml.cm

codegen-doc:
	smldoc --windowtitle="GCG Documentation" --directory=codegen-doc --linksource --showsummary --overview=doc/overview.html --linkoffline=../doc@doc/module-list src/codegen/codegen.cm

tags:
	cd src && etags --language=none --regex='/^\(datatype\|type\|val\|fun\|structure\|signature\)[ \t]*\([^ \t]*\)/\2/' *.sml *.sig codegen/*.sml codegen/*.sig
su4sml:
	(cd src && echo "val _ = use \"ROOT.ML\"; val _ = OS.Process.exit 0" | sml ) || true

rep_parser: 
	ml-build src/su4sml.cm RepParser.test src/rep_parser

repsecure_parser:
	ml-build src/su4sml.cm Rep_SecureUML_ComponentUML.test src/repsecure_parser

codegen: 
	ml-build src/codegen/codegen.cm Codegen.main codegen

