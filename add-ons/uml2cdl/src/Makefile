##############################################################################
# su4sml --- a converter from UML models to WS-CDL. part of su4sml
#            http://projects.brucker.ch/su4sml/
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

##
##
#
# program sources
SOURCES=cdl2xml.sml  cdl.sml  uml2cdl.sml  xmi2cdl.sml
#
# basic MLton setup:
MLTON=mlton
#
# setup for compiling and linking dlls
CC_DLL_OPT=-Wl,--add-stdcall-alias -shared
LD_DLL_OPT=-Wl,--add-stdcall-alias -shared -Wl,--export-all-symbols -Wl,--export-all-symbols
DLL_NAME=cyguml2cdl
#
# setp for compiling and linking dynamic libs 
CC_SO_OPT= -fPIC
LD_SO_OPT= -shared
SO_NAME=libuml2cdl.0.0.so
#
# system binaries
RM=rm
SH=/bin/sh
#
# compile targets
.PHONY: clean

uml2cdl: uml2cdl.mlb $(SOURCES)
	$(MLTON) uml2cdl.mlb 

uml2cdl-cygwin:  uml2cdl-cygwin.mlb $(SOURCES)
	$(MLTON) uml2cdl-cygwin.mlb

$(DLL_NAME): uml2cdl-cygwin.mlb $(SOURCES)
	$(MLTON) -codegen c -export-header uml2cdl-smlffi.h \
		 -output "$(DLL_NAME)"
	         -cc-opt "$(CC_DLL_OPT)" -link-opt "$(LD_DLL_OPT)" \
                 uml2cdl-cygwin.mlb 

$(SO_NAME): uml2cdl.mlb $(SOURCES)
	$(MLTON) -codegen c -export-header uml2cdl-smlffi.h \
	         -link-opt "-WL,-soname,$(SO_NAME)" -output $(SO_NAME) \
	         -cc-opt "$(CC_SO_OPT)" -link-opt "$(LD_SO_OPT)" \
                 uml2cdl.mlb 

clean: 
	$(RM) -f $(DLL_NAME) $(SO_NAME) uml2cdl uml2cdl-cygwin uml2cdl-smlffi.h
