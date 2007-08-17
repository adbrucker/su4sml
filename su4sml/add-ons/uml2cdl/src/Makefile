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
