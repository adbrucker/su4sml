.PHONY: doc apidoc tags

doc: 
	smldoc --directory=doc --linksource --showsummary --overview=doc/overview.html --windowtitle="SU4SML_Documentation" src/su4sml.cm

apidoc: 
	smldoc --windowtitle="SU4SML API Documentation" --hidebysig --directory=doc/api --linksource --showsummary --overview=doc/overview.html src/su4sml.cm

tags:
	cd src && etags --language=none --regex='/^\(datatype\|type\|val\|fun\|structure\|signature\)[ \t]*\([^ \t]*\)/\2/' *.sml *.sig codegen/*.sml codegen/*.sig
su4sml:
	(cd src && echo "val _ = use \"ROOT.ML\"; val _ = OS.Process.exit 0" | sml ) || true
