.PHONY: doc apidoc

doc: 
	smldoc --windowtitle="SU4SML Documentation" --directory=doc --linksource --showsummary --overview=doc/overview.html src/su4sml.cm

apidoc: 
	smldoc --windowtitle="SU4SML API Documentation" --hidebysig --directory=doc/api --linksource --showsummary --overview=doc/overview.html src/su4sml.cm

su4sml:
	(cd src && echo "val _ = use \"ROOT.ML\"; val _ = OS.Process.exit 0" | sml ) || true
