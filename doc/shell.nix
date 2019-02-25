with (import <nixpkgs> {});
stdenv.mkDerivation {
  name = "docsEnv";
  buildInputs = [
    (texlive.combine {
      inherit (texlive)
        #        scheme-small
                scheme-full
        
        
        # libraries
        unicode-math lm-math amsmath
        enumitem bclogo xcolor newunicodechar
        appendix syntax
#
#        stmaryrd # St Mary Road symbols for theoretical computer science
#        todonotes
#        forest
#        pgfopts
#        environ
#        inlinedef
#        trimspaces
#        IEEEtran
#        cleveref
        latexmk # build tools
        
        ;
    })
    cddl
    cbor-diag
    ];
}
