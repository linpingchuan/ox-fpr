* Pre-requisites:
    * XCode command line tools
    * Remove older versions

        $ uninstall-hs
        -- Versions found on this system
        7.10.2
        7.10.3
        -- To remove a version and all earlier: uninstall-hs thru VERSION
        -- To remove only a single version:     uninstall-hs only VERSION

        $ uninstall-hs thru 7.10.3
        -- Would remove version 7.10.3 and earlier
        ...
        ** /Library/Haskell is not empty, but has no more versions. Consider removing.
        -- To actually remove these files, sudo run the command again with --remove
        -- To generate a script to remove these files, run the command again with --script

        $ sudo uninstall-hs thru 7.10.3 --remove
        Password:
        -- Removing version 7.10.3 and earlier
        ** /Library/Haskell is not empty, but has no more versions. Consider removing.

* Download and Install Haskell Platform
[https://www.haskell.org/platform/mac.html](https://www.haskell.org/platform/mac.html)      

* Check version

        $ ghc --version
        The Glorious Glasgow Haskell Compilation System, version 7.10.3

* Install `ghc-mod`

        $ cabal update && cabal install ghc-mod

Ref: https://github.com/DanielG/ghc-mod        

* Local install info
file:///Users/anuragkapur/Library/Haskell/share/doc/x86_64-osx-ghc-7.10.3/index.html
        
* Install __lhs2tex__

        $ cabal install lhs2tex
        Resolving dependencies...
        Downloading lhs2tex-1.19...
        Configuring lhs2tex-1.19...
        Building lhs2tex-1.19...
        Installed lhs2tex-1.19
        Updating documentation index
        /Users/anuragkapur/Library/Haskell/share/doc/x86_64-osx-ghc-7.10.3/index.html            

    * Using lhs2tex
    Ensure lhs doc is wrapped in the tex code block:
    
        \documentclass{article}
        %include polycode.fmt
        \begin{document}
        
        Literate haskell script here...
        
        \end{document}
        
    Now convert to lhs to tex using the following command        
    
        lhs2TeX src/tfwh/chapter1.lhs -o ~/Desktop/test.tex
        
