* Download Haskell Platform
[https://www.haskell.org/platform/mac.html](https://www.haskell.org/platform/mac.html)      
Pre-requisites: 
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
    
* Check version

        $ ghc --version
        The Glorious Glasgow Haskell Compilation System, version 7.10.3

