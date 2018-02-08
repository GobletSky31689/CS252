This project depends on the some additional modules. You can install the modules manually using cabal or use the following build-depends configuration:

build-depends: base >= 4.7 && < 5, parsec, MissingH, containers, random


Once these modules are installed, use the following script:

$ runhaskell Obfuscator.hs test.java

This would create a new file named obf_test.java, which contains the obfuscated source code.