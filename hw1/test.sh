#!/bin/sh
runhaskell test.hs > output
runhaskell Calculator.hs < input >> output
diff output output_EXPECTED

