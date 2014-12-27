#!/bin/bash

testModules=$(find test -name '*.hs' | sed -e 's#^test/##' -e 's#\.hs$##' -e 's#/#.#g' | grep -ve '^Spec$' | awk 'BEGIN{fst=1}{if (fst) {fst=0} else {printf ", "} printf "%s", $0}')

cat hob.cabal.template | sed -e "s/##SPEC_MODULES##/$testModules/" > hob.cabal

