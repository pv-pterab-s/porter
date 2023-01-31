#!/bin/bash
# -*- compile-command: "./update-all-standalones.sh"; -*-
# setup standalone dirs. semi-automatically update
set -e

(
    cd ~/af-standalones
    git co .
    git clean -dfx
)

STANDALONE_NAMES="$(find ~/af-standalones -name '*.tar' | grep -v fail)"

# rm -rf out/convolve
# for I in /home/gpryor/af-standalones/master/convolve.tar; do
for I in $STANDALONE_NAMES; do
    echo $I
    if ! [ -d out/$(basename $I .tar) ]; then
        (
            cd out
            tar xf $I
            cd $(basename $I .tar)
            rm -f snap.sh

            # pull kernel source from arrayfire
            for HEADER in $(ls *.hpp); do
                if [ ! -f /home/gpryor/porter/out/batch-1-af/src/backend/oneapi/kernel/$HEADER ]; then continue; fi
                rm $HEADER
                ln -s /home/gpryor/porter/out/batch-1-af/src/backend/oneapi/kernel/$HEADER
            done

            # interp needs to pull from arrayfire
            if [ -f stubs/kernel/interp.hpp ]; then
                rm stubs/kernel/interp.hpp
                ln -sf /home/gpryor/porter/out/batch-1-af/src/backend/oneapi/kernel/interp.hpp $(readlink -f stubs/kernel/interp.hpp)
            fi

            FN=$(mktemp)
            tail -n +2 Makefile > $FN; cp $FN Makefile; rm $FN
            sed -i 's/ *\$(shell.*hpp) *//g' Makefile
            sed -i 's/-fsycl/-fsycl -fdiagnostics-absolute-paths/g' Makefile

            cat > .dir-locals.el <<EOF
((c++-mode . ((compile-command . "cd $(readlink -f .) && rm -rf out && make && ./out/main"))))
EOF
        )
    fi
    (  # make sure the thing builds
        cd out/$(basename $I .tar)
        # rm -rf out
        make
        ./out/main
    )
    (   # package up the new standalones
        cd out
        PACKAGE_NAME=$(basename $I .tar)
        tar --exclude=".git" --exclude=".ccls-cache" --exclude="out" --exclude=".dir-locals.el" -hc $PACKAGE_NAME > $PACKAGE_NAME.tar
        echo out/$PACKAGE_NAME.tar created standalone. copy to $I
        cp $PACKAGE_NAME.tar $I
    )
done
