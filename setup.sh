#!/bin/bash
# -*- compile-command: "./setup.sh setup"; -*-
set -e
source config.sh


function setup_standalone {
    local FUNCTION_NAME="$1"
    if [ -d out/$FUNCTION_NAME ]; then return; fi  # non-destructive
    if [ ! -d /tmp/af-standalones ]; then git clone gh:pv-pterab-s/af-standalones /tmp/af-standalones; fi
    (cd /tmp/af-standalones && git clean -dfx && git pull origin main) &> /dev/null

    rm -rf out/$FUNCTION_NAME

    local PKG_PATH=$(find /tmp/af-standalones -name $FUNCTION_NAME.tar)

    (cd out && tar xf $PKG_PATH)
    rm -f out/$FUNCTION_NAME/$FUNCTION_NAME.hpp
    ln -sf $(readlink -f out/batch-1-af/src/backend/oneapi/kernel/$FUNCTION_NAME.hpp) $(readlink -f out/$FUNCTION_NAME/$FUNCTION_NAME.hpp)
    cat > out/$FUNCTION_NAME/.dir-locals.el <<EOF
((c++-mode . ((compile-command . "cd $(readlink -f out/$FUNCTION_NAME) && rm -rf out && make && ./out/main"))))
EOF
    echo building $FUNCTION_NAME
    if ! (cd out/$FUNCTION_NAME && make) &> out/build-log-$FUNCTION_NAME.txt; then
        echo FAIL
        cat out/build-log-$FUNCTION_NAME.txt
        exit 1
    fi
}

function setup_clone_function {
    local FUNCTION_NAME="$1"
    if [ -d out/$FUNCTION_NAME ]; then return; fi  # non-destructive
    git clone gh:pv-pterab-s/new-dev out/$FUNCTION_NAME
    rm -f out/$FUNCTION_NAME/resize.hpp
    ln -sf $(readlink -f out/batch-1-af/src/backend/oneapi/kernel)/$FUNCTION_NAME.hpp $(readlink -f out/$FUNCTION_NAME)/$FUNCTION_NAME.hpp
    cat > out/$FUNCTION_NAME/.dir-locals.el <<EOF
((c++-mode . ((compile-command . "cd $(readlink -f out/$FUNCTION_NAME) && rm -rf out && make && ./out/main"))))
}

function harness_function {
    local FUNCTION_NAME="$1"
    cat > out/batch-1-af/.dir-locals.el <<EOF
((c++-mode . ((compile-command . "cd $(readlink -f out/$FUNCTION_NAME) && rm -rf out && make && ./out/main"))))
EOF
    cat > out/$FUNCTION_NAME/.dir-locals.el <<EOF
((c++-mode . ((compile-command . "cd $(readlink -f out/$FUNCTION_NAME) && rm -rf out && make && ./out/main"))))
EOF
}

function setup_env_for_function {
    local FUNCTION_NAME="$1"
    if [ ! -d "out/$FUNCTION_NAME" ]; then git clone gh:pv-pterab-s/new-dev out/$FUNCTION_NAME; fi

    rm -f out/$FUNCTION_NAME/resize.hpp
    ln -sf $(readlink -f out/batch-1-af/src/backend/oneapi/kernel)/$FUNCTION_NAME.hpp $(readlink -f out/$FUNCTION_NAME)/$FUNCTION_NAME.hpp


}

function backup {
    if [ ! -d out/batch-1-af ]; then return; fi
    rm -rf out/bkup-batch-1-af
    cp -r out/batch-1-af out/bkup-batch-1-af
}

function constexpr_commit_sha {  # this puts in the constexpr
    (
        cd out/batch-1-af
        git log origin/batch-1-a770-works --pretty=format:'%H' --grep="undo all constexpr" | head -n1
    )
}

function patch {
    backup
    local SHA="$(constexpr_commit_sha)"
    (
        cd out/batch-1-af
        git cherry-pick $SHA
    )
}

function current_state {
    local GIT_STATE
    if ! (cd out/batch-1-af && git diff-index --quiet HEAD --); then
        GIT_STATE=dirty
    else
        local ahead=$(cd out/batch-1-af && git log @{upstream}..HEAD --oneline | wc -l)
        local behind=$(cd out/batch-1-af && git log HEAD..@{upstream} --oneline | wc -l)
        if [ "$ahead" -gt 0 ]; then
            # echo "Local branch is ahead of remote by $ahead commits."
            GIT_STATE=ahead
        elif [ "$behind" -gt 0 ]; then
            # echo "Local branch is behind remote by $behind commits."
            GIT_STATE=behind
        else
            # echo "origin and local branches are equal"
            GIT_STATE=clean
        fi
    fi

    local PATCH_STATE
    if [ -n "$(cd out/batch-1-af && git --no-pager log batch-1 --pretty=format:'%H' --grep="undo all constexpr")" ]; then
        PATCH_STATE=patched
    else
        PATCH_STATE=unpatched
    fi

    echo ${GIT_STATE}-${PATCH_STATE}
}

function clone {
    rm -rf out/batch-1-af
    git clone -b batch-1 gh:pv-pterab-s/arrayfire out/batch-1-af
}

function build {
    (
        cd out/batch-1-af
        mkdir -p build
        cd build
        CXX=icpx CC=icx cmake .. -DCMAKE_CXX_FLAGS="-w" -DAF_BUILD_UNIFIED=OFF -DAF_BUILD_CPU=ON -DAF_BUILD_CUDA=OFF -DAF_BUILD_OPENCL=OFF -DAF_BUILD_ONEAPI=ON -DCMAKE_BUILD_TYPE=Debug -DCMAKE_EXPORT_COMPILE_COMMANDS=ON -DCMAKE_PREFIX_PATH=/usr/lib/x86_64-linux-gnu/cmake -DCMAKE_EXPORT_COMPILE_COMMANDS=ON
        make -j22
    )
}

function test_ {
    ./run-tests.sh
}

function unpatch {
    if [[ "$(current_state)" == "*-unpatched" ]]; then return; fi
    backup
    (
        cd out/batch-1-af
        local SHA="$(git --no-pager log batch-1 --pretty=format:'%H' --grep="undo all constexpr")"
        git rebase --onto ${SHA}^ ${SHA}
    )
}

function main {
    case "$1" in
        setup)
            mkdir -p out
            backup
            clone
            current_state
            [ "$(current_state)" = "clean-unpatched" ]
            patch
            [ "$(current_state)" = "ahead-patched" ]
            build
            test_    # weird name to avoid conflict with builtin `test`
            ;;
        test)
            test_
            ;;
        publish)
            backup
            unpatch
            [ "$(current_state)" = "ahead-unpatched" ] ||
                [ "$(current_state)" = "clean-unpatched" ]
            ;;
        patch)
            patch
            ;;
        unpatch)
            unpatch
            ;;
        build)
            build
            ;;
        state)
            current_state
            ;;
        backup)
            backup
            ;;
        undo)
            if [ ! -d out/bkup-batch-1-af ]; then echo no backup exists; exit 1; fi
            rm -rf out/batch-1-af
            mv out/bkup-batch-1-af out/batch-1-af
            ;;
        function)
            FUNCTION_TO_SETUP="$2"
            setup_standalone $FUNCTION_TO_SETUP
            setup_clone_function $FUNCTION_TO_SETUP
            harness_function $FUNCTION_TO_SETUP
            ;;
        *)
            cat <<EOF
Usage:
  setup.sh (setup|test|publish|function)

EOF
            ;;
esac
}
main $*
