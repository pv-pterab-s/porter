#!/bin/bash
# -*- compile-command: "./run-tests.sh"; -*-
set -e
source config.sh
cd out/batch-1-af/build

./test/tile_oneapi --gtest_filter="Tile/0.*:-Tile/0.Tile111:Tile/0.Tile3D111:Tile/0.Tile2D111"

for I in $NEW_FUNCTIONS; do
    if [ $I = lookup ]; then continue; fi  # missing tests?
    if [ $I = tile ]; then continue; fi  # already done
    echo "!!!!!!!!!!!!!!!!!!!!!!!!!!!! $I"
    ./test/${I}_oneapi --gtest_filter="${I^}/0.*" || :
done
