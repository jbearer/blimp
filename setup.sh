#!/usr/bin/env bash

build_opts="$@"

function make_build_dir()
{
    dir="$1"
    shift

    if mkdir "bld-$dir"
    then
        pushd "bld-$dir"
        cmake "$@" $build_opts ..
        popd
    fi
}

make_build_dir debug    -DCMAKE_BUILD_TYPE=Debug
make_build_dir release  -DCMAKE_BUILD_TYPE=Release
make_build_dir profile  -DCMAKE_BUILD_TYPE=Relase -DCMAKE_C_FLAGS="-g" ..
