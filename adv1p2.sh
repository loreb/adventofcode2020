#! /bin/sh
shellcheck "$0"

set -- $(cat) || exit

for i in "$@" ; do
        for j in "$@" ; do
                for k in "$@" ; do
                        test 2020 = $(( i + j + k )) || continue
                        echo $(( i * j * k ))
                        exit
                done
        done
done

