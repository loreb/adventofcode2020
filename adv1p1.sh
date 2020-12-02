#! /bin/sh
shellcheck "$0"

set -- $(cat) || exit

for i in "$@" ; do
        for j in "$@" ; do
                test 2020 = $(( i + j )) || continue
                echo $(( i * j))
                exit
        done
done

