#!/bin/bash
set -o errexit
set -o nounset
set -o pipefail

# Output a stream of things

old_data=
while true; do
    new_data=$(xsel -o)

    if [ "$old_data" != "$new_data" ]; then
        echo "$new_data"
    fi;
    old_data=$new_data
    sleep 0.1
done
