#!/bin/bash
#
# generates the static maps of roman numbers and their offsets into their respective sorted lists

generate() {
    NAME="$1"

    awk -v name=$NAME '
      BEGIN { print "    let " name " = vec![" }
            { print "        \"" $1 "\"," }
      END   { print "    ];" }
    '
}

generate before_m < before-M.txt > before-M.rs.txt
generate  after_m <  after-M.txt >  after-M.rs.txt
