#!/bin/bash
#
# generates the static maps of roman numbers. the rust code adds their offset into the sorted list
# when it converts the vec into a map, using enumerate()

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
