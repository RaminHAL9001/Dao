#!/bin/bash
#    set-debug.sh:
#    a script to turn Dao debugging ON or OFF for specified files.
# 
# Copyright (C) 2008-2012  Ramin Honary.
# 
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
# 
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
# 
# You should have received a copy of the GNU General Public License
# along with this program (see the file called "LICENSE"). If not, see
# <http://www.gnu.org/licenses/agpl.html>.

usage() {
    cat <<- ____END
USAGE:
    $0 [ON|OFF] [files]

You must specify either ON or OFF as the first parameter.
If [files] are not specified, 'grep' is used to search for
all possible files that could be modified.

Uses 'sed' to change every spcified file in [files] such that
debugging is turned ON or OFF. Every import directive for
'Dao.Debug.ON' or 'Dao.Debug.OFF' is changed according to the
ON/OFF parameter you specify. Also, "\$loc" and "xloc" statements
are changed accordingly, and '{-# LANGUAGE TemplateHaskell #-}'
will be uncommented/commented-out depending.
____END
}

DEBUG="$1"; shift;
DEBUG="${DEBUG^^}";

TEMPLATE='[{]-[#][[:space:]]*LANGUAGE[[:space:]]\+TemplateHaskell[[:space:]]*[#]-[}]';

case "$DEBUG" in
    ('ON')  XLOC='s,\<xloc\>,$loc,g';
            PRAGMA='s,^[[:space:]]*--\+[[:space:]]*\('"$TEMPLATE"'\),\1,';
            ;;
    ('OFF') XLOC='s,[$]loc\>,xloc,g';
            PRAGMA='s,^[[:space:]]*\('"$TEMPLATE"'\),-- \1,';
            ;;
    (*)     usage; exit 1;
            ;;
esac;

edit_files() {
    echo 'operating on files:';
    for file in "$@";
    do  bn=$(basename "$file");
        ERR=false;
        if [ "$bn" == 'Debug.hs' -o "$bn" == 'ON.hs' -o "$bn" == 'OFF.hs' ];
        then
            echo "ERROR: You must not modify debugging symbols in the \"$file\" file."; ERR=true;
        else
            echo "$file";
        fi;
    done;
    if "$ERR"; then return 1; fi;
    sed -e 's,\(import[[:space:]]\+\)\(qualified\|\)\([[:space:]]*Dao[.]Debug[.]\)\(ON\|OFF\),\1\2\3'"$DEBUG"',g' \
        -e 's,\(module[[:space:]]\+Dao[.]Debug[.]\)\(ON\|OFF\),\1'"$DEBUG"',g' \
        -e "$XLOC" \
        -e "$PRAGMA" \
        -i "$@" ;
}

if [ -z "$1" ];
then
    edit_files $( \
        grep -l 'Dao.Debug.\(ON\|OFF\)' -R . \
            --include='*.hs' \
            --exclude='OFF.hs' \
            --exclude='ON.hs' \
            --exclude='Debug.hs'; \
    );
else
    edit_files "$@";
fi

exit "$?"

