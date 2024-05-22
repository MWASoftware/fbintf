#!/bin/sh
FBSRC=$1/src

h2pas -i -e -o include/dyn_consts.inc $FBSRC/include/dyn_consts.h 
h2pas -i -e -o include/iberror.inc $FBSRC/include/iberror.h 
sed 's/(unsigned char) *\([0-9]\)/\1/
s/(unsigned short) *\([0-9]\)/\1/' $FBSRC/include/firebird/impl/blr.h >blr_tmp.h
h2pas -i -e -o include/blr.inc blr_tmp.h
h2pas -i -e -o include/consts_pub.inc $FBSRC/include/firebird/impl/consts_pub.h
h2pas -i -e -o include/inf_pub.inc $FBSRC/include/firebird/impl/inf_pub.h    


