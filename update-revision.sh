#!/bin/bash

#This script updates the changelog date for the current revision and
#copies revision information into the lpk files.

doupdate ()
{
	CHANGEDATE=`git log |grep '^Date:'|head -n 1|awk '{print $2 ", " $4, $3, $6, $5, $7};'`
	REVISION=`git rev-list --count HEAD`
	echo "Update in `pwd`"
	find . -name '*.lfm' -exec sed -i '/^  PPI =/d' '{}' \;
	sed -i "0,/([0-9]\+\.[0-9]\+-[0-9]\+ .*\$/{s/\(([0-9]\+\.[0-9]\+-[0-9]\+ Build [0-9]\+) \).*\$/\1$CHANGEDATE/}" changelog
	sed -i "0,/.*Change Log.*/{s/\(([0-9\+\.[0-9]\+-[0-9]\+\) Build [0-9]\+)/\1 Build $REVISION)/}" changelog

	VERSION=`grep 'version *([0-9]\+\.[0-9]\+-[0-9]\+ ' changelog | head -n 1| sed 's/.*version *(\([0-9]\+\.[0-9]\+-[0-9]\+\) .*/\1/'`
	V1=`echo "$VERSION"|sed 's/\([0-9]\+\)\.\([0-9]\+\)-\([0-9]\+\)/\1/'`
	V2=`echo "$VERSION"|sed 's/\([0-9]\+\)\.\([0-9]\+\)-\([0-9]\+\)/\2/'`
	V3=`echo "$VERSION"|sed 's/\([0-9]\+\)\.\([0-9]\+\)-\([0-9]\+\)/\3/'`
	TAG=$V1-$V2-$V3

	if [ -f IB.pas ]; then
	sed -i "s/FBIntf_Major.*/FBIntf_Major = $V1;/
		s/FBIntf_Minor.*/FBIntf_Minor = $V2;/
		s/FBIntf_Release.*/FBIntf_Release = $V3;/
		s/FBIntf_Version.*/FBIntf_Version = '$V1.$V2.$V3';/" IB.pas
	fi

	for PKG in `find . -name '*.lpk' -print`; do
		sed -i "/<CompilerOptions/,/<\/CompilerOptions/ ! { /<PublishOptions/,/<\/PublishOptions/ ! {s/<Version.*\/>/<Version Major=\"$V1\" Minor=\"$V2\" Release = \"$V3\" Build=\"$REVISION\" \/>/}}" $PKG
	done
	
	for DPRG in `find . -name '*.dproj' -print`; do
	  sed -i "s/\(MajorVer\">\)[0-9]\+</\1$V1</
	          s/\(MinorVer\">\)[0-9]\+</\1$V2</
	          s/\(Release\">\)[0-9]\+</\1$V3</	          
	          s/\(Build\">\)[0-9]\+</\1$REVISION</
	          s/\(FileVersion\">\)[0-9\.]\+</\1$V1.$V2.$V3.$REVISION</
	          s/\(ProductVersion\">\)[0-9\.]\+</\1$V1.$V2.$V3.$REVISION</"	 $DPRG          
	done
	
	find . -type f \( -name '*.odt' -o -name '*.ods' \) -print0 | while IFS= read -r -d '' DOC; do
	  PDF=`echo "$DOC" | sed 's/\(.*\)\.od[t|s]$/\1.pdf/'`
	  if [ ! -f "$PDF" ] || [ "$DOC" -nt "$PDF" ]; then
	    OUTDIR=`dirname "$DOC"`
	    libreoffice --invisible --convert-to pdf --outdir "$OUTDIR" "$DOC"
	    git add "$PDF" >/dev/null 2>&1
	  fi
	done
	
	git commit -a -m "Tagging Revision $TAG" 
	git tag -f R$TAG -m "Tagging Revision $V1.$V2.$V3.$REVISION"
}
if [ -n "`ps ax|grep libreoffice|grep -v grep`" ]; then
  echo "libreoffice is running. Please terminate all instances of libreoffice before running this script"
  exit 1
fi

doupdate
exit 0

