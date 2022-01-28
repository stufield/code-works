#!/bin/bash

CURPWD=$PWD
DESTDIR=train-data
if [ ! -d $DESTDIR ]; then
  mkdir $DESTDIR
fi
git archive --format=tar \
  --remote=ssh://git@bitbucket.sladmin.com:7999/sv/somareadr master \
  inst/data-raw/sample_adat.adat | \
  tar -xvf - --strip-components 2 -C $DESTDIR
cd $CURPWD
echo ----------------------------------------
echo -e "*\033[32m ADATs extracted to $DESTDIR/ ... \033[37m"
echo ----------------------------------------
