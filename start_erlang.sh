#!/bin/bash

WDIR=$( cd $(dirname $0) ; pwd)
ERLLIBS="$WDIR/.erllibs"

mkdir -p $ERLLIBS

for EXERCISE in $(ls ${WDIR})
do
  NEW_LIB="${WDIR}/${EXERCISE}"
  if [ -d ${NEW_LIB} ]
  then
    (
      set -x
      cd ${NEW_LIB}
      
      for erlang_file in $(ls *.erl)
      do
        erlc $erlang_file
      done

      mv *.beam $ERLLIBS/
    )
  fi
done

(
  set -x
  erl -pa $ERLLIBS
  rm -r $ERLLIBS
)
