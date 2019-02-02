#!/bin/bash

WDIR=$( cd $(dirname $0) ; pwd)

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
    )
    ERL_LIBS="$ERL_LIBS  -pa ${NEW_LIB} "
  fi
done

(
  set -x
  erl $ERL_LIBS
)

(
  for EXERCISE in $(ls ${WDIR})
  do
    NEW_LIB="${WDIR}/${EXERCISE}"
    if [ -d ${NEW_LIB} ]
    then
      (
        set -x
        cd ${NEW_LIB}
        rm *.beam
      )
    fi
  done
)