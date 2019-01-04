#!/bin/bash

WDIR=$( cd $(dirname $0) ; pwd)

for EXERCISE in $(ls ${WDIR})
do
  NEW_LIB=${WDIR}/${EXERCISE}
  if [ -d ${NEW_LIB} ]
  then
    ERL_LIBS=$ERL_LIBS:${NEW_LIB}
  fi
done

(
export ERL_LIBS=$ERL_LIBS 
erl
)