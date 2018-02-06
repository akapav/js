#!/bin/sh

# this script is just a trick to pretend that script runner in nashorn
# in order to avoid modifications in eshost library
# first parameter would be --language=es6 (see https://github.com/bterlson/eshost/blob/3804b6cdb5d5f5412b4924f659c1fe5a3dd35a8d/lib/agents/nashorn.js#L18)
if [ -e ./roswell/cl-js ]
then
  ./roswell/cl-js "$2"
else
  ./roswell/cl-js.ros "$2"
fi

