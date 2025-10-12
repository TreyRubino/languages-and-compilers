#!/bin/bash

# @author Trey Rubino

ROOT_DIR="$(dirname "$0")/.."
TEST_CASES="$ROOT_DIR/test/cool-examples"
TEST_EXE="$ROOT_DIR/scripts/test_cool.sh"

for t in "$TEST_CASES"/*.cl; do 
  "$TEST_EXE" -c "$t"
done


