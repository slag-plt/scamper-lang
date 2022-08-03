#!/bin/bash

npm run clean
npm run build

RED="\033[0;31m"
GREEN="\033[32m"
CLEAR="\033[0m"

TESTS=$(ls tests/*.scm)

total=0
failures=0

echo ${TESTS}
for f in $TESTS
do
  test=${f%.scm}
  echo -n "Running test: ${test}... "
  ((total += 1))
  DIFF=$(npm run --silent driver output ${test}.scm | diff ${test}.expected -)
  if [ "$DIFF" != "" ]
  then
    echo -e "${RED}failed!${CLEAR}"
    echo ${DIFF}
    ((failures += 1))
  else
    echo -e "${GREEN}success!${CLEAR}"
  fi
done

echo ""
if [ $failures -eq 0 ]
then
  echo -e "${GREEN}All tests passed!${CLEAR}"
else
  echo -e "${RED}${failures} failure(s)${CLEAR} out of ${total} tests"
fi
