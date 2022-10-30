#!/bin/bash

npm run clean
npm run build

RED="\033[0;31m"
GREEN="\033[32m"
CLEAR="\033[0m"

LEXING_TESTS=$(ls tests/lexing/*.scm)
RUNTIME_TESTS=$(find tests/runtime -name "*.scm")
PRELUDE_TESTS=$(find tests/prelude -name "*.scm")

if [[ $# -eq 1 ]] && [[ $1 == 'stepper' ]]
then
  echo 'Evaluating tests with stepper'
  SCAMPER="dist/driver/index.js --use-stepper"
else
  SCAMPER="dist/driver/index.js"
fi

total=0
failures=0

for f in $LEXING_TESTS
do
  test=${f%.scm}
  echo -n "Running lexing test: ${test}... "
  ((total += 1))
  DIFF=$($SCAMPER -c --emit-tokens ${test}.scm | diff ${test}.expected -)
  if [ "$DIFF" != "" ]
  then
    echo -e "${RED}failed!${CLEAR}"
    echo -e "${DIFF}"
    ((failures += 1))
  else
    echo -e "${GREEN}success!${CLEAR}"
  fi
done

for f in $RUNTIME_TESTS
do
  test=${f%.scm}
  echo -n "Running runtime test: ${test}... "
  ((total += 1))
  DIFF=$($SCAMPER ${test}.scm | diff ${test}.expected -)
  if [ "$DIFF" != "" ]
  then
    echo -e "${RED}failed!${CLEAR}"
    echo -e "${DIFF}"
    ((failures += 1))
  else
    echo -e "${GREEN}success!${CLEAR}"
  fi
done

for f in $PRELUDE_TESTS
do
  test=${f%.scm}
  echo -n "Running prelude test: ${test}... "
  ((total += 1))
  DIFF=$($SCAMPER ${test}.scm | diff ${test}.expected -)
  if [ "$DIFF" != "" ]
  then
    echo -e "${RED}failed!${CLEAR}"
    echo -e "${DIFF}"
    ((failures += 1))
  else
    echo -e "${GREEN}success!${CLEAR}"
  fi
done

echo ""
if [ $failures -eq 0 ]
then
  echo -e "${GREEN}All ${total} tests passed!${CLEAR}"
  exit 0
else
  echo -e "${RED}${failures} failure(s)${CLEAR} out of ${total} tests"
  exit 1
fi
