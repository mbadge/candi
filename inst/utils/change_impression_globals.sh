#!/usr/bin/env bash
set -euo pipefail
IFS=$'\n\t'

source ${HOME}/.aliases  # To use my custom grepR fxn

HEAD_DIR=${1:-"/media/marcus/Projects/candi/inst/examples"}

START_DIR=${PWD}
clean_up() {
  ARG=$?
  cd ${START_DIR}
  exit $ARG
}
trap clean_up EXIT
cd ${HEAD_DIR}


echo -e "\n\n---------------------------------------- Checking Config ----------------------------------------\n"
echo all hits in example apps:
grepR kINCLUDE_DEMOGRAPHICS

echo -e "\nsettings hits:"
grep -rn --include=\*.R --regex ^kINCLUDE_


TARGET_FILES=$( grep -rl --include=\*.R --regex ^kINCLUDE_ )
echo -e "\ntarget files:\n${TARGET_FILES}"


echo -e "\n\n---------------------------------------- Changing Values ----------------------------------------\n"
echo test run:
sed -n --regex 's/^(kINCLUDE_[A-Z]+\s<-\s)TRUE/\1FALSE/p' ${TARGET_FILES}

echo real run:
sed -i --regex 's/^(kINCLUDE_[A-Z]+\s<-\s)TRUE/\1FALSE/' ${TARGET_FILES}

exit 0

