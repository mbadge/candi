#!/usr/bin/env bash
set -euo pipefail
IFS=$'\n\t'

source ${HOME}/.aliases  # To use my custom grepR fxn


# FLAGS ----
HEAD_DIR=${1:-"/media/marcus/Projects/candi"}
OLD_NAME=imgIds2Cases
NEW_NAME=id_2Case
# ----


# FS Mgmt
START_DIR=${PWD}
clean_up() {
  ARG=$?
  cd ${START_DIR}
  exit $ARG
}
trap clean_up EXIT
cd ${HEAD_DIR}


# Main ----
echo -e "\n\n------------------------ Covered Change of Exported Package Function Names --------------------\n"


echo finding matches to ${OLD_NAME}...

TARGET_FILES=$( grep -rl --include=\*.R --regex ${OLD_NAME} )
echo -e "\ntarget files:\n${TARGET_FILES}"

echo -e "\ntarget lines:"
grep -rn --include=\*.R  ${OLD_NAME}



echo -e "\n\nChanging Values to ${NEW_NAME}..."
echo test run:
sed -n --regex "s/${OLD_NAME}/${NEW_NAME}/gp" ${TARGET_FILES}

echo real run:
sed -i --regex "s/${OLD_NAME}/${NEW_NAME}/g" ${TARGET_FILES}

exit 0

