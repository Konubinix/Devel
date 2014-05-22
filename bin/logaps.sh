#!/bin/bash -x

tmp_log=`mktemp`
cd "${KONIX_PERSO_DIRS}"
mkdir -p "${KONIX_PERSO_DIR}/data/gaps_logs/"
LOG="${KONIX_PERSO_DIR}/data/gaps_logs/gaps_log-$(hostname)_$(date "+%y%m%d_%H%M%S").txt"
echo "# ######################################################################
# Fixing Remotes
# ######################################################################"
mr -s fix
echo "# ######################################################################
# Gapsing Remotes
# ######################################################################"
mr -s gaps -d "$@" 2>&1 | tee "${tmp_log}"
RES=${PIPESTATUS[0]}
mv "${tmp_log}" "${LOG}"
exit $RES
