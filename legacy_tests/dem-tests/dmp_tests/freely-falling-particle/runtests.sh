#!/bin/bash -ex

set -euo pipefail

# MPIRANK only defined for DMP tests
MPIRANK=${MPIRANK:-}

# Some tests (tutorials) override default TSTOP
TSTOP=${TSTOP:-}

# EXEEXT should be ".exe" on Windows; empty string otherwise
EXEEXT=${EXEEXT:-}

MFIXSOLVER=./mfixsolver${EXEEXT}
if (($# > 0)); then
  MFIXSOLVER="$1"
fi

POSTMFIX=./postmfix${EXEEXT}
if (($# > 1)); then
  POSTMFIX="$2"
fi

if [ -f mfix.dat ]; then
  MFXS[0]=mfix.dat
else
  MFXS=(*.mfx)
fi

SOLVER_COMMAND=()

if [ -n "${MPIRANK}" ]; then
    SOLVER_COMMAND+=("mpirun" "--oversubscribe" "-np" "${MPIRANK}")
fi

SOLVER_COMMAND+=("${MFIXSOLVER}" "-f" "${MFXS[0]}")

if [[ ${TSTOP} ]]; then
    SOLVER_COMMAND+=("TSTOP=${TSTOP}")
fi

# Run the solver...
${SOLVER_COMMAND[*]}

# Check the results...
post_script=AUTOTEST/post.script.NEW
if [ -e ${post_script} ]; then
  env OMP_NUM_THREADS=1 "${POSTMFIX}" <${post_script}
fi

shopt -s nullglob
post_dats=(AUTOTEST/POST*.dat)

if [[ ${post_dats:-} ]]; then
    for test_post_file in "${post_dats[@]:0}"; do
        numdiff -a 0.000001 -r 0.05 "${test_post_file}" "$(basename "${test_post_file}")"
    done
fi
