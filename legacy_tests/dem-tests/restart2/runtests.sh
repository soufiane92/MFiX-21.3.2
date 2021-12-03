#!/bin/bash -lex

# EXEEXT should be ".exe" on Windows; empty string otherwise
EXEEXT=${EXEEXT:-}

MFIXSOLVER=./mfixsolver${EXEEXT}
if (($# > 0)); then
  MFIXSOLVER=$1
fi

rm -f BACKGROUND_IC* CUT_CELL.LOG LOOP_SEAL* SLICE* VTU_FRAME_INDEX.TXT
time -p ${MFIXSOLVER} -f loop_seal_dem_pseudo_2d.mfx TSTOP=0.00033 RUN_TYPE=\'NEW\'
rm -f LOOP_SEAL_DEM_PSEUDO_2D.SP? *.pvd *.vtp *.vtu
time -p ${MFIXSOLVER} -f loop_seal_dem_pseudo_2d.mfx TSTOP=0.001 RUN_TYPE=\'RESTART_2\'
