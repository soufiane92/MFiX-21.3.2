#!/bin/bash -elx

MFIXSOLVER=./mfixsolver
if (($# > 0)); then
  MFIXSOLVER=$1
fi

rm -f de_norms* MMS03.* tmp.dat

# Run mesh_8 (i.e., 8x8 for 2D, 8x8x8 for 3D)
cat base-mfix.dat mesh_8.dat >mfix.dat
${MFIXSOLVER} -f mfix.dat imax=8 jmax=8 kmax=8
cat de_norms.dat >>de_norms_collected.dat

# Evaluate observed orders
gfortran -o ooa_test ooa_test.f95
./ooa_test

numdiff -a 0.000001 -r 0.05 AUTOTEST/de_l2.dat de_l2.dat ||
  echo "Post de_l2 results differ"

numdiff -a 0.000001 -r 0.05 AUTOTEST/de_linf.dat de_linf.dat ||
  echo "Post de_linf results differ"
