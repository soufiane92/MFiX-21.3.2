# Default values for MFIX keys which are REQUIRED.
# User is not allowed to un-set these, so we need some defaults
#
# Note, these are here instead of constants.py since the constants defined there are internal to the GUI.

# generic values: (Per Jordan Musser 2017-08-14)

# gas density = 1.0 kg/m3
# solids density = 1000.0 kg/m3
# solids diameter = 100.0e-6 m
# temperature = 300. K

from mfixgui.project import FloatExp

ro_g0 = 1.2005
mu_g0 = FloatExp('1.8e-05')
mu_gmax = FloatExp('1.0e+03')
mw_avg = 29.0
t = 293.15
c_pg0 = 1005.0
k_g0 = 0.0257
dif_g0 = FloatExp('1.0e-05')
d_p0 = 0.001
ro_s0 = 1000.0
mu_s0 = FloatExp('1.0e-05')
c_ps0 = 830.0
k_g0 = 2.0
des_em = 0.0
phi = 30.0

# constant_mod.f:         IF (GRAVITY == UNDEFINED) GRAVITY = 9.80665D0 ! m/s2
gravity = -9.80665
