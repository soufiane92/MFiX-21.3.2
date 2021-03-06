! -*- f90 -*-
!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvC
!                                                                      C
!   Module name: DERIVED_TYPES                                         C
!   Purpose: contains derived type definitions and enum definitions    C
!                                                                      C
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^C

MODULE DERIVED_TYPES
  use param, only: dim_m

!-----------------------------------------------
! Modules
!-----------------------------------------------
  IMPLICIT NONE
!-----------------------------------------------

! Dynamic information related to computational (eulerian) fluid grid
!----------------------------------------------------------------->>>
! Dynamic variable. for each ijk computational fluid cell store the
! total number of particles and the id's of the particles in that cell
  TYPE iap1
     INTEGER, DIMENSION(:), POINTER:: p
  END TYPE iap1

  ! in order to facilitate the parallel processing the PIC is defined
  ! as single array IJK
  TYPE(iap1), DIMENSION(:), ALLOCATABLE:: pic  ! (DIMENSION_3)

! particle in cell related variable
  type iap2
     integer :: isize
     integer, dimension(:), pointer:: p
  end type iap2

  type(iap2), dimension(:),allocatable:: dg_pic

! Drag model options (see drag_gs for full details)
! default is syam_obrien (may enforce a corrected Umf by defining
! drag_c1 and drag_d1 accordingly)
  CHARACTER(64) :: DRAG_TYPE
  INTEGER :: DRAG_TYPE_ENUM

  ENUM, BIND(C)
     ENUMERATOR :: SYAM_OBRIEN=0
     ENUMERATOR :: GIDASPOW=1
     ENUMERATOR :: GIDASPOW_PCF=2
     ENUMERATOR :: GIDASPOW_BLEND=3
     ENUMERATOR :: GIDASPOW_BLEND_PCF=4
     ENUMERATOR :: WEN_YU=5
     ENUMERATOR :: WEN_YU_PCF=6
     ENUMERATOR :: KOCH_HILL=7
     ENUMERATOR :: KOCH_HILL_PCF=8
     ENUMERATOR :: BVK=9
     ENUMERATOR :: TPKKV=10
     ENUMERATOR :: HYS=11
     ENUMERATOR :: GAO=12
     ENUMERATOR :: SARKAR=13
     ENUMERATOR :: RADL=14
     ENUMERATOR :: TGS=15
     ENUMERATOR :: DIFELICE=16
     ENUMERATOR :: DIFELICE_GANSER=17
     ENUMERATOR :: USER_DRAG=18

  END ENUM

! filtered/subgrid corrections to the drag coefficient & granular
! stress terms including granular viscosity and solids pressure
! current options are 'igci' and 'milioli'
  CHARACTER(64) :: SUBGRID_TYPE

  INTEGER :: SUBGRID_TYPE_ENUM
  ENUM, BIND(C)
     ENUMERATOR :: UNDEFINED_SUBGRID_TYPE=0
     ENUMERATOR :: IGCI=1
     ENUMERATOR :: MILIOLI=2
  END ENUM

! Kinetic theory model options (see calc_mu_s for details)
! for m > 1 : IA_nonep, GHD, LUN_1984
! for m = 1 : LUN_1984, simonin, ahmadi, or
!             GD_99 for granular flow or GTSH for gas-solids flow
  CHARACTER(64) :: KT_TYPE
  INTEGER :: KT_TYPE_ENUM
  ENUM, BIND(C)
     ENUMERATOR :: ALGEBRAIC=0
     ENUMERATOR :: LUN_1984=1
     ENUMERATOR :: SIMONIN_1996=2
     ENUMERATOR :: AHMADI_1995=3
     ENUMERATOR :: GD_1999=4
     ENUMERATOR :: GTSH_2012=5
     ENUMERATOR :: IA_2005=6
     ENUMERATOR :: GHD_2007=7
  END ENUM


! Radial Distribution Function options (see g_0 for details).
! for m = 1 options are: carnahan_starling, ma_ahmadi
! for m > 1 options are: lebowitz, modified_lebowitz,
!                        mansoori, modified_mansoori.
  CHARACTER(64) :: RDF_TYPE
  INTEGER :: RDF_TYPE_ENUM
  ENUM, BIND(C)
     ENUMERATOR :: CARNAHAN_STARLING=1
     ENUMERATOR :: MA_AHMADI=2
     ENUMERATOR :: LEBOWITZ=3
     ENUMERATOR :: MODIFIED_LEBOWITZ=4
     ENUMERATOR :: MANSOORI=5
     ENUMERATOR :: MODIFIED_MANSOORI=6
  END ENUM


! filtered/subgrid corrections to the drag coefficient & granular
! stress terms including granular viscosity and solids pressure
! current options are 'igci' and 'milioli'
  INTEGER :: TURBULENCE_MODEL_ENUM
  ENUM, BIND(C)
     ENUMERATOR :: NO_TURBULENCE_ENUM=0
     ENUMERATOR :: MIXING_LENGTH_ENUM=1
     ENUMERATOR :: K_EPSILON_ENUM=2
  END ENUM

! Solids conductivity model options (see calc_k_s/des_conduction for details)
! Solids conductivity model
  CHARACTER(len=64), DIMENSION(DIM_M) :: KS_MODEL
  INTEGER, DIMENSION(DIM_M) :: KS_MODEL_ENUM
  ENUM, BIND(C)
     ENUMERATOR :: KS_NONE=0
     ENUMERATOR :: KS_CONSTEFF=1
     ENUMERATOR :: KS_BAUER=2
     ENUMERATOR :: KS_USR=3
     ENUMERATOR :: KS_MUSSER=4
  END ENUM


 END MODULE DERIVED_TYPES
