#include "error.inc"

!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv!
!                                                                      !
!  MODULE: COEFF                                                       !
!  Purpose: Contains logic flags that tells the code whether to        !
!           perform the indicated type of calculation when the         !
!           value is true                                              !
!                                                                      !
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^!
MODULE coeff

   USE error_manager

! Flags for calculating drag coefficient.
   use exchange_mod, only: DRAGCOEF
! Flags for calculating heat transfer coefficient.
   use exchange_mod, only: HEAT_TR

! Flags for calculating viscosity.
   use transport_prop_mod, only: VISC
! Flags for calculating conductivity.
   use transport_prop_mod, only: COND
! Flags for calculating diffusivity.
   use transport_prop_mod, only: DIFF
! Flags for calculating particle-particle energy dissipation.
   use transport_prop_mod, only: GRAN_DISS

! Flags used by PHYSICAL_PROP :: (0:DIMENSION_M)
!```````````````````````````````````````````````````````````````````````
   LOGICAL, ALLOCATABLE :: DENSITY(:)  ! Density
   LOGICAL, ALLOCATABLE :: SP_HEAT(:)  ! Specific heat
   LOGICAL, ALLOCATABLE :: PSIZE(:)    ! Particle diameter

contains

!**********************************************************************!
!  SUBROUTINE: DEBUG_COEFF                                             !
!                                                                      !
!  Purpose: Dump the coefficient arrays for debugging.                 !
!                                                                      !
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^!
      SUBROUTINE DEBUG_COEFF

      use compar
      use physprop, only: MMAX

      implicit none

      INTEGER :: M, MM

      if(myPE /= PE_IO) return

      write(*,"(/3x,'From DEBUG_COEFF:')")

      write(*,"(/3x,'Gas phase coefficients:')")
      write(*,"( 5x,'Density (RO_g):',1x,1L1)") DENSITY(0)
      write(*,"( 5x,'Specific heat (C_pg):',1x,1L1)") SP_HEAT(0)
      write(*,"( 5x,'Viscosity: (MU_g)',1x,1L1)") VISC(0)
      write(*,"( 5x,'Thermal conductivity (K_g):',1x,1L1)") COND(0)
      write(*,"( 5x,'Species diffusivity: (DIF_G)',1x,1L1)") DIFF(0)


      DO M=1, MMAX
         write(*,"(/3x,'Solids ',I1,' phase coefficients:')") M
         write(*,"( 5x,'Density: (RO_s)',1x,1L1)") DENSITY(M)
         write(*,"( 5x,'Specific heat (C_ps):',1x,1L1)") SP_HEAT(M)
         write(*,"( 5x,'Viscosity (MU_s):',1x,1L1)") VISC(M)
         write(*,"( 5x,'Thermal conductivity (K_s):',1x,1L1)") COND(M)
         write(*,"( 5x,'Species diffusivity (DIF_s):',1x,1L1)") DIFF(M)
         write(*,"( 5x,'Gran. Dissipation (D_p):',1x,1L1)") GRAN_DISS(M)
         write(*,"( 5x,'Diameter (D_p):',1x,1L1)") PSIZE(M)
      ENDDO


      write(*,"(/3x,'Interphase drag:')")
      write(*,"( 5x,'ref')",ADVANCE="NO")
      DO M=0, MMAX
         write(*,"(2x,I3)",ADVANCE="NO")M
      ENDDO
      write(*,"('')")

      DO M=0, MMAX
         write(*,"( 5x,I3)",ADVANCE="NO") M
         DO MM=0, MMAX
            write(*,"(2x,L3)",ADVANCE="NO")DRAGCOEF(M, MM)
         ENDDO
         write(*,"('')")
      ENDDO

      write(*,"(/3x,'Interphase heat transfer:')")
      write(*,"( 5x,'ref')",ADVANCE="NO")
      DO M=0, MMAX
         write(*,"(2x,I3)",ADVANCE="NO")M
      ENDDO
      write(*,"('')")
      DO M=0, MMAX
         write(*,"( 5x,I3)",ADVANCE="NO") M
         DO MM=0, MMAX
            write(*,"(2x,L3)",ADVANCE="NO")HEAT_TR(M, MM)
         ENDDO
         write(*,"('')")
      ENDDO

      write(*,"(/3x,'DEBUG_COEFF - Exit',3/)")

   END SUBROUTINE DEBUG_COEFF

END MODULE coeff
