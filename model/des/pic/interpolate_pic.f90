!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv!
!  Subroutine: INTERPOLATE_PIC_1                                       !
!  Author: J.Musser                                                    !
!  Review: M.Clarke                                                    !
!                                                                      !
!  Purpose: Manages solids pressure and solids velocity through        !
!           appropriate filter weights                                 !
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^!
      SUBROUTINE INTERPOLATE_PIC

      use discretelement, only: MAX_PIP, PIJK
! Flag to use interpolation
      use particle_filter, only: DES_INTERP_ON
! Interpolation cells and weights
      use particle_filter, only: FILTER_SIZE
      use particle_filter, only: FILTER_CELL, FILTER_WEIGHT

      use mfix_pic, only: EPG_P
      use fldvar, only: EP_G

      use functions, only: FLUID_AT
      use functions, only: IS_NONEXISTENT

      implicit none

! Local Variables
!---------------------------------------------------------------------//
! temporary variables used to calculate pressure at scalar cell edge
      INTEGER :: IJK, NP, LC
!......................................................................!

      DO NP=1,MAX_PIP

! Skip parcels that don't exist or are in a non-fluid cell.
         IF(IS_NONEXISTENT(NP) .OR. .NOT.FLUID_AT(PIJK(NP,4))) THEN
            EPG_P(NP) = 1.0d0

! Interpolated solids pressure and average solids velocity.
         ELSEIF(DES_INTERP_ON) THEN
            EPG_P(NP) = 0.0d0
            DO LC=1,FILTER_SIZE
               IJK = FILTER_CELL(LC,NP)
               EPG_P(NP) = EPG_P(NP) + EP_G(IJK)*FILTER_WEIGHT(LC,NP)
            ENDDO

! Centroid method.
         ELSE
            EPG_P(NP) = EP_G(PIJK(NP,4))
         ENDIF
      ENDDO

      RETURN
    END SUBROUTINE INTERPOLATE_PIC


!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv!
!                                                                      !
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^!
SUBROUTINE CALC_PIC_FIELDS

!-----------------------------------------------
! Modules
!-----------------------------------------------
  USE param, only: dimension_3
  USE param1, only: zero, one
  USE fldvar, only: rop_s, ro_s, ep_g, ro_g, rop_g
  USE geometry
  USE indices
  USE compar
  USE parallel
  USE sendrecv
  USE discretelement
  use desgrid
  use desmpi
  USE mfix_pic
  USE functions
  use param1, only: undefined
  ! packing limit
  use constant, only: ep_star

  use bilinear

  use sendrecvnode, only: DES_COLLECT_gDATA

  IMPLICIT NONE
!-----------------------------------------------
! Local variables
!-----------------------------------------------
! Loop counters: particles, filter cells, phases
  INTEGER NP
! Fluid cell index
  INTEGER IJK, lIJK
! Total Mth solids phase volume in IJK
  DOUBLE PRECISION :: sVOL(DIMENSION_3)
! PVOL times statistical weight, and times filter weight
  DOUBLE PRECISION :: lVOL, lMASS, WT, lVOLxWT
  INTEGER :: I, J, K, lI, lJ, lK, IC, JC, KC, Kub

  DOUBLE PRECISION :: Sx(0:1),  Sy(0:1),  Sz(0:1)
  DOUBLE PRECISION :: SxU(0:1), SyV(0:1), SzW(0:1)
  DOUBLE PRECISION :: lOoDX,    lOoDY,    lOoDZ

  double precision :: maxEPs, EPs
  double precision :: mloss, mrcvd, mtot
!-----------------------------------------------

  sVOL = ZERO
  RO_s = ZERO

  Kub = merge(1,0,DO_K)

  mtot = 0.0d0

  DO NP=1,MAX_PIP

     ! Skip parcels that don't exist.
     IF(IS_NORMAL(NP)) then

        call bilinear_x(NP, lI, Sx, SxU, lOoDX)
        call bilinear_y(NP, lJ, Sy, SyV, lOoDY)
        call bilinear_z(NP, lK, Sz, SzW, lOoDZ)

        ! Actual parcel volume and mass
        lvol =  pvol(np)*des_stat_wt(np)
        lmass = pmass(np)*des_stat_wt(np)

        DO KC=0,Kub
           DO JC=0,1
              DO IC=0,1

                 i = li+ic
                 j = lj+jc
                 k = lk+kc

                 if(is_on_mype_plus2layers(i,j,k)) then

                    ijk = funijk_map_c(i,j,k)

                    wt = sx(ic)*sy(jc)*sz(kc)

                    lvolxwt = lvol*wt

                    svol(ijk) = svol(ijk) + lvolxwt
                    ro_s(ijk,1) = ro_s(ijk,1) + lmass*wt

                    mtot = mtot + lvolxwt

                 endif
              enddo
           enddo
        enddo
     endif
  enddo

  mloss = 0.0d0 ! mass lost
  mrcvd = 0.0d0 ! mass recovered

  DO IJK = IJKSTART3, IJKEND3
     IF(.NOT.FLUID_AT(IJK) .and. sVOL(IJK) > 0.0d0 ) THEN
        mloss = mloss + svol(IJK)

        IF(FLUID_AT(IM_OF(IJK))) THEN
           lIJK = IM_OF(IJK)

           mrcvd = mrcvd + svol(IJK)
           sVOL(lIJK)   = sVOL(lIJK)   + sVOL(IJK);    sVOL(IJK)   = 0.0d0
           RO_s(lIJK,1) = RO_s(lIJK,1) + RO_s(IJK,1);  RO_s(IJK,1) = 0.0d0

        ELSEIF(FLUID_AT(IP_OF(IJK))) THEN
           lIJK = IP_OF(IJK)

           mrcvd = mrcvd + svol(IJK)
           sVOL(lIJK)   = sVOL(lIJK)   + sVOL(IJK);    sVOL(IJK)   = 0.0d0
           RO_s(lIJK,1) = RO_s(lIJK,1) + RO_s(IJK,1);  RO_s(IJK,1) = 0.0d0

        ELSEIF(FLUID_AT(JM_OF(IJK))) THEN
           lIJK = JM_OF(IJK)

           mrcvd = mrcvd + svol(IJK)
           sVOL(lIJK)   = sVOL(lIJK)   + sVOL(IJK);    sVOL(IJK)   = 0.0d0
           RO_s(lIJK,1) = RO_s(lIJK,1) + RO_s(IJK,1);  RO_s(IJK,1) = 0.0d0

        ELSEIF(FLUID_AT(JP_OF(IJK))) THEN
           lIJK = JP_OF(IJK)

           mrcvd = mrcvd + svol(IJK)
           sVOL(lIJK)   = sVOL(lIJK)   + sVOL(IJK);    sVOL(IJK)   = 0.0d0
           RO_s(lIJK,1) = RO_s(lIJK,1) + RO_s(IJK,1);  RO_s(IJK,1) = 0.0d0

        ELSEIF(DO_K .and. FLUID_AT(KM_OF(IJK))) THEN
           lIJK = KM_OF(IJK)

           mrcvd = mrcvd + svol(IJK)
           sVOL(lIJK)   = sVOL(lIJK)   + sVOL(IJK);    sVOL(IJK)   = 0.0d0
           RO_s(lIJK,1) = RO_s(lIJK,1) + RO_s(IJK,1);  RO_s(IJK,1) = 0.0d0

        ELSEIF(DO_K .and. FLUID_AT(KP_OF(IJK))) THEN
           lIJK = KP_OF(IJK)

           mrcvd = mrcvd + svol(IJK)
           sVOL(lIJK)   = sVOL(lIJK)   + sVOL(IJK);    sVOL(IJK)   = 0.0d0
           RO_s(lIJK,1) = RO_s(lIJK,1) + RO_s(IJK,1);  RO_s(IJK,1) = 0.0d0

        ELSEIF(FLUID_AT(IM_OF(JM_OF(IJK)))) THEN
           lIJK = IM_OF(JM_OF(IJK))

           mrcvd = mrcvd + svol(IJK)
           sVOL(lIJK)   = sVOL(lIJK)   + sVOL(IJK);    sVOL(IJK)   = 0.0d0
           RO_s(lIJK,1) = RO_s(lIJK,1) + RO_s(IJK,1);  RO_s(IJK,1) = 0.0d0

        ELSEIF(FLUID_AT(IM_OF(JP_OF(IJK)))) THEN
           lIJK = IM_OF(JP_OF(IJK))

           mrcvd = mrcvd + svol(IJK)
           sVOL(lIJK)   = sVOL(lIJK)   + sVOL(IJK);    sVOL(IJK)   = 0.0d0
           RO_s(lIJK,1) = RO_s(lIJK,1) + RO_s(IJK,1);  RO_s(IJK,1) = 0.0d0

        ELSEIF(FLUID_AT(IP_OF(JM_OF(IJK)))) THEN
           lIJK = IP_OF(JM_OF(IJK))

           mrcvd = mrcvd + svol(IJK)
           sVOL(lIJK)   = sVOL(lIJK)   + sVOL(IJK);    sVOL(IJK)   = 0.0d0
           RO_s(lIJK,1) = RO_s(lIJK,1) + RO_s(IJK,1);  RO_s(IJK,1) = 0.0d0

        ELSEIF(FLUID_AT(IP_OF(JP_OF(IJK)))) THEN
           lIJK = IP_OF(JP_OF(IJK))

           mrcvd = mrcvd + svol(IJK)
           sVOL(lIJK)   = sVOL(lIJK)   + sVOL(IJK);    sVOL(IJK)   = 0.0d0
           RO_s(lIJK,1) = RO_s(lIJK,1) + RO_s(IJK,1);  RO_s(IJK,1) = 0.0d0

        ELSEIF(DO_K .and. FLUID_AT(KM_OF(IM_OF(IJK)))) THEN
           lIJK = KM_OF(IM_OF(IJK))

           mrcvd = mrcvd + svol(IJK)
           sVOL(lIJK)   = sVOL(lIJK)   + sVOL(IJK);    sVOL(IJK)   = 0.0d0
           RO_s(lIJK,1) = RO_s(lIJK,1) + RO_s(IJK,1);  RO_s(IJK,1) = 0.0d0

        ELSEIF(DO_K .and. FLUID_AT(KM_OF(IP_OF(IJK)))) THEN
           lIJK = KM_OF(IP_OF(IJK))

           mrcvd = mrcvd + svol(IJK)
           sVOL(lIJK)   = sVOL(lIJK)   + sVOL(IJK);    sVOL(IJK)   = 0.0d0
           RO_s(lIJK,1) = RO_s(lIJK,1) + RO_s(IJK,1);  RO_s(IJK,1) = 0.0d0

        ELSEIF(DO_K .and. FLUID_AT(KM_OF(JM_OF(IJK)))) THEN
           lIJK = KM_OF(JM_OF(IJK))

           mrcvd = mrcvd + svol(IJK)
           sVOL(lIJK)   = sVOL(lIJK)   + sVOL(IJK);    sVOL(IJK)   = 0.0d0
           RO_s(lIJK,1) = RO_s(lIJK,1) + RO_s(IJK,1);  RO_s(IJK,1) = 0.0d0

        ELSEIF(DO_K .and. FLUID_AT(KM_OF(JP_OF(IJK)))) THEN
           lIJK = KM_OF(JP_OF(IJK))

           mrcvd = mrcvd + svol(IJK)
           sVOL(lIJK)   = sVOL(lIJK)   + sVOL(IJK);    sVOL(IJK)   = 0.0d0
           RO_s(lIJK,1) = RO_s(lIJK,1) + RO_s(IJK,1);  RO_s(IJK,1) = 0.0d0

        ELSEIF(DO_K .and. FLUID_AT(KP_OF(IM_OF(IJK)))) THEN
           lIJK = KP_OF(IM_OF(IJK))

           mrcvd = mrcvd + svol(IJK)
           sVOL(lIJK)   = sVOL(lIJK)   + sVOL(IJK);    sVOL(IJK)   = 0.0d0
           RO_s(lIJK,1) = RO_s(lIJK,1) + RO_s(IJK,1);  RO_s(IJK,1) = 0.0d0

        ELSEIF(DO_K .and. FLUID_AT(KP_OF(IP_OF(IJK)))) THEN
           lIJK = KP_OF(IP_OF(IJK))

           mrcvd = mrcvd + svol(IJK)
           sVOL(lIJK)   = sVOL(lIJK)   + sVOL(IJK);    sVOL(IJK)   = 0.0d0
           RO_s(lIJK,1) = RO_s(lIJK,1) + RO_s(IJK,1);  RO_s(IJK,1) = 0.0d0

        ELSEIF(DO_K .and. FLUID_AT(KP_OF(JM_OF(IJK)))) THEN
           lIJK = KP_OF(JM_OF(IJK))

           mrcvd = mrcvd + svol(IJK)
           sVOL(lIJK)   = sVOL(lIJK)   + sVOL(IJK);    sVOL(IJK)   = 0.0d0
           RO_s(lIJK,1) = RO_s(lIJK,1) + RO_s(IJK,1);  RO_s(IJK,1) = 0.0d0

        ELSEIF(DO_K .and. FLUID_AT(KP_OF(JP_OF(IJK)))) THEN
           lIJK = KP_OF(JP_OF(IJK))

           mrcvd = mrcvd + svol(IJK)
           sVOL(lIJK)   = sVOL(lIJK)   + sVOL(IJK);    sVOL(IJK)   = 0.0d0
           RO_s(lIJK,1) = RO_s(lIJK,1) + RO_s(IJK,1);  RO_s(IJK,1) = 0.0d0

        ELSEIF(DO_K .and. FLUID_AT(KM_OF(IM_OF(JM_OF(IJK))))) THEN
           lIJK = KM_OF(IM_OF(JM_OF(IJK)))

           mrcvd = mrcvd + svol(IJK)
           sVOL(lIJK)   = sVOL(lIJK)   + sVOL(IJK);    sVOL(IJK)   = 0.0d0
           RO_s(lIJK,1) = RO_s(lIJK,1) + RO_s(IJK,1);  RO_s(IJK,1) = 0.0d0

        ELSEIF(DO_K .and. FLUID_AT(KM_OF(IM_OF(JP_OF(IJK))))) THEN
           lIJK = KM_OF(IM_OF(JP_OF(IJK)))

           mrcvd = mrcvd + svol(IJK)
           sVOL(lIJK)   = sVOL(lIJK)   + sVOL(IJK);    sVOL(IJK)   = 0.0d0
           RO_s(lIJK,1) = RO_s(lIJK,1) + RO_s(IJK,1);  RO_s(IJK,1) = 0.0d0

        ELSEIF(DO_K .and. FLUID_AT(KM_OF(IP_OF(JM_OF(IJK))))) THEN
           lIJK = KM_OF(IP_OF(JM_OF(IJK)))

           mrcvd = mrcvd + svol(IJK)
           sVOL(lIJK)   = sVOL(lIJK)   + sVOL(IJK);    sVOL(IJK)   = 0.0d0
           RO_s(lIJK,1) = RO_s(lIJK,1) + RO_s(IJK,1);  RO_s(IJK,1) = 0.0d0

        ELSEIF(DO_K .and. FLUID_AT(KM_OF(IP_OF(JP_OF(IJK))))) THEN
           lIJK = KM_OF(IP_OF(JP_OF(IJK)))

           mrcvd = mrcvd + svol(IJK)
           sVOL(lIJK)   = sVOL(lIJK)   + sVOL(IJK);    sVOL(IJK)   = 0.0d0
           RO_s(lIJK,1) = RO_s(lIJK,1) + RO_s(IJK,1);  RO_s(IJK,1) = 0.0d0

        ELSEIF(DO_K .and. FLUID_AT(KP_OF(IM_OF(JM_OF(IJK))))) THEN
           lIJK = KP_OF(IM_OF(JM_OF(IJK)))

           mrcvd = mrcvd + svol(IJK)
           sVOL(lIJK)   = sVOL(lIJK)   + sVOL(IJK);    sVOL(IJK)   = 0.0d0
           RO_s(lIJK,1) = RO_s(lIJK,1) + RO_s(IJK,1);  RO_s(IJK,1) = 0.0d0

        ELSEIF(DO_K .and. FLUID_AT(KP_OF(IM_OF(JP_OF(IJK))))) THEN
           lIJK = KP_OF(IM_OF(JP_OF(IJK)))

           mrcvd = mrcvd + svol(IJK)
           sVOL(lIJK)   = sVOL(lIJK)   + sVOL(IJK);    sVOL(IJK)   = 0.0d0
           RO_s(lIJK,1) = RO_s(lIJK,1) + RO_s(IJK,1);  RO_s(IJK,1) = 0.0d0

        ELSEIF(DO_K .and. FLUID_AT(KP_OF(IP_OF(JM_OF(IJK))))) THEN
           lIJK = KP_OF(IP_OF(JM_OF(IJK)))

           mrcvd = mrcvd + svol(IJK)
           sVOL(lIJK)   = sVOL(lIJK)   + sVOL(IJK);    sVOL(IJK)   = 0.0d0
           RO_s(lIJK,1) = RO_s(lIJK,1) + RO_s(IJK,1);  RO_s(IJK,1) = 0.0d0

        ELSEIF(DO_K .and. FLUID_AT(KP_OF(IP_OF(JP_OF(IJK))))) THEN
           lIJK = KP_OF(IP_OF(JP_OF(IJK)))

           mrcvd = mrcvd + svol(IJK)
           sVOL(lIJK)   = sVOL(lIJK)   + sVOL(IJK);    sVOL(IJK)   = 0.0d0
           RO_s(lIJK,1) = RO_s(lIJK,1) + RO_s(IJK,1);  RO_s(IJK,1) = 0.0d0

        ENDIF
     ENDIF
  ENDDO

! Sum data interpolted into ghost cells into physical cells
!---------------------------------------------------------------------//
  CALL DES_COLLECT_gDATA(sVOL)
  CALL DES_COLLECT_gDATA(RO_s(:,1))


! Calculate the bulk density, average solids density and void fraction
!---------------------------------------------------------------------//

  maxEPs = min(0.995d0, 1.15d0*(ONE - EP_STAR))

  DO IJK = IJKSTART3, IJKEND3
     IF(FLUID_AT(IJK)) THEN

        IF(sVOL(IJK) > epsilon(0.0d0) .and. VOL(IJK) > epsilon(0.0d0)) THEN

           EPs = min(sVOL(IJK) / VOL(IJK), maxEPs)
           RO_s(IJK,1)  = RO_s(IJK,1) / sVOL(IJK)
           ROP_S(IJK,1) = RO_S(IJK,1)*EPs
           EP_g(IJK)  = 1.0d0 - EPs
           ROP_g(IJK)  = EP_G(IJK) * RO_G(IJK)

        ELSE
           RO_s(IJK,1)  = UNDEFINED
           ROP_s(IJK,1) = 0.0d0
           EP_g(IJK)    = 1.0d0
           ROP_g(IJK)  = EP_G(IJK) * RO_G(IJK)

        ENDIF

     ENDIF
  ENDDO

  ! Halo exchange of volume fraction data. Note that the send/recv on EPg
  ! can be avoided if we do the work over ROP_s and RO_s, call send/recv
  ! then back out EPg.
  call SEND_RECV(ROP_S(:,1),2)
  call SEND_RECV(RO_S(:,1), 2)
  call SEND_RECV(EP_g, 2)
  call SEND_RECV(ROP_g, 2)

end SUBROUTINE CALC_PIC_FIELDS
