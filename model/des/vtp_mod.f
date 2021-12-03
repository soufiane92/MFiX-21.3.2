#include "version.inc"
#include "error.inc"

MODULE vtp

   USE cdist, only: bdist_io
   USE compar, only: myPE, pe_io, numpes, mpierr
   USE des_rxns, only: des_x_s
   USE des_thermo, only: DES_T_s
   USE desmpi, only: dprocbuf, drootbuf, iprocbuf, irootbuf, igath_sendcnt, igathercnts, idispls
   use des_rxns, only: NO_OF_DES_RXNS
   USE discretelement, only: DES_VEL_NEW, IGHOST_CNT, ORIENTATION, DES_USR_VAR_SIZE, DES_USR_VAR
   USE discretelement, only: MAX_PIP, PIP, DES_POS_NEW, PIJK, VTP_DIR, USE_COHESION, PARTICLE_ORIENTATION
   USE discretelement, only: POSTCOHESIVE, S_TIME, IGLOBAL_ID, DES_RADIUS, VTP_FINDEX, OMEGA_NEW, RO_SOL
   USE discretelement, only: CGDEM, DES_CGP_RPR, DES_CGP_STW
   USE discretelement, only: FCHAINC,FCHAIN_MIDPOINT,FCHAIN_FN,FCHAIN_LENGTH,FCHAIN_FN_MAG,VTP_LUB,VTP_P_DATA
   USE discretelement, only: RESIDENCE_TIME
   USE error_manager, only: err_msg, loglevel_error, log_message, loglevel_status, ival, ivar
   USE fs_util, only: create_dir
   USE functions, only: is_normal, is_nonexistent, is_entering_ghost, is_exiting_ghost, is_ghost
   USE funits, only: unit_vtp, unit_pvd, unit_pvtp, dmp_log, unit_log
   USE mfix_pic, only: MPPIC, des_stat_wt
   USE mpi_comm_des, only: desmpi_gatherv, des_gather
   USE mpi_utility, only: allgather_1i, global_sum, global_all_sum, global_all_max, bcast
   USE output, only: FULL_LOG
   USE param, only: dim_i, dim_j, dim_k, dimension_n_s
   USE param1, only: ZERO
   USE parse, only: des_rxn_name
   USE pvd_mod, only: read_pvd_frames, open_pvd_file, update_and_close_pvd_file, write_pvd_frames
   USE run, only: RUN_TYPE, RUN_NAME, TIME, ENERGY_EQ, ANY_SPECIES_EQ
   USE rxns, only: SPECIES_ALIAS_s
   USE vtk, only: BELONGS_TO_VTK_SUBDOMAIN
   USE vtk, only: BUFFER, VTU_OFFSET, VTU_FILENAME,VTP_FILENAME
   USE vtk, only: DIMENSION_VTK, FRAME, VTK_REGION, VTK_DEFINED
   USE vtk, only: END_REC, BELONGS_TO_VTK_SUBDOMAIN
   USE vtk, only: NUMBER_OF_POINTS, BUFFER, END_REC, VTU_OFFSET, BELONGS_TO_VTK_SUBDOMAIN
   USE vtk, only: RESET_FRAME_AT_TIME_ZERO, PVTU_FILENAME, BUFFER, END_REC
   USE vtk, only: TIME_DEPENDENT_FILENAME, STRIP, ADD_VTU_DIRECTORY, FIND_VTK_BASENAME
   USE vtk, only: VTK_DBG_FILE, VTU_DIR
   USE vtk, only: VTK_DEFINED, VTK_DATA, FRAME
   USE vtk, only: VTK_FILEBASE
   USE vtk, only: VTK_NXS, VTK_NYS, VTK_NZS
   USE vtk, only: VTK_PART_ANGULAR_VEL, VTK_PART_ORIENTATION
   USE vtk, only: VTK_PART_COHESION, VTK_PART_RANK, VTK_PART_ID
   USE vtk, only: VTK_PART_DIAMETER, VTK_PART_VEL, VTK_PART_USR_VAR, VTK_PART_TEMP
   USE vtk, only: VTK_PART_PHYSICAL_DIAMETER, VTK_PART_CGP_STAT_WT
   USE vtk, only: VTK_PART_PHASE_ID, VTK_PART_X_S
   USE vtk, only: VTK_PHASE_FOR_DES_X_S, VTK_PART_DENSITY
   USE vtk, only: VTK_SLICE_TOL, VTK_SELECT_MODE, VTK_PART_PHASE
   USE vtk, only: VTK_PART_RESIDENCE_TIME
   USE vtk, only: VTK_X_E, VTK_X_W, VTK_Y_S, VTK_Y_N, VTK_Z_B, VTK_Z_T
   USE VTK, ONLY: SAVE_PART_RRATES, PART_RRATES_OUT, VTK_PART_RRATE
   USE, intrinsic :: iso_c_binding

#ifdef MPI
   USE mpi, only: mpi_comm_world
#endif

   IMPLICIT NONE

   INTEGER, PRIVATE :: GLOBAL_CNT
   INTEGER, PRIVATE :: LOCAL_CNT

   INTEGER :: DES_UNIT = 2000

! formatted file name
   CHARACTER(LEN=511) :: FNAME_VTP

   INTERFACE VTP_WRITE_DATA
      MODULE PROCEDURE VTP_WRITE_DP1
      MODULE PROCEDURE VTP_WRITE_DP2
      MODULE PROCEDURE VTP_WRITE_I1
   END INTERFACE VTP_WRITE_DATA

CONTAINS

!``````````````````````````````````````````````````````````````````````!
! Subroutine: VTP_WRITE_DP1                                            !
!                                                                      !
! Purpose: Collect and write 1D double precision arrays to the VTP     !
! file. This routine is designed to collect the data for parallel and  !
! serial runs. This routine also manages the distribted IO case.       !
!``````````````````````````````````````````````````````````````````````!
   SUBROUTINE VTP_WRITE_DP1(NAME, DATA)

      CHARACTER(len=*), INTENT(in) :: NAME
      DOUBLE PRECISION, INTENT(in) :: DATA(:)

      INTEGER :: LC, PC

      IF(bDist_IO) THEN

         WRITE(DES_UNIT,1000) NAME

         PC = 1
         DO LC = 1, MAX_PIP
            IF(PC > PIP) EXIT
            IF(IS_NONEXISTENT(LC)) CYCLE
            PC = PC+1
            IF(IS_GHOST(LC) .OR. IS_ENTERING_GHOST(LC) .OR. IS_EXITING_GHOST(LC)) CYCLE
            WRITE(DES_UNIT, 1001,ADVANCE="NO") real(DATA(LC))
         ENDDO
         WRITE(DES_UNIT,1002)

      ELSE

         allocate (dProcBuf(LOCAL_CNT) )
         allocate (dRootBuf(GLOBAL_CNT))

         CALL DES_GATHER(DATA)

         IF(myPE == PE_IO) THEN
            WRITE(DES_UNIT,1000) NAME
            DO LC=1, GLOBAL_CNT
               WRITE(DES_UNIT,1001,ADVANCE="NO") real(drootbuf(LC))
            ENDDO
            WRITE(DES_UNIT,1002)
         ENDIF

         deallocate(dProcBuf, dRootBuf)

      ENDIF

 1000 FORMAT('<DataArray type="Float32" Name="',A,'" format="ascii">')
 1001 FORMAT(ES14.6,1X)
 1002 FORMAT('</DataArray>')

      END SUBROUTINE VTP_WRITE_DP1

!``````````````````````````````````````````````````````````````````````!
! Subroutine: VTP_WRITE_DP2                                            !
!                                                                      !
! Purpose: Collect and write 2D double precision arrays to the VTP     !
! file. This routine is designed to collect the data for parallel and  !
! serial runs. This routine also manages the distribted IO case.       !
!``````````````````````````````````````````````````````````````````````!
      SUBROUTINE VTP_WRITE_DP2(NAME, DATA)

      CHARACTER(len=*), INTENT(in) :: NAME
      DOUBLE PRECISION, INTENT(in) :: DATA(:,:)

      DOUBLE PRECISION, ALLOCATABLE :: ltemp_array(:,:)

      CHARACTER(len=16) :: NOC
      INTEGER :: LB, UB
      INTEGER :: PC, LC1, LC2

      LB = LBOUND(DATA,2)
      UB = UBOUND(DATA,2)
      NOC=''; WRITE(NOC,*) (UB-LB)+1

      IF(bDist_IO) THEN

         WRITE(DES_UNIT,1000) NAME, trim(adjustl(NOC))

         PC = 1
         DO LC1 = 1, MAX_PIP
            IF(PC > PIP) EXIT
            IF(IS_NONEXISTENT(LC1)) CYCLE
            PC = PC+1
            IF(IS_GHOST(LC1) .OR. IS_ENTERING_GHOST(LC1) .OR. IS_EXITING_GHOST(LC1)) CYCLE
            DO LC2=LB, UB
               WRITE(DES_UNIT,1001,ADVANCE="NO") real(DATA(LC1,LC2))
            ENDDO
         ENDDO
         WRITE(DES_UNIT,1002)

      ELSE

         allocate (dProcBuf(LOCAL_CNT) )
         allocate (dRootBuf(GLOBAL_CNT))
         allocate (ltemp_array((UB-LB)+1,GLOBAL_CNT))

         DO LC1 = LB, UB
            CALL DES_GATHER(DATA(:,LC1))
            ltemp_array(LC1,:) = drootbuf(:)
         ENDDO

         IF(myPE == PE_IO) THEN
            WRITE(DES_UNIT,1000) NAME, trim(adjustl(NOC))
            DO LC1=1, GLOBAL_CNT
               DO LC2=LB, UB
                  WRITE(DES_UNIT,1001,ADVANCE="NO") &
                     real(ltemp_array(LC2,LC1))
               ENDDO
            ENDDO
            WRITE(DES_UNIT,1002)
         ENDIF

         deallocate (dProcBuf, dRootBuf, ltemp_array)

      ENDIF


 1000 FORMAT('<DataArray type="Float32" Name="',A,'" NumberOf',        &
         'Components="',A,'" format="ascii">')
 1001 FORMAT(ES14.6,1X)
 1002 FORMAT('</DataArray>')

      END SUBROUTINE VTP_WRITE_DP2



!``````````````````````````````````````````````````````````````````````!
! Subroutine: VTP_WRITE_I1                                             !
!                                                                      !
! Purpose: Collect and write 1D integer arrays to the VTP file. This   !
! routine is designed to collect the data for parallel and serial      !
! runs. This routine also manages the distribted IO case.              !
!``````````````````````````````````````````````````````````````````````!
      SUBROUTINE VTP_WRITE_I1(NAME, DATA)

      CHARACTER(len=*), INTENT(in) :: NAME
      INTEGER, INTENT(in) :: DATA(:)

      INTEGER :: LC, PC

      IF(bDist_IO) THEN

         WRITE(DES_UNIT,1000) NAME

         PC = 1
         DO LC = 1, MAX_PIP
            IF(PC > PIP) EXIT
            IF(IS_NONEXISTENT(LC)) CYCLE
            PC = PC+1
            IF(IS_GHOST(LC) .OR. IS_ENTERING_GHOST(LC) .OR. IS_EXITING_GHOST(LC)) CYCLE
            WRITE(DES_UNIT, 1001,ADVANCE="NO") DATA(LC)
         ENDDO
         WRITE(DES_UNIT,1002)

      ELSE

         allocate (iProcBuf(LOCAL_CNT) )
         allocate (iRootBuf(GLOBAL_CNT))

         CALL DES_GATHER(DATA)

         IF(myPE == PE_IO) THEN
            WRITE(DES_UNIT,1000) NAME
            DO LC=1, GLOBAL_CNT
               WRITE(DES_UNIT,1001,ADVANCE="NO") irootbuf(LC)
            ENDDO
            WRITE(DES_UNIT,1002)
         ENDIF

         deallocate(iProcBuf, iRootBuf)

      ENDIF

 1000 FORMAT('<DataArray type="Float32" Name="',A,'" format="ascii">')
 1001 FORMAT(I10,1X)
 1002 FORMAT('</DataArray>')

      END SUBROUTINE VTP_WRITE_I1


!``````````````````````````````````````````````````````````````````````!
! Subroutine: VTP_WRITE_ELEMENT                                        !
!                                                                      !
! Purpose: Write a string to the VTP file. It masks the need to check  !
! the logical before flushing.                                         !
!``````````````````````````````````````````````````````````````````````!
      SUBROUTINE VTP_WRITE_ELEMENT(ELEMENT)

      CHARACTER(len=*), INTENT(in) :: ELEMENT

      IF(bDist_IO .OR. myPE == PE_IO) &
         WRITE(DES_UNIT,"(A)") ELEMENT

      RETURN
      END SUBROUTINE VTP_WRITE_ELEMENT


!``````````````````````````````````````````````````````````````````````!
! Subroutine: VTP_OPEN_FILE                                            !
!                                                                      !
! Purpose: This routine opens the VTP file and calculates the offsets  !
! for dmp data collection.                                             !
!``````````````````````````````````````````````````````````````````````!
      SUBROUTINE VTP_OPEN_FILE(NoPc)

      IMPLICIT NONE

      CHARACTER(len=*) :: NoPc

      INTEGER :: NumberOfPoints

! Variables related to gather
      integer lgathercnts(0:numpes-1), lproc

! check whether an error occurs in opening a file
      INTEGER :: IOS
! Integer error flag.
      INTEGER :: IER

! logical used for testing is the data file already exists
      LOGICAL :: EXISTS_VTP
! status of the vtp file to be written
      CHARACTER(LEN=8) :: STATUS_VTP

      IF(TRIM(VTP_DIR)/='.'.AND.TRIM(VTP_DIR)/='' .AND. myPE == PE_IO) THEN
         CALL CREATE_DIR(trim(VTP_DIR))
      END IF

! Initial the global count.
      GLOBAL_CNT = 10
! Calculate the number of 'real' particles on the local process.
      LOCAL_CNT = PIP - iGHOST_CNT

! Distributed IO
      IF(bDIST_IO) THEN
         NumberOfPoints = LOCAL_CNT
         WRITE(NoPc,"(I10.10)") NumberOfPoints

         IF(TRIM(VTP_DIR)/='.'.AND.TRIM(VTP_DIR)/='') THEN
            WRITE(fname_vtp,'(A,"/",A,"_DES",I4.4,"_",I5.5,".vtp")') &
               trim(VTP_DIR), trim(run_name), vtp_findex, mype
         ELSE
            WRITE(fname_vtp,'(A,"_DES",I4.4,"_",I5.5,".vtp")') &
               trim(run_name), vtp_findex, mype
         ENDIF

! Serial IO
      ELSE

! Calculate the total number of particles system-wide.
         call global_sum(LOCAL_CNT, GLOBAL_CNT)
         NumberOfPoints = GLOBAL_CNT
         WRITE(NoPc,"(I10.10)") NumberOfPoints

! Set the send count from the local process.
         igath_sendcnt = LOCAL_CNT

! Collect the number of particles on each rank.all ranks.
         lgathercnts = 0
         lgathercnts(myPE) = LOCAL_CNT
         call global_sum(lgathercnts,igathercnts)

! Calculate the rank displacements.
         idispls(0) = 0
         DO lPROC = 1,NUMPEs-1
            idispls(lproc) = idispls(lproc-1) + igathercnts(lproc-1)
         ENDDO

! set the file name and unit number and open file
         IF(TRIM(VTP_DIR)/='.'.AND.TRIM(VTP_DIR)/='') THEN
            WRITE(fname_vtp,'(A,"/",A,"_DES_",I5.5,".vtp")') &
               trim(VTP_DIR),trim(run_name), vtp_findex
         ELSE
            WRITE(fname_vtp,'(A,"_DES_",I5.5,".vtp")') &
               trim(run_name), vtp_findex
         ENDIF
      ENDIF

      IER = 0
      IF(bDIST_IO .OR. myPE == PE_IO) THEN

! The file should be new but could exist due to restarting.
         STATUS_VTP = 'NEW'
! Check to see if the file already exists.
         INQUIRE(FILE=FNAME_VTP,EXIST=EXISTS_VTP)
! The given file should not exist if the run type is NEW.
         IF(EXISTS_VTP)THEN
! The VTP should never exist for a NEW run.
            IF(RUN_TYPE == 'NEW')THEN
               IER = 1
! The file may exist during a RESTART.
            ELSE
               STATUS_VTP = 'REPLACE'
            ENDIF
         ENDIF

! Open the file and record any errors.
         IF(IER == 0) THEN
            OPEN(UNIT=DES_UNIT, FILE=FNAME_VTP,   &
               STATUS=STATUS_VTP, IOSTAT=IOS)
            IF(IOS /= 0) IER = 2
         ENDIF
      ENDIF

      CALL GLOBAL_ALL_MAX(IER)

      IF(IER /= 0) THEN
         WRITE(ERR_MSG, 1100) IER
         CALL LOG_ERROR()
      ENDIF

 1100 FORMAT('Error 1100: Unable to open VTP file. This could be ',    &
         'caused by a VTP',/'file with the same file name already ',   &
         'existing. or an error code',/' returned by the OPEN ',       &
         'function.'/'Error code: ',I2,4x,'Aborting.')

      END SUBROUTINE VTP_OPEN_FILE

!......................................................................!
! SUBROUTINE: VTP_CLOSE_FILE                                           !
!                                                                      !
! Purpose: This routine closes the vtp file.                           !
!``````````````````````````````````````````````````````````````````````!
      SUBROUTINE VTP_CLOSE_FILE

      VTP_FINDEX=VTP_FINDEX+1

      IF(bDist_io .OR. (myPE .eq.pe_IO)) CLOSE(des_unit)

      END SUBROUTINE VTP_CLOSE_FILE


!......................................................................!
! SUBROUTINE: ADD_VTP_TO_PVD                                           !
!                                                                      !
! Purpose: This routine opens the pvd file.                            !
!``````````````````````````````````````````````````````````````````````!
      SUBROUTINE ADD_VTP_TO_PVD

!-----------------------------------------------
! Local Variables
!-----------------------------------------------
! Index position of desired character
      INTEGER IDX_f, IDX_b
! logical used for testing is the data file already exists
      LOGICAL :: EXISTS_PVD
! Generic input limited to 256 characters
      CHARACTER(LEN=256) INPUT

! formatted file name
      CHARACTER(LEN=64) :: FNAME_PVD = ''
! formatted time
      CHARACTER(LEN=64) :: cTIME = ''

      LOGICAL, SAVE :: FIRST_PASS = .TRUE.

! IO Status flag
      INTEGER :: IOS

! Variables related to gather
      integer :: IER

!-----------------------------------------------

! Initialize the error flag.
      IER = 0

! Obtain the file name and open the pvd file
      FNAME_PVD = TRIM(RUN_NAME)//'_DES.pvd'

! The PVD file is only written by PE_IO with serial IO.
      IF(myPE == PE_IO .AND. .NOT.bDist_IO) THEN

! Check to see if the file already exists.
         INQUIRE(FILE=FNAME_PVD,EXIST=EXISTS_PVD)

         IF(FIRST_PASS) THEN

! Open the "NEW" file and write the necessary header information.
            IF(RUN_TYPE /= 'RESTART_1')THEN

! The file exists but first_pass is also true so most likely an existing
! file from an earlier/other run is present in the directory. Exit to
! prevent accidentally overwriting the existing file.
               IF(EXISTS_PVD) THEN
                  IER = 1
               ELSE
                  OPEN(UNIT=UNIT_PVD,FILE=FNAME_PVD,STATUS='NEW')
                  WRITE(UNIT_PVD,"(A)")'<?xml version="1.0"?>'
                  WRITE(UNIT_PVD,"(A)")'<VTKFile type="Collection" &
                     &version="0.1" byte_order="BigEndian">'
                  WRITE(UNIT_PVD,"(3X,'<Collection>')")
               ENDIF

! This is the first pass of a restart run. Extra care is needed to make
! sure that the pvd file is ready to accept new data.
            ELSE ! a restart run
               IF(EXISTS_PVD) THEN
! Open the file at the beginning.
                  OPEN(UNIT=UNIT_PVD,FILE=FNAME_PVD,&
                     POSITION="REWIND",STATUS='OLD',IOSTAT=IOS)
                  IF(IOS /= 0) IER = 2
               ELSE ! a pvd file does not exist
                  IER = 3
               ENDIF

               IF(IER == 0) THEN
! Loop over the entries in the PVD file, looking for a match to the
! file that is being written. If no match is found, the data will be
! appended to the end of the pvd file, otherwise, the old data will
! be over-written.
                  DO
! Read in the entries of the PVD file.
                     READ(UNIT_PVD,"(A)",IOSTAT=IOS)INPUT
                     IF(IOS > 0) THEN
                        IER = 4
                        EXIT
                     ELSEIF(IOS<0)THEN
! The end of the pvd file has been reached without finding an entry
! matching the current record. Exit the loop.
                        BACKSPACE(UNIT_PVD)
                        BACKSPACE(UNIT_PVD)
                        BACKSPACE(UNIT_PVD)
                        EXIT
                     ENDIF
! Find the first instances of file=" and "/> in the read data.
                     IDX_f = INDEX(INPUT,'file="')
                     IDX_b = INDEX(INPUT,'"/>')
! Skip rows that do not contain file data
                     IF(IDX_f == 0 .AND. IDX_b == 0) CYCLE
! Truncate the file name from the read data
                     WRITE (INPUT,"(A)") INPUT(IDX_f+6:IDX_b-1)
! If the file name matches the current VTP record, break the loop to
! over-write this record.
                     IF(TRIM(FNAME_VTP) == TRIM(INPUT)) THEN
                        BACKSPACE(UNIT_PVD)
                        EXIT
                     ENDIF
                  ENDDO
               ENDIF ! No errors
            ENDIF ! run_type new or restart

         ELSE ! not FIRST_PASS
            OPEN(UNIT=UNIT_PVD,FILE=FNAME_PVD,&
               POSITION="APPEND",STATUS='OLD',IOSTAT=IOS)
            IF (IOS /= 0) IER = 2
         ENDIF

      ENDIF ! if myPE == PE_IO and not distributed IO


      CAlL GLOBAL_ALL_SUM(IER)
      IF(IER /= 0) THEN
         SELECT CASE(IER)
         CASE(1); WRITE(ERR_MSG,1101) trim(FNAME_PVD)
         CASE(2); WRITE(ERR_MSG,1102) trim(FNAME_PVD)
         CASE(3); WRITE(ERR_MSG,1103) trim(FNAME_PVD)
         CASE(4); WRITE(ERR_MSG,1104) trim(FNAME_PVD)
         CASE DEFAULT; WRITE(ERR_MSG,1105) trim(FNAME_PVD)
         END SELECT
         CALL LOG_ERROR()
      ENDIF

 1101 FORMAT('Error 1101: A PVD file was detected in the run ',        &
         'directory which should',/'not exist for a NEW run.',/        &
         'File: ',A)

 1102 FORMAT('Error 1102: Fatal error status returned while OPENING ', &
         'PVD file.',/'File: ', A)

 1103 FORMAT('Error 1103: PVD file MISSING from run directory.',/      &
         'File: ',A)

 1104 FORMAT('Error 1104: Fatal error status returned while READING ', &
         'PVD file.',/'File: ', A)

 1105 FORMAT('Error 1105:: Fatal unclassified error when processing ', &
         'PVD file.',/'File: ', A)


! If there were no errors, updated the file.
      IF(myPE == PE_IO .AND. .NOT.bDist_IO) THEN

! Remove the last two lines written so that additional data can be added
         IF(.NOT.FIRST_PASS) THEN
            BACKSPACE(UNIT_PVD)
            BACKSPACE(UNIT_PVD)
         ENDIF

         WRITE(cTIME,"(F12.6)") S_TIME
! Write the data to the file
         WRITE(UNIT_PVD,"(6X,A,A,A,A,A,A,A)")&
         '<DataSet timestep="',trim(adjustl(cTIME)),'" ',&
         'group="" part="0" ',& ! necessary file data
         'file="',TRIM(FNAME_VTP),'"/>' ! file name of vtp

! Write the closing tags
         WRITE(UNIT_PVD,"(3X,A)")'</Collection>'
         WRITE(UNIT_PVD,"(A)")'</VTKFile>'

         CLOSE(UNIT_PVD)
      ENDIF
! Identify that the files has been created and opened for next pass
      FIRST_PASS = .FALSE.

! Return to the calling routine
      RETURN

      END SUBROUTINE ADD_VTP_TO_PVD


!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvC
!                                                                      C
!  Module name: WRITE_VTP_FILE                                         C
!  Purpose: Writes particles data in VTK format (Polydata VTP)         C
!           Binary format                                              C
!                                                                      C
!  Author: Jeff Dietiker                              Date: 11-Feb-15  C
!  Reviewer:                                          Date:            C
!                                                                      C
!  Revision Number #                                  Date: ##-###-##  C
!  Author: #                                                           C
!  Purpose: #                                                          C
!                                                                      C
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^C
      SUBROUTINE WRITE_VTP_FILE(LCV,MODE)

      IMPLICIT NONE
      INTEGER :: M,N,LCV
      INTEGER(c_int64_t) :: L

      INTEGER :: PASS
      INTEGER :: WRITE_HEADER = 1
      INTEGER :: WRITE_DATA   = 2
      INTEGER :: MODE   ! MODE = 0 : Write regular VTK region file
                        ! MODE = 1 : Write debug   VTK region file (VTK_DBG_FILE = .TRUE.)

      DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE :: PARTICLE_RANK
      DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE :: PARTICLE_PHASE_ID
      DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE :: FCHAIN_FN_MAG_ND
      DOUBLE PRECISION :: SUM_FN,GLOBAL_SUM_FN,AVG_FN
      INTEGER :: GLOBAL_FCHAINC


      DOUBLE PRECISION, PARAMETER  :: THIRD = (1.0d0/3.0d0)

      VTK_REGION = LCV
! There is nothing to write if we are not in a defined vtk region
      IF(.NOT.VTK_DEFINED(VTK_REGION)) RETURN

      SELECT CASE (VTK_DATA(VTK_REGION))

      CASE('P') ! Regular Particle data
         VTP_LUB = MAX_PIP
         VTP_P_DATA = .TRUE.
      CASE('F') ! Force chain data
         VTP_LUB = FCHAINC
         VTP_P_DATA = .FALSE.
      CASE DEFAULT
         RETURN
         print*,'should not be here (vtk_data)',VTK_REGION,VTK_DATA(VTK_REGION)
      END SELECT


! Regular particle data      
      IF(VTK_DATA(VTK_REGION)/='P'.AND.VTK_DATA(VTK_REGION)/='F') RETURN
      IF(MODE==0.AND.(VTK_DBG_FILE(VTK_REGION))) RETURN
      IF(MODE==1.AND.(.NOT.VTK_DBG_FILE(VTK_REGION))) RETURN

      CALL SETUP_VTK_REGION_PARTICLES

      CALL OPEN_VTP_FILE_BIN(MODE)

! Only open pvd file when there are particles in vtk region
      IF(GLOBAL_CNT>0.AND.MODE==0) CALL OPEN_PVD_FILE

! First pass write the data header.
! Second pass writes the data (appended binary format).

      DO PASS=WRITE_HEADER,WRITE_DATA


         CALL WRITE_GEOMETRY_IN_VTP_BIN(PASS)

         IF(VTP_P_DATA) THEN ! Regular particle data

            IF(VTK_PART_DIAMETER(VTK_REGION).AND.ALLOCATED(DES_RADIUS)) THEN
               IF(MPPIC.AND.ALLOCATED(DES_STAT_WT)) THEN
                  CALL WRITE_SCALAR_IN_VTP_BIN('Diameter',2.0D0*DES_RADIUS*DES_STAT_WT**(THIRD),PASS)
               ELSE
                  CALL WRITE_SCALAR_IN_VTP_BIN('Diameter',2.0D0*DES_RADIUS,PASS)
               ENDIF
            ENDIF

            IF(VTK_PART_VEL(VTK_REGION).AND.ALLOCATED(DES_VEL_NEW)) &
               CALL WRITE_VECTOR_IN_VTP_BIN('Velocity',DES_VEL_NEW,PASS)
       
            IF(CGDEM) THEN
               IF(VTK_PART_PHYSICAL_DIAMETER(VTK_REGION).AND.ALLOCATED(DES_CGP_RPR)) &
                  CALL WRITE_SCALAR_IN_VTP_BIN('Physical_Diameter',2.0D0*DES_CGP_RPR,PASS)
               IF(VTK_PART_CGP_STAT_WT(VTK_REGION).AND.ALLOCATED(DES_CGP_STW)) &
                  CALL WRITE_SCALAR_IN_VTP_BIN('Statistical_Weight',DES_CGP_STW,PASS)
            ENDIF

            IF(VTK_PART_ANGULAR_VEL(VTK_REGION).AND.ALLOCATED(OMEGA_NEW)) &
               CALL WRITE_VECTOR_IN_VTP_BIN('Angular_velocity', OMEGA_NEW,PASS)

            IF(PARTICLE_ORIENTATION) THEN
               IF(VTK_PART_ORIENTATION(VTK_REGION).AND.ALLOCATED(ORIENTATION)) &
                  CALL WRITE_VECTOR_IN_VTP_BIN('Orientation', ORIENTATION,PASS)
            ENDIF

            DO N=1, DES_USR_VAR_SIZE
               IF(VTK_PART_USR_VAR(VTK_REGION,N).AND.ALLOCATED(DES_USR_VAR)) &
                 CALL WRITE_SCALAR_IN_VTP_BIN('User Defined Var '//trim(iVal(N)),DES_USR_VAR(N,:),PASS)
            ENDDO

            IF(ENERGY_EQ.AND.VTK_PART_TEMP(VTK_REGION).AND.ALLOCATED(DES_T_s)) &
              CALL WRITE_SCALAR_IN_VTP_BIN('Temperature', DES_T_s,PASS)

            IF(ANY_SPECIES_EQ) THEN
               DO N=1, DIMENSION_N_S
                  IF(VTK_PART_X_s(VTK_REGION,N).AND.ALLOCATED(DES_X_s)) THEN
                     M= VTK_PHASE_FOR_DES_X_S(VTK_REGION)
                     IF(M/=0) THEN
                        CALL WRITE_SCALAR_IN_VTP_BIN(trim(SPECIES_ALIAS_s(M,N)), DES_X_s(:,N),PASS)
                     ELSE
                        CALL WRITE_SCALAR_IN_VTP_BIN(trim(iVar('X_s',N)), DES_X_s(:,N),PASS)
                     ENDIF
                  ENDIF
               ENDDO
            ENDIF

            IF(SAVE_PART_RRATES) THEN
               DO N=1, NO_OF_DES_RXNS
                  IF(VTK_PART_RRATE(VTK_REGION,N).AND.ALLOCATED(PART_RRATES_OUT)) THEN
                     CALL WRITE_SCALAR_IN_VTP_BIN('RRate_'//trim(DES_RXN_NAME(N)), PART_RRATES_OUT(:,N),PASS)
                  ENDIF
               ENDDO
            ENDIF

            IF(VTK_PART_DENSITY(VTK_REGION).AND.ALLOCATED(RO_SOL)) &
               CALL WRITE_SCALAR_IN_VTP_BIN('Density', RO_SOL,PASS)

            IF(USE_COHESION.AND.VTK_PART_COHESION(VTK_REGION).AND.ALLOCATED(PostCohesive)) &
               CALL WRITE_SCALAR_IN_VTP_BIN('CohesiveForce', PostCohesive,PASS)

            IF(VTK_PART_RANK(VTK_REGION)) THEN
               IF(PASS==WRITE_DATA) THEN
                  ALLOCATE(PARTICLE_RANK(MAX_PIP))
                  DO L = 1, MAX_PIP
                     PARTICLE_RANK(L) = DBLE(MyPE)
                  ENDDO
               ENDIF

               CALL WRITE_SCALAR_IN_VTP_BIN('Particle_Rank', PARTICLE_RANK,PASS)
               IF(PASS==WRITE_DATA) DEALLOCATE(PARTICLE_RANK)
            ENDIF

            IF(VTK_PART_PHASE_ID(VTK_REGION)) THEN
               IF(PASS==WRITE_DATA) THEN
                  ALLOCATE(PARTICLE_PHASE_ID(MAX_PIP))
                  DO L = 1, MAX_PIP
                     PARTICLE_PHASE_ID(L) = DBLE(PIJK(L,5))
                  ENDDO
               ENDIF

               CALL WRITE_SCALAR_IN_VTP_BIN('Particle_Phase_ID', PARTICLE_PHASE_ID,PASS)
               IF(PASS==WRITE_DATA) DEALLOCATE(PARTICLE_PHASE_ID)
            ENDIF

            IF(VTK_PART_ID(VTK_REGION).AND.ALLOCATED(iGLOBAL_ID)) &
               CALL WRITE_SCALAR_IN_VTP_BIN('Particle_ID', DBLE(iGLOBAL_ID),PASS)

            IF(VTK_PART_RESIDENCE_TIME(VTK_REGION).AND.ALLOCATED(RESIDENCE_TIME)) &
               CALL WRITE_SCALAR_IN_VTP_BIN('Residence_Time', RESIDENCE_TIME,PASS)


         ELSE ! Force chain data

            CALL WRITE_SCALAR_IN_VTP_BIN('FORCE_CHAIN_LENGTH',FCHAIN_LENGTH,PASS)
            CALL WRITE_SCALAR_IN_VTP_BIN('FORCE_CHAIN_FN_MAG',FCHAIN_FN_MAG,PASS)
            CALL WRITE_VECTOR_IN_VTP_BIN('FORCE_CHAIN_FN',FCHAIN_FN,PASS)
            ! Compute average force magnitude and use it to non-dimensionalize
            ! FCHAIN_FN_MAG
            IF(PASS==WRITE_DATA) THEN
               SUM_FN = ZERO
               DO L = 1, FCHAINC
                  SUM_FN = SUM_FN + FCHAIN_FN_MAG(L)
               ENDDO
               call global_all_sum(SUM_FN, GLOBAL_SUM_FN)
               call global_all_sum(FCHAINC, GLOBAL_FCHAINC)

               IF(GLOBAL_SUM_FN>0) THEN
                  AVG_FN = GLOBAL_SUM_FN/GLOBAL_FCHAINC
                  DO L = 1, FCHAINC
                     FCHAIN_FN_MAG(L) = FCHAIN_FN_MAG(L)/AVG_FN
                  ENDDO
               ENDIF

            ENDIF
            CALL WRITE_SCALAR_IN_VTP_BIN('FORCE_CHAIN_FN_ND',FCHAIN_FN_MAG,PASS)

         ENDIF

      ENDDO ! PASS LOOP, EITHER HEADER OR DATA


! Write connectivity and offset. This let Paraview define cells (one
! vertex per cell), and apply Clip and Threshold filters on it
      IF(GLOBAL_CNT>0.AND.MODE==0) THEN
         IF(myPE == PE_IO) THEN
! Write connectivity
            WRITE(UNIT_VTP)  GLOBAL_CNT*c_sizeof(L)
            DO L=0, GLOBAL_CNT-1
               WRITE(UNIT_VTP)  L
            ENDDO
! Write offset
            WRITE(UNIT_VTP)  GLOBAL_CNT*c_sizeof(L)
            DO L=0, GLOBAL_CNT-1
               WRITE(UNIT_VTP)  L+1
            ENDDO
         ENDIF
      ENDIF

      CALL CLOSE_VTP_FILE_BIN(MODE)

! Only update pvd file when there are particles in vtk region
      IF(GLOBAL_CNT>0.AND.MODE==0) CALL UPDATE_AND_CLOSE_PVD_FILE

#ifdef MPI
      call MPI_barrier(MPI_COMM_WORLD,mpierr)
#endif

! Update Frames
      IF (myPE == PE_IO.AND.TIME_DEPENDENT_FILENAME) THEN
         CALL WRITE_PVD_FRAMES
      ENDIF

     IF (FULL_LOG.AND.myPE == PE_IO) THEN
        IF(GLOBAL_CNT>0.AND.MODE==0) THEN
           WRITE(ERR_MSG,20)' DONE.'
        ELSE
           WRITE(ERR_MSG,20)' VTP file not written (zero particle in vtk region).'
        ENDIF
        CALL LOG_STATUS()
     ENDIF

20    FORMAT(A,1X/)
      RETURN

      END SUBROUTINE WRITE_VTP_FILE

      SUBROUTINE UPDATE_FRAMES
         IMPLICIT NONE
         INTEGER :: L

         IF (TIME_DEPENDENT_FILENAME) THEN
            IF(MYPE==PE_IO) THEN
               CALL READ_PVD_FRAMES
               IF (RESET_FRAME_AT_TIME_ZERO.AND.TIME==ZERO) THEN
                  DO L = 1, DIMENSION_VTK
                     IF(L==VTK_REGION) FRAME(L)=-1
                  ENDDO
               ENDIF
               DO L = 1, DIMENSION_VTK
                  IF(L==VTK_REGION) FRAME(L) = FRAME(L) + 1
               ENDDO
            ENDIF
! Broadcast only required for Distributed IO
            IF(BDIST_IO) CALL BCAST(FRAME(1:DIMENSION_VTK))
         ENDIF
      END SUBROUTINE UPDATE_FRAMES

!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvC
!                                                                      C
!  Module name: OPEN_VTP_FILE_BIN                                      C
!  Purpose: Open a vtp file and writes the header                      C
!           Binary format                                              C
!                                                                      C
!  Author: Jeff Dietiker                              Date: 11-Feb-15  C
!  Reviewer:                                          Date:            C
!                                                                      C
!  Revision Number #                                  Date: ##-###-##  C
!  Author: #                                                           C
!  Purpose: #                                                          C
!                                                                      C
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^C
  SUBROUTINE OPEN_VTP_FILE_BIN(MODE)

      IMPLICIT NONE
      LOGICAL :: NEED_TO_WRITE_VTP
      INTEGER :: ISTAT
      CHARACTER(256) :: io_message
      INTEGER :: MODE   ! MODE = 0 : Write regular VTK region file
                        ! MODE = 1 : Write debug   VTK region file (VTK_DBG_FILE = .TRUE.)

      IF(BDIST_IO) THEN
         NEED_TO_WRITE_VTP = (LOCAL_CNT>0)
      ELSE
         NEED_TO_WRITE_VTP = (MyPE==0.AND.GLOBAL_CNT>0)
      ENDIF

! Only open the file from head node when not using distributed I/O
      IF (myPE /= PE_IO.AND.(.NOT.BDIST_IO)) THEN
         RETURN
      END IF

      CALL UPDATE_FRAMES

      CALL OPEN_VTP

   CONTAINS

      SUBROUTINE OPEN_VTP


         IF(.NOT.BDIST_IO) THEN
            VTP_FILENAME = trim(FIND_VTK_BASENAME(MODE)) // ".vtp"
         ELSE
            VTP_FILENAME = trim(FIND_VTK_BASENAME(MODE,MyPE)) // ".vtp"
         ENDIF

         IF(TRIM(VTU_DIR)/='.'.AND.TRIM(VTU_DIR)/='' .AND. myPE == PE_IO) THEN
            CALL CREATE_DIR(trim(VTU_DIR))
         END IF

! Echo
      IF (FULL_LOG) THEN
         IF (.NOT.BDIST_IO) THEN
            WRITE(ERR_MSG,10)' WRITING VTP FILE : ', TRIM(VTP_FILENAME),' .'
            CALL LOG_STATUS()
         ELSE
            WRITE (ERR_MSG, 20) ' WRITING PVTP FILE : ', trim(FIND_VTK_BASENAME(MODE)), '.pvtp (EACH PROCESSOR IS WRITING ITS OWN VTP FILE)'
            CALL LOG_STATUS()
         ENDIF
      ENDIF

! Open File

      IF (NEED_TO_WRITE_VTP) THEN

         OPEN(UNIT     = UNIT_VTP,           &
              FILE     = TRIM(VTP_FILENAME), &
              FORM     = 'UNFORMATTED',      &
              ACCESS   = 'STREAM',           &
              ACTION   = 'WRITE', CONVERT  = 'BIG_ENDIAN', IOSTAT=ISTAT, IOMSG=io_message)

         IF (ISTAT /= 0) THEN
            WRITE(ERR_MSG, "(/1X,/, A, &
               /10X, 'Unable to write to filename:  ',A, &
               /10X, 'Verify that VTU_DIR exists: ', A, /1X)") io_message, VTU_FILENAME, VTU_DIR
            call log_error()
         ENDIF

! Write file Header
         BUFFER='<?xml version="1.0"?>'
         WRITE(UNIT_VTP)TRIM(BUFFER)//END_REC

         WRITE(BUFFER,*)'<!-- Time =',TIME,' sec. -->'
         WRITE(UNIT_VTP)TRIM(BUFFER)//END_REC

         BUFFER='<VTKFile type="PolyData" version="0.1" byte_order="BigEndian">'
         WRITE(UNIT_VTP)TRIM(BUFFER)//END_REC

         BUFFER='  <PolyData>'
         WRITE(UNIT_VTP)TRIM(BUFFER)//END_REC

      ENDIF
! For distributed I/O, open .p))vtp file that combines all *.vtp files for a given FRAME
! this is a simple ASCII file

      IF (myPE == PE_IO.AND.BDIST_IO.AND.GLOBAL_CNT>0) THEN

         PVTU_FILENAME = FIND_VTK_BASENAME(MODE) // ".pvtp"

         OPEN(UNIT = UNIT_PVTP, FILE = TRIM(PVTU_FILENAME))

         WRITE(UNIT_PVTP,105) '<?xml version="1.0"?>'
         WRITE(UNIT_PVTP,110) '<!-- Time =',TIME,' sec. -->'
         WRITE(UNIT_PVTP,120) '<VTKFile type="PPolyData"',&
                  ' version="0.1" byte_order="BigEndian">'

         WRITE(UNIT_PVTP,*) '  <PPolyData GhostLevel="0">'
         WRITE(UNIT_PVTP,*) '      <PPoints>'
         WRITE(UNIT_PVTP,*) '        <PDataArray type="Float32" Name="coordinates" NumberOfComponents="3" &
              &format="appended" offset=" 0" />'
         WRITE(UNIT_PVTP,*) '      </PPoints>'
         WRITE(UNIT_PVTP,*) ''
         WRITE(UNIT_PVTP,*) '      <PPointData Scalars="Diameter" Vectors="Velocity">'

      ENDIF

105   FORMAT(A)
110   FORMAT(A,E14.7,A)
120   FORMAT(A,A)
10    FORMAT(/1X,3A)
15    FORMAT(/1X,A)
20    FORMAT(/1X,3A)

      RETURN

   END SUBROUTINE OPEN_VTP

   END SUBROUTINE OPEN_VTP_FILE_BIN

!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvC
!                                                                      C
!  Module name: WRITE_GEOMETRY_IN_VTP_BIN                              C
!  Purpose: Write Geometry and connectivity in a vtu file              C
!           Binary format                                              C
!                                                                      C
!  Author: Jeff Dietiker                              Date: 21-Feb-08  C
!  Reviewer:                                          Date:            C
!                                                                      C
!  Revision Number #                                  Date: ##-###-##  C
!  Author: #                                                           C
!  Purpose: #                                                          C
!                                                                      C
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^C
  SUBROUTINE WRITE_GEOMETRY_IN_VTP_BIN(PASS)

      IMPLICIT NONE

      REAL(c_float) :: float
      INTEGER(c_int) :: int
      ! INTEGER(c_long) :: long

      INTEGER ::     nbytes_vector
      INTEGER ::     offset_xyz

      INTEGER :: PASS
      INTEGER :: WRITE_HEADER = 1
      INTEGER :: WRITE_DATA   = 2

      DOUBLE PRECISION, ALLOCATABLE :: ltemp_array(:,:)  ! local
      DOUBLE PRECISION, ALLOCATABLE :: gtemp_array(:,:)  ! global

      INTEGER :: LB, UB
      INTEGER :: PC, LC1, LC2

! Loop through all particles and kee a list of particles belonging to a VTK region

! Since the data is appended (i.e., written after all tags), the
! offset, in number of bytes must be specified.  The offset includes
! the size of the data for each field, plus the size of the integer
! that stores the number of bytes.  this is why the offset of a field
! equals the offset of the previous field plus sizeof(int) plus the
! number of bytes of the field.

! Next, the actual data is written for the geometry (PASS=WRITE_DATA)
! The DATA is converted to single precision to save memory.

      IF (.NOT.BDIST_IO) THEN
! The number of points in the pvd file is the global number of particles
! computed from SETUP_VTK_REGION_PARTICLES

         NUMBER_OF_POINTS = GLOBAL_CNT

! Number of bytes of position field (vector,3 components)
         nbytes_vector       = NUMBER_OF_POINTS * 3 * c_sizeof(float)

! Offset of each field
         offset_xyz = 0


         IF(PASS==WRITE_HEADER) THEN
            IF(myPE == PE_IO) THEN

               WRITE(BUFFER,*)'    <Piece NumberOfPoints="',NUMBER_OF_POINTS, &
! JFD: I am turning off the vertex data until a solution is found. The issue is c_sizeof(long) doesn't return the same
! value on Linux and Windows. That makes the vtp file unreadable by Paraview and the GUI.
!                     '"  NumberOfVerts="',NUMBER_OF_POINTS,'" NumberOfLines ="0" NumberOfStrips="0" NumberOfPolys="0" >'
                     '"  NumberOfVerts="', 0,'" NumberOfLines ="0" NumberOfStrips="0" NumberOfPolys="0" >'
               WRITE(UNIT_VTP)TRIM(BUFFER)//END_REC

               WRITE(BUFFER,*)'      <Points>'
               WRITE(UNIT_VTP)TRIM(BUFFER)//END_REC

               WRITE(BUFFER,*)'        <DataArray type="Float32" Name="coordinates" NumberOfComponents="3" &
                                       &format="appended" offset="',offset_xyz,'" />'
               WRITE(UNIT_VTP)TRIM(BUFFER)//END_REC

               WRITE(BUFFER,*)'      </Points>'
               WRITE(UNIT_VTP)TRIM(BUFFER)//END_REC

               WRITE(BUFFER,*)'<PointData Scalars="Diameter" Vectors="Velocity"> '!preparing pointData
               WRITE(UNIT_VTP)TRIM(BUFFER)//END_REC

! calculate offset for next field
               VTU_offset = offset_xyz + c_sizeof(int) + nbytes_vector

            ENDIF

         ELSEIF(PASS==WRITE_DATA) THEN

            IF(myPE == PE_IO) THEN

               WRITE(BUFFER,*)'      </PointData>'
               WRITE(UNIT_VTP)TRIM(BUFFER)//END_REC

! Vertex data. This allows Paraview to apply Clip and Threshold filters
               WRITE(BUFFER,*)'      <Verts> </Verts>'
! JFD: I am turning off the vertex data until a solution is found. The issue is c_sizeof(long) doesn't return the same
! value on Linux and Windows. That makes the vtp file unreadable by Paraview and the GUI.
!               WRITE(UNIT_VTP)TRIM(BUFFER)//END_REC
!               VTU_offset = VTU_offset - c_sizeof(int) + c_sizeof(long) ! Correction because last offset assumed int data
!               WRITE(BUFFER,*)'         <DataArray type="Int64" Name="connectivity" format="appended" RangeMin="" RangeMax="" offset="',VTU_offset,'" />'
!               WRITE(UNIT_VTP)TRIM(BUFFER)//END_REC
!               VTU_offset = VTU_offset + 1*c_sizeof(long)*NUMBER_OF_POINTS + 1*c_sizeof(long)
!               WRITE(BUFFER,*)'         <DataArray type="Int64" Name="offsets" format="appended" RangeMin="" RangeMax="" offset="', VTU_offset,'" />'
!               WRITE(UNIT_VTP)TRIM(BUFFER)//END_REC
!               WRITE(BUFFER,*)'      </Verts>'
               WRITE(UNIT_VTP)TRIM(BUFFER)//END_REC

               WRITE(BUFFER,*)'      <Lines> </Lines>'
               WRITE(UNIT_VTP)TRIM(BUFFER)//END_REC

               WRITE(BUFFER,*)'      <Strips> </Strips>'
               WRITE(UNIT_VTP)TRIM(BUFFER)//END_REC

               WRITE(BUFFER,*)'      <Polys> </Polys>'
               WRITE(UNIT_VTP)TRIM(BUFFER)//END_REC

               WRITE(BUFFER,*)'    </Piece>'
               WRITE(UNIT_VTP)TRIM(BUFFER)//END_REC

               WRITE(BUFFER,*)'  </PolyData>'
               WRITE(UNIT_VTP)TRIM(BUFFER)//END_REC

               WRITE(BUFFER,*)'  <AppendedData encoding="raw">'
               WRITE(UNIT_VTP)TRIM(BUFFER)//END_REC


! Starting raw binary data with an underscore

               WRITE(BUFFER,*)'_'
               WRITE(UNIT_VTP)TRIM(BUFFER)

! Number of bytes for X,Y,Z coordinates
            WRITE(UNIT_VTP) nbytes_vector


         ENDIF

         IF(VTP_P_DATA) THEN
            LB = LBOUND(DES_POS_NEW,2) ! This should always be 1
            UB = UBOUND(DES_POS_NEW,2) ! This should always be 2
         ELSE
            LB = LBOUND(FCHAIN_MIDPOINT,2) ! This should always be 1
            UB = UBOUND(FCHAIN_MIDPOINT,2) ! This should always be 2
         ENDIF

         ALLOCATE (dProcBuf(LOCAL_CNT) )
         ALLOCATE (ltemp_array((UB-LB)+1,LOCAL_CNT))


         IF(myPE == PE_IO) THEN
            ALLOCATE (dRootBuf(GLOBAL_CNT))
            ALLOCATE (gtemp_array((UB-LB)+1,GLOBAL_CNT))
         ELSE
            ALLOCATE (dRootBuf(10))
            ALLOCATE (gtemp_array((UB-LB)+1,10))
         ENDIF

! Pack particle coordinates in a temporary local array
         IF(VTP_P_DATA) THEN ! Regular particle data
            PC = 0
            DO LC1 = 1, MAX_PIP
               IF(BELONGS_TO_VTK_SUBDOMAIN(LC1)) THEN
                  PC =PC + 1
                  DO LC2=LB, UB
                     ltemp_array(LC2,PC) = DES_POS_NEW(LC1,LC2)
                  ENDDO
               ENDIF
               IF(PC==LOCAL_CNT) EXIT
            ENDDO
         ELSE ! Force chain data
            PC = 0
            DO LC1 = 1, FCHAINC
               IF(BELONGS_TO_VTK_SUBDOMAIN(LC1)) THEN
                  PC =PC + 1
                  DO LC2=LB, UB
                     ltemp_array(LC2,PC) = FCHAIN_MIDPOINT(LC1,LC2)
                  ENDDO
               ENDIF
               IF(PC==LOCAL_CNT) EXIT
            ENDDO
         ENDIF

! For each coordinate (x,y, and z), gather the local list to global temporary array
         DO LC1 = LB, UB
            dprocbuf(1:LOCAL_CNT)=ltemp_array(LC1,1:LOCAL_CNT)
            CALL desmpi_gatherv(ptype=2)
            gtemp_array(LC1,:) = drootbuf(:)
         ENDDO

! Write the list of coordinates
         IF(myPE == PE_IO) THEN
            DO LC1=1, GLOBAL_CNT
               DO LC2=LB, UB
                  WRITE(UNIT_VTP)  real(gtemp_array(LC2,LC1))
               ENDDO
            ENDDO
         ENDIF

         deallocate (dProcBuf, dRootBuf, ltemp_array,gtemp_array)


         ENDIF


      ELSEIF(BDIST_IO.AND.LOCAL_CNT>0) THEN

         IF(LOCAL_CNT==0) RETURN
! The number of points in the pvd file is the local number of particles
! computed from SETUP_VTK_REGION_PARTICLES

         NUMBER_OF_POINTS = LOCAL_CNT

! Number of bytes of position field (vector,3 components)
         nbytes_vector       = NUMBER_OF_POINTS * 3 * c_sizeof(float)

! Offset of each field
         offset_xyz = 0

         IF(PASS==WRITE_HEADER) THEN

            WRITE(BUFFER,*)'    <Piece NumberOfPoints="',NUMBER_OF_POINTS, &
                  '"  NumberOfVerts="0" NumberOfLines ="0" NumberOfStrips="0" NumberOfPolys="0" >'
            WRITE(UNIT_VTP)TRIM(BUFFER)//END_REC

            WRITE(BUFFER,*)'      <Points>'
            WRITE(UNIT_VTP)TRIM(BUFFER)//END_REC

            WRITE(BUFFER,*)'        <DataArray type="Float32" Name="coordinates" NumberOfComponents="3" &
                                    &format="appended" offset="',offset_xyz,'" />'
            WRITE(UNIT_VTP)TRIM(BUFFER)//END_REC

            WRITE(BUFFER,*)'      </Points>'
            WRITE(UNIT_VTP)TRIM(BUFFER)//END_REC

            WRITE(BUFFER,*)'<PointData Scalars="Diameter" Vectors="Velocity"> '!preparing pointData
            WRITE(UNIT_VTP)TRIM(BUFFER)//END_REC

! calculate offset for next field
            VTU_offset = offset_xyz + c_sizeof(int) + nbytes_vector


         ELSEIF(PASS==WRITE_DATA) THEN

            WRITE(BUFFER,*)'      </PointData>'
            WRITE(UNIT_VTP)TRIM(BUFFER)//END_REC

            WRITE(BUFFER,*)'      <Verts> </Verts>'
            WRITE(UNIT_VTP)TRIM(BUFFER)//END_REC

            WRITE(BUFFER,*)'      <Lines> </Lines>'
            WRITE(UNIT_VTP)TRIM(BUFFER)//END_REC

            WRITE(BUFFER,*)'      <Strips> </Strips>'
            WRITE(UNIT_VTP)TRIM(BUFFER)//END_REC

            WRITE(BUFFER,*)'      <Polys> </Polys>'
            WRITE(UNIT_VTP)TRIM(BUFFER)//END_REC

            WRITE(BUFFER,*)'    </Piece>'
            WRITE(UNIT_VTP)TRIM(BUFFER)//END_REC

            WRITE(BUFFER,*)'  </PolyData>'
            WRITE(UNIT_VTP)TRIM(BUFFER)//END_REC

            WRITE(BUFFER,*)'  <AppendedData encoding="raw">'
            WRITE(UNIT_VTP)TRIM(BUFFER)//END_REC

! Starting raw binary data with an underscore

            WRITE(BUFFER,*)'_'
            WRITE(UNIT_VTP)TRIM(BUFFER)

! Number of bytes for X,Y,Z coordinates
            WRITE(UNIT_VTP) nbytes_vector

            IF(VTP_P_DATA) THEN
               LB = LBOUND(DES_POS_NEW,2) ! This should always be 1
               UB = UBOUND(DES_POS_NEW,2) ! This should always be 2
            ELSE
               LB = LBOUND(FCHAIN_MIDPOINT,2) ! This should always be 1
               UB = UBOUND(FCHAIN_MIDPOINT,2) ! This should always be 2
            ENDIF

            ALLOCATE (ltemp_array((UB-LB)+1,LOCAL_CNT))

! Pack particle coordinates in a temporary local array
            IF(VTP_P_DATA) THEN ! Regular particle data
               PC = 0
               DO LC1 = 1, MAX_PIP
                  IF(BELONGS_TO_VTK_SUBDOMAIN(LC1)) THEN
                     PC =PC + 1
                     DO LC2=LB, UB
                        ltemp_array(LC2,PC) = DES_POS_NEW(LC1,LC2)
                     ENDDO
                  ENDIF
                  IF(PC==LOCAL_CNT) EXIT
               ENDDO
            ELSE ! Force chain data
               PC = 0
               DO LC1 = 1, FCHAINC
                  IF(BELONGS_TO_VTK_SUBDOMAIN(LC1)) THEN
                     PC =PC + 1
                     DO LC2=LB, UB
                        ltemp_array(LC2,PC) = FCHAIN_MIDPOINT(LC1,LC2)
                     ENDDO
                  ENDIF
                  IF(PC==LOCAL_CNT) EXIT
               ENDDO
            ENDIF

! Write the list of coordinates
            DO LC1=1, LOCAL_CNT
               DO LC2=LB, UB
                  WRITE(UNIT_VTP)  real(ltemp_array(LC2,LC1))
               ENDDO
            ENDDO

            deallocate (ltemp_array)

         ENDIF

      ENDIF

      RETURN

      END SUBROUTINE WRITE_GEOMETRY_IN_VTP_BIN

!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvC
!                                                                      C
!  Module name: WRITE_SCALAR_IN_VTP_BIN                                C
!  Purpose: Write Scalar variable in a vtp file                        C
!           Binary format                                              C
!                                                                      C
!  Author: Jeff Dietiker                              Date: 11-Feb-15  C
!  Reviewer:                                          Date:            C
!                                                                      C
!  Revision Number #                                  Date: ##-###-##  C
!  Author: #                                                           C
!  Purpose: #                                                          C
!                                                                      C
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^C
  SUBROUTINE WRITE_SCALAR_IN_VTP_BIN(VAR_NAME,VAR,PASS)

      IMPLICIT NONE
      INTEGER :: I,LC1,PC

      CHARACTER (*) :: VAR_NAME
      DOUBLE PRECISION, INTENT(in) :: VAR(:)

      REAL(c_float) :: float

      INTEGER :: nbytes_scalar

      INTEGER :: PASS
      INTEGER :: WRITE_HEADER = 1
      INTEGER :: WRITE_DATA   = 2

      IF (.NOT.BDIST_IO) THEN

! Number of bytes for each scalar field
         nbytes_scalar = GLOBAL_CNT * c_sizeof(float)

         IF(PASS==WRITE_HEADER) THEN

! Remove possible white space with underscore
            DO I = 1,LEN_TRIM(VAR_NAME)
               IF(VAR_NAME(I:I) == ' ') VAR_NAME(I:I) = '_'
            ENDDO

            IF (myPE == PE_IO) THEN
! For each scalar, write a tag, with corresponding offset
               WRITE(BUFFER,90)'        <DataArray type="Float32" Name="', &
                    TRIM(VAR_NAME),'" format="appended" offset="',VTU_offset,'" />'
               WRITE(UNIT_VTP)TRIM(BUFFER)//END_REC
            ENDIF

! Prepare the offset for the next field
            VTU_offset = VTU_offset + c_sizeof(float) + nbytes_scalar


         ELSEIF(PASS==WRITE_DATA) THEN

           allocate (dProcBuf(LOCAL_CNT) )
           allocate (dRootBuf(GLOBAL_CNT))

! Pack scalar list in a local buffer before gathering to root
            PC = 0
            DO LC1 = 1, VTP_LUB !MAX_PIP for particles, FCHAINC for Force chain
               IF(BELONGS_TO_VTK_SUBDOMAIN(LC1)) THEN
                  PC =PC + 1
                  dProcBuf(PC) = VAR(LC1)
               ENDIF
               IF(PC==LOCAL_CNT) EXIT
            ENDDO

! Gather local buffer to root
         CALL desmpi_gatherv(ptype=2)

! Write the data, always preceded by its size in number of bytes
! Write root buffer to file

         IF(myPE == PE_IO) THEN
            WRITE(UNIT_VTP) nbytes_scalar
            DO LC1=1, GLOBAL_CNT
               WRITE(UNIT_VTP)  real(drootBuf(LC1))
            ENDDO
         ENDIF

         deallocate (dProcBuf, dRootBuf)


         ENDIF


      ELSEIF(BDIST_IO.AND.LOCAL_CNT>0) THEN

! Number of bytes for each scalar field
         nbytes_scalar = LOCAL_CNT * c_sizeof(float)

! Remove possible white space with underscore
         DO I = 1,LEN_TRIM(VAR_NAME)
            IF(VAR_NAME(I:I) == ' ') VAR_NAME(I:I) = '_'
         ENDDO

         IF(PASS==WRITE_HEADER) THEN

! For each scalar, write a tag, with corresponding offset
            WRITE(BUFFER,90)'        <DataArray type="Float32" Name="', &
                 TRIM(VAR_NAME),'" format="appended" offset="',VTU_offset,'" />'
            WRITE(UNIT_VTP)TRIM(BUFFER)//END_REC

! Prepare the offset for the next field
            VTU_offset = VTU_offset + c_sizeof(float) + nbytes_scalar


         ELSEIF(PASS==WRITE_DATA) THEN

            allocate (dProcBuf(LOCAL_CNT) )

! Pack scalar list in a local buffer before writing in file
            PC = 0
            DO LC1 = 1, VTP_LUB !MAX_PIP for particles, FCHAINC for Force chain
               IF(BELONGS_TO_VTK_SUBDOMAIN(LC1)) THEN
                  PC =PC + 1
                  dProcBuf(PC) = VAR(LC1)
               ENDIF
               IF(PC==LOCAL_CNT) EXIT
            ENDDO

! Write the data, always preceded by its size in number of bytes
! Write root buffer to file
            WRITE(UNIT_VTP) nbytes_scalar

            DO LC1=1, LOCAL_CNT
               WRITE(UNIT_VTP)  real(dProcBuf(LC1))
            ENDDO

            deallocate (dProcBuf)


         ENDIF

      ENDIF

! Update pvtu file with variable name
      IF(BDIST_IO.AND.MyPE==PE_IO.AND.PASS==WRITE_DATA) THEN
         WRITE(UNIT_PVTP,100) '        <PointArray type="Float32" Name="', &
                              TRIM(VAR_NAME),'" format="appended"  />'
      ENDIF

      IF (PASS==WRITE_DATA.AND.FULL_LOG.AND.myPE == PE_IO) WRITE(*,10,ADVANCE='NO')'.'

10    FORMAT(A)
90    FORMAT(A,A,A,I12,A)
100   FORMAT(A,A,A)

      RETURN

      END SUBROUTINE WRITE_SCALAR_IN_VTP_BIN

!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvC
!                                                                      C
!  Module name: WRITE_VECTOR_IN_VTP                                    C
!  Purpose: Write Vector variable in a vtp file                        C
!           Binary format                                              C
!                                                                      C
!  Author: Jeff Dietiker                              Date: 11-Feb-15  C
!  Reviewer:                                          Date:            C
!                                                                      C
!  Revision Number #                                  Date: ##-###-##  C
!  Author: #                                                           C
!  Purpose: #                                                          C
!                                                                      C
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^C
  SUBROUTINE WRITE_VECTOR_IN_VTP_BIN(VAR_NAME,VAR,PASS)

      IMPLICIT NONE

      CHARACTER (*) :: VAR_NAME
      DOUBLE PRECISION, INTENT(in) :: VAR(:,:)

      REAL(c_float) :: float

      INTEGER :: nbytes_vector

      INTEGER :: PASS
      INTEGER :: WRITE_HEADER = 1
      INTEGER :: WRITE_DATA   = 2

      DOUBLE PRECISION, ALLOCATABLE :: ltemp_array(:,:)  ! local
      DOUBLE PRECISION, ALLOCATABLE :: gtemp_array(:,:)  ! global

      INTEGER :: LB, UB
      INTEGER :: PC, LC1, LC2

      IF (.NOT.BDIST_IO) THEN

! Number of bytes for each vector field
         nbytes_vector = GLOBAL_CNT * 3 * c_sizeof(float)

         IF(PASS==WRITE_HEADER) THEN
! For each vector, write a tag, with corresponding offset

            IF (myPE == PE_IO) THEN
               WRITE(BUFFER,90)'        <DataArray type="Float32" Name="', &
                    TRIM(VAR_NAME),'"  NumberOfComponents="3" format="appended" offset="',VTU_offset,'" />'
               WRITE(UNIT_VTP)TRIM(BUFFER)//END_REC
            ENDIF

! Prepare the offset for the next field
            VTU_offset = VTU_offset + c_sizeof(float) + nbytes_vector


         ELSEIF(PASS==WRITE_DATA) THEN

            LB = LBOUND(VAR,2) ! This should always be 1
            UB = UBOUND(VAR,2) ! This should always be 2

            ALLOCATE (dProcBuf(LOCAL_CNT) )
            ALLOCATE (ltemp_array((UB-LB)+1,LOCAL_CNT))

            IF(myPE == PE_IO) THEN
               ALLOCATE (dRootBuf(GLOBAL_CNT))
               ALLOCATE (gtemp_array((UB-LB)+1,GLOBAL_CNT))
            ELSE
               ALLOCATE (dRootBuf(10))
               ALLOCATE (gtemp_array((UB-LB)+1,10))
            ENDIF

! For each vector component, pack component list in a local array
            PC = 0
            DO LC1 = 1, VTP_LUB !MAX_PIP for particles, FCHAINC for Force chain
               IF(BELONGS_TO_VTK_SUBDOMAIN(LC1)) THEN
                  PC =PC + 1
                  DO LC2=LB, UB
                     ltemp_array(LC2,PC) = VAR(LC1,LC2)
                  ENDDO
               ENDIF
               IF(PC==LOCAL_CNT) EXIT
            ENDDO


! For each component, gather the local list to global temporary array
         DO LC1 = LB, UB
            dprocbuf(1:LOCAL_CNT)=ltemp_array(LC1,1:LOCAL_CNT)
            CALL desmpi_gatherv(ptype=2)
            gtemp_array(LC1,:) = drootbuf(:)
         ENDDO

! Write the data, always preceded by its size in number of bytes
         IF(myPE == PE_IO) THEN
            WRITE(UNIT_VTP) nbytes_vector
            DO LC1=1, GLOBAL_CNT
               DO LC2=LB, UB
                  WRITE(UNIT_VTP)  real(gtemp_array(LC2,LC1))
               ENDDO
            ENDDO
         ENDIF

         deallocate (dProcBuf, dRootBuf, ltemp_array,gtemp_array)

         ENDIF

      ELSEIF(BDIST_IO.AND.LOCAL_CNT>0) THEN

! Number of bytes for each vector field
         nbytes_vector = LOCAL_CNT * 3 * c_sizeof(float)

         IF(PASS==WRITE_HEADER) THEN
! For each vector, write a tag, with corresponding offset

            WRITE(BUFFER,90)'        <DataArray type="Float32" Name="', &
                 TRIM(VAR_NAME),'"  NumberOfComponents="3" format="appended" offset="',VTU_offset,'" />'
            WRITE(UNIT_VTP)TRIM(BUFFER)//END_REC

! Prepare the offset for the next field
            VTU_offset = VTU_offset + c_sizeof(float) + nbytes_vector


         ELSEIF(PASS==WRITE_DATA) THEN

            LB = LBOUND(VAR,2) ! This should always be 1
            UB = UBOUND(VAR,2) ! This should always be 2

            ALLOCATE (ltemp_array((UB-LB)+1,LOCAL_CNT))

! For each vector component, pack component list in a local array
            PC = 0
            DO LC1 = 1, VTP_LUB !MAX_PIP for particles, FCHAINC for Force chain
               IF(BELONGS_TO_VTK_SUBDOMAIN(LC1)) THEN
                  PC =PC + 1
                  DO LC2=LB, UB
                     ltemp_array(LC2,PC) = VAR(LC1,LC2)
                  ENDDO
               ENDIF
               IF(PC==LOCAL_CNT) EXIT
            ENDDO


! Write the data, always preceded by its size in number of bytes
            WRITE(UNIT_VTP) nbytes_vector
            DO LC1=1, LOCAL_CNT
               DO LC2=LB, UB
                  WRITE(UNIT_VTP)  real(ltemp_array(LC2,LC1))
               ENDDO
            ENDDO

            deallocate (ltemp_array)


         ENDIF

      ENDIF

! Update pvtu file with variable name
      IF(BDIST_IO.AND.MyPE==PE_IO.AND.PASS==WRITE_DATA) THEN
         WRITE(UNIT_PVTP,100) '        <PointArray type="Float32" Name="', &
                              TRIM(VAR_NAME),'"  NumberOfComponents="3" format="appended"  />'
      ENDIF

      IF (PASS==WRITE_DATA.AND.FULL_LOG.AND.myPE == PE_IO) WRITE(*,10,ADVANCE='NO')'.'

10    FORMAT(A)
90    FORMAT(A,A,A,I12,A)
100   FORMAT(A,A,A)

      RETURN

      END SUBROUTINE WRITE_VECTOR_IN_VTP_BIN

!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvC
!                                                                      C
!  Module name: CLOSE_VTP_FILE_BIN                                     C
!  Purpose: Close a vtp file                                           C
!           Binary format                                              C
!                                                                      C
!  Author: Jeff Dietiker                              Date: 11-Feb-15  C
!  Reviewer:                                          Date:            C
!                                                                      C
!  Revision Number #                                  Date: ##-###-##  C
!  Author: #                                                           C
!  Purpose: #                                                          C
!                                                                      C
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^C
  SUBROUTINE CLOSE_VTP_FILE_BIN(MODE)

      IMPLICIT NONE

      INTEGER:: N
      CHARACTER (LEN=256) :: VTU_NAME
      INTEGER, DIMENSION(0:numPEs-1) :: ALL_PART_CNT
      INTEGER :: IERR
      INTEGER :: MODE   ! MODE = 0 : Write regular VTK region file
                        ! MODE = 1 : Write debug   VTK region file (VTK_DBG_FILE = .TRUE.)

      IF((myPE == PE_IO.AND.(.NOT.BDIST_IO)).OR.(BDIST_IO.AND.LOCAL_CNT>0)) THEN

! Write last tags and close the vtp file
      WRITE(BUFFER, *)'  </AppendedData>'
      WRITE(UNIT_VTP)END_REC//TRIM(BUFFER)//END_REC

      WRITE(BUFFER, *)'</VTKFile>'
      WRITE(UNIT_VTP)TRIM(BUFFER)//END_REC

      CLOSE(UNIT_VTP)

      ENDIF

! Update pvtu file and close

      IF(BDIST_IO)  THEN
         CALL allgather_1i (LOCAL_CNT,ALL_PART_CNT,IERR)

         IF (myPE == PE_IO.AND.GLOBAL_CNT>0) THEN
            WRITE(UNIT_PVTP, *) '      </PPointData>'

            DO N = 0,NumPEs-1
               IF(ALL_PART_CNT(N)>0) THEN
! The pvtp and vtp files are in the same directory (either project dir or VTU_DIR)
! The VTU_DIR should not be prepended to the basename (see .FALSE. as last argument)
                  VTU_NAME = FIND_VTK_BASENAME(MODE, N, .FALSE.) // ".vtp"
                  WRITE(UNIT_PVTP,110) '      <Piece Source="',TRIM(VTU_NAME),'"/>'
               ENDIF
            ENDDO

            WRITE(UNIT_PVTP, *) '  </PPolyData>'
            WRITE(UNIT_PVTP, *) '</VTKFile>'
            CLOSE(UNIT_PVTP)
         ENDIF
      ENDIF

110   FORMAT(A,A,A)

      RETURN

      END SUBROUTINE CLOSE_VTP_FILE_BIN


!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvC
!                                                                      C
!  Module name: SETUP_VTK_REGION_PARTICLES                             C
!                                                                      C
!  Purpose: Filter the particles  based on the VTK region bounds and   C
!           set the flag BELONGS_TO_VTK_SUBDOMAIN to .TRUE.            C
!           to keep the particle.                                      C
!                                                                      C
!  Author: Jeff Dietiker                              Date: 11-Feb-15  C
!  Reviewer:                                          Date:            C
!                                                                      C
!  Revision Number #                                  Date: ##-###-##  C
!  Author: #                                                           C
!  Purpose: #                                                          C
!                                                                      C
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^C
      SUBROUTINE SETUP_VTK_REGION_PARTICLES

      IMPLICIT NONE

      INTEGER :: PC,LC1,M
      INTEGER :: NXS,NYS,NZS,NS
      DOUBLE PRECISION :: X_SLICE(DIM_I),Y_SLICE(DIM_J),Z_SLICE(DIM_K)
      DOUBLE PRECISION :: XE,XW,YS,YN,ZB,ZT
      DOUBLE PRECISION :: XP,YP,ZP,XP1,YP1,ZP1,XP2,YP2,ZP2,R

      DOUBLE PRECISION :: SLICE_TOL
      LOGICAL :: KEEP_XDIR,KEEP_YDIR,KEEP_ZDIR

! Variables related to gather
      integer lgathercnts(0:numpes-1), lproc

      CHARACTER(LEN=1) :: SELECT_PARTICLE_BY

! Get VTK region bounds
      XE = VTK_X_E(VTK_REGION)
      XW = VTK_X_W(VTK_REGION)
      YS = VTK_Y_S(VTK_REGION)
      YN = VTK_Y_N(VTK_REGION)
      ZB = VTK_Z_B(VTK_REGION)
      ZT = VTK_Z_T(VTK_REGION)

      NXS = VTK_NXS(VTK_REGION)
      NYS = VTK_NYS(VTK_REGION)
      NZS = VTK_NZS(VTK_REGION)

      SLICE_TOL = VTK_SLICE_TOL(VTK_REGION)

      SELECT_PARTICLE_BY = VTK_SELECT_MODE(VTK_REGION)

! get slice(s) location
      DO NS = 1,NXS
         X_SLICE(NS) = XW + (XE-XW)/FLOAT(NXS-1)*FLOAT(NS-1)
      ENDDO

      DO NS = 1,NYS
         Y_SLICE(NS) = YS + (YN-YS)/FLOAT(NYS-1)*FLOAT(NS-1)
      ENDDO

      DO NS = 1,NZS
         Z_SLICE(NS) = ZB + (ZT-ZB)/FLOAT(NZS-1)*FLOAT(NS-1)
      ENDDO


! Loop through all particles on local rank and keep a list of particles
! belonging to VTK region

      IF(ALLOCATED(BELONGS_TO_VTK_SUBDOMAIN)) DEALLOCATE(BELONGS_TO_VTK_SUBDOMAIN)
      ALLOCATE(BELONGS_TO_VTK_SUBDOMAIN(VTP_LUB))

      BELONGS_TO_VTK_SUBDOMAIN = .FALSE.

      LOCAL_CNT = 0
      PC = 1
      DO LC1 = 1, VTP_LUB !MAX_PIP for particles, FCHAINC for Force chain

         IF(VTP_P_DATA) THEN ! Particle data
            IF(.NOT.IS_NORMAL(LC1)) CYCLE
            IF(PC > PIP) EXIT
            ! IF(IS_NONEXISTENT(LC1)) CYCLE
            PC = PC+1
            ! IF(IS_GHOST(LC1) .OR. IS_ENTERING_GHOST(LC1) .OR. IS_EXITING_GHOST(LC1)) CYCLE
   
            M = PIJK(LC1,5)
   
            IF(.NOT.VTK_PART_PHASE(VTK_REGION,M)) CYCLE

               XP = DES_POS_NEW(LC1,1)
               YP = DES_POS_NEW(LC1,2)
               ZP = DES_POS_NEW(LC1,3)

               R = DES_RADIUS(LC1)

               XP1 = DES_POS_NEW(LC1,1) - R
               YP1 = DES_POS_NEW(LC1,2) - R
               ZP1 = DES_POS_NEW(LC1,3) - R

               XP2 = DES_POS_NEW(LC1,1) + R
               YP2 = DES_POS_NEW(LC1,2) + R
               ZP2 = DES_POS_NEW(LC1,3) + R

         ELSE ! Force chain
              ! Only selection mode available is 'C' (Force chain midpoint must be inside vtk region)
               XP = FCHAIN_MIDPOINT(LC1,1)
               YP = FCHAIN_MIDPOINT(LC1,2)
               ZP = FCHAIN_MIDPOINT(LC1,3)

               
         ENDIF

         SELECT CASE(SELECT_PARTICLE_BY)
            CASE('C')  ! Particle center must be inside vtk region

! X-direction
               KEEP_XDIR=.FALSE.
               IF(NXS==0) THEN
                  IF(XW<=XP.AND.XP<=XE) KEEP_XDIR=.TRUE.
               ELSE
                  DO NS = 1,NXS
                     IF((X_SLICE(NS)-SLICE_TOL)<=XP.AND.XP<=(X_SLICE(NS)+SLICE_TOL)) KEEP_XDIR=.TRUE.
                  ENDDO
               ENDIF

! Y-direction
               KEEP_YDIR=.FALSE.
               IF(NYS==0) THEN
                  IF(YS<=YP.AND.YP<=YN) KEEP_YDIR=.TRUE.
               ELSE
                  DO NS = 1,NYS
                     IF((Y_SLICE(NS)-SLICE_TOL)<=YP.AND.YP<=(Y_SLICE(NS)+SLICE_TOL)) KEEP_YDIR=.TRUE.
                  ENDDO
               ENDIF

! Z-direction
               KEEP_ZDIR=.FALSE.
               IF(NZS==0) THEN
                  IF(ZB<=ZP.AND.ZP<=ZT) KEEP_ZDIR=.TRUE.
               ELSE
                  DO NS = 1,NZS
                     IF((Z_SLICE(NS)-SLICE_TOL)<=ZP.AND.ZP<=(Z_SLICE(NS)+SLICE_TOL)) KEEP_ZDIR=.TRUE.
                  ENDDO
               ENDIF


            CASE('P')  ! Entire particle must be inside vtk region

! X-direction
               KEEP_XDIR=.FALSE.
               IF(NXS==0) THEN
                  IF(XW<=XP1.AND.XP2<=XE) KEEP_XDIR=.TRUE.
               ELSE
                  DO NS = 1,NXS
                     IF((X_SLICE(NS)-SLICE_TOL)<=XP1.AND.XP2<=(X_SLICE(NS)+SLICE_TOL)) KEEP_XDIR=.TRUE.
                  ENDDO
               ENDIF

! Y-direction
               KEEP_YDIR=.FALSE.
               IF(NYS==0) THEN
                  IF(YS<=YP1.AND.YP2<=YN) KEEP_YDIR=.TRUE.
               ELSE
                  DO NS = 1,NYS
                     IF((Y_SLICE(NS)-SLICE_TOL)<=YP1.AND.YP2<=(Y_SLICE(NS)+SLICE_TOL)) KEEP_YDIR=.TRUE.
                  ENDDO
               ENDIF

! Z-direction
               KEEP_ZDIR=.FALSE.
               IF(NZS==0) THEN
                  IF(ZB<=ZP1.AND.ZP2<=ZT) KEEP_ZDIR=.TRUE.
               ELSE
                  DO NS = 1,NZS
                     IF((Z_SLICE(NS)-SLICE_TOL)<=ZP1.AND.ZP2<=(Z_SLICE(NS)+SLICE_TOL)) KEEP_ZDIR=.TRUE.
                  ENDDO
               ENDIF


            CASE('I')  ! Particle must be inside or intersect the edge of the vtk region

! X-direction
               KEEP_XDIR=.FALSE.
               IF(NXS==0) THEN
                  IF(.NOT.(XE<=XP1.OR.XP2<=XW)) KEEP_XDIR=.TRUE.
               ELSE
                  DO NS = 1,NXS
                     IF(.NOT.((X_SLICE(NS)+SLICE_TOL)<=XP1.OR.XP2<=(X_SLICE(NS)-SLICE_TOL))) KEEP_XDIR=.TRUE.
                  ENDDO
               ENDIF

! Y-direction
               KEEP_YDIR=.FALSE.
               IF(NYS==0) THEN
                  IF(.NOT.(YN<=YP1.OR.YP2<=YS)) KEEP_YDIR=.TRUE.
               ELSE
                  DO NS = 1,NYS
                     IF(.NOT.((Y_SLICE(NS)+SLICE_TOL)<=YP1.OR.YP2<=(Y_SLICE(NS)-SLICE_TOL))) KEEP_YDIR=.TRUE.
                  ENDDO
               ENDIF

! Z-direction
               KEEP_ZDIR=.FALSE.
               IF(NZS==0) THEN
                  IF(.NOT.(ZT<=ZP1.OR.ZP2<=ZB)) KEEP_ZDIR=.TRUE.
               ELSE
                  DO NS = 1,NZS
                     IF(.NOT.((Z_SLICE(NS)+SLICE_TOL)<=ZP1.OR.ZP2<=(Z_SLICE(NS)-SLICE_TOL))) KEEP_ZDIR=.TRUE.
                  ENDDO
               ENDIF

            CASE DEFAULT
               print*,'should not be here (select particle by)'
         END SELECT

! Now combine
         IF(KEEP_XDIR.AND.KEEP_YDIR.AND.KEEP_ZDIR) THEN
            BELONGS_TO_VTK_SUBDOMAIN(LC1) = .TRUE.
            LOCAL_CNT = LOCAL_CNT + 1
         ENDIF
      ENDDO ! particle loop

! Calculate the total number of particles system-wide.
      GLOBAL_CNT=10
      call global_sum(LOCAL_CNT, GLOBAL_CNT)

! No need to set the send/reccv when using distributed IO
      IF (BDIST_IO) RETURN
! Set the send count from the local process.
      igath_sendcnt = LOCAL_CNT

! Collect the number of particles on each rank.all ranks.
      lgathercnts = 0
      lgathercnts(myPE) = LOCAL_CNT
      call global_sum(lgathercnts,igathercnts)

! Calculate the rank displacements.
      idispls(0) = 0
      DO lPROC = 1,NUMPEs-1
         idispls(lproc) = idispls(lproc-1) + igathercnts(lproc-1)
      ENDDO

      RETURN

   END SUBROUTINE SETUP_VTK_REGION_PARTICLES

END MODULE VTP
