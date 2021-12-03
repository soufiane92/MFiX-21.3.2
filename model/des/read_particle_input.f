#include "error.inc"

MODULE READ_PAR_INPUT_MOD

   USE error_manager
   USE read_database_mod, only: read_database

CONTAINS
!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv!
! Subroutine: READ_PAR_INPUT                                           !
!                                                                      !
! Purpose: Read the particle input and broadcasts the particle data to !
! respective processors.                                               !
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^!
      SUBROUTINE READ_PAR_INPUT

!-----------------------------------------------
! Modules
!-----------------------------------------------
      USE discretelement
      use cdist
      use compar
      use desmpi
      use functions
      use funits
      use geometry, only: NO_K
      use mpi_init_des, only: des_scatter_particle
      use mpi_utility

      implicit none
!-----------------------------------------------
! Local variables
!-----------------------------------------------
! indices
      integer :: k
! index of particle
      INTEGER :: lcurpar
! local unit
      INTEGER, PARAMETER :: lunit=10
! local filename
      character(255) lfilename
! IO Status:
      INTEGER :: IOS
! Flag to indicate if file exists.
      LOGICAL :: lEXISTS
! Read dimension: 2D vs 3D data
      integer :: RDMN
! local filename
      character(11) Version_string
!-----------------------------------------------

      IOS = 0
      RDMN = merge(2,3,NO_K)

! Setup the file name based on distributed or serial IO.
      IF(bDIST_IO) THEN
         lFILENAME = ''
         WRITE(lFILENAME,'("particle_input_",I4.4,".dat")') myPE
      ELSE
         lFILENAME= "particle_input.dat"
      ENDIF

! Check the that file exists and open it.
! Get version of the file
      IF(myPE == PE_IO) THEN
         INQUIRE(FILE=lFILENAME, EXIST=lEXISTS)
         IF(.NOT.LEXISTS) THEN
! Version 2.0 and above do not support Distributed IO. The file is named 'particel_input.dat'
            lFILENAME= "particle_input.dat"
            INQUIRE(FILE=lFILENAME, EXIST=lEXISTS)
         ENDIF
         IF(.NOT.LEXISTS) THEN
            WRITE(ERR_MSG, 1100)
            CALL LOG_WARNING()
            IOS = 1
         ELSE
            OPEN(UNIT=lUNIT, FILE=lFILENAME, FORM="FORMATTED")
            read(lUNIT,'(A11)') Version_string
            backspace(lUNIT)
         ENDIF
      ENDIF

      call bcast(VERSION_STRING, PE_IO)  

! Collect the error message and quit.
      CALL GLOBAL_ALL_SUM(IOS)
      IF(IOS /= 0) call LOG_ERROR()

 1100 FORMAT('Error 1100: FATAL - DEM particle input file not found!')

! Read the file
!----------------------------------------------------------------->>>

      SELECT CASE (Version_string)

         CASE('Version 2.0')

            P_INPUT_DAT_VERSION = '2.0'
            WRITE(ERR_MSG, 1200) P_INPUT_DAT_VERSION
            CALL LOG_INFO()
            CALL READ_PAR_INPUT_V2P0

         CASE DEFAULT

            P_INPUT_DAT_VERSION = '1.0'
            WRITE(ERR_MSG, 1200) P_INPUT_DAT_VERSION
            CALL LOG_INFO()
            CALL CHECK_PARTICLE_INPUT_FILTER_WITH_V1
            CALL READ_PAR_INPUT_V1P0

      END SELECT


 1200 FORMAT('Info: Reading DEM particle input file version: ', A)

      RETURN

      call log_error()

   END SUBROUTINE READ_PAR_INPUT

!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv!
! Subroutine: READ_PAR_INPUT_V2P0                                      !
!                                                                      !
! Purpose: Read the particle input and broadcasts the particle data to !
! respective processors.                                               !
! Version 2.0                                                          !
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^!
      SUBROUTINE READ_PAR_INPUT_V2P0

!-----------------------------------------------
! Modules
!-----------------------------------------------
      USE discretelement
      use cdist
      use compar
      use des_allocate, only: particle_grow
      use desmpi
      use desmpi_wrapper, only: DES_MPI_STOP
      use des_thermo, only: des_t_s
      use discretelement, only: des_usr_var_size
      use functions
      use funits
      use geometry, only: NO_K
      use mass_outflow_dem_mod, only: delete_particle
      use mpi_comm_des, only: desmpi_scatterv
      use mpi_init_des, only: des_scatter_particle
      use mpi_utility
      use parallel_mpi
      use param, only: dimension_n_s
      use run, only: any_species_eq, energy_eq
      use des_rxns, only: des_x_s

      implicit none
!-----------------------------------------------
! Local variables
!-----------------------------------------------
! indices
      integer :: i,k,ilines,m,n
! index of particle
      INTEGER :: lcurpar, LC1
! local unit
      INTEGER, PARAMETER :: lunit=10
! local filename
      character(255) lfilename
! IO Status:
      INTEGER :: IOS
! Flag to indicate if file exists.
      LOGICAL :: lEXISTS
! Read dimension: 2D vs 3D data
      integer :: RDIMN
! Buffer
      character(32) Buffer
! Option to read or use default value (T/F)
      character(1) option
! Integer buffer
      integer :: int_buffer
! Double precision buffer, can be a list of up to 100 numbers
      integer, dimension(100) :: dp_buffer
! Length of list of variables to read (number of columns)
      integer :: n_vars
      integer :: XYZ_start
      double precision, dimension(3) :: Dummy_xyz
      integer :: Phase_ID_start
      double precision :: Uniform_Phase_ID
      integer :: Diameter_start
      double precision :: Uniform_Diameter
      integer :: Density_start
      double precision :: Uniform_Density
      integer :: Velocity_start
      double precision, dimension(3) :: Uniform_Velocity
      integer :: Temperature_start
      double precision :: Uniform_Temperature
      integer :: Species_start
      double precision, dimension(100) :: Uniform_Species
      integer :: User_scalar_start
      double precision, dimension(100) :: Uniform_User_scalar

      double precision, allocatable, dimension(:,:) :: part_data
      integer :: lproc,lbuf,lpacketsize
      integer :: lproc_parcnt(0:numpes-1)
      integer, allocatable, dimension(:) :: lpar_proc
      logical :: x_test, y_test, z_test, xyz_test
! Particle filter
      LOGICAL, allocatable, dimension(:) :: keep_particle
      LOGICAL :: keep_me
      character(32) :: label
!-----------------------------------------------

      n_vars = 0  

      IOS = 0
      RDIMN = merge(2,3,NO_K)

      IF (myPE .eq. PE_IO) THEN
! Read Header
! Skip Instructions
         ilines = 18
         do i = 1, ilines
            read(lunit,*) buffer
         enddo

! Dimension
         read(lunit,*) buffer,int_buffer
         if(RDIMN/=int_buffer) then
            WRITE(ERR_MSG,100) int_buffer, RDIMN
            CALL LOG_ERROR()
         endif

! Number f particles
         read(lunit,*) buffer, particles

! Skip header       
         do i = 1, 3
            read(lunit,*) buffer
         enddo

! Coordinates option
         call parse_vector_option('Coordinates', .TRUE., rdimn, Dummy_xyz(1:rdimn), XYZ_start, n_vars)

! Phase ID option
         call parse_scalar_option('Phase_ID', .TRUE., Uniform_Phase_ID, Phase_ID_start, n_vars)

! Diameter option
         call parse_scalar_option('Diameter', .TRUE., Uniform_Diameter, Diameter_start, n_vars)

! Density option
         call parse_scalar_option('Density', .TRUE., Uniform_Density, Density_start, n_vars)

! Velocity option
          call parse_vector_option('Velocity', .TRUE., rdimn, Uniform_Velocity(1:rdimn), Velocity_start, n_vars)

! Temperature option
         call parse_scalar_option('Temperature', ENERGY_EQ, Uniform_Temperature, Temperature_start, n_vars)

! Species option
          call parse_vector_option('Species', ANY_SPECIES_EQ, DIMENSION_N_S, Uniform_Species(1:DIMENSION_N_S), Species_start, n_vars)

! User Scalar option
          call parse_vector_option('User scalar', (DES_USR_VAR_SIZE>0), DES_USR_VAR_SIZE, Uniform_User_scalar(1:DES_USR_VAR_SIZE), User_scalar_start, n_vars)

    
 100 FORMAT('Error 100: Error reported when reading particle input file.',&
           /'Dimension (2D or 3D) does not match simulation setup file.',&
           /'Dimension in particle_input.dat    = ',I3,&
           /'Dimension in simulation setup file = ',I3)


! Skip the next three line. This should get us to line 35 where the data
! starts
! Skip header       
         do i = 1, 3
            read(lunit,*) buffer
         enddo


      ENDIF !  IF (myPE .eq. PE_IO)

! Broadcast column indices and default values to all PEs    
      call bcast(n_vars,pe_io)

      call bcast(XYZ_start,pe_io)
      call bcast(Phase_ID_start,pe_io)
      call bcast(Uniform_Phase_ID,pe_io)
      call bcast(Diameter_start,pe_io)
      call bcast(Uniform_Diameter,pe_io)
      call bcast(Density_start,pe_io)
      call bcast(Uniform_Density,pe_io)
      call bcast(Velocity_start,pe_io)
      call bcast(Uniform_Velocity,pe_io)
      call bcast(Temperature_start,pe_io)
      call bcast(Uniform_Temperature,pe_io)
      call bcast(Species_start,pe_io)
      call bcast(Uniform_Species,pe_io)
      call bcast(User_scalar_start,pe_io)
      call bcast(Uniform_User_scalar,pe_io)

! Read the file. Distributed I/O is not supported
! Read into temporary variable and scatter
      IF (myPE .eq. PE_IO) THEN

! Allocate and initialize temporary variables.
         ALLOCATE (part_data(particles,n_vars)); part_data=0.0D0
         ALLOCATE (lpar_proc(particles))

! Loop through the input file.
         DO lcurpar = 1, particles
            read (lunit,*,IOSTAT=IOS) (part_data(lcurpar,k),k=1,n_vars)

! Report read errors.
            IF(IOS > 0) THEN
               WRITE(ERR_MSG,1200)
               CALL LOG_ERROR()
               EXIT
 1200 FORMAT('Error 1200: Error reported when reading particle input ',&
         'file.',/'A common error is 2D input for 3D cases.')

! Report End-of-File errors.
            ELSEIF(IOS < 0) THEN
               WRITE(ERR_MSG,1201) &
                     trim(iVal(lcurpar)), trim(iVal(Particles))
               CALL LOG_WARNING()
               EXIT
 1201 FORMAT('Error 1201: Error reported when reading particle input ',&
         'file.',/'End-of-File found for particle ',A,' and ',A,1X,    &
         'entries are expected.')

            ENDIF

         ENDDO ! Loop over particles

      ENDIF ! (myPE == PE_IO)

      CALL GLOBAL_ALL_SUM(IOS)
      IF(IOS /= 0) CALL LOG_ERROR()

! Scatter particles         

! set the packet size for transfer
      lpacketsize = n_vars


! build the send buffer in PE_IO proc
! first pass to get the count of particles

      lproc_parcnt(:) = 0
      if(myPE.eq.pe_io) then
         lpar_proc(:) =-1
         do lcurpar = 1,particles
            do lproc= 0,numpes-1
! test if particles belongs to processor's domain
! Do individual tests in each direction (x,y, and z)
               x_test = (part_data(lcurpar,1).ge.xe(istart1_all(lproc)-1).and. &
                         part_data(lcurpar,1).lt.xe(iend1_all(lproc)))
               y_test = (part_data(lcurpar,2).ge.yn(jstart1_all(lproc)-1).and. &    
                         part_data(lcurpar,2).lt.yn(jend1_all(lproc)))  
               xyz_test = x_test.and.y_test    
               if(do_k) then
                  z_test = (part_data(lcurpar,3).ge.zt(kstart1_all(lproc)-1).and. &
                            part_data(lcurpar,3).lt.zt(kend1_all(lproc))) 
                  xyz_test = xyz_test.and.z_test    
               endif

               if ( xyz_test ) then
                  lpar_proc(lcurpar) = lproc
                  lproc_parcnt(lproc) = lproc_parcnt(lproc) + 1
                  exit
               end if
            end do ! (lproc= 0,numpes-1)
            if (lpar_proc(lcurpar).eq.-1) then
               WRITE(*,501) lcurpar
               call des_mpi_stop
            endif
         enddo ! (lcurpar = 1,particles)
      endif ! if (my_pe.eq.pe_io)

      call bcast(lproc_parcnt(0:numpes-1),pe_io)

! second pass: set and allocate scatter related variables
      pip = lproc_parcnt(mype)
      call PARTICLE_GROW(pip)
      max_pip = max(pip,max_pip)
      iscr_recvcnt = pip*lpacketsize
      allocate (dprocbuf(iscr_recvcnt))
      if (mype.eq.pe_io) then
         allocate (drootbuf(particles*lpacketsize))
      else
         allocate (drootbuf(10))
      endif

! in the IO processor build the drootbuffer and idispls required
! for mpi communication
      if(mype.eq.pe_io) then
         idispls(0) = 0
         iscattercnts(0) = lproc_parcnt(0)*lpacketsize
         do lproc = 1,numpes-1
            idispls(lproc) = idispls(lproc-1) + iscattercnts(lproc-1)
            iscattercnts(lproc) = lproc_parcnt(lproc)*lpacketsize
         end do
         lproc_parcnt(:) = 0
         do lcurpar = 1,particles
            lproc = lpar_proc(lcurpar)
            lbuf = idispls(lproc)+lproc_parcnt(lproc)*lpacketsize+1
            drootbuf(lbuf:lbuf+lpacketsize-1) = part_data(lcurpar,:)
            lproc_parcnt(lproc) = lproc_parcnt(lproc) + 1
         enddo
      endif

      call desmpi_scatterv(ptype=2)

! unpack the particles in each processor and set the pip
      lc1 = 0
      do lcurpar = 1,pip
         lbuf = (lcurpar-1)*lpacketsize

         lc1 = lc1 + 1 ! initialize potential new particle ID
         keep_me = .true.  ! used to filter particles based on variable range

! Particle position (x,y,z, coordinates)
         des_pos_new(lc1,1:rdimn) = dprocbuf(lbuf + XYZ_start:lbuf + XYZ_start + rdimn - 1 )

         call filter_single_particle_based_on_min_max_var(des_pos_new(lc1,1),'x-coordinate',part_in_x_min,part_in_x_max,part_in_x_exclude,keep_me)
         call filter_single_particle_based_on_min_max_var(des_pos_new(lc1,2),'y-coordinate',part_in_y_min,part_in_y_max,part_in_y_exclude,keep_me)
         call filter_single_particle_based_on_min_max_var(des_pos_new(lc1,3),'z-coordinate',part_in_z_min,part_in_z_max,part_in_z_exclude,keep_me)

! Particle Phase ID
         if(Phase_ID_start>0) then
            PIJK(lc1,5) = INT(dprocbuf(lbuf + Phase_ID_start))
         else
            PIJK(lc1,5) = INT(Uniform_Phase_ID)
         endif

         IF(.NOT.part_in_phase(PIJK(lc1,5))) keep_me = .FALSE.

! Particle Radius
         if(Diameter_start>0) then
            des_radius(lc1) = 0.5D0*dprocbuf(lbuf + Diameter_start)
         else
            des_radius(lc1) = 0.5D0*Uniform_Diameter
         endif

         call filter_single_particle_based_on_min_max_var(2.0D0*des_radius(lc1),'Diameter',part_in_diameter_min,part_in_diameter_max,part_in_diameter_exclude,keep_me)

! Statistical weight, radius and physical radius (CGDEM only)
! The diameter in particle_input.dat is the coarse-grained (not physical) diameter
         if(CGDEM) then
            m = PIJK(lc1,5)
            des_cgp_stw(lc1) = cgp_stat_wt(m)
            des_cgp_rpr(lc1) = des_radius(lc1)/(des_cgp_stw(lc1)**(1.0d0/3.0d0))
         endif

! Particle Density
         if(Density_start>0) then
            ro_sol(lc1) = dprocbuf(lbuf + Density_Start)
         else
            ro_sol(lc1) = Uniform_Density
         endif

         call filter_single_particle_based_on_min_max_var(ro_sol(lc1),'Density',part_in_density_min,part_in_density_max,part_in_density_exclude,keep_me)

! Particle Velocity
         if(Velocity_start>0) then
            des_vel_new(lc1,1:rdimn) = dprocbuf(lbuf + Velocity_start:lbuf + Velocity_start + rdimn - 1)
         else
            des_vel_new(lc1,1:rdimn) = Uniform_Velocity(1:rdimn)
         endif

         call filter_single_particle_based_on_min_max_var(des_vel_new(lc1,1),'x-velocity',part_in_u_min,part_in_u_max,part_in_u_exclude,keep_me)
         call filter_single_particle_based_on_min_max_var(des_vel_new(lc1,2),'y-velocity',part_in_v_min,part_in_v_max,part_in_v_exclude,keep_me)
         call filter_single_particle_based_on_min_max_var(des_vel_new(lc1,3),'z-velocity',part_in_w_min,part_in_w_max,part_in_w_exclude,keep_me)

! Particle Temperature
         if(ENERGY_EQ) then
            if(Temperature_start>0) then
               des_t_s(lc1) = dprocbuf(lbuf + Temperature_Start)
            else
               des_t_s(lc1) = Uniform_Temperature
            endif

            call filter_single_particle_based_on_min_max_var(des_t_s(lc1),'Temperature',part_in_temp_min,part_in_temp_max,part_in_temp_exclude,keep_me)

         endif

! Particle Species: Always need DIMENSION_N_S values for all particles
         if(ANY_SPECIES_EQ) then
            if(Species_start>0) then
               des_x_s(lc1,1:DIMENSION_N_S) = dprocbuf(lbuf + Species_start:lbuf + Species_start + DIMENSION_N_S - 1)
            else
               des_x_s(lc1,1:DIMENSION_N_S) = Uniform_Species(1:DIMENSION_N_S)
            endif

            do n = 1,DIMENSION_N_S
               write(label,'("Species",I2)') n
               call filter_single_particle_based_on_min_max_var(des_x_s(lc1,n),label,part_in_x_s_min(n),part_in_x_s_max(n),part_in_x_s_exclude(n),keep_me)
            enddo

         endif

! Particle Scalar: CAUTION THIS ARRAY IS TRANSPOSED, FIRST INDEX IS USR_VAR ID, SECOND INDEX IS PARTICLE ID
         if(DES_USR_VAR_SIZE>0) then
            if(User_scalar_start>0) then
               des_usr_var(1:DES_USR_VAR_SIZE,lc1) = dprocbuf(lbuf + User_scalar_start:lbuf + User_scalar_start + DES_USR_VAR_SIZE - 1)
            else
               des_usr_var(1:DES_USR_VAR_SIZE,lc1) = Uniform_user_scalar(1:DES_USR_VAR_SIZE)
            endif

            do n = 1,DES_USR_VAR_SIZE
                write(label,'("User scalar # ",I2)') n
                call filter_single_particle_based_on_min_max_var(des_usr_var(n,lc1),label,part_in_usr_var_min(n),part_in_usr_var_max(n),part_in_usr_var_exclude(n),keep_me)
            enddo

         endif

! Keep or reject particle based on filtering
         if(keep_me) then
! Set particle status to "normal"
            call set_normal(lc1)
         else
! Reject current particle and go back in the list of particles
            lc1 = lc1 - 1
         endif
         
      enddo

      deallocate (dprocbuf,drootbuf)

! Set particle count after filtering
      pip = lc1
      max_pip = pip

 500  FORMAT(/2X,'From: DES_SCATTER_PARTICLE: (0)',/2X,&
         'ERROR: Unable to locate the particle (no. ',I10,&
         ') inside the domain')
 501  FORMAT(/2X,'From: DES_SCATTER_PARTICLE: (1)',/2X,&
         'ERROR: Unable to locate the particle (no. ',I10,&
         ') inside the domain')

      IF(myPE == PE_IO) deallocate (part_data,lpar_proc)

!-----------------------------------------------------------------<<<

      IF(myPE == PE_IO) CLOSE(lUNIT)


      RETURN

      call log_error()



      contains

      Subroutine parse_scalar_option(Scalar_name, Scalar_condition, Default_scalar, Starting_index, n_cols)

      USE make_upper_case_mod, only: make_upper_case

      IMPLICIT NONE
! Scalar name 
      character(LEN=*), intent(in) :: scalar_name
! Condition to read the option
      logical, intent(in) :: Scalar_condition
! Option to read or use default value (T/F)
      character(1) :: scalar_option
! Default value
      double precision, intent(out) :: Default_scalar
! Column index
      integer, intent(out) :: Starting_index
! Total number of columns      
      integer, intent(inout) :: n_cols
! Buffer
      character(32) Buffer
! IO Status:
      INTEGER :: IOS

      IOS = 0

      read(lunit,*) buffer, Scalar_option

      if(Scalar_condition) then

         CALL MAKE_UPPER_CASE (scalar_option,1)

         if(scalar_option=='F') then
            backspace(lUNIT)
            read(lunit,*,IOSTAT=IOS) buffer, buffer, Default_scalar
! Report read errors.
            IF(IOS > 0) THEN
               WRITE(ERR_MSG,110) TRIM(Scalar_name)
               CALL LOG_ERROR()
            ENDIF
            Starting_index   = 0 
         elseif(scalar_option == 'T') then
            starting_index  = n_vars + 1 
            n_vars          = starting_index
         else
            WRITE(ERR_MSG,120) trim(Scalar_name), Scalar_option
            CALL LOG_ERROR()
         endif

      endif

      return

 110 FORMAT('Error 110: Error reported when reading particle input file.',&
           /'Invalid or missing default value for : ',A)

 120 FORMAT('Error 120: Error reported when reading particle input file.',&
           /'Invalid option for : ',A,' = ',A,&
           /'Valid options are "T" or "F" ')
      end Subroutine parse_scalar_option

      Subroutine parse_vector_option(Vector_name, Vector_condition, Vector_size, Default_vector, Starting_index, n_cols)

      USE make_upper_case_mod, only: make_upper_case

      IMPLICIT NONE
! Vector name 
      character(LEN=*), intent(in) :: Vector_name
! Condition to read the option
      logical, intent(in) :: Vector_condition
! Vector size
      integer, intent(in) :: Vector_size
! Option to read or use default value (T/F)
      character(1) :: vector_option
! Default value
      double precision, intent(out), dimension(Vector_size) :: Default_vector
! Column index
      integer, intent(out) :: Starting_index
! Total number of columns      
      integer, intent(inout) :: n_cols
! Buffer
      character(32) Buffer
! IO Status:
      INTEGER :: IOS

      IOS = 0

      read(lunit,*) buffer, Vector_option

      if(Vector_name=='Coordinates'.AND.Vector_option/='T') then
         WRITE(ERR_MSG,110) trim(Vector_name), Vector_option
         CALL LOG_ERROR()
      endif


      if(Vector_condition) then

         CALL MAKE_UPPER_CASE (Vector_option,1)

         if(Vector_option=='F') then
            backspace(lUNIT)
            read(lunit,*,IOSTAT=IOS) buffer, buffer, Default_vector(1:Vector_size)
! Report read errors.
               IF(IOS > 0) THEN
                  WRITE(ERR_MSG,120) TRIM(Vector_name),Vector_size
                  CALL LOG_ERROR()
               ENDIF
            Starting_index   = 0 
         elseif(Vector_option == 'T') then
            starting_index  = n_vars + 1 
            n_vars          = starting_index + Vector_size - 1
         else
            WRITE(ERR_MSG,130) trim(Vector_name), Vector_option
            CALL LOG_ERROR()
         endif

      endif

      return

 110 FORMAT('Error 110: Error reported when reading particle input file.',&
           /'Invalid option for : ',A,' = ',A,&
           /'Only Valid options is "T" ')

 120 FORMAT('Error 120: Error reported when reading particle input file.',&
           /'Invalid or missing default value(s) for : ',A,&
           /'Expected number of components = ',I2)

 130 FORMAT('Error 130: Error reported when reading particle input file.',&
           /'Invalid option for : ',A,' = ',A,&
           /'Valid options are "T" or "F" ')
      end Subroutine parse_vector_option




   END SUBROUTINE READ_PAR_INPUT_V2P0

!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv!
! Subroutine: READ_PAR_INPUT_V1P0                                      !
!                                                                      !
! Purpose: Read the particle input and broadcasts the particle data to !
! respective processors.                                               !
! Version 1.0 (legacy)                                                 !
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^!
      SUBROUTINE READ_PAR_INPUT_V1P0

!-----------------------------------------------
! Modules
!-----------------------------------------------
      USE discretelement
      use cdist
      use compar
      use desmpi
      use functions
      use funits
      use geometry, only: NO_K
      use mpi_init_des, only: des_scatter_particle
      use mpi_utility

      implicit none
!-----------------------------------------------
! Local variables
!-----------------------------------------------
! indices
      integer :: k
! index of particle
      INTEGER :: lcurpar
! local unit
      INTEGER, PARAMETER :: lunit=10
! local filename
      character(255) lfilename
! IO Status:
      INTEGER :: IOS
! Flag to indicate if file exists.
      LOGICAL :: lEXISTS
! Read dimension: 2D vs 3D data
      integer :: RDMN
!-----------------------------------------------

      IOS = 0
      RDMN = merge(2,3,NO_K)


! Read the file
!----------------------------------------------------------------->>>
! In distributed IO the first line of the file will be number of
! particles in that processor
      IF (bdist_io) then
         read(lunit,*) pip
         DO lcurpar = 1,pip
            call set_normal(lcurpar)
            read (lunit,*) (des_pos_new(lcurpar,k),k=1,RDMN),&
               des_radius(lcurpar), ro_sol(lcurpar),&
               (des_vel_new(lcurpar,k),k=1,RDMN)
         ENDDO

! Serial IO (not bDIST_IO)
      ELSE
!----------------------------------------------------------------->>>

! Read into temporary variable and scatter
         IF (myPE .eq. PE_IO) THEN

! Allocate and initialize temporary variables.
            ALLOCATE (dpar_pos(particles,3)); dpar_pos=0.0
            ALLOCATE (dpar_vel(particles,3)); dpar_vel=0.0
            ALLOCATE (dpar_rad(particles));   dpar_rad=0.0
            ALLOCATE (dpar_den(particles));   dpar_den = 0.0
! Loop through the input file.
            DO lcurpar = 1, particles
               read (lunit,*,IOSTAT=IOS)                               &
               (dpar_pos(lcurpar,k),k=1,RDMN),dpar_rad(lcurpar),       &
               dpar_den(lcurpar),(dpar_vel(lcurpar,k),k=1,RDMN)

! Report read errors.
               IF(IOS > 0) THEN
                  WRITE(ERR_MSG,1200)
                  CALL LOG_WARNING()
                  EXIT
 1200 FORMAT('Error 1200: Error reported when reading particle input ',&
         'file.',/'A common error is 2D input for 3D cases.')

! Report End-of-File errors.
               ELSEIF(IOS < 0) THEN
                  WRITE(ERR_MSG,1201) &
                     trim(iVal(lcurpar)), trim(iVal(Particles))
                  CALL LOG_WARNING()
                  EXIT
 1201 FORMAT('Error 1201: Error reported when reading particle input ',&
         'file.',/'End-of-File found for particle ',A,' and ',A,1X,    &
         'entries are expected.')

               ENDIF

            ENDDO

         ENDIF

         CALL GLOBAL_ALL_SUM(IOS)
         IF(IOS /= 0) CALL LOG_ERROR()

         CALL DES_SCATTER_PARTICLE

         IF(myPE == PE_IO) &
            deallocate (dpar_pos,dpar_vel,dpar_rad,dpar_den)

      ENDIF   ! end if/else bdist_io
!-----------------------------------------------------------------<<<

      IF(bDIST_IO .OR. myPE == PE_IO) CLOSE(lUNIT)

      RETURN

      call log_error()

   END SUBROUTINE READ_PAR_INPUT_V1P0

!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv!
! Subroutine: READ_PAR_INPUT_OLD                                       !
!                                                                      !
! Purpose: Read the particle input and broadcasts the particle data to !
! respective processors.                                               !
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^!
      SUBROUTINE READ_PAR_INPUT_OLD

!-----------------------------------------------
! Modules
!-----------------------------------------------
      USE discretelement
      use cdist
      use compar
      use desmpi
      use functions
      use funits
      use geometry, only: NO_K
      use mpi_init_des, only: des_scatter_particle
      use mpi_utility

      implicit none
!-----------------------------------------------
! Local variables
!-----------------------------------------------
! indices
      integer :: k
! index of particle
      INTEGER :: lcurpar
! local unit
      INTEGER, PARAMETER :: lunit=10
! local filename
      character(255) lfilename
! IO Status:
      INTEGER :: IOS
! Flag to indicate if file exists.
      LOGICAL :: lEXISTS
! Read dimension: 2D vs 3D data
      integer :: RDMN
!-----------------------------------------------

      IOS = 0
      RDMN = merge(2,3,NO_K)

! Setup the file name based on distributed or serial IO.
      IF(bDIST_IO) THEN
         lFILENAME = ''
         WRITE(lFILENAME,'("particle_input_",I4.4,".dat")') myPE
      ELSE
         lFILENAME= "particle_input.dat"
      ENDIF

! Check the the file exists and open it.
      IF(bDIST_IO .OR. myPE == PE_IO) THEN
         INQUIRE(FILE=lFILENAME, EXIST=lEXISTS)
         IF(.NOT.LEXISTS) THEN
            WRITE(ERR_MSG, 1100)
            CALL LOG_WARNING()
            IOS = 1
         ELSE
            OPEN(UNIT=lUNIT, FILE=lFILENAME, FORM="FORMATTED")
         ENDIF
      ENDIF

! Collect the error message and quit.
      CALL GLOBAL_ALL_SUM(IOS)
      IF(IOS /= 0) call LOG_ERROR()

 1100 FORMAT('Error 1100: FATAL - DEM particle input file not found!')

! Read the file
!----------------------------------------------------------------->>>
! In distributed IO the first line of the file will be number of
! particles in that processor
      IF (bdist_io) then
         read(lunit,*) pip
         DO lcurpar = 1,pip
            call set_normal(lcurpar)
            read (lunit,*) (des_pos_new(lcurpar,k),k=1,RDMN),&
               des_radius(lcurpar), ro_sol(lcurpar),&
               (des_vel_new(lcurpar,k),k=1,RDMN)
         ENDDO

! Serial IO (not bDIST_IO)
      ELSE
!----------------------------------------------------------------->>>

! Read into temporary variable and scatter
         IF (myPE .eq. PE_IO) THEN

! Allocate and initialize temporary variables.
            ALLOCATE (dpar_pos(particles,3)); dpar_pos=0.0
            ALLOCATE (dpar_vel(particles,3)); dpar_vel=0.0
            ALLOCATE (dpar_rad(particles));   dpar_rad=0.0
            ALLOCATE (dpar_den(particles));   dpar_den = 0.0
! Loop through the input file.
            DO lcurpar = 1, particles
               read (lunit,*,IOSTAT=IOS)                               &
               (dpar_pos(lcurpar,k),k=1,RDMN),dpar_rad(lcurpar),       &
               dpar_den(lcurpar),(dpar_vel(lcurpar,k),k=1,RDMN)

! Report read errors.
               IF(IOS > 0) THEN
                  WRITE(ERR_MSG,1200)
                  CALL LOG_WARNING()
                  EXIT
 1200 FORMAT('Error 1200: Error reported when reading particle input ',&
         'file.',/'A common error is 2D input for 3D cases.')

! Report End-of-File errors.
               ELSEIF(IOS < 0) THEN
                  WRITE(ERR_MSG,1201) &
                     trim(iVal(lcurpar)), trim(iVal(Particles))
                  CALL LOG_WARNING()
                  EXIT
 1201 FORMAT('Error 1201: Error reported when reading particle input ',&
         'file.',/'End-of-File found for particle ',A,' and ',A,1X,    &
         'entries are expected.')

               ENDIF

            ENDDO

         ENDIF

         CALL GLOBAL_ALL_SUM(IOS)
         IF(IOS /= 0) CALL LOG_ERROR()

         CALL DES_SCATTER_PARTICLE

         IF(myPE == PE_IO) &
            deallocate (dpar_pos,dpar_vel,dpar_rad,dpar_den)

      ENDIF   ! end if/else bdist_io
!-----------------------------------------------------------------<<<

      IF(bDIST_IO .OR. myPE == PE_IO) CLOSE(lUNIT)

      RETURN

      call log_error()

   END SUBROUTINE READ_PAR_INPUT_OLD

!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv!
! Subroutine: WRITE_PART_OUTPUT_V2P0                                   !
!                                                                      !
! Purpose: Write a particle_output.dat file at the end of a simulation.!
! If the file is renamed "particle_input.dat", it can be used          !
! as initial condition for another simulation                          ! 
! Version 2.0                                                          !
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^!
      SUBROUTINE WRITE_PART_OUTPUT_V2P0

!-----------------------------------------------
! Modules
!-----------------------------------------------
      USE discretelement
      use cdist
      use compar
      use des_allocate, only: particle_grow
      use desmpi
      use desmpi_wrapper, only: DES_MPI_STOP
      use des_thermo, only: des_t_s
      use discretelement, only: des_usr_var_size
      use functions
      use funits
      use geometry, only: NO_K
      use mpi_comm_des, only: desmpi_scatterv
      use mpi_comm_des, only: desmpi_gatherv, des_gather
      use mpi_init_des, only: des_scatter_particle
      use mpi_utility
      use parallel_mpi
      use param, only: dimension_n_s
      use run, only: any_species_eq, energy_eq
      use des_rxns, only: des_x_s

      implicit none
!-----------------------------------------------
! Local variables
!-----------------------------------------------
! indices
      integer :: i,k,ilines,m,n
! index of particle
      INTEGER :: lcurpar
! local unit
      INTEGER, PARAMETER :: lunit=10
! local filename
      character(255) lfilename
! IO Status:
      INTEGER :: IOS
! Flag to indicate if file exists.
      LOGICAL :: lEXISTS
! Read dimension: 2D vs 3D data
      integer :: RDMN
! File version
      character(11) Version_string
! local and global Particle count
      integer :: local_cnt, global_cnt
! Buffer
      character(32) Buffer
! Option to read or use default value (T/F)
      character(1) option
! Integer buffer
      integer :: int_buffer
! Double precision buffer, can be a list of up to 100 numbers
      integer, dimension(100) :: dp_buffer
      logical :: is_uniform
! Length of list of variables to read (number of columns)
      integer :: n_vars
      integer :: XYZ_start
      double precision, dimension(3) :: Dummy_xyz
      integer :: Phase_ID_start
      double precision :: Uniform_Phase_ID
      integer :: Diameter_start
      double precision :: Uniform_Diameter
      integer :: Density_start
      double precision :: Uniform_Density
      integer :: Velocity_start
      double precision, dimension(3) :: Uniform_Velocity
      integer :: Temperature_start
      double precision :: Uniform_Temperature
      integer :: Species_start
      double precision, dimension(100) :: Uniform_Species
      integer :: User_scalar_start
      double precision, dimension(100) :: Uniform_User_scalar

      double precision, allocatable, dimension(:,:) :: part_data
      integer :: lproc,lbuf,lpacketsize
      integer :: lproc_parcnt(0:numpes-1)
      integer, allocatable, dimension(:) :: lpar_proc
      logical :: x_test, y_test, z_test, xyz_test
!-----------------------------------------------
      character(255):: Phase_ID_setting, Diameter_setting, Density_setting, Velocity_setting
      character(255):: Temperature_setting, Species_setting, User_scalar_setting
      character(255):: column_header
      integer :: header_bar_length
      character(255):: header_bar
      character(32) :: label

      DOUBLE PRECISION, ALLOCATABLE :: ltemp_array(:,:)
      DOUBLE PRECISION, ALLOCATABLE :: gtemp_array(:,:)  ! global
! Variables related to gather
      integer lgathercnts(0:numpes-1)

      INTEGER :: LB, UB
      INTEGER :: PC, LC1, LC2

! Particle filter
      LOGICAL, allocatable, dimension(:) :: keep_particle

      IF(.NOT.WRITE_PART_OUT)  RETURN



      WRITE(ERR_MSG, 10) 
      CALL LOG_INFO()
10    FORMAT('Info: Writing DEM particle_output.dat file, version: 2.0', &
             /'Please rename the file as "particle_input.dat" to use for', &
             /'another simulation initial condition')



      n_vars = 0  


!-----------------------------------------------

      IOS = 0
      RDMN = merge(2,3,NO_K)
      Version_string = 'Version 2.0'



! Initial the global count.
      GLOBAL_CNT = 10
! Calculate the number of 'real' particles on the local process.
      ! LOCAL_CNT = PIP - iGHOST_CNT
      LOCAL_CNT = 0

! Filter particles

      if(allocated(keep_particle)) deallocate (keep_particle)
      allocate (keep_particle(MAX_PIP))
      keep_particle(:) = .TRUE.

      DO LC1 = 1, MAX_PIP
         IF(.NOT.IS_NORMAL(LC1)) keep_particle(LC1) = .FALSE.
      ENDDO

      call filter_particle_output(keep_particle)

      DO LC1 = 1, MAX_PIP
         IF(keep_particle(LC1)) LOCAL_CNT = LOCAL_CNT + 1
      ENDDO   


! Calculate the total number of particles system-wide.
      GLOBAL_CNT=10
      call global_sum(LOCAL_CNT, GLOBAL_CNT)

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



! Coordinates option: We always write (x,y) in 2D or (x,y,z) in 3D
      column_header = "|       x       |       y       |"
      n_vars = RDMN

      if(RDMN==3) column_header = trim(column_header)//"       z       |"
       

! Phase ID option
      IF(DES_MMAX==1) THEN
         Phase_ID_setting = 'Phase_ID:        F          1' 
         Phase_ID_start   = -1
      ELSE
         column_header = trim(column_header)//"    Phase_ID   |"
         Phase_ID_setting = 'Phase_ID:        T' 
         Phase_ID_start   = n_vars + 1
         n_vars           = n_vars + 1
      ENDIF

! ! Diameter option
      call check_if_uniform(2.0D0*des_radius,is_uniform, Uniform_Diameter)
      IF(is_uniform) THEN
         write(Diameter_setting,'(A,A)') 'Diameter:        F          ', ival(Uniform_Diameter)
         Diameter_start   = -1
      ELSE
         column_header    = trim(column_header)//"    Diameter   |"
         Diameter_setting = 'Diameter:        T'
         Diameter_start   = n_vars + 1
         n_vars           = n_vars + 1
      ENDIF

! Density option
      call check_if_uniform(ro_sol,is_uniform, Uniform_Density)
      IF(is_uniform) THEN
         write(Density_setting,'(A,A)') 'Density:         F          ', ival(Uniform_Density)
         Density_start   = -1
      ELSE
         column_header   = trim(column_header)//"    Density    |"
         Density_setting = 'Density:         T'
         Density_start   = n_vars + 1
         n_vars          = n_vars + 1
      ENDIF

! Velocity option
      IF(PART_OUT_ZERO_VEL) THEN
         write(Velocity_setting,'(A,3(F3.1,2X))') 'Velocity:        F          ',(0.0, i = 1,RDMN)
         Velocity_start  = -1
      ELSE
         column_header = trim(column_header)//"       u       |       v      |"
         if(RDMN==3) column_header = trim(column_header)//"       w       |"
         Velocity_setting = 'Velocity:        T'
         Velocity_start   = n_vars + 1
         n_vars           = n_vars + RDMN
      ENDIF

! Temperature option
      IF(ENERGY_EQ) THEN
         call check_if_uniform(des_t_s,is_uniform, Uniform_Temperature)
         IF(is_uniform) THEN
            write(Temperature_setting,'(A,F14.8)') 'Temperature:     F  ', Uniform_Temperature
            Temperature_start   = -1
         ELSE
            column_header       = trim(column_header)//"  Temperature  |"
            Temperature_setting = 'temperature:     T'
            Temperature_start   = n_vars + 1
            n_vars              = n_vars + 1
         ENDIF

      ELSE
         Temperature_setting = 'Temperature:     T          (Ignored if energy eq. is not solved)'
         Temperature_start   = -1

      ENDIF

! Species option
      IF(ANY_SPECIES_EQ) THEN
         do n = 1,DIMENSION_N_S
             write(label,'     ("    x_s(",I2,")    |")    ') n
             column_header = trim(column_header)//trim(label)
         enddo
         Species_setting = 'Species:         T'
         Species_start   = n_vars + 1
         n_vars          = n_vars + DIMENSION_N_S
      ELSE
         Species_setting = 'Species:         T          (Ignored if species eq.  are not solved)'
         Species_start   = -1
      ENDIF

! User Scalar option
      IF(DES_USR_VAR_SIZE>0) THEN
         do n = 1,DES_USR_VAR_SIZE
             write(label,'     ("  usr_var(",I2,")  |")    ') n
             column_header = trim(column_header)//trim(label)
         enddo
         User_scalar_setting = 'User_Scalar:     T'
         User_scalar_start   = n_vars + 1
         n_vars              = n_vars + DES_USR_VAR_SIZE
      ELSE
         User_scalar_setting = 'User_Scalar:     T          (Ignored if no user scalars are defined)'
         User_scalar_start   = -1
      ENDIF


      ! IF(myPE == PE_IO) THEN
      !    print*,'n_vars            = ',n_vars
      !    print*,'Phase_ID_start    = ',Phase_ID_start
      !    print*,'Diameter_start    = ',Diameter_start
      !    print*,'Density_start     = ',Density_start
      !    print*,'Velocity_start    = ',Velocity_start
      !    print*,'Temperature_start = ',Temperature_start
      !    print*,'Species_start     = ',Species_start
      !    print*,'User_scalar_start = ',User_scalar_start
      ! ENDIF


! Setup the file name
      lFILENAME= "particle_output.dat"

! Check the that file exists and open it.
! Write header of the file
      IF(myPE == PE_IO) THEN


         header_bar_length = max(len(trim(column_header)),72)
         header_bar = ''
         do n = 1,header_bar_length
            header_bar = trim(header_bar)//'='
         enddo


         OPEN(UNIT=lUNIT, FILE=lFILENAME, FORM="FORMATTED")
         write(lUNIT,'(A11)') Version_string

         write(lUNIT,100) ival(RDMN), ival(GLOBAL_CNT), &
                          trim(Phase_ID_setting), &
                          trim(Diameter_setting), &
                          trim(Density_setting),  &
                          trim(Velocity_setting), &
                          trim(Temperature_setting), &
                          trim(Species_setting), &
                          trim(User_scalar_setting), &
                          trim(header_bar), &
                          trim(column_header), &
                          trim(header_bar)



 100 FORMAT('========================================================================', &
           /'Instructions:', &
           /'Dimension: Enter "2" for 2D, "3" for 3D (Integer)', &
           /'Particles: Number of particles to read from this file (Integer)', &
           /'For each variable, enter whether it will be read from the file', &
           /'("T" to be read (True), "F" to not be read (False)). When not read, enter the', &
           /'default values assign to all particles.', &
           /'Coordinates are always read (X,Y in 2D, X,Y,Z in 3D)', &
           /'Phase_ID, Diameter, Density, Temperature are scalars', &
           /'Velocity requires U,V in 2D, U,V,W in 3D', &
           /'Temperature is only read/set if the energy equation is solved', &
           /'Species are only read/set if the species equations are solved, and requires' , &
           /'all species to be set (if phase 1 has 2 species and phase 2 has 4 species,', &
           /'all particles must have 4 entries, even phase 1 particles (pad list with zeros)', &
           /'User scalars need DES_USR_VAR_SIZE values', &
           /'Data starts at line 35. Each column correspond to a variable.', &
           /'========================================================================', &
           /'Dimension:    ',A, &
           /'Particles:    ',A, &
           /'========================================================================', &
           /'Variable      Read(T/F)     Default value (when Read=F)' &
           /'========================================================================', &
           /'Coordinates:     T          Must always be T', &
           /A, &  ! Phase ID
           /A, &  ! Diameter
           /A, &  ! Density
           /A, &  ! Velocity
           /A, &  ! Temperature
           /A, &  ! Species
           /A, &  ! User scalar
           /A, &  ! header bar
           /A, & ! column header
           /A)   ! header bar

      ENDIF


      ALLOCATE (dProcBuf(LOCAL_CNT) )
      ALLOCATE (ltemp_array(n_vars,LOCAL_CNT))

      IF(myPE == PE_IO) THEN
         ALLOCATE (dRootBuf(GLOBAL_CNT))
         ALLOCATE (gtemp_array(n_vars,GLOBAL_CNT))
      ELSE
         ALLOCATE (dRootBuf(10))
         ALLOCATE (gtemp_array(n_vars,10))
      ENDIF

! For each vector component, pack component list in a local array
      PC = 0
      DO LC1 = 1, MAX_PIP
         IF(.NOT.KEEP_PARTICLE(LC1)) CYCLE
!         IF(.NOT.IS_NORMAL(LC1)) CYCLE
!         ! IF(IS_NONEXISTENT(LC1)) CYCLE
!         ! IF(IS_GHOST(LC1) .OR. IS_ENTERING_GHOST(LC1) .OR. IS_EXITING_GHOST(LC1)) CYCLE

         PC =PC + 1

! Coordinates (x,y) in 2D, (x,y,z) in 3D
         DO LC2=1,RDMN
            ltemp_array(LC2,PC) = DES_POS_NEW(LC1,LC2)
         ENDDO
! Phase ID      
         IF(Phase_ID_start>0) THEN
            ltemp_array(Phase_ID_start,PC) = dfloat(PIJK(LC1,5))
         ENDIF

! Diameter
         IF(Diameter_start>0) THEN
            ltemp_array(Diameter_start,PC) = 2.0D0*DES_RADIUS(LC1)
         ENDIF

! Density
         IF(Density_start>0) THEN
            ltemp_array(Density_start,PC) = ro_sol(LC1)
         ENDIF

! Velocity
         IF(Velocity_start>0) THEN
            DO LC2=1,RDMN
               ltemp_array(Velocity_start+LC2-1,PC) = DES_VEL_NEW(LC1,LC2)
            ENDDO
         ENDIF

! Temperature
         IF(Temperature_start>0) THEN
            ltemp_array(Temperature_start,PC) = des_t_s(LC1)
         ENDIF

! Species
         IF(Species_start>0) THEN
            DO LC2=1,DIMENSION_N_S
               ltemp_array(Species_start+LC2-1,PC) = des_x_s(LC1,LC2)
            ENDDO
         ENDIF

! Particle Scalar: CAUTION THIS ARRAY IS TRANSPOSED, FIRST INDEX IS USR_VAR ID, SECOND INDEX IS PARTICLE ID
         IF(User_scalar_start>0) THEN
            DO LC2=1,DES_USR_VAR_SIZE
               ltemp_array(User_scalar_start+LC2-1,PC) = des_usr_var(LC2,LC1)
            ENDDO
         ENDIF

      ENDDO ! particle loop


! For each component, gather the local list to global temporary array
      DO LC1 = 1,n_vars
         dprocbuf(1:LOCAL_CNT)=ltemp_array(LC1,1:LOCAL_CNT)
         CALL desmpi_gatherv(ptype=2)
         gtemp_array(LC1,:) = drootbuf(:)
      ENDDO

! Write the data, always preceded by its size in number of bytes
      IF(myPE == PE_IO) THEN
         DO LC1=1, GLOBAL_CNT
            IF(Phase_ID_start==3) THEN
               WRITE(lUNIT,1000)  gtemp_array(1:2,LC1), int(gtemp_array(3,LC1)),(gtemp_array(LC2,LC1), LC2=4,n_vars)
            ELSEIF(Phase_ID_start==4) THEN
               WRITE(lUNIT,1010)  gtemp_array(1:3,LC1), int(gtemp_array(4,LC1)),(gtemp_array(LC2,LC1), LC2=5,n_vars)
            ELSE
               WRITE(lUNIT,1020)  (gtemp_array(LC2,LC1), LC2=1,n_vars)
            ENDIF
         ENDDO
      ENDIF

1000  FORMAT(2(E16.8),6X,I3,7X,100(E16.8))
1010  FORMAT(3(E16.8),6X,I3,7X,100(E16.8))
1020  FORMAT(100(E16.8))

      deallocate (dProcBuf, dRootBuf, ltemp_array, gtemp_array)

      IF(myPE == PE_IO) CLOSE(lUNIT)

      RETURN

      ! call log_error()


      contains

      subroutine check_if_uniform(data,uniform, uniform_value)

      USE param1, only: undefined

      DOUBLE PRECISION, INTENT(in) :: DATA(:)
      LOGICAL, INTENT(OUT) :: uniform
      DOUBLE PRECISION, INTENT(OUT) :: uniform_value
      DOUBLE PRECISION :: lmin,lmax,gmin,gmax
      DOUBLE PRECISION, PARAMETER :: tol = 1.0D-6
      INTEGER :: NP

      lmin =  UNDEFINED
      lmax = -UNDEFINED
      DO NP=1,MAX_PIP                                                                                                                                      
         IF(.NOT.IS_NORMAL(NP)) CYCLE
         lmin = MIN(lmin,DATA(NP))
         lmax = MAX(lmax,DATA(NP))
      ENDDO

      call global_all_min(lmin, gmin)
      call global_all_max(lmax, gmax)

      if(dabs(gmax-gmin)<tol) then
         uniform = .True.
         uniform_value = gmin
      else
         uniform = .false.
         uniform_value = undefined
      endif

      return

      end subroutine check_if_uniform


      subroutine filter_particle_output(keep_particle)

      USE param1, only: undefined

      LOGICAL, INTENT(inout) :: KEEP_PARTICLE(:)
      INTEGER :: n,NP,M
      character(32) :: label

      DO NP=1,MAX_PIP
         IF(.NOT.IS_NORMAL(NP)) KEEP_PARTICLE(NP)=.FALSE.
! phase id
         M = PIJK(NP,5)
         IF(.NOT.part_out_phase(M)) KEEP_PARTICLE(NP)=.FALSE.
      ENDDO

! x, y and z-coordinate
      call filter_based_on_min_max_var(des_pos_new(:,1),'x-coordinate',part_out_x_min,part_out_x_max,part_out_x_exclude,keep_particle)
      call filter_based_on_min_max_var(des_pos_new(:,2),'y-coordinate',part_out_y_min,part_out_y_max,part_out_y_exclude,keep_particle)
      call filter_based_on_min_max_var(des_pos_new(:,3),'z-coordinate',part_out_z_min,part_out_z_max,part_out_z_exclude,keep_particle)


      ! call filter_based_on_phase_id('Phase ID',part_out_phase,keep_particle)

      call filter_based_on_min_max_var(2.0D0*des_radius,'Diameter',part_out_diameter_min,part_out_diameter_max,part_out_diameter_exclude,keep_particle)
      call filter_based_on_min_max_var(ro_sol,'Density',part_out_density_min,part_out_density_max,part_out_density_exclude,keep_particle)

      call filter_based_on_min_max_var(des_vel_new(:,1),'x-velocity',part_out_u_min,part_out_u_max,part_out_u_exclude,keep_particle)
      call filter_based_on_min_max_var(des_vel_new(:,2),'y-velocity',part_out_v_min,part_out_v_max,part_out_v_exclude,keep_particle)
      call filter_based_on_min_max_var(des_vel_new(:,3),'z-velocity',part_out_w_min,part_out_w_max,part_out_w_exclude,keep_particle)

      IF(ENERGY_EQ) THEN
         call filter_based_on_min_max_var(des_t_s,'Temperature',part_out_temp_min,part_out_temp_max,part_out_temp_exclude,keep_particle)
      ENDIF

      IF(ANY_SPECIES_EQ) THEN
         do n = 1,DIMENSION_N_S
             write(label,'("Species",I2)') n
             call filter_based_on_min_max_var(des_x_s(:,n),label,part_out_x_s_min(n),part_out_x_s_max(n),part_out_x_s_exclude(n),keep_particle)
         enddo
      ENDIF

! Particle Scalar: CAUTION THIS ARRAY IS TRANSPOSED, FIRST INDEX IS USR_VAR ID, SECOND INDEX IS PARTICLE ID
      IF(DES_USR_VAR_SIZE>0) THEN
         do n = 1,DES_USR_VAR_SIZE
             write(label,'("User scalar # ",I2)') n
             call filter_based_on_min_max_var(des_usr_var(n,:),label,part_out_usr_var_min(n),part_out_usr_var_max(n),part_out_usr_var_exclude(n),keep_particle)
         enddo
      ENDIF


      return

      end subroutine filter_particle_output


   END SUBROUTINE WRITE_PART_OUTPUT_V2P0


      subroutine filter_based_on_min_max_var(data,label,min_value,max_value,exclude_within_range,keep_particle)

      USE discretelement
      USE param1, only: undefined

      DOUBLE PRECISION, INTENT(in) :: DATA(:)
      CHARACTER(LEN=*), INTENT(in) :: label
      DOUBLE PRECISION, INTENT(in) :: min_value,max_value
      LOGICAL, INTENT(in) :: exclude_within_range
      LOGICAL, INTENT(inout) :: KEEP_PARTICLE(:)
      INTEGER :: NP

! Skip filtering altogether if min and max values are undefined.
! This leaves KEEP_PARTICLE unchanged

      IF(min_value==-UNDEFINED.and.max_value==UNDEFINED) RETURN

      WRITE(ERR_MSG, 100) trim(label)
      CALL LOG_INFO()

 100 FORMAT('Info: Filtering DEM particle_[in/out]put.dat file based on: ', A)
! if one of min_value or max_value is defined, filter the data.
! If keep_within_range is .True., we keep particles if   min_value<=data<=max_value
! If keep_within_range is .False., we keep particles if  data<min_value  and  data>max_value
! Note how this is coded differently since we use the flag KEEP_PARTICLE.

      IF(exclude_within_range) then
         DO NP=1,MAX_PIP
            IF(.NOT.KEEP_PARTICLE(NP)) CYCLE
            IF(min_value<=DATA(NP).AND.DATA(NP)<=max_value) KEEP_PARTICLE(NP)=.FALSE.
         ENDDO
      ELSE
         DO NP=1,MAX_PIP
            IF(.NOT.KEEP_PARTICLE(NP)) CYCLE
            IF(DATA(NP)<min_value.OR.DATA(NP)>max_value) KEEP_PARTICLE(NP)=.FALSE.
         ENDDO
      ENDIF

      return

      end subroutine filter_based_on_min_max_var

!       subroutine filter_based_on_phase_id(label,keep_phase,keep_particle)
!
!       USE discretelement
!       USE param1, only: undefined
!
!       CHARACTER(LEN=*), INTENT(in) :: label
!       LOGICAL, INTENT(in) :: keep_phase(:)
!       LOGICAL, INTENT(inout) :: KEEP_PARTICLE(:)
!       INTEGER :: NP, M
!
!
!       WRITE(ERR_MSG, 100) trim(label)
!       CALL LOG_INFO()
!
!  100 FORMAT('Info: Filtering DEM particle_[in/out]put.dat file based on: ', A)
!
! ! Only keep particles that belong to the phases having keep_phase=.True.
!
!       DO NP=1,MAX_PIP
!          M = PIJK(:,5)
!          IF(.NOT.keep_phase(M)) KEEP_PARTICLE(NP)=.FALSE.
!       ENDDO
!
!       return
!
!       end subroutine filter_based_on_phase_id

      subroutine filter_single_particle_based_on_min_max_var(data,label,min_value,max_value,exclude_within_range,keep_particle)

      USE discretelement
      USE param1, only: undefined

      DOUBLE PRECISION, INTENT(in) :: DATA
      CHARACTER(LEN=*), INTENT(in) :: label
      DOUBLE PRECISION, INTENT(in) :: min_value,max_value
      LOGICAL, INTENT(in) :: exclude_within_range
      LOGICAL, INTENT(inout) :: KEEP_PARTICLE
      INTEGER :: NP

! Single particle version, used when reading particle_input.dat file
! Skip filtering altogether if min and max values are undefined.
! This leaves KEEP_PARTICLE unchanged

      IF(min_value==-UNDEFINED.and.max_value==UNDEFINED) RETURN
      IF(.NOT.KEEP_PARTICLE) RETURN

!      WRITE(ERR_MSG, 100) trim(label)
!      CALL LOG_INFO()

! 100 FORMAT('Info: Filtering DEM particle_[in/out]put.dat file based on: ', A)
! if one of min_value or max_value is defined, filter the data.
! If keep_within_range is .True., we keep particles if   min_value<=data<=max_value
! If keep_within_range is .False., we keep particles if  data<min_value  and  data>max_value
! Note how this is coded differently since we use the flag KEEP_PARTICLE.

      IF(exclude_within_range) then
            IF(min_value<=DATA.AND.DATA<=max_value) KEEP_PARTICLE=.FALSE.
      ELSE
            IF(DATA<min_value.OR.DATA>max_value) KEEP_PARTICLE=.FALSE.
      ENDIF

      return

      end subroutine filter_single_particle_based_on_min_max_var

      subroutine check_particle_input_filter_with_V1

      USE discretelement
      USE param1, only: undefined
      use param, only: dimension_n_s
      use run, only: energy_eq

      INTEGER :: M,N
      LOGICAL :: PART_IN_FILTER

! If PART_IN_* are not the default values and we use version 1.0 of
! particle_input.dat, flag this as a fatal error.

      PART_IN_FILTER = .FALSE.
      IF(PART_IN_X_MIN/=-UNDEFINED.OR.PART_IN_X_MAX/=UNDEFINED.OR.PART_IN_X_EXCLUDE) PART_IN_FILTER = .TRUE.
      IF(PART_IN_Y_MIN/=-UNDEFINED.OR.PART_IN_Y_MAX/=UNDEFINED.OR.PART_IN_Y_EXCLUDE) PART_IN_FILTER = .TRUE.
      IF(PART_IN_Z_MIN/=-UNDEFINED.OR.PART_IN_Z_MAX/=UNDEFINED.OR.PART_IN_Z_EXCLUDE) PART_IN_FILTER = .TRUE.
      DO M = 1,DES_MMAX
         IF(.NOT.PART_IN_PHASE(M)) PART_IN_FILTER = .TRUE.
      END DO
      IF(PART_IN_DIAMETER_MIN/=-UNDEFINED.OR.PART_IN_DIAMETER_MAX/=UNDEFINED.OR.PART_IN_DIAMETER_EXCLUDE) PART_IN_FILTER = .TRUE.
      IF(PART_IN_DENSITY_MIN/=-UNDEFINED.OR.PART_IN_DENSITY_MAX/=UNDEFINED.OR.PART_IN_DENSITY_EXCLUDE) PART_IN_FILTER = .TRUE.
      IF(PART_IN_U_MIN/=-UNDEFINED.OR.PART_IN_U_MAX/=UNDEFINED.OR.PART_IN_U_EXCLUDE) PART_IN_FILTER = .TRUE.
      IF(PART_IN_V_MIN/=-UNDEFINED.OR.PART_IN_V_MAX/=UNDEFINED.OR.PART_IN_V_EXCLUDE) PART_IN_FILTER = .TRUE.
      IF(PART_IN_W_MIN/=-UNDEFINED.OR.PART_IN_W_MAX/=UNDEFINED.OR.PART_IN_W_EXCLUDE) PART_IN_FILTER = .TRUE.
      IF(ENERGY_EQ) THEN
         IF(PART_IN_TEMP_MIN/=-UNDEFINED.OR.PART_IN_TEMP_MAX/=UNDEFINED.OR.PART_IN_TEMP_EXCLUDE) PART_IN_FILTER = .TRUE.
      ENDIF
      DO N = 1,DIMENSION_N_S
         IF(PART_IN_X_S_MIN(N)/=-UNDEFINED.OR.PART_IN_X_S_MAX(N)/=UNDEFINED.OR.PART_IN_X_S_EXCLUDE(N)) PART_IN_FILTER = .TRUE.
      ENDDO
      DO N = 1,DES_USR_VAR_SIZE
         IF(PART_IN_USR_VAR_MIN(N)/=-UNDEFINED.OR.PART_IN_USR_VAR_MAX(N)/=UNDEFINED.OR.PART_IN_USR_VAR_EXCLUDE(N)) PART_IN_FILTER = .TRUE.
      ENDDO

      IF(PART_IN_FILTER.AND.P_INPUT_DAT_VERSION == '1.0') THEN
         WRITE(ERR_MSG, 100)
         CALL LOG_ERROR()
      ENDIF

100 FORMAT('ERROR 100: Filtering particle_input.dat is not supported for version 1.0.', &
          /'Please use particle_input.dat version 2.0 or above', &
          /'or remove all filtering parameters to keep using version 1.0.')

      RETURN

      end subroutine check_particle_input_filter_with_V1
END MODULE READ_PAR_INPUT_MOD
