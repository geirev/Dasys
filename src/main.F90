program main
   use iso_c_binding
!   use iso_fortran_env

   use mod_dimensions
   use mod_measurements
   use m_ansi_colors
   use m_assimilation
   use m_average_uvw
   use m_check_input_files
   use m_checkensrun
   use m_cline
   use m_count_parameters
   use m_diag
   use m_get_observed_measurements
   use m_get_predicted_measurements
   use m_iniens
   use m_input_files_def
   use m_localdefs
   use m_loaduvw
   use m_normal
   use m_params
   use m_read_dasys_file
   use m_read_parameters
   use m_ref_uvw
   use m_resetensemble
   use m_runensemble
   use m_runmodel

   implicit none

   interface
   subroutine sleep(seconds) bind(C, name="sleep")
      import :: c_int
      integer(c_int), value :: seconds
   end subroutine sleep
   end interface



   character(len=3) comm

   logical :: lyobs=.false.
   logical :: lmeas=.false.
   logical :: liniens=.false.
   logical :: lensrun=.false.
   logical :: lreference=.false.
   logical :: luvwloaded=.false.
   integer nr,m,j,i !,itmp,jtmp,reclA

   integer :: nrobs=0    ! Number of measurements
   integer :: nrens=0    ! Number of ensemble realizations
   integer :: ndim=0     ! Number of uncertain parameters (n in EnKF code)

   integer :: istep=1000
   integer :: it=1

   integer :: mode=11
   integer :: iunit
   logical :: ex
   integer nrensold
   character(len=6) cistep
   character(len=100) directory

   type(uncertain_parameters), allocatable :: params(:)

   real, allocatable, dimension(:)    :: mean
   real, allocatable, dimension(:,:)  :: Y,D,E,A,S,O

   real(kind=4), allocatable :: u(:,:,:,:)
   real(kind=4), allocatable :: v(:,:,:,:)
   real(kind=4), allocatable :: w(:,:,:,:)

   integer(kind=1), allocatable :: blanking(:,:,:)
   real(kind=4), allocatable :: uave(:,:,:)
   real(kind=4), allocatable :: vave(:,:,:)
   real(kind=4), allocatable :: wave(:,:,:)
   real(kind=4), allocatable :: velave(:,:,:)

   real(kind=4), allocatable :: ustd(:,:,:)
   real(kind=4), allocatable :: vstd(:,:,:)
   real(kind=4), allocatable :: wstd(:,:,:)
   real(kind=4), allocatable :: velstd(:,:,:)



   call get_environment_variable("PWD", workdir)
   print *, "Current working directory: <", trim(workdir), ">"

  !call set_random_seed2()

   call check_input_files()

   ndim= count_parameters(parameter_file)

   ! Menu constructs
   comm=' '
   do
! Reading the model run command
      call read_dasys_file(model_run_command_file,runcommand)

! Reading the ensemble size
      call read_dasys_file(model_ensemble_size_file,nrens)
      if (nrens /= nrensold) then
         if (allocated(u)) deallocate(u)
         if (allocated(v)) deallocate(v)
         if (allocated(w)) deallocate(w)
         luvwloaded=.false.
      endif

      if (.not. allocated(u)) allocate(u(nx,ny,nz,0:nrens))
      if (.not. allocated(v)) allocate(v(nx,ny,nz,0:nrens))
      if (.not. allocated(w)) allocate(w(nx,ny,nz,0:nrens))
      nrensold=nrens

! Reading the experiment name
      call read_dasys_file(experiment_name_file,experiment)

! Calulating and reading the number of uncertain parameters
      nr=ndim
      ndim= count_parameters(parameter_file)
      if (nr /= ndim) then
         print *,'nr, ndim',nr,ndim
         if (allocated(params)) deallocate(params)
         if (allocated(A))      deallocate(A)
         call system('rm -f iniens.uf')
      endif

      if (ndim > 0) then
         if (.not.allocated(params)) allocate(params(ndim))
         call  read_parameters(parameter_file,params,ndim)

         if (.not.allocated(A)) allocate(A(ndim,nrens))

!         inquire(file='iniens.uf',exist=ex)
!         if (ex) then
!            inquire(iolength=reclA)ndim,nrens,A(1,:)
!            open(newunit=iunit, file='iniens.uf', form='unformatted', access='direct', recl=reclA)
!               do j=1,nrens
!                  read(iunit,rec=j)itmp,jtmp,A(:,j)
!               enddo
!            close(iunit)
!            liniens=.true.
!         endif
      endif

! Fetching measurement locations
      open(newunit=iunit,file='measurement_loc.in',status='old',action='read')
         m=0
         do
           read(iunit,*,end=200)i
           m=m+1
         enddo
         200 nrobs=m
      close(iunit)

! Checking if reference case has run
      lreference=.false.
      directory=trim(experiment)//'/mem0000'
      write(cistep,'(i6.6)')istep
      inquire(file=trim(directory)//'/uvw'//cistep//'.uf',exist=lreference)

! Checking if initial ensemble has been run
      lensrun=.false.
      call checkensrun(u,v,w,lensrun,nrens,1,istep)


      print *
      print '(a)',                '    ##########################################################################################'
      print '(a)',                '    #   DASYS for running data assimilation experiments                             #  help  #'
      print '(a)',                '    #   Author: Geir.Evensen@gmail.com                                              #  cmds  #'
      print '(a)',                '    ##########################################################################################'
      print '(a)',                '    # Initialization                                                                #        #'
      print '(a)',                '    #   a: Open infile.in using vi                                                  #        #'
      print '(a)',                '    #   b: Open infile.X  using vi                                                  #        #'
      call cline(trim(runcommand),'        c: Define the run command for the model')
      call cline(nrens,           '        d: Set ensemble size')
      call cline(trim(experiment),'        e: Experiment name')
      call cline(ndim,            '        f: Define uncertain parameters')
      call cline(nrobs,           '        g: Define observations')
      print '(a)',                '    ##########################################################################################'
      print '(a)',                '    # Model runs                                                                    #        #'
      call cline(lreference,      '        1: Run single realization using default infile.in')
      call cline(liniens,         '        2: Compute initial ensemble')
      call cline(lensrun,         '        3: Run ensemble of realizations')
      call cline(lyobs,           '        4: Fetch ensemble of predicted measurements')
      call cline(lmeas,           '        5: Fetch observations from reference')
      print '(a)',                '    #   6: Compute ES update                                                        #        #'
      print '(a)',                '    #   7: Run posterior ES ensemble                                                #        #'
      print '(a)',                '    #   8: Compute and dump diagnostics                                             #        #'


      print '(a)',                '    ##########################################################################################'
      print '(a)',                '    # Running assimilation experiment                                               #        #'
      print '(a)',                '    #  10: Run full ES experiment                                                   #        #'
      print '(a)',                '    ##########################################################################################'
      print '(a)',                '    #  11: Diagnostics                                                              #        #'
      print '(a)',                '    ##########################################################################################'
      print '(a)',                '    # Special commands                                                              #        #'
      print '(a)',                '    #   S: Swap or replace an ensemble member                                       #        #'
      print '(a)',                '    #   h: Help                                                                     #        #'
      print '(a)',                '    #   q: Quit                                                                     #        #'
      print '(a)',                '    ##########################################################################################'
      write(*,'(a)',advance='no')'Command: '
      read(*,*)comm

      select case (trim(comm))
      case('q')
         exit

      case default
         print '(a)','Invalid command: try again!'

      case('a')
         call system('vim infile.in')

      case('b')
         call system('vim infile.X')

      case('c')
         print '(a)','Write the model run command in the following file (return to continue)'
         call system('vim '//trim(model_run_command_file))

      case('hb')
         !call printhelp('poroperm.txt')

      case('d')
         print '(a)','Give the ensemble size'
         call sleep(1)
         nrensold=nrens
         call system('vi '//trim(model_ensemble_size_file))

      case('e')
         print '(a)','Choose the experiment name'
         call sleep(1)
         call system('vi '//trim(experiment_name_file))

      case('f')
         print '(a)','Define the uncertain parameters in the follwing format'
         print '(a)','#parameter  mean   standard-deviation, e.g.           '
         print '(a)','#speed       1.0    1.0                               '
         print '(a)','(return to continue)                                  '
         read(*,*)
         call system('vi '//trim(parameter_file))

      case('g')
         call system('vim measurement_loc.in')
         inquire(file='measurement_loc.in',exist=ex)
         if (.not.ex) then
            print *,'The file measurement_loc.in does not exist'
            print *,'You need to define it to run an assimilation experiment'
            print *,'format: i-loc  j-loc type(u,v) one line per measurment'

         else
            open(newunit=iunit,file='measurement_loc.in',status='old',action='read')
               m=0
               do
                 read(iunit,*,end=100)i
                 m=m+1
            print *,m
               enddo
               100 nrobs=m
            close(iunit)
         endif


      case('1')
         call runmodel(u,v,w,istep,lreference)

      case('2')
         if (ndim > 0) then
            call iniens(A,nrens,params,ndim)
            liniens=.true.
            lyobs=.false.
         else
            print *,'First define uncertain parameters (f).'
         endif


      case('3')
         if (liniens) then
            call runensemble(nrens,params,ndim,1,A)
            lyobs=.false.
            lensrun=.false.
         else
            print *,'ensemble is not initialized, run iniens(2) first.'
         endif

      case('4')
         call get_predicted_measurements(lyobs,nrobs, nrens, 1, 1000)
         if (allocated(Y))    deallocate(Y)   ; allocate(Y(nrobs,nrens))
         if (allocated(S))    deallocate(S)   ; allocate(S(nrobs,nrens))
         if (allocated(mean)) deallocate(mean); allocate(mean(nrobs))
         mean=0.0
         do j=1,nrens
         do m=1,nrobs
            Y(m,j)= yensemble(m,j)
            mean(m)=mean(m)+Y(m,j)
         enddo
         enddo

         mean=mean/real(nrens)
         do j=1,nrens
         do m=1,nrobs
            S(m,j)= Y(m,j)-mean(m)
         enddo
         enddo

      case('5')
         call get_observed_measurements(nrobs,1000,lmeas)
         if (allocated(E)) deallocate(E); allocate(E(nrobs,nrens))
         if (allocated(D)) deallocate(D); allocate(D(nrobs,nrens))
         if (allocated(O))    deallocate(O)   ; allocate(O(nrobs,nrens))
         do j=1,nrens
         do m=1,nrobs
            E(m,j)=normal()*0.01*abs(obs(m)%d)
            D(m,j)=obs(m)%d + E(m,j)
            O(m,j)=D(m,j)-Y(m,j)
         enddo
         enddo

      case('6') ! ES update
         if (.not. liniens) print '(a)','First generate or initialize initial ensemble'; call sleep(1)
         if (.not. lyobs)   print '(a)','First load observations'                      ; call sleep(1)
         if (.not. lmeas)   print '(a)','First load predicted measurements'            ; call sleep(1)
         if (liniens .and. lyobs .and. lmeas) then
            print '(a)','Calling assimilation'

            mode=13
            print '(a,10f10.4)','Pri A: ',A(:,1:5)
            call assimilation(ndim,nrens,nrobs,A,O,E,Y,S,mode)
            print '(a,10f10.4)','Pos A: ',A(:,1:5)
         endif

      case('7') ! Run posterior ensemble
            call runensemble(nrens,params,ndim,2,A)

      case('8') ! Dump diagnostics
         write(*,'(a)',advance='no')'Choose iteration (1-prior, 2/N-posterior): '
         read(*,*)it

         call loaduvw(u,v,w,luvwloaded,nrens,it,istep)

            if (luvwloaded) then
               if (.not. allocated(velave)) allocate(velave(nx,ny,nz))
               if (.not. allocated(velstd)) allocate(velstd(nx,ny,nz))
               if (.not. allocated(uave)) allocate(uave(nx,ny,nz))
               if (.not. allocated(vave)) allocate(vave(nx,ny,nz))
               if (.not. allocated(wave)) allocate(wave(nx,ny,nz))
               if (.not. allocated(ustd)) allocate(ustd(nx,ny,nz))
               if (.not. allocated(vstd)) allocate(vstd(nx,ny,nz))
               if (.not. allocated(wstd)) allocate(wstd(nx,ny,nz))

               call average_uvw(u,v,w,uave,vave,wave,velave,ustd,vstd,wstd,velstd,nrens)

! Printing grid
               if (.not. allocated(blanking)) allocate(blanking(nx,ny,nz))
               blanking=0
               call diag(1,istep,it,blanking,uave,vave,wave,velave,ustd,vstd,wstd,velstd)

! Printing ensemble averages and standard deviations
               call diag(2,istep,it,blanking,uave,vave,wave,velave,ustd,vstd,wstd,velstd)

! Printing reference solution
               call ref_uvw(u,v,w,uave,vave,wave,velave,ustd,vstd,wstd,velstd,nrens)
               call diag(2,istep,0,blanking,uave,vave,wave,velave,ustd,vstd,wstd,velstd)

         endif
      case('10') ! Run full experiment from scratch
         print '(a)','Running reference simulation starting from infile.ref.'
         call runmodel(u,v,w,istep,lreference)

         print '(a)','Initializing the ensemble of uncertain parameters.'
         call iniens(A,nrens,params,ndim)

         print '(a)','Running prior ensemble.'
         call runensemble(nrens,params,ndim,1,A)


         print '(a)','Assembling predicted measurements.'
         call get_predicted_measurements(lyobs,nrobs, nrens, 1, 1000)
         if (allocated(Y))    deallocate(Y)   ; allocate(Y(nrobs,nrens))
         if (allocated(S))    deallocate(S)   ; allocate(S(nrobs,nrens))
         if (allocated(mean)) deallocate(mean); allocate(mean(nrobs))
         mean=0.0
         do j=1,nrens
         do m=1,nrobs
            Y(m,j)= yensemble(m,j)
            mean(m)=mean(m)+Y(m,j)
         enddo
         enddo

         mean=mean/real(nrens)
         do j=1,nrens
         do m=1,nrobs
            S(m,j)= Y(m,j)-mean(m)
         enddo
         enddo


         print '(a)','Getting observed measurements.'
         call get_observed_measurements(nrobs,1000,lmeas)
         if (allocated(E)) deallocate(E); allocate(E(nrobs,nrens))
         if (allocated(D)) deallocate(D); allocate(D(nrobs,nrens))
         if (allocated(O))    deallocate(O)   ; allocate(O(nrobs,nrens))
         do j=1,nrens
         do m=1,nrobs
            E(m,j)=normal()*0.01*abs(obs(m)%d)
            D(m,j)=obs(m)%d + E(m,j)
            O(m,j)=D(m,j)-Y(m,j)
         enddo
         enddo

         print '(a)','ES assimilation step.'
         mode=13
         call assimilation(ndim,nrens,nrobs,A,O,E,Y,S,mode)

         print '(a)','Run posterior ensemble'
         call runensemble(nrens,params,ndim,2,A)

         print *,'Diagnoistics:'
         do it=1,2

            call loaduvw(u,v,w,luvwloaded,nrens,it,istep)

            if (luvwloaded) then
               if (.not. allocated(velave)) allocate(velave(nx,ny,nz))
               if (.not. allocated(velstd)) allocate(velstd(nx,ny,nz))
               if (.not. allocated(uave)) allocate(uave(nx,ny,nz))
               if (.not. allocated(vave)) allocate(vave(nx,ny,nz))
               if (.not. allocated(wave)) allocate(wave(nx,ny,nz))
               if (.not. allocated(ustd)) allocate(ustd(nx,ny,nz))
               if (.not. allocated(vstd)) allocate(vstd(nx,ny,nz))
               if (.not. allocated(wstd)) allocate(wstd(nx,ny,nz))

               call average_uvw(u,v,w,uave,vave,wave,velave,ustd,vstd,wstd,velstd,nrens)

! Printing grid
               if (.not. allocated(blanking)) allocate(blanking(nx,ny,nz))
               blanking=0
               call diag(1,istep,it,blanking,uave,vave,wave,velave,ustd,vstd,wstd,velstd)

! Printing ensemble averages and standard deviations
               call diag(2,istep,it,blanking,uave,vave,wave,velave,ustd,vstd,wstd,velstd)

! Printing reference solution
               call ref_uvw(u,v,w,uave,vave,wave,velave,ustd,vstd,wstd,velstd,nrens)
               call diag(2,istep,0,blanking,uave,vave,wave,velave,ustd,vstd,wstd,velstd)

            endif
         enddo

      end select


   enddo

end program

