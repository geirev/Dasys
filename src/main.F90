program main
   use iso_c_binding
!   use iso_fortran_env

   use mod_dimensions
   use mod_measurements
   use m_readinfile

   use m_ansi_colors

   use m_assimilation
   use m_average_uvw
   use m_check_input_files
   use m_count_parameters
   use m_count_measurements
   use m_diag
   use m_get_observed_measurements
   use m_get_predicted_measurements
   use m_iniens
   use m_input_files_def
   use m_loaduvw
   use m_normal
   use m_params
   use m_read_parameters
   use m_ref_uvw
   use m_runensemble
   use m_runmodel
   use m_tecens

   implicit none

   interface
   subroutine sleep(seconds) bind(C, name="sleep")
      import :: c_int
      integer(c_int), value :: seconds
   end subroutine sleep
   end interface




   logical :: lyobs=.false.
   logical :: lmeas=.false.
   logical :: liniens=.false.
   logical :: lreference=.false.
   logical :: luvwloaded=.false.
   integer m,j


   integer :: istep=1000
   integer :: it=1

   integer :: mode=13

   real, allocatable :: ref(:)
   character(len=200) :: cmd
   character(len=100) :: tmpfile

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



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Initialization
  !call set_random_seed2()

   call readinfile()

   call check_input_files()

! Calulating and reading the number of uncertain parameters "ndim" and allocating them
   ndim= count_parameters(parameter_file)

! allocate ensemble of model solution variables
   if (.not. allocated(u)) allocate(u(nx,ny,nz,0:nrens))
   if (.not. allocated(v)) allocate(v(nx,ny,nz,0:nrens))
   if (.not. allocated(w)) allocate(w(nx,ny,nz,0:nrens))

! Allocate statistics variables
   if (.not. allocated(velave)) allocate(velave(nx,ny,nz))
   if (.not. allocated(velstd)) allocate(velstd(nx,ny,nz))
   if (.not. allocated(uave)) allocate(uave(nx,ny,nz))
   if (.not. allocated(vave)) allocate(vave(nx,ny,nz))
   if (.not. allocated(wave)) allocate(wave(nx,ny,nz))
   if (.not. allocated(ustd)) allocate(ustd(nx,ny,nz))
   if (.not. allocated(vstd)) allocate(vstd(nx,ny,nz))
   if (.not. allocated(wstd)) allocate(wstd(nx,ny,nz))

   if (ndim > 0) then
      if (.not.allocated(params)) allocate(params(ndim))
      call  read_parameters(parameter_file,params,ndim)
      if (.not.allocated(A)) allocate(A(ndim,nrens))
      if (.not.allocated(ref)) allocate(ref(ndim))
   else
      print *,'ndim = 0'
      stop
   endif

! Counting the number of measurements "nrobs"
   nrobs= count_measurements('measurement_loc.in')



! Running reference solution
      print '(a)','Running reference simulation starting from infile.ref.'
      call runmodel(u,v,w,istep,lreference)

      print '(a)','Saving diagnostics for reference run'
! Printing grid
      if (.not. allocated(blanking)) allocate(blanking(nx,ny,nz))
      blanking=0
      call diag(1,istep,it,blanking,uave,vave,wave,velave,ustd,vstd,wstd,velstd)

! Printing reference
      call ref_uvw(u,v,w,uave,vave,wave,velave,ustd,vstd,wstd,velstd,nrens,istep)
      call diag(2,istep,0,blanking,uave,vave,wave,velave,ustd,vstd,wstd,velstd)

      print '(a)','Getting observed measurements.'
      call get_observed_measurements(nrobs,1000,lmeas)

      print '(a)','Initializing the ensemble of uncertain parameters.'
      call iniens(A,nrens,params,ndim)
      liniens=.true.

      it=1
      print '(a)','Running prior ensemble.'
      call runensemble(nrens,params,ndim,it,A)

      print *,'Diagnoistics it=',it
      print *,'loaduvw'
      call loaduvw(u,v,w,luvwloaded,nrens,it,istep)
      print *,'average'
      call average_uvw(u,v,w,uave,vave,wave,velave,ustd,vstd,wstd,velstd,nrens)
      print *,'diag'
      call diag(2,istep,it,blanking,uave,vave,wave,velave,ustd,vstd,wstd,velstd)
      tmpfile = 'tmp_uini.txt'
      cmd = "grep uini infile.ref | awk '{print $1, $2}' > " // trim(tmpfile)
      call system(cmd)
      open(unit=10, file=trim(tmpfile), status='old', action='read')
      read(10, *) ref(1:ndim)
      close(10)
      !  cmd = "rm -f " // trim(tmpfile)
      !  call system(cmd)


      call tecens(ref,ndim,1,nmda,0)
      call tecens(A,ndim,nrens,nmda,it)

      do it=2,nmda+1

         print '(a)','Assembling predicted measurements.'
         call get_predicted_measurements(lyobs,nrobs, nrens, it-1, 1000)
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


         if (allocated(E)) deallocate(E); allocate(E(nrobs,nrens))
         if (allocated(D)) deallocate(D); allocate(D(nrobs,nrens))
         if (allocated(O))    deallocate(O)   ; allocate(O(nrobs,nrens))
         do j=1,nrens
         do m=1,nrobs
            E(m,j)=normal()*relobserr*abs(obs(m)%d)*sqrt(real(nmda))
            D(m,j)=obs(m)%d + E(m,j)
            O(m,j)=D(m,j)-Y(m,j)
         enddo
         enddo

         print '(a)','ES assimilation step.'
         call assimilation(ndim,nrens,nrobs,A,O,E,Y,S,mode)

         print '(a)','Run posterior ensemble'
         call runensemble(nrens,params,ndim,it,A)

         print *,'Diagnoistics it=',it
         call loaduvw(u,v,w,luvwloaded,nrens,it,istep)
         call average_uvw(u,v,w,uave,vave,wave,velave,ustd,vstd,wstd,velstd,nrens)
         call diag(2,istep,it,blanking,uave,vave,wave,velave,ustd,vstd,wstd,velstd)
         call tecens(A,ndim,nrens,nmda,it)

      enddo

end program

