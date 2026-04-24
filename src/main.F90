program main
   use iso_c_binding
!   use iso_fortran_env

   use m_set_random_seed
   use m_simulate_forcing
   use m_reference_forcing
   use m_ensemble_forcing

   use mod_dimensions
   use mod_measurements
   use m_readinfile

   use m_ansi_colors

   use m_assimilation
   use m_average_uvw

   use m_parameters

   use m_ref_parameters

   use m_count_measurements
   use m_diag
   use m_get_observed_measurements
   use m_get_predicted_measurements
   use m_iniens
   use m_input_files_def
   use m_loaduvw
   use m_normal
   use m_ref_uvw
   use m_runensemble
   use m_runmodel
   use m_tecens
   use m_finens

   implicit none

   interface
   subroutine sleep(seconds) bind(C, name="sleep")
      import :: c_int
      integer(c_int), value :: seconds
   end subroutine sleep
   end interface




   integer m,j
   integer nrobst,nrobs_steps,mtime


   integer :: istep
   integer :: iunit
   integer :: mstep
   integer :: it

   integer :: mode=13

   real, allocatable :: ref(:)

   real, allocatable, dimension(:)    :: mean
   real, allocatable, dimension(:,:)  :: Y,D,E,A,S,O

   real(kind=4), allocatable :: u(:,:,:,:)
   real(kind=4), allocatable :: v(:,:,:,:)
   real(kind=4), allocatable :: w(:,:,:,:)

   integer(kind=4), allocatable :: blanking(:,:,:)
   real(kind=4), allocatable :: uave(:,:,:)
   real(kind=4), allocatable :: vave(:,:,:)
   real(kind=4), allocatable :: wave(:,:,:)
   real(kind=4), allocatable :: velave(:,:,:)

   real(kind=4), allocatable :: ustd(:,:,:)
   real(kind=4), allocatable :: vstd(:,:,:)
   real(kind=4), allocatable :: wstd(:,:,:)
   real(kind=4), allocatable :: velstd(:,:,:)


   integer ndim

 !  call set_random_seed()

   call readinfile()

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


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Counting and reading the number of uncertain parameters "ndim" and allocating them
   call parameters(ndim)
   if (.not.allocated(A)) allocate(A(ndim,nrens))
   if (.not.allocated(ref)) allocate(ref(ndim))


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Counting the number of measurements "nrobs"
   nrobst     = count_measurements()
   nrobs_steps= (meas_last-meas_first+meas_dt)/meas_dt
   nrobs=nrobst*nrobs_steps
   allocate(obs(nrobs))
   allocate(yensemble(nrobs,nrens))
   print *
   print '(a,i0)','Number of measurements per step is     nrobst= ',nrobst
   print '(a,i0)','Number of steps with measurements nrobs_steps= ',nrobs_steps
   print '(a,i0)','Total number of measurments is          nrobs= ',nrobs
   print *

   !call simulate_forcing(ndim,nwin,nrens)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Defining the uncertain parameters of the reference case

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Running reference solution
   print '(a)','Running reference simulation.'
   call reference_forcing()
   call runmodel()

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Printing grid
   if (.not. allocated(blanking)) allocate(blanking(nx,ny,nz))
   open(newunit=iunit,file=trim(experiment)//'/mem0000/blanking3D.uf',form="unformatted", status='old')
      read(iunit)blanking
   close(iunit)
   istep=0
   it=0
   call diag(1,istep,it,blanking,uave,vave,wave,velave,ustd,vstd,wstd,velstd)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Printing reference
   it=0
   do istep=meas_first,meas_last,meas_dt
      call ref_uvw(u,v,w,uave,vave,wave,velave,ustd,vstd,wstd,velstd,nrens,istep)
      call diag(2,istep,it,blanking,uave,vave,wave,velave,ustd,vstd,wstd,velstd)
   enddo

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   print '(a)','Getting observed measurements.'
   mstep=0
   do mtime=meas_first,meas_last,meas_dt
      print *,'Getting measurements for iteration=',mtime,nrobs,nrobst
      mstep=mstep+1
      call get_observed_measurements(nrobst,mstep,mtime)
   enddo

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   print '(a)','Initializing the ensemble of uncertain parameters.'
!   call iniens(A,trim(experiment),nrens,ndim)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   it=1
   print '(a)','Running prior ensemble.'
   call ensemble_forcing(A,nrens,ndim)
   call runensemble(A,nrens,ndim,it)

   print *,'Diagnoistics of prior ensemble it=',it
   do istep=meas_first,meas_last,meas_dt
      call loaduvw(u,v,w,nrens,it,istep)
      call average_uvw(u,v,w,uave,vave,wave,velave,ustd,vstd,wstd,velstd,nrens)
      call diag(2,istep,it,blanking,uave,vave,wave,velave,ustd,vstd,wstd,velstd)
   enddo
   call tecens(A,ndim,nrens,it,nmda)

   do it=2,nmda+1
      print '(a,i0)','Assembling predicted measurements, nrobs=',nrobs
      do j=1,nrens
      mstep=0
      do mtime=meas_first,meas_last,meas_dt
         mstep=mstep+1
         call get_predicted_measurements(nrobst, mstep, mtime, j, it-1)
      enddo
      enddo



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
      if (allocated(O)) deallocate(O)   ; allocate(O(nrobs,nrens))
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
      call runensemble(A,nrens,ndim,it)

      print *,'Diagnoistics it=',it
      do istep=meas_first,meas_last,meas_dt
         call loaduvw(u,v,w,nrens,it,istep)
         call average_uvw(u,v,w,uave,vave,wave,velave,ustd,vstd,wstd,velstd,nrens)
         call diag(2,istep,it,blanking,uave,vave,wave,velave,ustd,vstd,wstd,velstd)
      enddo

   enddo

   call tecens(A,ndim,nrens,it-1,nmda)
   call finens(A,ndim,nrens)

end program

