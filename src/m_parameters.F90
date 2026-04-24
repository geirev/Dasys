module m_parameters
   use m_input_files_def
   implicit none
   private

   public :: uncertain_parameters
   public :: params
   public :: parameters
   public :: parnrtime,pardt,parnr,lcorr

   integer :: parnrtime   ! Number of parameter values in time over a DA window
   integer :: pardt       ! Delta time between parameter values (s)
   integer :: parnr       ! Number of different paramters
   integer :: lcorr       ! Decorrelation time (s)

   type uncertain_parameters
      character(len=10) :: parname
      real              :: mean
      real              :: stdev
   end type uncertain_parameters

   type(uncertain_parameters), allocatable :: params(:)

contains

subroutine parameters(ndim)
   implicit none
   integer, intent(out) :: ndim        ! total dimension = parnr * parnrtime

   logical :: ex
   integer :: iunit, i, ios
   character(len=256) :: line

   inquire(file=trim(parameter_file), exist=ex)
   if (.not. ex) then
      open(10, file=trim(parameter_file), status='new')
         write(10,'(a)') '5                    ! Number of parameter values in time'
         write(10,'(a)') '60                   ! Delta t between parameter values (s)'
         write(10,'(a)') '1000                 ! De-correlation time (s)'
         write(10,'(a)') '#vel  8.0   1.0'
         write(10,'(a)') '#dir  0.0  10.0'
      close(10)
      print '(3a)', 'Created default: "', trim(parameter_file), '" for uncertain vel and dir'
      print '(a)',  'Please edit and restart dasys'
      stop 1
   end if

   open(newunit=iunit, file=trim(parameter_file), status='old', action='read')

   ! Read header values
   read(iunit,*) parnrtime
   read(iunit,*) pardt
   read(iunit,*) lcorr

   ! Count parameter definitions
   parnr = 0
   do
      read(iunit,'(A)', end=100, err=100) line
      if (len_trim(line) == 0) cycle
      if (line(1:1) == '#') parnr = parnr + 1
   end do
100   continue

   ndim = parnr * (parnrtime+1)

   print '(a,i0)', 'The number of parameter types is:          parnr      = ', parnr
   print '(a,i0)', 'The number of parameter values in time is: parnrtime+1= ', parnrtime+1
   print '(a,i0)', 'The delta t between parameter values is:   pardt      = ', pardt
   print '(a,i0)', 'The de-correlation time is:                lcorr      = ', lcorr
   print '(a,i0)', 'The total number of parameter values is:   ndim       = ', ndim

   ! Allocate module array
   if (allocated(params)) deallocate(params)
   allocate(params(parnr))

   ! Rewind and read again
   rewind(iunit)
   read(iunit,*) parnrtime
   read(iunit,*) pardt
   read(iunit,*) lcorr

   print '(a)', 'Uncertain parameters defined:'

   i = 0
   do
      read(iunit,'(A)', end=200, err=200) line
      if (len_trim(line) == 0) cycle
      if (line(1:1) /= '#') cycle

      i = i + 1
      if (i > parnr) then
         print '(a)', 'Warning: more parameter definitions in file than expected'
         exit
      end if

      read(line(2:), *, iostat=ios) params(i)%parname, params(i)%mean, params(i)%stdev
      if (ios /= 0) then
         print '(2a)', 'Error reading parameter line: ', trim(line)
         stop
      end if

      write(*,'(i4,1x,a10,2f12.4)') i, trim(params(i)%parname), params(i)%mean, params(i)%stdev
   end do
 200   continue

   close(iunit)

   if (i < parnr) then
      print '(a,i0,a,i0)', 'Warning: expected ', parnr, ' parameters, but read only ', i
   end if

end subroutine

end module
