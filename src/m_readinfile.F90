module m_readinfile
   character(len=100)experiment  ! Experiment name
   character(len=100)runcommand  ! Runcommand
   integer :: nrobs=0            ! Number of measurements
   integer :: nrens=0            ! Number of ensemble realizations
   integer :: nmda=0             ! Number of ESMDA steps
   real    :: relobserr=0.0      ! relative observation errors
   integer :: meas_first         ! time of first measurement set (in it nr)
   integer :: meas_last          ! time of last measurement set (in it nr)
   integer :: meas_dt            ! time betweem meaurements (in it nr)
   integer :: len                ! length of time domain for experiment (nt1-nt0+1)
   integer :: cor                ! decorrelation length in time domain for parameters
   logical :: itdirs=.false.     ! if true each iteration is run in a separate dir (e.g., mem0001/it05)
contains
subroutine readinfile()
   use mod_dimensions 
   use m_mkinfile
   implicit none
   logical ex
   character(len=8) :: fname='dasys.in'

   inquire(file='main.F90',exist=ex)
   if (ex) stop 'You are executing dasys in the src/ catalog'

   inquire(file=trim(fname), exist=ex)
   if (.not.ex) then
      print '(a)','Did not find inputfile dasys.in'
      print '(a)','Generating new template dasys.in.....'
      call mkinfile(fname)
      print '(a)','Please edit and relaunch dasys'
      stop
   endif

   open(unit=10, file=trim(fname), status='old', action='read')
      read(10,*,err=100)experiment
      read(10,*,err=100)runcommand
      read(10,*,err=100)nrens
      read(10,*,err=100)nmda
      read(10,*,err=100)relobserr
      read(10,*,err=100)meas_first,meas_last,meas_dt
      read(10,*,err=100)len,cor
   close(10)

   return
   100 close(10)
   call system('mv -i dasys.in dasys_backup.in')
   call mkinfile(fname)
   print *,'dasys.in problem; dasys.in moved to dasys_backup.in and generated new template dasys.in'
   stop

end subroutine
end module
