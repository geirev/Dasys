module m_get_observed_measurements
use mod_measurements
contains
subroutine get_observed_measurements(nrobs,itime,lmeas)
   use m_localdefs, only : experiment
   implicit none
   integer, intent(in)     :: itime
   integer, intent(inout)  :: nrobs
   logical, intent(out)    :: lmeas

   character(len=100) :: filename
   character(len=100) :: fname
   character(len=5)   :: ctime
   logical ex
   integer m,i,iunit


   nrobs=0

   write(ctime,'(i5.5)')itime
   filename='measurements_'//ctime//'.dat'

! Count lines in mem0000/measurements_01000.dat
   fname=trim(experiment)//'/mem0000/'//trim(filename)
   inquire(file=trim(fname),exist=ex)
   if (ex) then
      open(newunit=iunit,file=trim(fname), status='old', action='read')
      do
        read(iunit,*,end=999,err=999)i
        nrobs= nrobs + 1
     enddo
     999 close(iunit)
   else
      print '(3a)', 'The file: ',trim(fname),' does not exist'
      return
   endif

! return if no measurements
   if (nrobs == 0) then
      print *,'No measurements stored'
      return
   endif

! read all the predicted measurements

   if (allocated(obs)) deallocate(obs)
   allocate(obs(nrobs))
   fname=trim(experiment)//'/mem0000/'//trim(filename)
   inquire(file=trim(fname),exist=ex)
   if (ex) then
      open(newunit=iunit,file=trim(fname), status='old', action='read')
      do m=1,nrobs
         read(iunit,*)i,obs(m)%c,obs(m)%i,obs(m)%j,obs(m)%d
         write(*,'(i5,tr1,a1,i4,i4,f10.5)')m,obs(m)%c,obs(m)%i,obs(m)%j,obs(m)%d
      enddo
      close(iunit)
      lmeas=.true.
   else
      print '(3a)', 'The file: ',trim(fname),' does not exist'
      return
   endif


end subroutine
end module

