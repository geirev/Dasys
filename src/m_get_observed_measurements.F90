module m_get_observed_measurements
use mod_measurements
contains
subroutine get_observed_measurements(nrobst,mstep,mtime)
   use m_readinfile, only : experiment
   implicit none
   integer, intent(in)  :: mtime
   integer, intent(in)  :: nrobst
   integer, intent(in)  :: mstep

   character(len=100) :: filename
   character(len=6)   :: ctime
   logical ex
   integer i,nn,m,iunit

! read all the predicted measurements
   write(ctime,'(i6.6)')mtime
   filename=trim(experiment)//'/mem0000'//'/measurements_'//ctime//'.dat'
   inquire(file=trim(filename),exist=ex)
   if (ex) then
      print *,'reading file: ',trim(filename)
      open(newunit=iunit,file=trim(filename), status='old', action='read')
         do i=1,nrobst
            m=(mstep-1)*nrobst+i
            read(iunit,*)nn,obs(m)%c,obs(m)%i,obs(m)%j,obs(m)%k,obs(m)%d
            write(*,'(i5,tr1,a1,3i4,f10.5)')m,obs(m)%c,obs(m)%i,obs(m)%j,obs(m)%k,obs(m)%d
         enddo
      close(iunit)
   else
      print *,'file: +',trim(filename),'+ does not exist'
   endif


end subroutine
end module

