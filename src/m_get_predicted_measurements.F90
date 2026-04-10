module m_get_predicted_measurements
use mod_measurements
contains
subroutine get_predicted_measurements(nrobst, mstep, mtime, jens, it)
   use m_readinfile, only : experiment
   implicit none
   integer, intent(in)  :: nrobst
   integer, intent(in)  :: mstep
   integer, intent(in)  :: mtime
   integer, intent(in)  :: jens
   integer, intent(in)  :: it

   type(measurement), allocatable :: yobs(:)

   character(len=100) :: filename
   character(len=100) :: fname
   character(len=5)   :: ctime
   character(len=2)   :: cit
   character(len=4)   :: cens
   logical ex
   integer i,m,nn,iunit


   allocate(yobs(nrobst))

   write(ctime,'(i5.5)')mtime
   filename='measurements_'//ctime//'.dat'

! read all the predicted measurements
   write(cit,'(i2.2)')it
   write(cens,'(i4.4)')jens
   fname=trim(experiment)//'/mem'//cens//'/it'//cit//'/'//trim(filename)
   print *,trim(fname)
   inquire(file=trim(fname),exist=ex)
   if (ex) then
         open(newunit=iunit,file=trim(fname), status='old', action='read')
         do i=1,nrobst
            m=(mstep-1)*nrobst+i
            read(iunit,*)nn,yobs(i)%c,yobs(i)%i,yobs(i)%j,yobs(i)%k,yobs(i)%d
            yensemble(m,jens)=yobs(i)%d
         enddo
         close(iunit)
   else
         print '(3a)', 'The file: ',trim(fname),' does not exist'
         return
   endif

   if (allocated(yobs)) deallocate(yobs)

end subroutine
end module

