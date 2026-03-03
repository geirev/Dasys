module m_get_predicted_measurements
use mod_measurements
contains
subroutine get_predicted_measurements(lyobs, nrobs, nrens,  iter, itime)
   use m_readinfile, only : experiment
   implicit none
   logical, intent(out)    :: lyobs
   integer, intent(inout)  :: nrobs
   integer, intent(in)     :: nrens
   integer, intent(in)     :: iter
   integer, intent(in)     :: itime

   type(measurement), allocatable :: yobs(:)

   character(len=100) :: filename
   character(len=100) :: fname
   character(len=5)   :: ctime
   character(len=2)   :: citer
   character(len=4)   :: cens
   logical ex
   integer m,i,j,iunit


   nrobs=0

   write(ctime,'(i5.5)')itime
   filename='measurements_'//ctime//'.dat'

! Count lines in mem0001/it01/measurements_00100.dat
   write(citer,'(i2.2)')1
   write(cens,'(i4.4)')1
   fname=trim(experiment)//'/mem'//cens//'/it'//citer//'/'//trim(filename)
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
   if (allocated(yobs))       deallocate(yobs)      ; allocate(yobs(nrobs))
   if (allocated(yensemble))  deallocate(yensemble) ; allocate(yensemble(nrobs,nrens))
   write(citer,'(i2.2)')iter
   do j=1,nrens
      write(cens,'(i4.4)')j
      fname=trim(experiment)//'/mem'//cens//'/it'//citer//'/'//trim(filename)
      inquire(file=trim(fname),exist=ex)
      if (ex) then
         open(newunit=iunit,file=trim(fname), status='old', action='read')
         do m=1,nrobs
            read(iunit,*)i,yobs(m)%c,yobs(m)%i,yobs(m)%j,yobs(m)%d
            write(*,'(i5,tr1,a1,i4,i4,f10.5)')m,yobs(m)%c,yobs(m)%i,yobs(m)%j,yobs(m)%d
            yensemble(m,j)=yobs(m)%d
         enddo
         close(iunit)
      else
         print '(3a)', 'The file: ',trim(fname),' does not exist'
         return
      endif
   enddo
   lyobs=.true.
   if (allocated(yobs)) deallocate(yobs)


end subroutine
end module

