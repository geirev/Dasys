module m_tecens
contains

subroutine tecens(A,ndim,nrens,it,nmda)
   use m_readinfile, only : experiment, iwin
   use m_parameters, only : parnr,parnrtime,pardt
   implicit none

   integer, intent(in) :: ndim, nrens, it, nmda
   real, target, intent(in) :: A(ndim,nrens)

   integer :: i, n, iunit
   character(len=200) :: fname, line, fmt
   character(len=20)  :: cit
   character(len=50)  :: basetitle
   real, pointer :: A3(:,:,:)
   real :: time

   A3(0:parnrtime,1:parnr,1:nrens) => A

   write(cit,'(i0)') it

   fname = trim(experiment)//'/tecstate_'//trim(cit)//'.dat'

   if (it == i) then
      basetitle = 'Prior'
   elseif (it == nmda+1) then
      basetitle = 'Posterior'
   else
      basetitle = 'Mdastep'//trim(cit)
   endif

   open(newunit=iunit,file=trim(fname),status='replace',action='write')

   write(iunit,'(a)') 'TITLE = "State variables"'
   write(iunit,'(a)') 'VARIABLES = "time", "vel", "dir"'

   write(fmt,'(a,i0,a)')'(',parnr+1,'3(1x,f12.4))'

   do n = 1, nrens
      write(line,'(a,a,a,i0,a,i0,a)') 'ZONE T="', trim(basetitle), '_ens', n, '", I=', parnrtime+1, ', DATAPACKING=POINT'
      write(iunit,'(a)') trim(line)
      do i = 0, parnrtime
         time = real((iwin-1)*parnrtime + i) * real(pardt)
         write(iunit,trim(fmt)) time, A3(i,:,n)
      end do
   end do

   close(iunit)

end subroutine

end module
