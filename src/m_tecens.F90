module m_tecens
contains

subroutine tecens(A,ndim,nrens,parnr,parnrtime,pardt,it,nmda)
   use m_readinfile, only : experiment
   implicit none

   integer, intent(in) :: ndim, nrens, parnr, parnrtime, pardt, it, nmda
   real, target, intent(in) :: A(ndim,nrens)

   integer :: i, n, iunit
   character(len=200) :: fname, line
   character(len=20)  :: cit
   character(len=50)  :: basetitle
   real, pointer :: A3(:,:,:)
   real :: t

   if (ndim /= parnrtime*parnr) error stop "tecens: ndim /= parnrtime*parnr"
   if (parnr < 2) error stop "tecens: parnr must be at least 2"

   A3(1:parnrtime,1:parnr,1:nrens) => A

   write(cit,'(i0)') it

   fname = trim(experiment)//'/tecstate_'//trim(cit)//'.dat'

   if (it == 0) then
      basetitle = 'Prior'
   elseif (it == nmda+1) then
      basetitle = 'Posterior'
   else
      basetitle = 'Mdastep'//trim(cit)
   endif

   open(newunit=iunit,file=trim(fname),status='replace',action='write')

   write(iunit,'(a)') 'TITLE = "State variables"'
   write(iunit,'(a)') 'VARIABLES = "time", "vel", "dir"'

   do n = 1, nrens
      write(line,'(a,a,a,i0,a,i0,a)') 'ZONE T="', trim(basetitle), '_ens', n, &
           '", I=', parnrtime, ', DATAPACKING=POINT'
      write(iunit,'(a)') trim(line)

      do i = 1, parnrtime
         t = real(i-1) * real(pardt)
         write(iunit,'(3(1x,es16.8))') t, A3(i,1,n), A3(i,2,n)
      end do
   end do

   close(iunit)

end subroutine

end module
