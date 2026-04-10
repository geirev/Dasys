module m_finens
contains
subroutine finens(A,ndim,nrens)
   implicit none
   integer, intent(in) :: ndim
   integer, intent(in) :: nrens
   real, intent(in) :: A(ndim,nrens)
   integer j,reclA,iunit

   inquire(iolength=reclA)ndim,nrens,A(:,1)
   open(newunit=iunit, file='finens.uf', form='unformatted', access='direct', recl=reclA)
      do j=1,nrens
         write(iunit,rec=j)ndim,nrens,A(:,j)
      enddo
   close(iunit)
   print *,'finens: final A written to finens.uf'
end subroutine
end module
