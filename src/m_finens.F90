module m_finens
contains
subroutine finens(A,ndim,nrens)
   use m_readinfile, only : iwin, experiment
   implicit none
   integer, intent(in) :: ndim
   integer, intent(in) :: nrens
   real, intent(in) :: A(ndim,nrens)
   integer j,reclA,iunit
   character(len=100) ensfile
   character(len=4) :: cwin

   write(cwin,'(i4.4)')iwin
   !ensfile=trim(experiment)//'/ensemble'//cwin//'A.uf'
   ensfile=trim(experiment)//'/ensemble.uf'
   inquire(iolength=reclA)ndim,nrens,A(:,1)
   open(newunit=iunit, file=trim(ensfile), form='unformatted', access='direct', recl=reclA)
      do j=1,nrens
         write(iunit,rec=j)ndim,nrens,A(:,j)
      enddo
   close(iunit)
   print *,'finens: final A written to ',trim(ensfile)
end subroutine
end module
