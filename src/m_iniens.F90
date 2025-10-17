module m_iniens
contains
subroutine iniens(A,nrens,params,ndim)
   use m_params
   use m_normal
   use m_resetensemble
   implicit none
   integer, intent(in) :: nrens
   integer, intent(in) :: ndim
   type(uncertain_parameters), intent(in) :: params(ndim)
   real, intent(out) :: A(ndim,nrens)

   character(len=1)yn
   logical ex

   integer i,j,iunit,reclA,jtmp

   inquire(iolength=reclA)ndim,nrens,A(1,:)

   yn='n'
   inquire(file='iniens.uf',exist=ex)
   if (ex) then
      open(newunit=iunit, file='iniens.uf', form='unformatted', access='direct', recl=reclA)
      read(iunit,rec=1)i,j
      close(iunit)
      print '(a,i0,a,i0,a)','Read iniens.uf of dimensions: (',i,'x',j,') (y/n)?'
      read(*,*)yn
   endif

   if (ex .and. yn.eq.'y' .and. i==ndim .and. j==nrens) then
      open(newunit=iunit, file='iniens.uf', form='unformatted', access='direct', recl=reclA)
         do j=1,nrens
            read(iunit,rec=j)i,jtmp,A(:,j)
         enddo
      close(iunit)
      print *,'iniens: initial A read from iniens.uf!'
   else
      do j=1,nrens
         do i=1,ndim
            A(i,j)=params(i)%mean+params(i)%stdev*normal()
         enddo
      enddo
      print *,'iniens: A initialization done!'
      print *

      call system('rm -f iniens.uf')
      inquire(iolength=reclA)ndim,nrens,A(1,:)
      open(newunit=iunit, file='iniens.uf', form='unformatted', access='direct', recl=reclA)
         do j=1,nrens
            write(iunit,rec=j)ndim,nrens,A(:,j)
         enddo
      close(iunit)
      print *,'iniens: A initialization done and saved to iniens.uf'
      call resetensemble(nrens,1)
   endif
   print '(a)','Initial parameters:'
   do j=1,nrens
      print '(i0,a,10f10.5)',j,': ',A(1:min(10,ndim),j)
   enddo

end subroutine
end module

