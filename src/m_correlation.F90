module m_correlation
contains
subroutine correlation(Cxy,A,Y,meanA,stddevA,meanY,stddevY,ndim,nrobs,nrens)
   implicit none
   integer, intent(in) :: ndim,nrens,nrobs
   real, intent(in)    :: A(ndim,nrens)
   real, intent(in)    :: Y(nrobs,nrens)
   real, intent(out)   :: Cxy(ndim,nrobs)
   real, intent(in)    :: meanA(ndim)
   real, intent(in)    :: stddevA(ndim)
   real, intent(in)    :: meanY(ndim)
   real, intent(in)    :: stddevY(ndim)

   real Al(ndim,nrens)
   real Yl(nrobs,nrens)
   real x
   integer i,j,m

   do j=1,nrens
      Al(:,j)=A(:,j)-meanA(:)
      Yl(:,j)=Y(:,j)-meanY(:)
   enddo

   Cxy=matmul(Al,transpose(Yl))
   Cxy=Cxy/real(nrens)

   do m=1,nrobs
   do i=1,ndim
      Cxy(i,m)=Cxy(i,m)/(stddevA(i)*stddevY(m)+tiny(x))
   enddo
   enddo


end subroutine
end module


