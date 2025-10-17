module m_average
contains
subroutine average(A,mean,n,nrens)
   implicit none
   integer, intent(in) :: n,nrens
   real, intent(out) :: mean(n)
   real, intent(in)  :: A(n,nrens)
   integer i,j

   mean(:)=0.0
   do j=1,nrens
   do i=1,n
      mean(i)=mean(i)+A(i,j)
   enddo
   enddo
   mean=mean/real(nrens)


end subroutine
end module

