module m_standarddev
contains
subroutine standarddev(A,mean,stddev,n,nrens)
   implicit none
   integer, intent(in) :: n,nrens
   real, intent(in) :: mean(n)
   real, intent(out) :: stddev(n)
   real, intent(in)  :: A(n,nrens)
   integer i,j
   real x

   stddev(:)=0.0
   do j=1,nrens
   do i=1,n
      stddev(i)=stddev(i)+(A(i,j)-mean(i))*(A(i,j)-mean(i))
   enddo
   enddo
   stddev=stddev/real(nrens-1)
   stddev=sqrt(stddev+tiny(x))


end subroutine
end module


