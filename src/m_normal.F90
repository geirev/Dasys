module m_normal
contains

   real function normal()
      implicit none
      real :: w1, w2
      real, parameter :: pi = 3.14159265358979323846

      call random_number(w1)
      call random_number(w2)

      ! Avoid log(0) without biasing distribution
      if (w1 < 1.0e-12) w1 = 1.0e-12

      normal = sqrt(-2.0*log(w1)) * cos(2.0*pi*w2)

   end function normal

end module m_normal

!module m_normal
!contains
!real function normal()
!!  Returns a vector of random values N(variance=1,mean=0)
!   implicit none
!   real :: w1
!   real :: w2
!   real, parameter   ::  pi=3.141592653589
!
!   call random_number(w1)
!   call random_number(w2)
!   normal= sqrt(-2.0*log(w1+tiny(1.0)))*cos(2.0*pi*w2)
!
!end function
!end module
