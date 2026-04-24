module m_smooth_random_series
contains
   subroutine smooth_random_series(x, n, dt, lc, x0, v0, xend, vend)
      use m_normal
      implicit none

      integer, intent(in) :: n
      real,    intent(in) :: dt, lc
      real,    intent(in) :: x0, v0
      real,    intent(out) :: x(n)
      real,    intent(out) :: xend, vend

      real :: lambda, e, a11, a12, a21, a22
      real :: p11, p12, p22
      real :: q11, q12, q22
      real :: l11, l21, l22
      real :: z1, z2
      real :: xx, vv, xx_new, vv_new
      integer :: k

      if (n < 1) stop 'smooth_random_series_state: n must be >= 1'
      if (dt <= 0.0) stop 'smooth_random_series_state: dt must be > 0'
      if (lc <= 0.0) stop 'smooth_random_series_state: lc must be > 0'

      lambda = sqrt(3.0) / lc

      ! Exact discrete transition matrix
      e   = exp(-lambda*dt)
      a11 = e * (1.0 + lambda*dt)
      a12 = e * dt
      a21 = -e * lambda*lambda * dt
      a22 = e * (1.0 - lambda*dt)

      ! Stationary covariance of [x,v]^T
      p11 = 1.0
      p12 = 0.0
      p22 = lambda*lambda

      ! Exact discrete process-noise covariance Q = P - F P F^T
      q11 = p11 - (a11*a11*p11 + 2.0*a11*a12*p12 + a12*a12*p22)
      q12 = p12 - (a11*a21*p11 + (a11*a22 + a12*a21)*p12 + a12*a22*p22)
      q22 = p22 - (a21*a21*p11 + 2.0*a21*a22*p12 + a22*a22*p22)

      if (q11 < 0.0 .and. q11 > -1.0e-12) q11 = 0.0
      if (q22 < 0.0 .and. q22 > -1.0e-12) q22 = 0.0

      if (q11 < 0.0) stop 'smooth_random_series_state: Q not positive semidefinite'
      l11 = sqrt(q11)

      if (l11 > 0.0) then
         l21 = q12 / l11
      else
         l21 = 0.0
      end if

      q22 = q22 - l21*l21
      if (q22 < 0.0 .and. q22 > -1.0e-12) q22 = 0.0
      if (q22 < 0.0) stop 'smooth_random_series_state: Cholesky failed'
      l22 = sqrt(q22)

      ! Initial state
      xx = x0
      vv = v0
      x(1) = xx

      do k = 2, n
         z1 = normal()
         z2 = normal()

         xx_new = a11*xx + a12*vv + l11*z1
         vv_new = a21*xx + a22*vv + l21*z1 + l22*z2

         xx = xx_new
         vv = vv_new
         x(k) = xx
      end do

      xend = xx
      vend = vv

   end subroutine

end module m_smooth_random_series
