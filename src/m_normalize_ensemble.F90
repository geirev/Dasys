module m_normalize_ensemble
contains
subroutine normalize_ensemble(x, n, nens)
   implicit none
   integer, intent(in) :: n, nens
   real,    intent(inout) :: x(n, nens)

   integer :: i, j
   real :: ave, var, s

   do i = 2, n
      ave = sum(x(i,:)) / real(nens)
      var = sum( (x(i,:) - ave)**2 ) / real(nens)

      if (var <= 0.0) stop 'normalize_ensemble_timewise_from2: bad variance'

      s = sqrt(var)

      do j = 1, nens
         x(i,j) = (x(i,j) - ave) / s
      end do
   end do
end subroutine
end module
