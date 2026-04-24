module m_set_random_seed
contains
subroutine set_random_seed()
! Sets a random seed based on the system and wall clock time
   implicit none

   integer , dimension(8)::val
   integer sze,i
   integer, allocatable, dimension(:):: pt
 
   integer :: clock_max
   integer :: clock_rate
   integer :: clock_reading

   call RANDOM_SEED(size=sze)
   allocate(pt(sze))

      do i=1,sze
         call DATE_AND_TIME(values=val)
         call system_clock(clock_reading, clock_rate, clock_max)
         pt(i) = val(8)*val(7)+clock_reading
      enddo

   call RANDOM_SEED(put=pt)
   deallocate(pt)
end subroutine
end module
