module m_reference_forcing
contains
subroutine reference_forcing()
   use m_normal
   use m_smooth_random_series
   use m_readinfile, only : iwin, experiment
   use m_parameters, only : parnr, parnrtime, pardt, lcorr, params
   implicit none

   real, allocatable :: zforce(:,:), refforce(:,:), endval(:), endder(:)
   integer :: i, l
   real :: startval, startder
   real :: lambda, time
   character(len=100) :: fname

   allocate(zforce(0:parnrtime,parnr), refforce(0:parnrtime,parnr), endval(parnr), endder(parnr))

   lambda = sqrt(3.0)/real(lcorr)

   if (iwin > 1) then
      fname = trim(experiment)//'/uvel_time_start.ref'
      open(10,file=trim(fname),status='old')
      do l = 1,parnr
         read(10,*) endval(l), endder(l)
      end do
      close(10)
   end if

   do l = 1,parnr
      if (iwin == 1) then
         startval = normal()
         startder = lambda * normal()
!         print *,'START:',startval,startder
      else
         startval = endval(l)
         startder = endder(l)
      end if

      call smooth_random_series(zforce(0,l), parnrtime+1, real(pardt), real(lcorr),  startval, startder, endval(l), endder(l))
!      print *,'params:',l,params(l)%mean , params(l)%stdev

      refforce(0:parnrtime,l) = params(l)%mean + params(l)%stdev * zforce(0:parnrtime,l)
   end do

   fname = trim(experiment)//'/uvel_time.ref'
   open(10,file=trim(fname))
   do i = 0,parnrtime
      time = real((iwin-1)*parnrtime + i) * real(pardt)
      write(10,'(100f13.5)') time, refforce(i,1:parnr)
      write(* ,'(100f13.5)') time, refforce(i,1:parnr)
   end do
   close(10)

   fname = trim(experiment)//'/uvel_time_start.ref'
   open(10,file=trim(fname))
   do l = 1,parnr
      write(10,*) endval(l), endder(l)
   end do
   close(10)

   deallocate(zforce, refforce, endval, endder)

end subroutine
end module
