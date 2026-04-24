module m_simulate_forcing
contains
subroutine simulate_forcing(ndim,nwin,nrens)
   use m_normal
   use m_smooth_random_series
   use m_normalize_ensemble
   use m_parameters, only : parnr,parnrtime,pardt,lcorr
   implicit none

   integer, intent(in) :: ndim      ! dimension of parameter state vector (3*dawin*parnr)
   integer, intent(in) :: nwin      ! Number of steps in a data assimilation window
   integer, intent(in) :: nrens     ! Ensemble size
   real, allocatable :: ft2(:,:)    ! temporary forcing function
   integer i,l
   real startval,endval
   real startder,endder
   real ave
   real var
   real lambda
   real time
   character(len=50) :: fmt

   print *,'parnr=     ',parnr
   print *,'parnrtime= ',parnrtime
   print *,'pardt=     ',pardt
   print *,'nrens=     ',nrens

   allocate(ft2(0:parnrtime,nrens))

   write(fmt,'("(i5,1x,f12.2,",i0,"f7.3)")') nrens + 2

   open(10,file='ts.dat')
      do l=1,10
         do i=1,nrens
            if (l==1) then
               startval=normal()
               lambda = sqrt(3.0)/real(lcorr)
               startder = lambda * normal()
            else
               startval = ft2(parnrtime,i)
               startder = (ft2(parnrtime,i) - ft2(parnrtime-1,i)) / real(pardt)
            endif
            call smooth_random_series(ft2(0,i), parnrtime+1, real(pardt), real(lcorr), startval, startder, endval, endder)
         enddo

         !call normalize_ensemble(ft2,parnrtime,nrens)

         do i=1,10
            ave = sum(ft2(i,:)) / real(nrens)
            var = sum( (ft2(i,:) - ave)**2 ) / real(nrens)
            time=real( (l-1)*parnrtime + i ) ! in seconds
            write(10,fmt) i, time, ave, sqrt(var), ft2(i,1:nrens)
         enddo
      enddo
   close(10)
   deallocate(ft2)
   stop

end subroutine
end module

