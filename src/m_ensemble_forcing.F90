module m_ensemble_forcing
contains
subroutine ensemble_forcing(A,nrens,ndim)
   use m_normal
   use m_smooth_random_series
   use m_readinfile, only : iwin, experiment
   use m_parameters, only : parnr, parnrtime, pardt, lcorr, params
   implicit none
   integer, intent(in) :: nrens
   integer, intent(in) :: ndim
   real, target, intent(inout) :: A(ndim,nrens)
   real, pointer ::  A3(:,:,:)

   real, allocatable :: zforce(:,:), ensforce(:,:), endval(:), endder(:)
   real, allocatable :: ave(:,:), var(:,:), alpha(:)
   integer :: i, j, k, iunit, reclA, ia, ib
   real :: startval, startder
   real :: lambda
   character(len=100) :: ensfile
   character(len=4) :: cwin

   allocate(zforce(0:parnrtime,parnr), ensforce(0:parnrtime,parnr), endval(parnr), endder(parnr))
   allocate(ave(parnrtime-1:parnrtime,parnr), var(parnrtime-1:parnrtime,parnr))
   allocate(alpha(0:parnrtime))

   A3(0:parnrtime, 1:parnr, 1:nrens) => A

   lambda = sqrt(3.0)/real(lcorr)

   if (iwin > 1) then
! read ensemble for all assimilation windows but the first to ensure continuity and smoothness from one window to the next
      write(cwin,'(i4.4)')iwin-1
!      ensfile=trim(experiment)//'/ensemble'//cwin//'A.uf'
      ensfile=trim(experiment)//'/ensemble.uf'
      inquire(iolength=reclA)ndim,nrens,A(1:ndim,1)
      open(newunit=iunit, file=trim(ensfile), form='unformatted', access='direct', recl=reclA)
         do j=1,nrens
            read(iunit,rec=j)ia,ib,A(:,j)
         enddo
      close(iunit)

! Compute ensemble mean and variance
      do i=1,parnr
      do k=parnrtime-1,parnrtime
         ave(k,i) = sum(  A3(k,i,1:nrens) ) / real(nrens)
         var(k,i) = sum( (A3(k,i,1:nrens) - ave(k,i))**2 ) / real(nrens)
      enddo
      enddo

! Subtract the ensemble mean, since I will use the final values as starting point of the new simulated forcings
! Maybe I should also scale the variance to one?
      do j=1,nrens
      do i=1,parnr
      do k=parnrtime-1,parnrtime
         A3(k,i,j)=(A3(k,i,j) - ave(k,i)) / params(i)%stdev
      enddo
      enddo
      enddo

   endif

   do k=0,parnrtime
      alpha(k)=exp( - pardt*real(k-0)/real(lcorr))
   enddo

   do j=1,nrens
      do i = 1,parnr
         if (iwin == 1) then
            startval = normal()
            startder = lambda * normal()
         else
            startval = A3(parnrtime,i,j)
            startder = (A3(parnrtime,i,j)-A3(parnrtime-1,i,j) ) /real(pardt)
         end if

         call smooth_random_series(zforce(0,i), parnrtime+1, real(pardt), real(lcorr),  startval, startder, endval(i), endder(i))

         if (iwin==1) then
             ensforce(:,i) = params(i)%mean + params(i)%stdev * zforce(:,i)
         else
!            ensforce(:,i) = ave(parnrtime,i) +  params(i)%stdev * zforce(:,i)
            do k=0,parnrtime
               ensforce(k,i) = (1.0 - alpha(k)) * params(i)%mean + alpha(k) * (ave(parnrtime,i) +  params(i)%stdev * zforce(k,i))
            enddo
         endif
      enddo
      A3(0:parnrtime,1:parnr,j)=ensforce(0:parnrtime,1:parnr)
   enddo

   write(cwin,'(i4.4)')iwin
   !ensfile=trim(experiment)//'/ensemble'//cwin//'F.uf'
   ensfile=trim(experiment)//'/ensemble.uf'
   call system('rm -f '//trim(ensfile))
   inquire(iolength=reclA)ndim,nrens,A(1:ndim,1)
   open(newunit=iunit, file=trim(ensfile), form='unformatted', access='direct', recl=reclA)
      do j=1,nrens
         write(iunit,rec=j)ndim,nrens,A(:,j)
      enddo
   close(iunit)

   deallocate(zforce, ensforce, endval, endder)
   deallocate(ave,var, alpha)

end subroutine
end module
