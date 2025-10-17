module m_assimilation
contains
subroutine assimilation(ndim,nrens,nrobs,A,D,E,Y,S,mode)
   use m_average
   use m_standarddev
   use m_correlation
   implicit none
   integer,  intent(in)    :: ndim
   integer,  intent(in)    :: nrens
   integer,  intent(in)    :: nrobs
   integer,  intent(in)    :: mode
   real,     intent(inout) :: A(ndim,nrens)
   real,     intent(in)    :: D(nrobs,nrens)
   real,     intent(in)    :: E(nrobs,nrens)
   real,     intent(in)    :: Y(nrobs,nrens)
   real,     intent(in)    :: S(nrobs,nrens)

   real,     allocatable   :: R(:,:)
   real,     allocatable   :: innov(:)

   real,     allocatable   :: meanA(:)
   real,     allocatable   :: stddevA(:)
   real,     allocatable   :: meanY(:)
   real,     allocatable   :: stddevY(:)
   real,     allocatable   :: Cxy(:,:)

   logical :: lrandrot=.false.
   logical :: lupdate_randrot=.true.
   logical :: lsymsqrt=.true.
   integer :: inflate=0
   integer :: nre=1
   real    :: infmult=1.0
   real    :: truncation=0.9
   integer i


   if (allocated(R))        deallocate(R)      ; allocate(R(nrobs,nrobs))
   if (allocated(innov))    deallocate(innov)  ; allocate(innov(nrobs))

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Test section
   if (allocated(meanA))    deallocate(meanA)  ; allocate(meanA(ndim))
   if (allocated(meanY))    deallocate(meanY)  ; allocate(meanY(nrobs))
   if (allocated(stddevA))  deallocate(stddevA); allocate(stddevA(ndim))
   if (allocated(stddevY))  deallocate(stddevY); allocate(stddevY(nrobs))
   if (allocated(Cxy))      deallocate(Cxy)    ; allocate(Cxy(ndim,nrobs))

   call average(A,meanA,ndim,nrens)
   call standarddev(A,meanA,stddevA,ndim,nrens)
   call average(Y,meanY,nrobs,nrens)
   call standarddev(Y,meanY,stddevY,nrobs,nrens)
   call correlation(Cxy,A,Y,meanA,stddevA,meanY,stddevY,ndim,nrobs,nrens)

   print '(a)','***********************************************************************************************************'
   print '(a)','Prior mean and stadard deviation of Y:'
   do i=1,ndim
      print '(i0,2g13.5)',i,meanY(i),stddevY(i)
   enddo
   print *

   print '(a)','Prior mean and stadard deviation of A:'
   do i=1,ndim
      print '(i0,2g13.5)',i,meanA(i),stddevA(i)
   enddo
   print *

   print '(a)','Correlation matrix:'
   print '(a)','--------------------------------------------------'
   do i=1,ndim
      write(*,'(100g13.5)')Cxy(i,1:nrobs)
   enddo
   print '(a)','--------------------------------------------------'
   print '(a)','***********************************************************************************************************'
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


   print *,'calling analysis()',ndim,nrens,nrobs,mode
   call analysis(A, R, E, S, D, innov, ndim, nrens, nrobs, .true., truncation, mode, &
                              lrandrot, lupdate_randrot, lsymsqrt, inflate, infmult, nre)

   call average(A,meanA,ndim,nrens)
   call standarddev(A,meanA,stddevA,ndim,nrens)
   print '(a)','Posterior mean and stadard deviation:'
   do i=1,ndim
      print '(i0,2f10.4)',i,meanA(i),stddevA(i)
   enddo
end subroutine
end module

