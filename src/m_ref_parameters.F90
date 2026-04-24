module m_ref_parameters
contains
subroutine ref_parameters()
   use m_parameters, only : params,pardt,parnr,parnrtime,lcorr
   use m_pseudo1D
   use m_readinfile, only : nwin,experiment
   implicit none
   character(len=100)  :: fname

   real, allocatable :: samples(:,:)

   character(len=1)yn
   logical ex
   integer i,j
   real dx,cor1,xlength
   integer n1

   fname=trim(experiment)//'/uvel_time.ref'
   yn='n'
   inquire(file=trim(fname),exist=ex)
   if (ex) then
      print '(a,i0,a,i0,a)','Use parameters in existing uvel_time.ref for reference simulation (y/n)?'
      read(*,*)yn
      if (yn.eq.'y') return
   endif

   allocate(samples(parnrtime,parnr))

! Correction for Gaussian variogram 
   cor1=real(lcorr)
   cor1=cor1/sqrt(3.0)
   xlength=real(nwin)
   dx=xlength/float(parnrtime)
   n1=2*nint(real(parnrtime)*cor1/(2.0*dx))
   print '(2(a,i5),a,f10.3)','parnrtime=',parnrtime,', n1=',n1,', dt=',dx
   call pseudo1D(samples,parnrtime,parnr,cor1,dx,n1,.true.)

   do j=1,parnr
      do i=1,parnrtime
         samples(i,j)=params(j)%mean+params(j)%stdev*samples(i,j)
      enddo
   enddo

   if (ex) call system('mv '//trim(fname)//' '//trim(fname)//'bak')

   open(10,file=trim(fname))
      do i=1,parnrtime
         write(10,'(100f13.5)')real(i-1)*pardt,samples(i,1:parnr)
      enddo
   close(10)


   deallocate(samples)
end subroutine
end module

