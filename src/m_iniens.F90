module m_iniens
contains
subroutine iniens(A,experiment,nrens,params,ndim,parnr,parnrtime,pardt)
   use m_params
   use m_pseudo1D
   use m_normal
   use m_resetensemble
   use m_readinfile, only : cor,len
   implicit none
   integer, intent(in) :: nrens
   integer, intent(in) :: ndim
   integer, intent(in) :: parnr
   integer, intent(in) :: parnrtime
   integer, intent(in) :: pardt
   character(len=*), intent(in) :: experiment

   type(uncertain_parameters), intent(in) :: params(parnr)

   real, target, intent(out) :: A(ndim,nrens)
   real, pointer :: A2(:,:)
   real, pointer :: A3(:,:,:)

   character(len=1)yn
   logical ex

   integer i,j,n,l,iunit,reclA,jtmp
   real dx,cor1,xlength
   integer n1
   character(len=100) ensfile
  ! Remap the same storage: no allocation, no copy
   if (ndim /= parnrtime*parnr) error stop "iniens: ndim /= parnrtime*parnr"
   A3(1:parnrtime, 1:parnr, 1:nrens) => A
   A2(1:parnrtime, 1:parnr*nrens) => A


   inquire(iolength=reclA)ndim,nrens,A(:,1)

   do l=1,2
      if (l==1) then
         ensfile=trim(experiment)//'/iniens.uf'
      elseif (l==2) then
         ensfile=trim(experiment)//'/finens.uf'
      else
         stop 'iniens error'
      endif

      yn='n'
      inquire(file=trim(ensfile),exist=ex)
      if (ex) then
         open(newunit=iunit, file=trim(ensfile), form='unformatted', access='direct', recl=reclA)
         read(iunit,rec=1)i,j
         close(iunit)
         print '(3a,i0,a,i0,a)','Read ',trim(ensfile),' of dimensions: (',i,'x',j,') (y/n)?'
         read(*,*)yn
         if (yn.eq.'y' .and. i==ndim .and. j==nrens) then
            open(newunit=iunit, file=trim(ensfile), form='unformatted', access='direct', recl=reclA)
               do j=1,nrens
                  read(iunit,rec=j)i,jtmp,A(:,j)
               enddo
            close(iunit)
            print *,'iniens: initial A read from ',trim(ensfile)
            return
         endif
      endif
   enddo



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! if yn='n' then regenerate initial ensemble

   cor1=real(cor)
   cor1=cor1/sqrt(3.0)
   xlength=real(len)
   dx=xlength/real(parnrtime-1)
   n1=2*nint(real(parnrtime)*cor1/(2.0*dx))
   print '(2(a,i5),a,f10.3)','parnrtime=',parnrtime,', n1=',n1,', dt=',dx
   call pseudo1D(A2,parnrtime,nrens*parnr,cor1,dx,n1,.true.)


   do n=1,nrens
      do j=1,parnr
         do i=1,parnrtime
            A3(i,j,n)=params(j)%mean+params(j)%stdev*A3(i,j,n)
         enddo
      enddo
   enddo

   ensfile=trim(experiment)//'/iniens.uf'
   call system('rm -f '//trim(ensfile))
   inquire(iolength=reclA)ndim,nrens,A(:,1)
   open(newunit=iunit, file=trim(ensfile), form='unformatted', access='direct', recl=reclA)
      do j=1,nrens
         write(iunit,rec=j)ndim,nrens,A(:,j)
      enddo
   close(iunit)

   print *,'iniens: A initialization done and saved to ',trim(ensfile)
end subroutine
end module

