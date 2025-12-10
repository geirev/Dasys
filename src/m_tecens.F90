module m_tecens
contains
subroutine tecens(A,ndim,nrens,nmda,it)
   use m_localdefs, only : experiment
   implicit none
   integer, intent(in) :: ndim
   integer, intent(in) :: nrens
   real,    intent(in) :: A(ndim,nrens)
   integer, intent(in) :: nmda
   integer, intent(in) :: it
   integer iunit,j
   character(len=100) :: fname
   character(len=2)   :: cnmda

   print *,'tecens A'
   write(cnmda,'(i2.2)')nmda
   fname=trim(experiment)//'/tecstate'//cnmda//'.dat'
   print *,'tecens B',trim(fname)
   if (it==0) then
      call system('rm -f '//trim(fname))
      open(newunit=iunit,file=trim(fname),status='new',action='write')
         write(iunit,'(3a)')'TITLE = "Statevariables nmda=',cnmda,'"'
         write(iunit,'(a)')'VARIABLES = "j" "vel" "dir"'
         j=1
         write(iunit,'(a,i3.3,a)')'ZONE T="reference", I=',j,', DATAPACKING=POINT'
         write(iunit,'(i3,100g13.5)')j,A(:,j)
      close(iunit)
   else
      open(newunit=iunit,file=trim(fname),status='old',action='write',position='append')
         if (it==1) then
            write(iunit,'(a,i3.3,a)')'ZONE T="prior", I=',nrens,', DATAPACKING=POINT'
         elseif(it==nmda+1) then
            write(iunit,'(a,i3.3,a)')'ZONE T="posterior", I=',nrens,', DATAPACKING=POINT'
         else
            write(iunit,'(a,i2.2,a,i3.3,a)')'ZONE T="it',it,'", I=',nrens,', DATAPACKING=POINT'
         endif
         do j=1,nrens
            write(iunit,'(i3,100g13.5)')j,A(:,j)
         enddo
      close(iunit)
   endif


end subroutine
end module

