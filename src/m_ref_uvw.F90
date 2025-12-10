module m_ref_uvw
use mod_dimensions
contains
subroutine ref_uvw(u,v,w,uave,vave,wave,velave,ustd,vstd,wstd,velstd,nrens,istep)
   use mod_dimensions
   use m_localdefs
   use m_read_uvw
   integer, intent(in)  :: nrens
   integer, intent(in)  :: istep
   real(kind=4), intent(out) :: u(nx,ny,nz,0:nrens)
   real(kind=4), intent(out) :: v(nx,ny,nz,0:nrens)
   real(kind=4), intent(out) :: w(nx,ny,nz,0:nrens)

   real(kind=4), intent(out) :: uave(nx,ny,nz)
   real(kind=4), intent(out) :: vave(nx,ny,nz)
   real(kind=4), intent(out) :: wave(nx,ny,nz)
   real(kind=4), intent(out) :: velave(nx,ny,nz)

   real(kind=4), intent(out) :: ustd(nx,ny,nz)
   real(kind=4), intent(out) :: vstd(nx,ny,nz)
   real(kind=4), intent(out) :: wstd(nx,ny,nz)
   real(kind=4), intent(out) :: velstd(nx,ny,nz)


   integer i,j,k,iens

   character(len=100) fname
   character(len=6) cistep
   character(len=4) ciens
   logical ex


   write(cistep,'(i6.6)')istep

! load uvw from reference run
   iens=0
   write(ciens,'(i4.4)')iens
   fname=trim(experiment)//'/mem'//ciens//'/uvw'//cistep//'.uf'
   inquire(file=trim(fname),exist=ex)
   if (ex) then
      call read_uvw(fname,u,v,w)
      print *,'u',u(1:10,ny/2,1,0)
   else
      print '(a)','Could not load u,v, and w for reference case'
   endif

! Mean
   do k=1,nz
   do j=1,ny
   do i=1,nx
      uave(i,j,k) = u(i,j,k,0)
      vave(i,j,k) = v(i,j,k,0)
      wave(i,j,k) = w(i,j,k,0)
      velave(i,j,k)= sqrt(u(i,j,k,0)**2 + v(i,j,k,0)**2 + w(i,j,k,0)**2)
   enddo
   enddo
   enddo

   ustd=0.0
   vstd=0.0
   wstd=0.0
   velstd=0.0

end subroutine
end module
