module m_ref_uvw
use mod_dimensions
contains
subroutine ref_uvw(u,v,w,uave,vave,wave,velave,ustd,vstd,wstd,velstd,nrens)
   use mod_dimensions
   integer, intent(in)  :: nrens
   real(kind=4), intent(in)  :: u(nx,ny,nz,0:nrens)
   real(kind=4), intent(in)  :: v(nx,ny,nz,0:nrens)
   real(kind=4), intent(in)  :: w(nx,ny,nz,0:nrens)

   real(kind=4), intent(out) :: uave(nx,ny,nz)
   real(kind=4), intent(out) :: vave(nx,ny,nz)
   real(kind=4), intent(out) :: wave(nx,ny,nz)
   real(kind=4), intent(out) :: velave(nx,ny,nz)

   real(kind=4), intent(out) :: ustd(nx,ny,nz)
   real(kind=4), intent(out) :: vstd(nx,ny,nz)
   real(kind=4), intent(out) :: wstd(nx,ny,nz)
   real(kind=4), intent(out) :: velstd(nx,ny,nz)


   integer i,j,k

! Mean
   do k=1,nz
   do j=1,ny
   do i=1,nx
      uave(i,j,k) = u(i,j,k,1)
      vave(i,j,k) = v(i,j,k,1)
      wave(i,j,k) = w(i,j,k,1)
      velave(i,j,k)= sqrt(u(i,j,k,1)**2 + v(i,j,k,1)**2 + w(i,j,k,1)**2)
   enddo
   enddo
   enddo

   ustd=0.0
   vstd=0.0
   wstd=0.0
   velstd=0.0

end subroutine
end module
