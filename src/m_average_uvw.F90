module m_average_uvw
use mod_dimensions
contains
subroutine average_uvw(u,v,w,uave,vave,wave,velave,ustd,vstd,wstd,velstd,nrens)
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

   real(kind=4) :: vel(nx,ny,nz,nrens)

   integer i,j,k,n

! Mean
   uave=0.0
   vave=0.0
   wave=0.0
   velave=0.0

   do n=1,nrens
      do k=1,nz
      do j=1,ny
      do i=1,nx
         uave(i,j,k) =uave(i,j,k) +u(i,j,k,n)
         vave(i,j,k) =vave(i,j,k) +v(i,j,k,n)
         wave(i,j,k) =wave(i,j,k) +w(i,j,k,n)
         vel(i,j,k,n)= sqrt(u(i,j,k,n)**2 + v(i,j,k,n)**2 + w(i,j,k,n)**2)
         velave(i,j,k) =velave(i,j,k) + vel(i,j,k,n)
      enddo
      enddo
      enddo
   enddo

   uave=uave/real(nrens,kind=4)
   vave=vave/real(nrens,kind=4)
   wave=wave/real(nrens,kind=4)
   velave=velave/real(nrens,kind=4)

! stdiance
   ustd=0.0
   vstd=0.0
   wstd=0.0
   velstd=0.0

   do n=1,nrens
      do k=1,nz
      do j=1,ny
      do i=1,nx
           ustd(i,j,k) =  ustd(i,j,k) + (  u(i,j,k,n)  -  uave(i,j,k)) * (  u(i,j,k,n) -   uave(i,j,k))
           vstd(i,j,k) =  vstd(i,j,k) + (  v(i,j,k,n)  -  vave(i,j,k)) * (  v(i,j,k,n) -   vave(i,j,k))
           wstd(i,j,k) =  wstd(i,j,k) + (  w(i,j,k,n)  -  wave(i,j,k)) * (  w(i,j,k,n) -   wave(i,j,k))
         velstd(i,j,k) =velstd(i,j,k) + (vel(i,j,k,n) - velave(i,j,k)) * (vel(i,j,k,n) - velave(i,j,k))
      enddo
      enddo
      enddo
   enddo

   ustd=ustd/real(nrens-1,kind=4)
   vstd=vstd/real(nrens-1,kind=4)
   wstd=wstd/real(nrens-1,kind=4)
   velstd=velstd/real(nrens-1,kind=4)

   ustd=sqrt(ustd)
   vstd=sqrt(vstd)
   wstd=sqrt(wstd)
   velstd=sqrt(velstd)

end subroutine
end module
