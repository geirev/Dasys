module m_read_uvw
contains
subroutine read_uvw(fname,u,v,w)
   use mod_dimensions, only : nx,ny,nz
   implicit none
   real(kind=4), intent(out) :: u(nx,ny,nz)
   real(kind=4), intent(out) :: v(nx,ny,nz)
   real(kind=4), intent(out) :: w(nx,ny,nz)
   character(len=*), intent(in) :: fname
   integer iunit

   open(newunit=iunit,file=trim(fname),form="unformatted", status='unknown')
         read(iunit)u,v,w
   close(iunit)

end subroutine
end module

