module m_loaduvw
contains
subroutine loaduvw(u,v,w,nrens,it,istep)
   use mod_dimensions
   use m_readinfile, only : experiment
   use m_params
   use m_read_uvw
   implicit none
   integer, intent(in)  :: nrens
   integer, intent(in)  :: it
   integer, intent(in)  :: istep
   real(kind=4), intent(inout) :: u(nx,ny,nz,0:nrens)
   real(kind=4), intent(inout) :: v(nx,ny,nz,0:nrens)
   real(kind=4), intent(inout) :: w(nx,ny,nz,0:nrens)

   character(len=100) fname
   character(len=6) cistep
   character(len=4) ciens
   character(len=2) cit

   logical ex
   integer iens

   write(cit,'(i2.2)')it
   write(cistep,'(i6.6)')istep


! load uvw from ensemble run
   do iens=1,nrens
      write(ciens,'(i4.4)')iens
      fname=trim(experiment)//'/mem'//ciens//'/it'//cit//'/uvw'//cistep//'.uf'
      inquire(file=trim(fname),exist=ex)
      if (ex) then
          call read_uvw(trim(fname),u(:,:,:,iens),v(:,:,:,iens),w(:,:,:,iens))
      else
         print *,'FILE:',trim(fname),' DOES NOT EXIST, COULD NOT LOAD ENSEMBLE OF U, V, AND W'
         exit
      endif
   enddo

end subroutine
end module
