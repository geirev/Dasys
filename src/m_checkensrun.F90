module m_checkensrun
contains
subroutine checkensrun(u,v,w,lensrun,nrens,it,istep)
   use mod_dimensions
   use m_localdefs
   use m_params
   implicit none
   integer, intent(in)  :: nrens
   integer, intent(in)  :: it
   integer, intent(in)  :: istep
   logical, intent(out) :: lensrun
   real(kind=4), intent(inout) :: u(nx,ny,nz,0:nrens)
   real(kind=4), intent(inout) :: v(nx,ny,nz,0:nrens)
   real(kind=4), intent(inout) :: w(nx,ny,nz,0:nrens)

   character(len=100) directory
   character(len=100) fname
   character(len=6) cistep
   character(len=4) ciens
   character(len=2) cit

   logical ex
   integer iens

   write(cistep,'(i6.6)')istep
   write(cit,'(i2.2)')it
   lensrun=.true.
   do iens=1,nrens
      write(ciens,'(i4.4)')iens
      directory=trim(experiment)//'/mem'//ciens//'/it'//cit
      fname=trim(directory)//'/uvw'//cistep//'.uf'
      inquire(file=trim(fname),exist=ex)
      if (.not.ex) then
         print '(a,a,l1)',trim(fname),': ',ex
         lensrun=.false.
         exit
      endif
   enddo

end subroutine
end module
