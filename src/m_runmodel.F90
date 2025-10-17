module m_runmodel
contains
subroutine runmodel(u,v,w,istep,lstat)
   use mod_dimensions
   use m_localdefs
   use m_params
   use m_read_uvw
   implicit none
   logical, intent(out) ::lstat
   real(kind=4), intent(out) :: u(nx,ny,nz)
   real(kind=4), intent(out) :: v(nx,ny,nz)
   real(kind=4), intent(out) :: w(nx,ny,nz)
   integer,      intent(in)  :: istep
   integer :: iens=0
   character(len=6) cistep
   character(len=4) ciens
   logical exd,exf,exuvw
   character(len=100) directory
   character(len=100) fname


   inquire(file='infile.ref',exist=exf)
   if (.not.exf) then
      print *,'Make sure the infile.ref exists for running the reference case'
      return
   endif

   write(ciens,'(i4.4)')iens
   directory=trim(experiment)//'/mem'//ciens
   print '(a)', 'mkdir '//trim(directory)
   call system('mkdir -p '//trim(directory))
   inquire(file=trim(directory),exist=exd)
   if (.not.exd) then
      print *,'runmodel: Problem with exd; run directory does not exist'
      return
   endif

! setting up and running the model
   call system('cp measurement_loc.in '//trim(directory))
   call system('cp infile.ref '//trim(directory)//'/infile.in')
   call system('rm -f '//trim(directory)//'/uvw'//cistep//'.uf')
   call system('cd '//trim(directory)//'; '//trim(runcommand))

! Checking success (uvw*.uf) exits
   lstat=.false.
   write(cistep,'(i6.6)')istep
   fname=trim(directory)//'/uvw'//cistep//'.uf'
   inquire(file=trim(fname),exist=exuvw)
   if (exuvw) then
      lstat=.true.
      call read_uvw(fname,u,v,w)
   endif

   if (.not. lstat) print *,'runmodel for reference case failed'

end subroutine
end module
