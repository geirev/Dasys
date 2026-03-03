module m_diag
contains
subroutine diag(filetype,istep,it,blanking,uave,vave,wave,velave,ustd,vstd,wstd,velstd)
   use mod_dimensions
   use m_tecout
   use m_readinfile, only : experiment
   implicit none
   integer, intent(in)   :: filetype
   integer, intent(in)   :: it
   integer, intent(in)   :: istep
   integer(kind=1), intent(in)   :: blanking(nx,ny,nz)
   real(kind=4),    intent(in)   :: uave(nx,ny,nz)
   real(kind=4),    intent(in)   :: vave(nx,ny,nz)
   real(kind=4),    intent(in)   :: wave(nx,ny,nz)
   real(kind=4),    intent(in)   :: velave(nx,ny,nz)
   real(kind=4),    intent(in)   :: ustd(nx,ny,nz)
   real(kind=4),    intent(in)   :: vstd(nx,ny,nz)
   real(kind=4),    intent(in)   :: wstd(nx,ny,nz)
   real(kind=4),    intent(in)   :: velstd(nx,ny,nz)
   character(len=20) cistep

   character(len=200) :: variables
   integer num_of_vars


   if (filetype==0) then
      write(cistep,'(a1,i6.6,a1,i3.3)')'F',istep,'_',it
      variables='i,j,k,blanking,uave,vave,wave,velave,ustd,vstd,wstd,velstd'
      num_of_vars=11

   elseif (filetype==1) then
      cistep='_GRID'
      variables='i,j,k,blanking'
      num_of_vars=4

   elseif (filetype==2) then
      write(cistep,'(i6.6)')istep
      write(cistep,'(i6.6,a1,i3.3)')istep,'_',it
      variables='uave,vave,wave,velave,ustd,vstd,wstd,velstd'
      num_of_vars=8
   endif

   call tecout(filetype,trim(experiment)//'/tecda'//trim(cistep)//'.plt',it,trim(variables),num_of_vars,blanking,&
                            uave,vave,wave,velave,ustd,vstd,wstd,velstd)

end subroutine
end module
