module m_tecout
implicit none

contains
subroutine tecout(filetype,filename,it,variables_string,num_of_variables,blanking,&
                                        uave,vave,wave,velave,ustd,vstd,wstd,velstd)
   use mod_dimensions
   use m_tecplot
   implicit none
   character(len=*), intent(in) :: filename         ! Output filename
   integer,          intent(in) :: filetype           ! Save only geometry when filetype=1
   integer,          intent(in) :: it               ! Timestep index
   character(len=*), intent(in) :: variables_string ! Variables to print separated by ,
   integer,          intent(in) :: num_of_variables ! Number of vaiables to print

   integer(kind=1), intent(in)   :: blanking(nx,ny,nz)
   real(kind=4),    intent(in)   :: uave(nx,ny,nz)
   real(kind=4),    intent(in)   :: vave(nx,ny,nz)
   real(kind=4),    intent(in)   :: wave(nx,ny,nz)
   real(kind=4),    intent(in)   :: velave(nx,ny,nz)
   real(kind=4),    intent(in)   :: ustd(nx,ny,nz)
   real(kind=4),    intent(in)   :: vstd(nx,ny,nz)
   real(kind=4),    intent(in)   :: wstd(nx,ny,nz)
   real(kind=4),    intent(in)   :: velstd(nx,ny,nz)


   ! define a tecplot object
   type(tecplot_time_file) :: plt_file
   integer,allocatable :: locations(:)
   integer,allocatable :: type_list(:)
   integer,allocatable :: shared_list(:)
   integer :: i,j,k,d
   real(kind=4), allocatable :: your_datas(:,:,:,:)
   real(kind=4) :: physics_time
   real :: xyz(3)
   integer dd

   physics_time=real(it)
   print '(5a,f10.2)','tecout: ',trim(filename),' ',trim(variables_string),' iteration=',physics_time

   allocate(your_datas(nx,ny,nz,num_of_variables))
   allocate(locations(num_of_variables))
   allocate(type_list(num_of_variables))
   allocate(shared_list(num_of_variables))

   ! locations = 0 means data in node, 1 means data in cell(not supported yet)
   locations = 0
   ! shared_list(i)=-1 means the i-th data is not shared in this zone. If shared_list(i)=m,
   ! it means the i-th data is shared with zone m in this file
      shared_list = -1
   ! type_list(i) = 1 means the i-th data is of type float. (Other data type not supported yet.)
   type_list = 1

   ! call init subroutine first
   ! nx, ny, nz means the dimension of the data
   ! 'x,y,z,u,v,w' is a string contains names of variables, must be divided by ','
   call plt_file%init(filename,nx,ny,nz,filetype,'LBM3D output',filetype,trim(variables_string))

   ! for each zone, call the two subroutines
   ! physics_time can be any value, it will only be used when there are more than 1 zone in a file.
   call plt_file%write_zone_header(filename(4:9), physics_time, 0, locations)

   ! your_datas(:,:,:,1:3) =  x,y,z coordinates(Variable assignment is omitted in this example)
   ! ALL datas are stored in sequence like (((x(ix,iy,iz),ix=1,nx),iy=1,ny),iz=1,nz)
   ! set coordinate

! static grid data
   if ((filetype == 0) .or. (filetype == 1)) then
      do d = 1, 3
         do concurrent(i=1:nx, j=1:ny, k=1:nz)
            xyz = [i-1., j-1., k-1.]
            your_datas(i,j,k,d) = xyz(d)
         end do
      end do
      dd=3+1; your_datas(:,:,:,dd)  = blanking(:,:,:)
   endif

! solution data
   if ((filetype == 0) .or. (filetype == 2)) then
      if (filetype == 2) dd=0
      dd=dd+1; your_datas(:,:,:,dd)  = uave(:,:,:)
      dd=dd+1; your_datas(:,:,:,dd)  = vave(:,:,:)
      dd=dd+1; your_datas(:,:,:,dd)  = wave(:,:,:)
      dd=dd+1; your_datas(:,:,:,dd)  = velave(:,:,:)
      dd=dd+1; your_datas(:,:,:,dd)  = ustd(:,:,:)
      dd=dd+1; your_datas(:,:,:,dd)  = vstd(:,:,:)
      dd=dd+1; your_datas(:,:,:,dd)  = wstd(:,:,:)
      dd=dd+1; your_datas(:,:,:,dd)  = velstd(:,:,:)
   endif

   call plt_file%write_zone_data(type_list, shared_list, your_datas)

   ! before exit, you must call complete subroutine
   call plt_file%complete


end subroutine

end module
