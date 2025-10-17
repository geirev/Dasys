module m_read_parameters
contains

subroutine read_parameters(fname, params, ndim)
   use m_params
   implicit none
   character(len=*), intent(in)            :: fname
   integer, intent(in)                     :: ndim
   type(uncertain_parameters), intent(out) :: params(ndim)
   logical :: ex
   integer :: iunit, i, ios
   character(len=256) :: line

   i = 0
   inquire(file=trim(fname), exist=ex)
   if (.not. ex) then
      print '(3a)', 'The file: ', trim(fname), ' does not exist'
      return
   endif

   print '(a)','Uncertain parameters defined:'
   open(newunit=iunit, file=trim(fname), status='old', action='read')
   do
      read(iunit,'(A)',end=999,err=999) line
      if (len_trim(line) == 0) cycle    ! skip empty lines
      if (line(1:1) == '#') then
         i = i + 1
         if (i > ndim) then
            print *, "Warning: more parameters in file than ndim"
            exit
         endif
         ! parse the rest of the line: string + 2 numbers
         read(line(2:), *, iostat=ios) params(i)%parname, params(i)%mean, params(i)%stdev
         if (ios /= 0) then
            print *, "Error reading line: ", trim(line)
         endif
         write(*,'(i4.4,a10,2f10.4)')i, trim(params(i)%parname), params(i)%mean, params(i)%stdev
      endif
   end do
999 close(iunit)

  end subroutine

end module

