module m_count_parameters

contains

integer function count_parameters(fname)
    implicit none
    character(len=*), intent(in) :: fname
    logical ex
    integer iunit
    character(len=1) ch

    count_parameters=0
    inquire(file=trim(fname), exist=ex)
    if (ex) then
       open(newunit=iunit, file=trim(fname), status='old', action='read')
       do
          read(iunit,'(a1)',end=999,err=999)ch
          if (ch == '#') count_parameters= count_parameters + 1
       enddo
       999 close(iunit)
    else
       print '(3a)', 'The file: ',trim(fname),' does not exist'
    endif


end function
end module
