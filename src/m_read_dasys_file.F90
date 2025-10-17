module m_read_dasys_file
  implicit none

  interface read_dasys_file
     module procedure read_dasys_file_char
     module procedure read_dasys_file_int
  end interface

contains

  subroutine read_dasys_file_char(fname, string)
    implicit none
    character(len=*), intent(in)  :: fname
    character(len=*), intent(out) :: string
    logical :: ex
    integer :: iunit

    inquire(file=trim(fname), exist=ex)
    if (ex) then
       open(newunit=iunit, file=trim(fname), status='old', action='read')
       read(iunit,'(A)') string
       close(iunit)
    else
       print '(A)', 'The file: ',trim(fname),' does not exist'
       string='undefined'
    endif
  end subroutine read_dasys_file_char


  subroutine read_dasys_file_int(fname, inum)
    implicit none
    character(len=*), intent(in) :: fname
    integer, intent(out)         :: inum
    logical :: ex
    integer :: iunit

    inquire(file=trim(fname), exist=ex)
    if (ex) then
       open(newunit=iunit, file=trim(fname), status='old', action='read')
       read(iunit,*) inum
       close(iunit)
    else
       print '(A)', 'The file: ',trim(fname),' does not exist'
       inum=-999
    endif
  end subroutine read_dasys_file_int

end module m_read_dasys_file

