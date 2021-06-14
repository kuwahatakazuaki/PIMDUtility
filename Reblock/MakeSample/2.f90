program main
implicit none
integer :: leng
integer :: i
character(:), allocatable :: input_file, input_data, output
character(len=128) line

  call get_command_argument(1, length=leng)
  allocate(character(leng) :: input_data)
  call get_command_argument(1, input_data)

  open(21,file=input_data,status='old')
    do i = 1, 10
      read(21,'(a)',end=100) line
!      read(21,'(a)') line
      print *, line
    end do
    100 continue
  close(21)
  stop "HERE"
end program main

