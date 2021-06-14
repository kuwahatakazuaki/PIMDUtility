! "================================================================="
! " usage : ./a.out [input data] [start step] [end step]"
! " You can omitte [start step] and [end step]"
! " In default, [start step] = 1 and [end step] = end of the file"
! "================================================================="
program reblocking
implicit none
integer :: i,j,k, leng
integer :: Natom, Nstart, Nstep, Neach !, Nhist
integer :: Nheader
integer :: NEstep, Nargument
real(8) :: bond_ave, bond_res, bond_dev, bond_err
real(8), allocatable  :: bonds(:), bonds2(:)
character(:), allocatable :: input_file, input_data, output
character(:), allocatable :: Nstart_char, Nstep_char
character(len=128) :: line

output="reblock.out"


! +++ Reading argument +++
Nargument = command_argument_count()
if     ( Nargument == 0 .or. Nargument >= 4)  then
  call print_usage
else
  call get_command_argument(1, length=leng)
  allocate(character(leng) :: input_data)
  call get_command_argument(1, input_data)

  if ( Nargument >= 2) then
    call get_command_argument(2, length=leng)
    allocate(character(leng) :: Nstart_char)
    call get_command_argument(2,Nstart_char)
    read(Nstart_char,*) Nstart

    if ( Nargument == 3) then
      call get_command_argument(3, length=leng)
      allocate(character(leng) :: Nstep_char)
      call get_command_argument(3,Nstep_char)
      read(Nstep_char,*) Nstep
    else  ! Nargument == 2
      call read_Nstep(Nstep,input_data,len(input_data))
    end if

  else  ! Nargument == 1
    Nstart = 1
    call read_Nstep(Nstep,input_data,len(input_data))
  end if

end if
print *, "Nstart and Nstep ", Nstart, Nstep
! +++ End Reading argument +++


! +++ Checking header +++
  Nheader = 0
  open(21,file=input_data,status='old')
    do
      read(21,'(a)',end=900) line
!      print *, line, index(line,"#")
      if (index(line,"#") == 0) exit
      Nheader = Nheader + 1
    end do
    print *, "Nheader ", Nheader
  close(21)
  Nstep = Nstep - Nheader
! +++ End Checking header +++

allocate(bonds(Nstep))
allocate(bonds2(Nstep))

! +++ Reading data +++
  open(21,file=input_data,status='old')
    rewind(21)
    do i = 1, Nheader
      read(21,'()')
    end do
    do j=1,Nstep
      read(21,*) i, bonds(j)
    enddo
  close(21)
! +++ End Reading data +++


  bond_res = 0.0d0
  do j=1,Nstep
    bond_res = bond_res + bonds(j)*bonds(j)
  enddo

  bond_ave = sum(bonds(:))/dble(Nstep)
  bond_res = bond_res/dble(Nstep)
!  print *, "res, ave", bond_res - bond_ave*bond_ave

  bond_dev = bond_res - bond_ave*bond_ave
  bond_dev = dsqrt( bond_dev/dble(Nstep-1) )
  bond_err = bond_dev / dsqrt(2.0d0*(dble(Nstep-1)))
!  print '(A,2E13.5)', " dev, err", bond_dev, bond_err

  open(22,file=output,status='unknown')
  write(22,'("# test")')
  write(22,'(i10,2E13.5)') 0, bond_dev, bond_err

  neach=Nstep
!  NEstep = Nstep
  i = 0
  do
    i = i + 1
    print *, "neach", neach
    if(mod(neach,2)==0) then
       neach=neach/2
    else
       neach=(neach-1)/2
    endif
    if (neach <= 1) exit

    do j=1,neach
      bonds2(j) = 0.5d0 * (bonds(2*j-1)+bonds(2*j))
    end do

    bond_res = 0.0d0
    Do j=1,neach
      bond_res = bond_res + bonds2(j)*bonds2(j)
    EndDo
    bond_ave = sum(bonds2(1:neach))/dble(neach)
    bond_res = bond_res/dble(neach)

    bond_dev = bond_res - bond_ave*bond_ave
    bond_dev = dsqrt( bond_dev/dble(neach-1) )
    bond_err = bond_dev / dsqrt(2.0d0*(dble(neach-1)))

    write(22,'(i10,E13.5,E13.5)') i, bond_dev, bond_err
    bonds(1:neach) = bonds2(1:neach)
  enddo

  close(22)

  deallocate(bonds)
  deallocate(bonds2)

  stop "Normal termination"
  900 stop 'All line include "#"'
end program

subroutine print_usage
print '("=================================================================")'
print '(" usage : ./a.out [input data] [start step] [end step]")'
print '(" You can omitte [start step] and [end step]")'
print '(" In default, [start step] = 1 and [end step] = end of the file")'
print '("=================================================================")'
print *
stop
end subroutine print_usage

subroutine read_Nstep(Nstep,input_data,Nchar)
implicit none
integer, intent(out) :: Nstep
integer, intent(in) :: Nchar
character(Nchar), intent(in) :: input_data
  Nstep = 0
  open(21,file=input_data,status='old')
    do
      read(21,*,end=100)
      Nstep = Nstep + 1
    enddo
  100 continue
  close(21)
return
end subroutine read_Nstep

