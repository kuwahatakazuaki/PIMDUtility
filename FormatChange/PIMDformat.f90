program main
implicit none
integer :: i,j, leng
integer :: Natom_Nbead, Nstep, Istep
character(len=2), allocatable :: Catom(:), Ratom(:)
character(:), allocatable :: Finput, Foutput
real(8), allocatable :: r(:,:,:)  !r(xyz,Natom_Nbead,Nstep)

if ( command_argument_count() == 0) then
  call usage
else
  call get_command_argument(1, length=leng)
    allocate(character(leng) :: Finput)
    call get_command_argument(1, Finput)
  print '(a,a,/)', "   Reading from ", '"'//Finput//'"'
end if

open(21, file=Finput,status='old',err=900)
  read(21,*) Natom_Nbead
  read(21,'()')
  allocate(Catom(Natom_Nbead))
  allocate(Ratom(Natom_Nbead))
  do i = 1, Natom_Nbead
    read(21,*) Catom(i)
  end do

rewind(21)
  Nstep = 0
  do
    read(21,'()',end=100)
    read(21,'()',end=100)
    do i = 1, Natom_Nbead
!      read(21,'()',end=100)
      read(21,*,end=100) Ratom(i)
      if ( Catom(i) /= Ratom(i) ) then
        print *, "Error at :", Nstep*Natom_Nbead + i
        stop "Error termination"
      end if
    end do
    Nstep = Nstep + 1
  end do
  100 continue
  print *, "Nstep = ", Nstep
  allocate(r(3,Natom_Nbead,Nstep))

rewind(21)
  do i = 1,Nstep
    read(21,'()')
    read(21,'()')
    do j = 1, Natom_Nbead
      read(21,*,end=101) Catom(j), r(:,j,i)
    end do
  end do
  101 continue
close(21)

open(22,file='coor_binary', form='unformatted', access='stream', status='replace')
  do i = 1, Nstep
    do j = 1, Natom_Nbead
      write(22) r(:,j,i)
    end do
  end do
close(22)


print '(a)', 'Data is saved in "coor_binary"'
stop "Normal termination"
900 print *, "Tere is no input file"
stop "Error termination !!!"


contains

  subroutine usage
  implicit none
    print '("   There is no argument")'
    print '("   This program make a binary coordinate file")'
    print '("   Choose the coordinate file, usually coor.xyz")'
    stop  "Error termination !!!"
  end subroutine usage

end program main


