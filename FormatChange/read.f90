program main
implicit none
integer :: i,j, leng
!integer :: Natom = 8, Nbead = 32
integer :: Natom_Nbead, Nstep, Istep
!character(len=2), allocatable :: atom(:), label(:)
character(:), allocatable :: Finput, Foutput
real(8), allocatable :: r(:,:,:)  !r(xyz,Natom_Nbead,Nstep)
real(8) :: dummy(3)


!Natom_Nbead = Natom * Nbead
Natom_Nbead = 32
Nstep = 6
allocate(r(3,Natom_Nbead,Nstep))
if ( command_argument_count() == 0) then
  call usage
else
  call get_command_argument(1, length=leng)
    allocate(character(leng) :: Finput)
    call get_command_argument(1, Finput)
  print '(a,a,/)', "   Reading from ", '"'//Finput//'"'
end if

open(21, file=Finput, form='unformatted', access='stream', status='old',err=900)
  do i = 1, 10
    do j = 1, Natom_Nbead
      read(21,end=911) dummy(:)
    end do
  end do

  do i = 1, Nstep
    do j = 1, Natom_Nbead
      read(21) r(:,j,i)
    end do
  end do
close(21)

open(22,file='temp.xyz', status='replace')
  do i = 1, Nstep
    write(22,'(I5)') Natom_Nbead
    write(22,'(I10)') i-1
    do j = 1, Natom_Nbead
      write(22,'(3(x,E16.9))') r(:,j,i)
    end do
  end do
close(22)


stop "Normal termination"
900 print *, "Tere is no input file"
911 print *, "Erro"
stop "Error termination !!!"
end program main

subroutine usage
implicit none
  print '("   There is no argument")'
  stop  "Error termination !!!"
end subroutine usage

