PROGRAM DIM1_HIST
  implicit none
  Double Precision, Parameter  :: bohr             = 1.d0/0.52918d0
  integer :: i,j,k,l,m,n
  integer :: natom,nbeads,nstep,neach,nhist,nstart
  integer :: iatom,jatom,katom,latom,matom,oatom,patom
  double precision,allocatable :: x(:),y(:),z(:) 
  double precision,allocatable :: ux(:),uy(:),uz(:) 
  double precision,allocatable :: hist(:),xhist(:),bonds(:)
  character(len=1),allocatable :: a(:)
  double precision :: length1,length2,length3,length4,length5,length6,length7,length8
  double precision :: angle1,angle2,angle3,angle4,angle5
  double precision :: dihedral1,dihedral2,dihedral3
  double precision :: bond_ave,bond_sig,bond_min,bond_max,dhist
  character(len=90) :: afile1,afile2  

  read(5,*) nstep
  read(5,*) nhist
  read(5,'(a)') afile1
  read(5,'(a)') afile2

  open(95,file=trim(afile1),form='formatted',status='unknown')
  open(96,file=trim(afile2),form='formatted',status='unknown')

  allocate(bonds(nstep))
  allocate(x(nstep))
  allocate(y(nstep))

  k=0
  n=0
  do l=1,nstep
     read(95,*) i,j,k,bonds(l)
  enddo
  close(95)
  length1=0.0d0
  length2=0.0d0
  do l=1,nstep
     y(l)=bonds(l)
     length1=length1+bonds(l)/dble(nstep)
     length2=length2+bonds(l)*bonds(l)/dble(nstep)
  enddo
  length7=dsqrt(dabs(length2-length1*length1))/dsqrt(dble(nstep-1))
  length8=length7/dsqrt(2.0D0*(dble(nstep)-1.0D0))
  nstart=0
  write(96,'(i10,E13.5,E13.5)') nstart,length7,length8

  neach=nstep
  nstart=1
  do i=1,nhist
     if(mod(neach,2)==0) then
        neach=neach/2
     else
        neach=(neach-1)/2
     endif
     Do j=1,neach
        x(j)=y(2*(j-1)+1)+y(2*(j-1)+2)
        x(j)=x(j)/2.0D00
     EndDo
     length1=0.0D0
     length2=0.0D0
     Do j=1,neach
        length1=length1+x(j)/dble(neach)
        length2=length2+x(j)*x(j)/dble(neach)
     EndDo
     length3=dsqrt(dabs(length2-length1*length1))/dsqrt(dble(neach-1))
     length4=length3/dsqrt(2.0D0*(dble(neach)-1.0D0))
     write(96,'(i10,E13.5,E13.5)') i,length3,length4
     Do j=1,neach
        y(j)=x(j)
     EndDo
  enddo

  close(96)

  deallocate(bonds)
  deallocate(x)
  deallocate(y)

  stop
end program
