!Average the electrostatic potentials in each xy plane
program main
  implicit none
  integer g1, g2, g3
  real,allocatable:: pot(:)
  integer atom(1:20)
  integer i, j, k, natom

  open(unit=11,file='LOCPOT',status='old')
  open(unit=12,file='zpot.dat')

  !HEADER
  do i=1,5
     read(11,*)
  end do

  !ATOMIC COORDINATES
  call atomtype(11, natom)
  atom=0
  read(11,*) atom(1:natom)

  do i=1,sum(atom)+2
     read(11,*)
  end do

  !GRID
  read(11,*) g1, g2, g3
  allocate(pot(1:g1*g2*g3))

  do i=1,g1*g2*g3/5
     read(11,*) pot(i*5-4),pot(i*5-3),pot(i*5-2),pot(i*5-1),pot(i*5)
  end do

  if(mod(g1*g2*g3,5)>0)then
     read(11,*) pot( (g1*g2*g3+1-mod(g1*g2*g3,5)):(g1*g2*g3) )
  end if

  do k=1,g3
     write(12,'(I4,e12.4)') k, sum( pot(((k-1)*g1*g2+1):(k*g1*g2)) )/g1/g2
  end do
end program


subroutine atomtype(filenum, atom_type)
  implicit none
  integer i, flag, filenum
  integer atom_type
  character(250) string

  atom_type= 0
  flag= 0
  read(filenum,'(a)') string
  do i=1, 250
     if( (string(i:i)/=' ').and.(flag==0) )then
        flag= 1
        atom_type= atom_type+1
     else if( (string(i:i)==' ').and.(flag==1) )then
        flag= 0
     end if
  end do
end subroutine
