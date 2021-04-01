program tryMPI
  use arraySizes
  implicit none
  
  include 'mpif.h'

  integer process_Rank, size_Of_Cluster, ierror, tag
  double precision, dimension (nTraining, nDimensions, nAtoms, nAtoms) :: exponentials
  double precision sum
  integer i,j,Delta, Gamma

  print *,exp(1.d0)
  
  do i=1, nTraining
     do j=1, nDimensions
        do Delta=1,nAtoms
           do Gamma=1,nAtoms
              exponentials( i,j,Delta,Gamma) = exp( -(Delta-Gamma)**2/(1.d0*nAtoms**2*i*j))
           end do
        end do
     end do
  end do


  sum =0.d0
  do i=1, nTraining
     do j=1, nDimensions
        do Delta=1,nAtoms
           do Gamma=1,nAtoms
              sum = sum + exponentials( i,j,Delta,Gamma) 
           end do
        end do
     end do
  end do

  print *,sum
  

  call MPI_INIT(ierror)
  call MPI_COMM_SIZE(MPI_COMM_WORLD, size_Of_Cluster, ierror)
  call MPI_COMM_RANK(MPI_COMM_WORLD, process_Rank, ierror)

  print *, 'Hello World from process: ', process_Rank, 'of ', size_Of_Cluster

  call MPI_FINALIZE(ierror)

end program tryMPI
