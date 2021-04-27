program tryMPI
  use arraySizes
  implicit none
  
  include 'mpif.h'

  integer process_Rank, size_Of_Cluster, ierror, tag
  double precision, dimension (nTraining, nDimensions, nAtoms, nAtoms) :: exponentials
  double precision sum
  double precision:: myNumber = 21.d0
  integer i,j,Delta, Gamma, message_Item

  print *,process_Rank
  STOP
  
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


  DO i = 0, 10
     IF(i == process_Rank) THEN
        print *, 'Hello World from process: ', process_Rank, 'of ', size_Of_Cluster
     END IF
     call MPI_BARRIER( MPI_COMM_WORLD, ierror)
  END DO

  
  IF(process_Rank == 0) THEN
     message_Item = 42
     call MPI_SEND(message_Item, 1, MPI_INT, 1, 1, MPI_COMM_WORLD, ierror)  
     print *, "Sending message containing: ", message_Item
  ELSE IF(process_Rank == 1) THEN
     call MPI_RECV(message_Item, 1, MPI_INT, 0, 1, MPI_COMM_WORLD, MPI_STATUS_IGNORE, ierror)
     print *, "Received message containing: ", message_Item
  END IF
  
  call MPI_FINALIZE(ierror)

end program tryMPI
