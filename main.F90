program tryMPI
  use mpi
  use mpi_variables
  use sum_func
  implicit none

  integer answer
  integer:: nTerms=8*9*10
   call MPI_INIT(ierror)
   call MPI_COMM_SIZE(MPI_COMM_WORLD, size_Of_Cluster, ierror)
   call MPI_COMM_RANK(MPI_COMM_WORLD, process_Rank, ierror)

  
  !print *, 'Hello World from process: ', process_Rank, 'of ', size_Of_Cluster

  call MPI_Barrier(  MPI_COMM_WORLD, ierror)
  
  answer = sumFunc( nTerms)


  call MPI_Barrier(  MPI_COMM_WORLD, ierror)
  if( process_Rank == 0 ) then
     print *,answer, 10*nTerms + (nTerms+1)*nTerms/2
  end if
 call MPI_FINALIZE(ierror)
  
  !call MPI_FINALIZE(ierror)
END PROGRAM tryMPI

