program tryMPI
  use mpi
  use mpi_variables
  use sum_func
  implicit none

  integer answer, i
  integer:: nTerms=20
   call MPI_INIT(ierror)
   call MPI_COMM_SIZE(MPI_COMM_WORLD, size_Of_Cluster, ierror)
   call MPI_COMM_RANK(MPI_COMM_WORLD, process_Rank, ierror)

  call MPI_Barrier(  MPI_COMM_WORLD, ierror)

  do i=1,3
     call MPI_Barrier(  MPI_COMM_WORLD, ierror)
     answer = sumFunc( nTerms)
     call MPI_Barrier(  MPI_COMM_WORLD, ierror)
     if( process_Rank == 0 ) then
        print *,'Actual answer ',answer,', expected answer ', 10*nTerms + (nTerms+1)*nTerms/2
     end if
  enddo
  
 call MPI_FINALIZE(ierror)

 if (process_Rank == root) then
    print *,'Program ran sucessfully on ', size_Of_Cluster,' processes'
 end if

END PROGRAM tryMPI

