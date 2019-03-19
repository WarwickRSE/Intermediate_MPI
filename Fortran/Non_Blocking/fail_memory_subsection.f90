PROGRAM non_block

  USE mpi
  IMPLICIT NONE

  INTEGER, PARAMETER :: tag = 100
  INTEGER, PARAMETER :: n_elements = 1000

  INTEGER :: rank
  INTEGER, DIMENSION(n_elements, n_elements) :: send_val, recv_val
  INTEGER :: nproc
  INTEGER :: left, right
  INTEGER, DIMENSION(2) :: requests
  INTEGER, DIMENSION(MPI_STATUS_SIZE, 2) :: statuses
  INTEGER :: ierr
  REAL :: time1, time2, dtime

  CALL MPI_Init(ierr)

  CALL MPI_Comm_size(MPI_COMM_WORLD, nproc, ierr)
  CALL MPI_Comm_rank(MPI_COMM_WORLD, rank, ierr)

  !Set up periodic domain
  left = rank - 1
  IF (left < 0) left = nproc - 1
  right = rank + 1
  IF (right > nproc - 1) right = 0

  send_val = rank
  CALL MPI_Isend(send_val(1,:), n_elements, MPI_INTEGER, right, tag, &
      MPI_COMM_WORLD, requests(1), ierr)
  CALL MPI_Irecv(recv_val(1,:), n_elements, MPI_INTEGER, left, tag, &
      MPI_COMM_WORLD, requests(2), ierr)

  CALL MPI_Waitall(2, requests, statuses, ierr)

  PRINT *,"Rank ", rank, " got minimal value from rank ", left, " of ", &
      MINVAL(recv_val(1,:))
  CALL MPI_Barrier(MPI_COMM_WORLD, ierr)
  IF (rank == 0) PRINT *, "It took ", dtime, " seconds to complete"

  CALL MPI_Finalize(ierr)

END PROGRAM non_block
