MODULE fns

  USE mpi
  IMPLICIT NONE

  INTEGER, PARAMETER :: tag = 100

  CONTAINS

  SUBROUTINE send_vals(dest, nels, request)
    INTEGER, INTENT(IN) :: dest, nels
    INTEGER, DIMENSION(nels) :: vals
    INTEGER, INTENT(INOUT) :: request
    INTEGER :: ierr

    vals = 10
    CALL MPI_Isend(vals, nels, MPI_INTEGER, dest, tag, &
        MPI_COMM_WORLD, request, ierr)

  END SUBROUTINE send_vals

END MODULE fns


PROGRAM non_block

  USE mpi
  USE fns
  IMPLICIT NONE

  INTEGER, PARAMETER :: n_elements = 100000

  INTEGER :: rank
  INTEGER, DIMENSION(n_elements) :: recv_val
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

  CALL send_vals(right, n_elements, requests(1))
  CALL MPI_Irecv(recv_val, n_elements, MPI_INTEGER, left, tag, MPI_COMM_WORLD, &
        requests(2), ierr)

  CALL MPI_Waitall(2, requests, statuses, ierr)

  PRINT *,"Rank ", rank, " got minimal value from rank ", left, " of ", &
      MINVAL(recv_val)
  CALL MPI_Barrier(MPI_COMM_WORLD, ierr)
  IF (rank == 0) PRINT *, "It took ", dtime, " seconds to complete"

  CALL MPI_Finalize(ierr)

END PROGRAM non_block
