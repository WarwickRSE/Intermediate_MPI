PROGRAM dch

  USE mpi
  IMPLICIT NONE

  INTEGER, PARAMETER :: tag = 100

  INTEGER :: rank, recv_rank
  INTEGER :: nproc
  INTEGER :: left, right
  INTEGER :: ierr
  INTEGER, DIMENSION(2) :: requests
  INTEGER, DIMENSION(MPI_STATUS_SIZE, 2) :: statuses
  INTEGER :: repcount
  REAL :: time1, time2, time3, dtime1, dtime2

  CALL MPI_Init(ierr)

  CALL MPI_Comm_size(MPI_COMM_WORLD, nproc, ierr)
  CALL MPI_Comm_rank(MPI_COMM_WORLD, rank, ierr)

  !Set up periodic domain
  left = rank - 1
  IF (left < 0) left = nproc - 1
  right = rank + 1
  IF (right > nproc - 1) right = 0

  time1 = MPI_Wtime()
  CALL MPI_Send_init(rank, 1, MPI_INTEGER, right, tag, MPI_COMM_WORLD, &
    requests(1), ierr)
  CALL MPI_Recv_init(recv_rank, 1, MPI_INTEGER, left , tag, MPI_COMM_WORLD, &
    requests(2), ierr)
  time2 = MPI_Wtime()
  DO repcount = 1, 1000000
    CALL MPI_Startall(2, requests, ierr)
    CALL MPI_Waitall(2, requests, statuses, ierr)
  END DO
  time3 = MPI_Wtime()

  dtime1 = time3-time1
  CALL MPI_Reduce(dtime1, MPI_IN_PLACE, 1, MPI_REAL, MPI_MAX, 0, &
      MPI_COMM_WORLD, ierr)
  dtime2 = time2 - time1
  CALL MPI_Reduce(dtime2, MPI_IN_PLACE, 1, MPI_REAL, MPI_MAX, 0, &
      MPI_COMM_WORLD, ierr)

  PRINT *,"Rank ", rank, " got message from rank ", left, " of ", recv_rank
  CALL MPI_Barrier(MPI_COMM_WORLD, ierr)
  IF (rank == 0) PRINT *, "It took ", dtime1, " seconds to complete send/receives"
  IF (rank == 0) PRINT *, "It took ", dtime2, " seconds to complete setup"

  CALL MPI_Finalize(ierr)

END PROGRAM dch
