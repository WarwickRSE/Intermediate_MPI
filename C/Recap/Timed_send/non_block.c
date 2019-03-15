#include <stdio.h>
#include <mpi.h>

#define TAG 100

int main(int argc, char ** argv)
{

  int rank, recv_rank, nproc, left, right, repcount;
  double time1, time2, dtime;
  MPI_Request requests[2];
  MPI_Status statuses[2];


  MPI_Init(&argc, &argv);
  MPI_Comm_size(MPI_COMM_WORLD, &nproc);
  MPI_Comm_rank(MPI_COMM_WORLD, &rank);

  //Set up periodic domain
  left = rank - 1;
  if (left < 0) left = nproc - 1;
  right = rank + 1;
  if (right > nproc - 1) right = 0;

  time1 = MPI_Wtime();
  for (repcount = 0; repcount <100000; ++repcount) {
    MPI_Isend(&rank, 1, MPI_INT, right, TAG, MPI_COMM_WORLD, &requests[0]);
    MPI_Irecv(&recv_rank, 1, MPI_INT, left, TAG, MPI_COMM_WORLD,
        &requests[1]);
    MPI_Waitall(2, requests, statuses);
  }
  time2 = MPI_Wtime();

  dtime = time2 - time1;

  printf("Rank %3d got message from rank %3d of %3d\n", rank, left, recv_rank);
  MPI_Barrier(MPI_COMM_WORLD);
  if (rank == 0) {
    printf("It took %f seconds to complete\n", dtime);
  }

  MPI_Finalize();

}
