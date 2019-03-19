#include <stdio.h>
#include <mpi.h>

#define TAG 100

void send_vals(int dest, int val, MPI_Request *request){

  /*This is invalid because C passes by value. "val" only exists inside
  this function*/
  MPI_Isend(&val, 1, MPI_INT, dest, TAG, MPI_COMM_WORLD,
      request);
}

int main(int argc, char ** argv)
{

  int rank, nproc, left, right;
  int recv_val;
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

  send_vals(right, 25, &requests[0]);
  MPI_Irecv(&recv_val, 1, MPI_INT, left, TAG, MPI_COMM_WORLD,
      &requests[1]);
  MPI_Waitall(2, requests, statuses);

  printf("Rank %3d got message from rank %3d of %3d\n", rank, left, 
      recv_val);

  MPI_Finalize();

}
