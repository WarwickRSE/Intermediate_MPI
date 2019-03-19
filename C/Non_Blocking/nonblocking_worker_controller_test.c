#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include <math.h>
#include <mpi.h>


/* A worker-controller model for finding all primes within a given interval.
Each worker checked primality of a single number
  This is optimised a bit - it tests only the numbers in small_primes first, then from max_small_prime it tests every odd number, returning at the first factor it finds
The master dispatches the numbers to test.

In the Non-blocking version, the master also does some cheap work to trim the list of candidates using a sieve method. This significantly reduces the number of tests the workers have to do

THIS CODE IS NOT OPTIMAL - it is an example. For instance, we should ignore even numbers from the start, and do not.
*/

#define ISPRIME 2
#define ISCOMP 1
#define ISUNCHECKED 0

//In real working code this should be done more carefully
const long small_primes_len = 20;
const long small_primes[20] = {2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47, 53, 59, 61, 67, 71};
const long max_small_prime = 71;

char check_prime(long num);
MPI_Status precheck_flags_all(char* flags, long len, long lower, MPI_Request request);


  void dispatcher_fn(long lower, long upper, long len, char* flag_array)
    {

    int nproc, worker_num;
    MPI_Status stat;
    MPI_Request request;
    long next_package_start, next_precheck_index, skip_precheck;
    const long stop_precheck = 10;
    char result;
    long offset, dummy_dat;
    long * current_packages;

    double * start_times, * end_times, * cum_times;
    long * packages_processed;

    // Number of packages currently out
    int inflight;

    MPI_Comm_size(MPI_COMM_WORLD, &nproc);

    // Allocate all the arrays per worker ...

    // Keep track of what each worker is working on
    current_packages = (long *) calloc(nproc, sizeof(long));

    //These track how long each package takes, and cumulative totals per worker
    start_times = (double *) malloc(nproc*sizeof(double));
    end_times = (double *) malloc(nproc*sizeof(double));
    cum_times = (double *) calloc(nproc, sizeof(double));
    packages_processed = (long *) calloc(nproc, sizeof(long));


    // Index of next lowest number to check. When this hits len we are done
    next_package_start = 0;
    inflight = 0;

    //Master has its own work to do
    next_precheck_index = 0;
    skip_precheck = 0;

    for(;;) {

      //Recieve and unpack the results

      //Tag is used for operational info - is this a ready/done signal, or a result?

      //Wait to receive any data. On the first pass tag will be 0 which is
      //Just the workers saying that they are ready
      MPI_Irecv(&result, 1, MPI_CHAR, MPI_ANY_SOURCE, MPI_ANY_TAG,
          MPI_COMM_WORLD, &request);

      //Controller does some low effort work while workers can run
      if(skip_precheck !=1) stat = precheck_flags_all(flag_array, len, lower, request);

      //We have to get this information from the status variable because
      //we received the message with MPI_ANY_SOURCE and MPI_ANY_TAG
      if (stat.MPI_TAG > 0) {
        //Capture end time and add to this workers stats
        end_times[stat.MPI_SOURCE-1] = MPI_Wtime();
        cum_times[stat.MPI_SOURCE-1] += end_times[stat.MPI_SOURCE-1] - start_times[stat.MPI_SOURCE-1];
        packages_processed[stat.MPI_SOURCE-1] ++;
        //Package recieved and understood
        inflight --;
        //Now set the correct flag for this package
        offset = current_packages[stat.MPI_SOURCE-1] - lower;
        flag_array[offset] = result;
      }

      //If you've still got work to give out then find the next candidate and dispatch
      if (next_package_start <= len) {

        //Now find the next candidate and send it to the worker that just reported
        //that it had finished.
        //The tag is "1" indicating this is real work

        for(;;){
          //Skip over numbers already tested
          if( flag_array[next_package_start] == 0) break;
          next_package_start ++;
          if(next_package_start > len) break;
        }

        current_packages[stat.MPI_SOURCE-1] = lower + next_package_start;
        //printf("Dispatching %3li \n", lower+next_package_start);
        next_package_start ++;

        inflight ++;
        //Record the start time
        start_times[stat.MPI_SOURCE-1] = MPI_Wtime();

        //Send the work package with the tag of the work index to the source
        //of the previously received message
        MPI_Send(&current_packages[stat.MPI_SOURCE-1], 1, MPI_LONG, stat.MPI_SOURCE,
            1, MPI_COMM_WORLD);
      } else {
        //No more work to do so shut down the worker. Here, this is just
        //Sending a message with a zero tag
        dummy_dat = -1;
        MPI_Send(&dummy_dat, 1, MPI_LONG, stat.MPI_SOURCE, 0, MPI_COMM_WORLD);
      }
      if(inflight == 0) break;
    }

    for(worker_num = 0; worker_num < nproc-1; ++worker_num){
      printf("Worker %3i checked %3li candidates in %5.4g seconds\n",
          worker_num+1, packages_processed[worker_num], cum_times[worker_num]);
    }
  }


  void worker_fn(void)
    {
    MPI_Status stat;
    long package;
    char dat, result;

    //First just call home to say "I'm here and I'm waiting"
    //So send -1 message and 0 tag to rank 0
    dat = -1;
    MPI_Send(&dat, 1, MPI_CHAR, 0, 0, MPI_COMM_WORLD);

    for(;;){
      //Receive the work package from rank 0
      MPI_Recv(&package, 1, MPI_LONG, 0, MPI_ANY_TAG,
          MPI_COMM_WORLD, &stat);

      //Need to have a termination condition, here tag = 0
      if (stat.MPI_TAG == 0) break;

      //Do the work
      result = check_prime(package);

      //Send the result back using the same tag
      MPI_Send(&result, 1, MPI_CHAR, 0 , stat.MPI_TAG, MPI_COMM_WORLD);
    }
  }

  char check_prime(long num){

    char result;
    long index, end;

    end = ceil(sqrt((double) num));

    result = ISPRIME;
    //First check against the small primes
    for(index = 0; index < small_primes_len; index++){
      if (num%small_primes[index] == 0){
        return ISCOMP;
      }
    }

    //Test higher numbers, skipping all the evens
    for (index = max_small_prime + 2; index <= end; index += 2){
      if (num%index == 0){
        return ISCOMP;
      }
    }
    return result;
  }

MPI_Status precheck_flags_all(char* flags, long len, long lower, MPI_Request request){
  //Knock out all the multiples of small_primes
  //This function should smoothly restart after an exit
  //It also takes an MPI request and should exit once there is other work to be done

  MPI_Status stat;
  int request_flag;
  static long stride_index=0;
  static long index_reached=0;
  static int finished = 0, prechecker_runs = 0;
  long st, stride;


  if (!finished){
    prechecker_runs ++;
    for(; stride_index < small_primes_len; stride_index++){

      //See if time to exit or should go on
      MPI_Test(&request, &request_flag, &stat);
      if (request_flag) return stat;

      stride = small_primes[stride_index];
      st = stride-lower%stride;
      if(index_reached == 0) index_reached = st;
      for(; index_reached<len; index_reached+=stride){
        flags[index_reached] = ISCOMP;
        /* We could also do the test here, but it is heavy, so this works but
        kills our performance
        /MPI_Test(&request, &request_flag, &stat);
        if (request_flag) return stat;*/

      }
      index_reached = 0;
    }
  }
  if(!finished) printf("Prechecker ran %i times\n", prechecker_runs);
  finished = 1;
  //All done, wait until request completes
  MPI_Wait(&request, &stat);
  return stat;
  
}

int main(int argc, char** argv)
{
  int rank, nproc, i, total;
  long lower_bound, upper_bound, len;
  char * flags;

  MPI_Init(&argc, &argv);
  MPI_Comm_rank(MPI_COMM_WORLD, &rank);
  MPI_Comm_size(MPI_COMM_WORLD, &nproc);

  if (nproc == 1) {
    printf("This example requires more than 1 processor\n");
    MPI_Abort(MPI_COMM_WORLD, 0);
  }
  if (argc != 3) {
    printf("Please supply the ends of range to check");
    MPI_Abort(MPI_COMM_WORLD, 0);
  }

  lower_bound = atoi(argv[1]);  upper_bound = atoi(argv[2]);
  len = upper_bound - lower_bound;

  if (len <= 0) {
    printf("Lower bound must be < upper bound");
    MPI_Abort(MPI_COMM_WORLD, 0);
  }

  //Array of flags corresponding to numbers to test
  // Flag is 0 for unchecked, ISCOMP for composite and ISPRIME for prime
  flags = (char *) calloc(len, sizeof(char));

  if (rank == 0) {
    dispatcher_fn(lower_bound, upper_bound, len, flags);
    total = 0;
    for( i=0; i < len; i++){
      if(flags[i] == ISPRIME){
        //printf("%li\n", lower_bound+i);
        total ++;
      }
    }
    printf("\nFound %i primes\n", total);
  } else {
    worker_fn();
  }

/*  if(rank ==0) printf("Is prime %d\n", check_prime(32452843));
  if(rank ==0) printf("Is prime %d\n", check_prime(32452842));
*/

  MPI_Finalize();

  return 0;
}
