from mpi4py import MPI as MPI
import time
from math import sqrt, trunc
import numpy

primes = [2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47, 53, 59, 61, 67, 71]

def check_prime(num):

  has_divisor = False

  #Check 2 specially
  if num%2 == 0:
    return False
  #Ignore all other as 2 was false
  for i in range(3, trunc(sqrt(num))+1, 2):
    if num%i == 0:
      has_divisor = True
      break
  return not has_divisor

def precheck_flags(lower, length, flags, index):

  has_divisor = False
  global primes

  stride = primes[index]
  start = stride - lower%stride
  for i in range(start, length, stride):
    #Mark as composite
    flags[i] = 1

def controller(lower, upper):

  comm = MPI.COMM_WORLD
  nproc = comm.Get_size()
  rank = comm.Get_rank()

  length = upper - lower
  flags = numpy.zeros(length)
  current_val = 0
  inflight = 0

  precheck_num = 0
  #How many primes to process
  precheck_to = 20


  vals_in_use = numpy.zeros(nproc-1)
  processed = numpy.zeros(nproc-1)
  start_time = numpy.zeros(nproc-1)
  cum_time = numpy.zeros(nproc-1)
  end_time = numpy.zeros(nproc-1)


  info = MPI.Status()
  request = MPI.Request()
  while True:

    #Use non-blocking commands although this variant could just as well use blocking
    #and not post the recieve until after it did the pre-check

    # Unlike normal MPI, irecv here takes a buffer size only and
    # the actual result is returned by the wait
    # First param is buffer size in bytes.
    request = comm.irecv(4, source=MPI.ANY_SOURCE, tag=MPI.ANY_TAG)

    if precheck_num < precheck_to:
      precheck_flags(lower, length, flags, precheck_num)
      precheck_num = precheck_num + 1

    result = request.wait(status=info)

    if info.tag > 0:
      end_time[info.source-1] = time.time()
      cum_time[info.source-1] = cum_time[info.source-1] + (end_time[info.source-1] - start_time[info.source-1])
      processed[info.source-1] = processed[info.source-1] + 1
      offset = vals_in_use[info.source-1] - lower
      #Cheat - if prime mark as 2, (True + 1)
      flags[int(offset)] = result + 1
      inflight = inflight - 1

    if current_val < length:

      #Skip any values that have already been checked
      while flags[current_val] != 0 and current_val < length:
        current_val = current_val + 1

      vals_in_use[info.source-1] = lower + current_val
      #print("Dispatching ", lower+current_val)

      start_time[info.source-1] = time.time()
      current_val = current_val + 1
      comm.send(vals_in_use[info.source-1], dest=info.source, tag=1)
      inflight = inflight + 1
    else:
      #No more work, shut down the worker
      comm.send(1, dest=info.source, tag=0)

    if inflight == 0:
      break

  #Summarize findings
  for i in range(0, nproc-1):
    print("Worker ", i, " processed ", int(processed[i-1]), " packets in ", cum_time[i-1], "s")

  print("Found ", int(numpy.sum(flags[flags == 2])/2), " primes")


def worker():
  comm = MPI.COMM_WORLD

  data = False
  comm.send(data, dest=0, tag=0)

  while True:
    tag = 0
    info = MPI.Status()
    candidate = comm.recv(source=0, tag=MPI.ANY_TAG, status=info)
    tag = info.tag
    if(tag > 0):
      result = check_prime(candidate)
      comm.send(result, dest=0, tag=tag)
    else:
      return

def main(lower, upper):
  comm = MPI.COMM_WORLD
  rank = comm.Get_rank()

  if rank == 0:
    controller(lower, upper)
  else:
    worker()


if __name__ == "__main__":

   comm = MPI.COMM_WORLD
   rank = comm.Get_rank()

   if(rank ==0):
     try:
       lower = int(input('Enter lower bnd: '))
     except:
       print("I didn't understand. I'll try 10000")
       lower = 10000
     try:
       upper = int(input('Enter upper bnd: '))
     except:
       print("I didn't understand. I'll try 20000")
       upper = 20000
   else:
     lower = 0
     upper = 0

   main(lower, upper)

