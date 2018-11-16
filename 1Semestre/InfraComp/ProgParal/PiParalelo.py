from multiprocessing import Process, Queue
import time

PROCS = 5

def pi(start, end, step):
    print("Start: " + str(start))
    print("End: " + str(end))
    print("Step: " + str(step))
    sum = 0.0

    for i in range(start, end):
      x = (i+0.5) * step                       # tamanho do passo?
      sum = sum + 4.0/(1.0+x*x)                # 
    return(sum)
    
if __name__ == "__main__":
    num_steps = 100000000        #100.000.000
    sums = 0.0
    step = 1.0/num_steps
    proc_size = num_steps/PROCS
    tic = time.time()
    workers = []

    for i in range(PROCS):
      worker = Process(target=pi, args=(i*proc_size, (i+1)*proc_size - 1, step, ))
      workers.append(worker)

    for worker in workers :
      worker.start()

    for worker in workers :
      worker.join()

    toc = time.time()
    print("Pi: %.8f s" %(toc-tic))
