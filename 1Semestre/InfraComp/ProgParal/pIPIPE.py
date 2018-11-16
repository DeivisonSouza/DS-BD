from multiprocessing import Process , Pipe, cpu_count
import itertools
import time

proc_num = cpu_count()

total = 0.0


def pi (start, end, step, Pipe, proc_num):
    sum = 0.0
    for i in range(start, end):
        x = (i+0.5) * (step)
        sum = sum + 4.0/(1.0+x*x)
    print(sum)
    Pipe.send(sum*step)


if __name__ == "__main__":
    a, b = Pipe()
    num_steps = 100000000 #100.000.000
    sum = 0.0
    step = 1.0/num_steps
    proc_size = num_steps/proc_num

    ti = time.time ()

    workers = []

    for i in range(proc_num):
        worker = Process(target=pi, args=(i*proc_size, (i+1)*proc_size-1, step, a, i, ))
        workers.append(worker)

    for worker in workers:
        worker.start()

    for worker in workers:
        worker.join()

    #for i in range(0, ITEMS.qsize()):
        total= total+ b.recv()
    print(total)

    tf = time.time ()
                                                                                                                                                     

