from multiprocessing import Process, Pipe, cpu_count, Lock
import itertools
import time

PROCS = cpu_count()*10                                                                   # conta o número de processadores disponíveis para uso no computador. (tenho um processador com 8 cores)...
total = 0.0                                                                               # uma variável para receber a soma dos elementos da FILA...

def pi(inicio, num_passos, passo, Pipe, lock, PROCS):
    print("Inicio: " + str(inicio))                                                       # imprime: ponto de partida do cálculo...
    print("Final: " + str(num_passos))                                                    # imprime: número total de passos...
    print("Passo: " + str(passo))                                                         # imprime: tamanho do passo...
    print("PROCS: " + str(PROCS)) 
    #print("Pipe: " + str(Pipe)) 
    sum = 0.0                                    

    for i in range(inicio, num_passos):                                                   # loop for: Inicio do loop sobre o número de passos...
      x = (i + 0.5)*passo                                                                 # define o início do primeiro passo em (i+0,5)...
      sum = sum + 4.0/(1.0 + x*x)                  
    #print(sum*passo)
    #print("Pi: %.10f" %(sum))                                                            
    
    lock.acquire()
    try:
      Pipe.send(sum*passo)                                                                # envia para fila?...
    finally:
      lock.release()

if __name__ == "__main__":                                                                # define "__main__" como método de "__name__"...
    a, b = Pipe()                                                                         # define que existem duas pontas (a, b)...
    num_passos = 100000000                                                                # define o número de passos...
    sum = 0.0
    passo = 1.0/num_passos                                                                # define o tamanho do passo...
    proc_size = int(num_passos/PROCS)                                                     # divide a quantidade de passos pelo número de processos...
    lock = Lock()
    tic = time.time()                                                                     # inicio do contador...
    workers = []                                                                          # cria uma lista vazia...

    for i in range(PROCS):                                                                # loop for: inicio do loop sobre o número de processos...
      worker = Process(target=pi, args=(i*proc_size, (i+1)*proc_size - 1, passo, a, lock, i, )) # função process: target recebe a função "pi" e args os parâmetros da função "pi"...
      workers.append(worker)

    for worker in workers:                                                                # loop for: iniciar a execução dos processos em "worker"...
      worker.start()

    for worker in workers:                                                                # loop for: junta explicitamente todos os processos iniciados e terminados...
      worker.join()

    #for i in range(0, ITEMS.qsize()):
      total = total + b.recv()
    print("Pi =", total)
 
    toc = time.time()                                                                     # final do contador...
    print("Tempo: %.8f s" %(toc-tic))                                                     # imprime: tempo de execução...  

