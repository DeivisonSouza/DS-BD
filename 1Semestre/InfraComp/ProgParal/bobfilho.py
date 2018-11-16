from multiprocessing import Process    # chama apenas a funçao Process do pacote multiprocessing...

def f(name):
  print('hello', name)

if __name__ == '__main__':
  p = Process(target=f, args=('bob filho', )) # primeiro processo

  p.start()    # iniciar o processo
  p.join()     # juntar os processos (para esperar cada processo terminar e depois juntar todos) 

# função Process cria um novo processo...(um processo apenas)
# target = destino é executar a funçao f...

