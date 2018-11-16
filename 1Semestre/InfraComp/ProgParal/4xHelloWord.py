from multiprocessing import Process
def f(name, id):
    print('hello', name, id)

if __name__ == '__main__':
   procs = []
   for i in range(4):
     p = Process(target=f, args=('bob filho', i, ))
     procs.append(p)

   print('hello', 'bob pai')
   for i in range(4):
     procs[i].start()
   for i in range(4):
     procs[i].join()
