import time

def pi(start, end, step):
    print "Start: " + str(start)
    print "End: " + str(end)
    sum = 0.0

for i in range(start, end):
    x = (i+0.5) * step                             # i+0,5 (para pegar o ponto no meio do retângulo)
    sum = sum + 4.0/(1.0+x*x)
return(sum)

if __name__ == "__main__":
    num_steps = 100000000 #100.000.000
    sums = 0.0
    step = 1.0/num_steps
    tic = time.time()
    sums = pi(0, num_steps, step)
    toc = time.time()
    pi = step * sums
print(pi)
print("Pi: %.8f s" %(toc-tic))
