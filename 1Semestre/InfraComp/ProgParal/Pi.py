num_steps = 100000
step = 1.0/num_steps
sum = 0.0

for i in range(num_steps):
  x = (i + 0.5)*step
  sum += 4.0/(1.0 + x*x)
sum = sum * step
print(sum)
