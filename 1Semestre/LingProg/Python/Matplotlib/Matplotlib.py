from matplotlib import pyplot as plt
import numpy as np

plt.plot([1,1,1,1,-1,1,0,-1], 'r')    #r = red
plt.show()

plt.plot([1,1,1,1,-1,1,0,-1], 'ro')   #r = red and point
plt.show()

#plt.plot([1,1,1,1,-1,1,0,-1], 'r')   #r = red
#plt.show()

#plt.plot([1,1,1,1,-1,1,0,-1], 'r')   #r = red
#plt.show()

plt.plot([1,-1,1,-1],[1,-1,1,-1], 'r')   #r = red (passando duas listas: em que uma será 'x' e outra 'y'
plt.show()

x = [4,2,9,-1,5,0]
y = [16,4,81,1,25,0]
plt.plot(x,y,'b')
plt.show()


x = np.array([1,2,3])
y = x**3
plt.plot(x, y, 'y')
plt.show()

