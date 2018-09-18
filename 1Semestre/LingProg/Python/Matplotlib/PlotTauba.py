from matplotlib import pyplot as plt
import numpy as np
import pandas as pd


data = pd.read_csv('taubaM.csv', sep=',')

print(data.head(10))
#print(data[:5])
print(data.columns)

#data2 = data.sort_values(by = ['Sunshine Hours','Rainfall (mm)'], ascending=[False,False], inplace=True)
#print(data2)
#print(type(data2))

#plt.plot(data["TAUBATE"], data["Sunshine Hours"])
#plt.show()

# duas informaçoes no mesmo gráfico:
#plt.plot(data["TAUBATE"], data["Sunshine Hours"])
#plt.plot(data["TAUBATE"], data["Average Temperature"])
#plt.show()

# subfiguras
plt.figure(1)
plt.subplot(211)
plt.plot(data["TAUBATE"], data["Sunshine Hours"])
plt.subplot(212)
plt.plot(data["TAUBATE"], data["Rainfall (mm)"])
plt.show()

# Outro exemplo
ax = plt.subplot(111)
t = np.arange(0.0, 5.0, 0.01)
s = np.cos(2*np.pi*t)
line, = plt.plot(t,s,lw=2)

plt.annotate('local max', xy = (2,1), xytext=(3,1.5), arrowprops=dict(facecolor='black', shrink=0.05),)

plt.ylim(-2,2)
plt.show()


plot_sorted(data, "TAUBATE", "Sunshine Hours", kind='scatter')

#
#table=data.pivot_table(index='TAUBATE', values = "Relative Humidity", 
                   #aggfunc = [len, min, max, np.sum, np.mean, np.std, np.var],
                   #margins=True, margins_name='Total')

#table.columns = ['_'.join(str(s).strip() for s in col if s) for col in table.columns]

#table.reset_index()

#print(table)
