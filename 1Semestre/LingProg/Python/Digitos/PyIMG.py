import numpy as np

#Abrir arquivo de imagem
arq = open('ballons.pgm')
print(arq)

formato = arq.readline()
print(formato)

col,lin = arq.readline().split()
print(col)
print(lin)

col = int(col)
lin = int(lin)
print(lin)

pixelaum = eval(arq.readline())
print(pixelaum)

dados = arq.read().split()
#print(dados)

npdados = np.array(dados, dtype="int").reshape((col,lin))
print(npdados)

limiar = pixelaum/2

# Um loop for na matriz para valiar o limiar menor atribuir zero (0)
for i in range(col):
  for j in range(lin):
    if (npdados[i][j] < limiar):
        npdados[i][j] = 0
    else:
        npdados[i][j] = 255

arqSaida = open("saida.pgm", "w")

cabecalho = formato + str(col) +" "+ str(lin) +"\n"+str(pixelaum)+"\n"
print(cabecalho)

np.savetxt("saida.pmg", npdados,header=cabecalho, fmt = "%s", comments="")

#arqSaida = open("saida.pgm", "w")
#arqSaida.write(formato)
#print(formato)

#arqSaida.write("%d %d" %(col, lin))

#arqSaida.write(str(pixelaum))

#print(npdados)

#np.savetxt()





