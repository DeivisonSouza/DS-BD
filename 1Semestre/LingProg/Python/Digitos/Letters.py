from PIL import Image                    # PIL (Python Imaging Library) - biblioteca para manipular imagens
import numpy as np

img = Image.open("cdf0000_14_4_3.tif")   # carrega uma imagem
print(img)

print(img.format)
print(img.size)                          # tupla informando as dimensões da imagem
#print(img.show())

#np.array(img)                           # importar a imagem
#nparray=np.array(img,dtype="int")       # importa no formato de inteiro (tanto faz qual forma importar)
#print(nparray)

nparray2=np.array(img,dtype="int") .reshape((1,28*34))
print(nparray2)

arq = open("files.txt")                  # ler arquivo "files.txt" que contem os nomes de todos os arquivos
#print(arq)

arqList = arq.read().splitlines()        # para transformar o arquivo "files" em lista (algo compreensivel pelo python)
#print(arqList)

teste = arqList[0].split
print(teste)


linha = teste[0]+" "+teste[1]
print(linha)

nome, rotulo = linha.split()
nomeA = nome.split("/")[1]

print(nomeA)  


#lar = []                             # uma lista de arquivos e rotulos

#for linha in arqList:
  
  
  
  
   

