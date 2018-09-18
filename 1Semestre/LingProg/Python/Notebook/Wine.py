import numpy as np

# 1) Ler conjunto de dados (usando a função np.loadtxt):

# a) Red-Wine

dat1 = np.loadtxt('winequality-red.csv', skiprows=1, delimiter=";")  #skiprows=1 (pular primeira linha)
print(dat1)
print(type(dat1))
print(dat1.dtype.name)
print(dat1.shape)

# b) White-Wine

dat2 = np.loadtxt('winequality-white.csv', skiprows=1, delimiter=";")
print(dat2)
print(type(dat2))
print(dat2.dtype.name)
print(dat2.shape)

# 2) Concatena os conjuntos de dados:

alldata = np.append(dat1,dat2, axis=0)
print(alldata.shape)

# 3) Cria um array com labels 1 e 2 com comprimentos de shape[1] de cada conjunto de dados:

labels = np.append(np.repeat(1, dat1.shape[0]), np.repeat(2, dat2.shape[0]))
labels = np.array(labels).reshape((alldata.shape[0],1))                          #Retorna uma matriz contendo os mesmos dados com uma nova forma.
print(labels.shape)
print(labels)

# 4) Faz um append dos labels ao conjunto concatenado (alldata):

finaldata = np.append(alldata, labels, axis=1)
print(type(finaldata))
print(finaldata.shape)
print(finaldata)

# 5) Adiciona um cabeçalho e salva em um novo arquivo:

import numpy
#r = reader (apenas lê o arquivo). 
#readline = lê uma única linha do arquivo.
# rstrip = remove espaço final da linha 
cabecalho = open('winequality-white.csv', 'r',encoding='UTF8').readline().rstrip() + str(";labels")

print(type(cabecalho))
print(cabecalho)

fmt = ";".join(["%10.6e"]* (finaldata.shape[1]))
numpy.savetxt("finaldata.csv", finaldata, fmt=fmt, header=cabecalho, comments='')

# 6) Ler arquivo "finaldata" com cabeçalho:

data = np.genfromtxt("finaldata.csv", delimiter=";", names = True, skip_header=0, case_sensitive='upper', dtype=None)
print(type(data))
print(data.dtype.name)
print(data.shape)
print(data.dtype)
print(data,)

# 7) Médias para a variável "QUALITY":

## i) Média aritmética p/ variável "quality" (red-wine )

print("A média aritmética para a variável *QUALITY* no conjunto red-wine é:")
print(data[data['LABELS']==1]['QUALITY'].mean(),"\n")

## ii) Média aritmética p/ variável "quality" (white-wine )

print("A média aritmética para a variável *QUALITY* no conjunto white-wine é:")
print(data[data['LABELS']==2]['QUALITY'].mean(),"\n")

## iii) Média aritmética geral p/ variável "quality" (red + white)

print("A média aritmética para a variável *QUALITY* para os conjuntos unidos é:")
print(data['QUALITY'].mean(),"\n")

# 8) Resumo em um dicionário:

print("As médias aritméticas para a variável *QUALITY* são:\n")
media = {'red-wine': data[data['LABELS']==1]['QUALITY'].mean(),
         'white-wine':data[data['LABELS']==2]['QUALITY'].mean(), 
         'all': data['QUALITY'].mean()}
print(media,"\n")


# 9) Criando uma tabela descritiva final com Pandas:

import pandas as pd

print("Essa é uma tabela descritiva usando a função pivot_table do Pandas:\n")

DATA = pd.DataFrame(data)

table = DATA.pivot_table(index='LABELS', values = "QUALITY", 
                aggfunc = [len, min, max, np.mean, np.std, np.var],
                margins=True, margins_name='Total')

table.columns = ['_'.join(str(s).strip() for s in col if s) for col in table.columns]

table.reset_index()

print(table)


