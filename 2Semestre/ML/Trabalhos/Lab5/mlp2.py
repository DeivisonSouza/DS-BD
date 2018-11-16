#!/usr/bin/python

#######################################################################################
# Multilayer Perceptron (MLP) para classificação binária
#######################################################################################

############## ANOTAÇÕES MINHAS #######################################################
# instalar tensorflow = pip install tensorflow
# instalar keras = pip install keras
# instalar h5py = pip install h5py (biblioteca para salvar modelos treinados)
#######################################################################################
# somente com estes comandos consegui fazer funcionar:
# conda create -n meu_env anaconda python=3.5         (se tiver o anaconda instalado)
# source activate meu_env
# pip install tensorflow keras
#######################################################################################
# comandos terminal para executar o script:
# source activate meu_env
# python mlp2.py train.txt test.txt
#######################################################################################
# O keras roda sobre o framework Tensorflow. Portanto, é necessário ter o tensorflow
# instalado na máquina.
#######################################################################################

#-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# CARREGA BIBLIOTECAS
#-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

import numpy as np
import pandas
import keras
from keras.models import Sequential                                       # para construir uma rede neural sequencial (os pesos fluem da camada de entrada, para a(s) oculta (s), camada de saida)
from keras.layers import Dense, Dropout                                   # Dense = para trabalhar com rede neural do tipo "Densa" (cada um dos neuronios conecta-se com todos os outros)
from sklearn.datasets import load_svmlight_file  
from sklearn.metrics import confusion_matrix, accuracy_score              # obter a matriz de confusão
import matplotlib.pyplot as plt                                           # gerar gráficos

#-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# LER OS ARQUIVOS DE TREINO E TESTE
#-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#numpy.random.seed(7)                                                      # Fixa uma semente para reprodutibilidade (não funcionou?)
X_train, y_train = load_svmlight_file('train.txt')
X_test, y_test = load_svmlight_file('test.txt')


print('shape X_train = ', X_train.shape)
print('shape y_train = ', y_train.shape)
print('shape X_test = ', X_test.shape)
print('shape y_test = ', y_test.shape)


# Encontrar os números exclusivos dos rótulos em y_train
classes = np.unique(y_train)
nClasses = len(classes)
print('O número total de classes é', nClasses)
print('As classes para previsão são:', classes)


## save for the confusion matrix
label = y_test
print('label = ', label[1:10,])
print('label = ', label[20000:20010,])

#-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# CONVERTE OS LABELS PARA DUAS CATEGORIAS? Pq ele fez isso ao inves de treinar um mlp para multi-classes?
#-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
## converts the labels to a categorical one-hot-vector

y_train = keras.utils.to_categorical(y_train, num_classes=2)
y_test = keras.utils.to_categorical(y_test, num_classes=2)
print('head y_train = ', y_train[1:10,])
print('head y_test = ', y_test[1:10,])

#-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# MODELO SEQUENCIAL (definicão das camadas e do processo de aprendizagem)
#-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Inicia o modelo sequencial (obs.: não é necessário passar quaisquer argumentos). 
#Em seguida, faz-se a configuração de RNA com a definicão das camadas ocultas e de saída.

model = Sequential()                                                                   
# Dense(50) is a fully-connected layer with 50 hidden units.
# in the first layer, you must specify the expected input data shape:
# here, 100-dimensional vectors.

# O método add() adiciona as camadas desejadas para RNA. Cada vez que "chamo" o método add() estou criando uma camada oculta somente.
# units = quantos neurônios na respectiva camada oculta?
# activation = função de ativação a ser usada para ativar o neurônio?
# input_dim = número de preditores na camada de entrada? Neste caso, será 200. Para verificar o número de preditores basta fazer: print('shape X_train = ', X_train.shape)

model.add(Dense(units=1, activation='relu', input_dim=200))       # 1 Camada oculta: será constituida de: a) 200 neuronios; b) funcão de ativação = "relu" (Rectifier); c) input_dim=200.
#model.add(Dropout(0.5))
#model.add(Dense(units=200, activation='relu'))                   # 2 Camada oculta: será constituida de: a) 200 neuronios; b) funcão de ativação = "relu" (Rectifier).
#model.add(Dropout(0.5))
#model.add(Dense(units=200, activation='relu'))                   # 3 Camada oculta: será constituida de: a) 200 neuronios; b) funcão de ativação = "relu" (Rectifier).
#model.add(Dropout(0.5))
#model.add(Dense(units=200, activation='relu'))                   # 4 Camada oculta: será constituida de: a) 200 neuronios; b) funcão de ativação = "relu" (Rectifier).
#model.add(Dropout(0.5))
#model.add(Dense(units=200, activation='relu'))                   # 5 Camada oculta: será constituida de: a) 200 neuronios; b) funcão de ativação = "relu" (Rectifier).
#model.add(Dropout(0.5))
#model.add(Dense(units=200, activation='relu'))                   # 6 Camada oculta: será constituida de: a) 200 neuronios; b) funcão de ativação = "relu" (Rectifier).
#model.add(Dropout(0.5))
model.add(Dense(units=2, activation='softmax'))                  # Camada de saída: a) units = 2 (2, haja vista que é um problema binário); b) activation = 'softmax'. (Obs.: Professor pediu no exercício para usar 'softmax')          
#model.add(Dense(units=2, activation='sigmoid'))                 # Camada de saída: Vi na documentação do Keras que no caso de classificaçao binária o ideal é usar a função 'sigmoid'...(colegas disseram que a 'softmax' é uma generalização da 'sigmoid', então funciona bem tanto para classificação binária quanto para multi-classe - Mas, preciso confirmar isso!)

# Variando o número de neurônios na camada oculta...
#model.add(Dense(units=50, activation='relu', input_dim=200))
#model.add(Dense(units=150, activation='relu', input_dim=200))
#model.add(Dense(units=200, activation='relu', input_dim=200))

# Camada de saída: a) units = 2; activation = 'sigmoid'. 
# A função softmax é recomendada quando se tem mais de duas categorias para previsão. 
# No caso de um problema binário (2 classes) recomenda-se o uso da função 'sigmoid'.



# Configuração do processo de aprendizagem usando o método "compile":
# optimizer = Indicar o otimizador a ser usado para fazer o ajuste dos pesos (wi) da rede. Outras alternativas = 'adam', 'sgd', ...
# loss = Indicar a função de perda (função objetivo) a ser usada. Se o modelo tiver várias saídas, você poderá usar uma perda diferente em cada saída passando um dicionário ou uma lista de perdas. Para problemas multi-classes
# usar 'categorical-crossentropy'?
# O valor da perda que será minimizado pelo modelo será então a soma de todas as perdas individuais. Outras alternativas são: 'mean_squared_error', 'mean_absolute_error', 'mean_absolute_percentage_error', ...
# metrics = Uma lista de métricas.

model.compile(loss='binary_crossentropy',
							optimizer='rmsprop',
							metrics=['accuracy'])
model.summary()
#-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# TREINAMENTO DA REDE NEURAL
#-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# epochs = número de épocas ou iteraçoes da rede; Significa o número de vezes em que os pesos (wi) serão ajustados.
# batch_size (default = 32) = de quantos em quantos registros os pesos (wi) serão atualizados? batch_size=128 significa que o erro será calculado para 128 registros (exemplos) e, em seguida, será feita a atualização dos pesos (wi)?
# X_train = previsores da base de treinamento;
# y_train = classes (labels) da base de treinamento;
# validation_split = Flutuante entre 0 e 1. Fração dos dados de treinamento a serem usados ​​como dados de validação. O modelo irá separar essa fração dos dados de treinamento, não irá treiná-los e avaliará a perda e quaisquer métricas de modelo sobre esses dados no final de cada época. Os dados de validação são selecionados a partir das últimas amostras no xe nos  ydados fornecidos, antes de embaralhar.
# shuffle (default = TRUE) = Booleano (para embaralhar os dados de treinamento antes de cada época);
# A função 'fit' é usada para treinar o modelo.

print("Training...")
history = model.fit(X_train, y_train, validation_split=0.33, epochs=20, batch_size=128, verbose=1) # Neste caso, 33% dos dados de treino ficarão disponíveis para validação cruzada.
#print(history.history['loss'])
#print(history.history['acc'])
#print(history.history['val_loss'])
#print(history.history['val_acc'])
#-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# DESEMPENHO DO MODELO NO CONJUNTO DE TESTE
#-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

score = model.evaluate(X_test, y_test, batch_size=128, verbose=0)     # Aqui está avaliando o modelo treinado que está em "model" sobre o conjunto de teste (X_test = preditores do conj. teste; y_test = classes do conj. teste)
print('Test loss = ', score[0])                                       # loss no conj. de teste;
print('Test accuracy = ', score[1])                                   # Acurácia no conj. de teste.

print("Generating test predictions...")
y_pred = model.predict_classes(X_test)                                # usa o modelo treinado para fazer previsões no conjunto de teste, dado os preditores do conjunto de teste (X_test)...

cm = confusion_matrix(label, y_pred)                                  # matriz de confusão para o conjunto de teste...
print('confusion matrix = ', cm)

#acuracia = accuracy_score(label, y_pred)                             # Acuracia do classificador no conjunto de teste...(accuracy = VP+VN/Total de instâncias (25000 instâncias))
#print('accuracy score = ', acuracia)

# Executando o script no terminal, pode-se observar a saída detalhada para cada 'epoca', onde mostra-se a perda (loss) e a precisão (metric) no conjunto de treinamento e validação.
# loss = Perda no conjunto de treinamento;
# acc = Acurácia no conjunto de treinamento;
# val_loss = Perda no conjunto de validação; e
# val_acc = Acurácia no conjunto de de validação.

# list all data in history
print(history.history.keys())

# plot train and validation loss and accuracy

fig = plt.figure(figsize=(15, 4))

# sintaxe para add subplots
ax1 = fig.add_subplot(121)      #(linha, coluna, indice)
ax2 = fig.add_subplot(122)

# plot train and validation accuracy
ax1.plot(history.history['acc'])
ax1.plot(history.history['val_acc'])
#ax1.set_title('Model accuracy \n (units=300, epochs=200, softmax)')
ax1.set_title('Model accuracy (Perceptron) \n (units=1, epochs=20, one hidden layer, softmax)')
ax1.set_ylabel('Accuracy')
ax1.set_xlabel('Epoch')
ax1.legend(['Train', 'Validation'], loc='center right')


# plot train and validation loss
ax2.plot(history.history['loss'])
ax2.plot(history.history['val_loss'])
#ax2.set_title('Model loss \n (units=300, epochs=200, softmax)')
ax2.set_title('Model loss (Perceptron) \n (units=1, epochs=20, one hidden layer, softmax)')
ax2.set_ylabel('Loss')
ax2.set_xlabel('Epoch')
ax2.legend(['Train', 'Validation'], loc='center right')
plt.show()


#plt.plot(history.history['acc'])
#plt.plot(history.history['val_acc'])
#plt.title('Model accuracy \n (units=200, epochs=200, six hidden layer, softmax)')
#plt.title('Model accuracy (units=200, epochs=200, two hidden layer)')
#plt.ylabel('Accuracy')
#plt.xlabel('Epoch')
#plt.legend(['Train', 'Validation'], loc='upper left')


# plot train and validation loss
#ax2.plot(history.history['loss'],history.history['val_loss'])

#plt.plot(history.history['loss'])
#plt.plot(history.history['val_loss'])
#plt.title('Model train vs validation loss')
#plt.ylabel('Loss')
#plt.xlabel('Epoch')
#plt.legend(['Train', 'Validation'], loc='upper left')
#plt.show()




