#!/usr/bin/python


import keras
from keras.datasets import mnist
from keras.models import Sequential
from keras.layers import Dense, Dropout, Flatten
from keras.layers import Conv2D, MaxPooling2D, AveragePooling2D, GlobalMaxPooling2D, GlobalAveragePooling2D
from keras import backend as K
from PIL import Image
import numpy as np
import os
import cv2
from sklearn.metrics import confusion_matrix
import matplotlib.pyplot as plt

#==========================================================================
# Explorando uma imagem (adição minha

#try:
    #original = Image.open('ad0001.tif')                                                                  # carrega uma imagem
#except:
    #print("Não é possível carregar a imagem")                                                            # imprime mensagem de erro, caso não seja possível abrir ou encontrar a imagem


#print("As características da imagem são:")
#print('\n Formato:', original.format,'\n', 'Tamanho:', original.size, '\n', 'Modo:', original.mode,'\n') # size = uma tupla com largura e altura (em pixels). Perceber que é uma imagem de modo “1” (preto e branco)

#original.show()                                                                                         # Exibe a imagem

#==========================================================================

num_classes = 12          # Quantidade de meses do ano (classes)
train_file = 'train.txt'
test_file = 'test.txt'

print('train_file:', train_file)

# input image dimensions
img_rows, img_cols = 64, 64


#==========================================================================

def load_images(image_paths, convert=False):

	x = []
	y = []
	for image_path in image_paths:

		path, label = image_path.split(' ')
		
		path= './data/' + path 

		if convert:
			image_pil = Image.open(path).convert('RGB') 
		else:
			image_pil = Image.open(path).convert('L')

		img = np.array(image_pil, dtype=np.uint8)

		x.append(img)
		y.append([int(label)])


	x = np.array(x)
	y = np.array(y)

	if np.min(y) != 0: 
		y = y-1

	return x, y
	
	

def load_dataset(train_file, test_file, resize, convert=False, size=(224,224)):

	arq = open(train_file, 'r')
	texto = arq.read()
	train_paths = texto.split('\n')
	
	print('Size : ', size)

	train_paths.remove('') #remove empty lines
	train_paths.sort()
	x_train, y_train = load_images(train_paths, convert)

	arq = open(test_file, 'r')
	texto = arq.read()
	test_paths = texto.split('\n')

	test_paths.remove('') #remove empty lines
	test_paths.sort()
	x_test, y_test = load_images(test_paths, convert)

	if resize:
		print("Resizing images...")
		x_train = resize_data(x_train, size, convert)
		x_test = resize_data(x_test, size, convert)

	if not convert:
		x_train = x_train.reshape(x_train.shape[0], size[0], size[1], 1)
		x_test = x_test.reshape(x_test.shape[0], size[0], size[1], 1)


	print(np.shape(x_train))
	return (x_train, y_train), (x_test, y_test)

def resize_data(data, size, convert):

	if convert:
		data_upscaled = np.zeros((data.shape[0], size[0], size[1], 3))
	else:
		data_upscaled = np.zeros((data.shape[0], size[0], size[1]))
	for i, img in enumerate(data):
		large_img = cv2.resize(img, dsize=(size[1], size[0]), interpolation=cv2.INTER_CUBIC)
		data_upscaled[i] = large_img

	print(np.shape(data_upscaled))
	return data_upscaled

#==========================================================================

print("Loading database...")

# gray scale
#input_shape = (img_rows, img_cols, 1)
#(x_train, y_train), (x_test, y_test) = load_dataset(train_file, test_file, resize=True, convert=False, size=(img_rows, img_cols))

# rgb
input_shape = (img_rows, img_cols, 3)
print('input_shape:',input_shape)

(x_train, y_train), (x_test, y_test) = load_dataset(train_file, test_file, resize=True, convert=True, size=(img_rows, img_cols))

### save for the confusion matrix
label = []
for i in range(len(x_test)):
	label.append(y_test[i][0])
	

#normalize images
x_train = x_train.astype('float32')
x_test = x_test.astype('float32')
x_train /= 255                            # Aqui redimensiona-se os dados da imagem para que cada pixel fique no intervalo [0, 1] ao invés de [0, 255]? (Será isso?)
x_test /= 255
print('x_train shape:', x_train.shape)

print(x_train.shape[0], 'train samples')
print(x_test.shape[0], 'test samples')

# convert class vectors to binary class matrices
y_train = keras.utils.to_categorical(y_train, num_classes)
y_test = keras.utils.to_categorical(y_test, num_classes)


#-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# MODELO SEQUENCIAL (definicão das camadas e do processo de aprendizagem) - Criando um modelo CNN
#-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Inicia o modelo sequencial (obs.: não é necessário passar quaisquer argumentos). Em seguida, faz-se a configuração de RNA com a definicão das camadas ocultas e de saída.
# O método add() adiciona as camadas desejadas para RNA. Cada vez que "chamo" o método add() estou criando uma camada oculta somente.
# A função de ativação para tornar nossa saída não linear. A saída da convolução é passada através da função de ativação.
# A função de pooling é reduzir continuamente a dimensionalidade para reduzir o número de parâmetros e computação na rede. Isso reduz o tempo de treinamento e controla o overfitting. 
# O tipo mais comum de pool é o pool máximo, que usa o valor máximo em cada janela. Esses tamanhos de janela precisam ser especificados antecipadamente. Isso diminui o tamanho do mapa de recursos e, ao mesmo tempo, 
# mantém as informações importantes.
# Assim, ao usar uma CNN, os quatro hiperparâmetros importantes que temos que decidir são:

# a) o tamanho do kernel;
# b) a contagem de filtros (ou seja, quantos filtros queremos usar);
# c) stride (quão grandes são as etapas do filtro);
# d) preenchimento.

# A estrutura básica da CNN é a seguinte: Convolution -> Pooling -> Convolution -> Pooling -> Fully Connected Layer -> Output
# Convolution; é o ato de pegar os dados originais e criar mapas de recursos a partir dele.
# kernel_size = Um inteiro ou tupla, especificando a altura e a largura da janela de convolução 2D. Pode ser um único inteiro para especificar o mesmo valor para todas as dimensões espaciais.

# keras.layers.Conv2D(filters, kernel_size, strides=(1, 1), padding='valid', data_format=None, dilation_rate=(1, 1), activation=None, use_bias=True, kernel_initializer='glorot_uniform', bias_initializer='zeros', kernel_regularizer=None, bias_regularizer=None, activity_regularizer=None, kernel_constraint=None, bias_constraint=None)
# keras.layers.MaxPooling2D(pool_size=(2, 2), strides=None, padding='valid', data_format=None)
# keras.layers.AveragePooling2D(pool_size=(2, 2), strides=None, padding='valid', data_format=None)

# filters : Integer, a dimensionalidade do espaço de saída (ou seja, o número de filtros de saída na convolução).
# kernel_size : Um inteiro ou tupla / lista de 2 inteiros, especificando a altura e a largura da janela de convolução 2D. Pode ser um único inteiro para especificar o mesmo valor para todas as dimensões espaciais.
# strides : um inteiro ou tupla / lista de 2 inteiros, especificando os passos da convolução ao longo da altura e largura. Pode ser um único inteiro para especificar o mesmo valor para todas as dimensões espaciais. A especificação de qualquer valor de passada! = 1 é incompatível com a especificação de qualquer dilation_ratevalor! = 1.
# padding (preenchimento ): um dos "valid"ou "same"(não diferencia maiúsculas de minúsculas). Observe que "same"é um pouco inconsistente nos backends com  strides! = 1, conforme descrito aqui

print('input_shape:',input_shape)

#-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Modelo de ajuste - sem mudanças
#-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Camada de convolução 2D (convolução espacial sobre imagens). kernel_size = (2,2): tamanho da janela de convolução. input_shape = (64, 64, 3), 3 é refere-se ao número de canais da imagem (RGB). strides = (1,1): tamanho do passo da convolução ao longo da altura e largura da imagem.

#model = Sequential()
#model.add(Conv2D(filters=32, kernel_size=(3, 3), activation='relu', strides=(1, 1), input_shape=input_shape)) 
#model.add(Conv2D(filters=64, kernel_size=(3, 3), activation='relu'))        # Camada de convolução 2D. kernel_size=(3, 3), activation='relu'.
#model.add(MaxPooling2D(pool_size=(2, 2)))                       # pool máximo: é uma técnica de subamostragem. Neste caso, a janela de pooling é 2 x 2. Usando pool máximo o maior valor será tomado, que representará o novo valor p/ região
#model.add(Dropout(0.25))                                        # dropout: É uma técnica de regularização usada p/ evitar overfitting.
#model.add(Flatten())                                            # flatten: converter os dados do formato 3D para 1D.
#model.add(Dense(128, activation='relu'))                        # Camada oculta: constituida de: a) 128 neuronios; b) funcão de ativação = "relu" (Rectifier)
#model.add(Dropout(0.5))                                         # dropout: É uma técnica de regularização usada p/ evitar overfitting.
#model.add(Dense(num_classes, activation='softmax'))             # modelo denso com função de ativação "softmax" na última camada. num_classes = 12 (número de classes de saída, no caso número de meses). As saídas expressam probabilidades de ser uma determinada imagem. A maior probabilidade é usada como resposta final.

#-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Modelo de ajuste - p/ testes
#-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

model = Sequential()
model.add(Conv2D(filters=64, kernel_size=(2, 2), activation='relu', strides=(1, 1), input_shape=input_shape))
model.add(MaxPooling2D(pool_size=(2, 2)))
model.add(Conv2D(filters=128, kernel_size=(2, 2), activation='relu'))
model.add(MaxPooling2D(pool_size=(2, 2)))
model.add(Dropout(0.25))
model.add(Flatten())       
model.add(Dense(256, activation='relu'))         
model.add(Dropout(0.5))
model.add(Dense(num_classes, activation='softmax'))

# GlobalMaxPooling2D, GlobalAveragePooling2D

#-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

# print cnn layers
print('Network structure ----------------------------------')
for i, layer in enumerate(model.layers):
	print(i,layer.name)
	if hasattr(layer, 'output_shape'):
		print(layer.output_shape)
print('----------------------------------------------------')

model.compile(loss=keras.losses.categorical_crossentropy, optimizer=keras.optimizers.Adadelta(), metrics=['accuracy'])

history = model.fit(x_train, y_train, batch_size=128, epochs=20, verbose=1, validation_data=(x_test, y_test))

score = model.evaluate(x_test, y_test, verbose=0)
print('Test accuracy:', score[1])                                     # Acurácia no conj. de teste.
print('Test loss: ', score[0])                                       # loss no conj. de teste.                                 

#print model.predict_classes(x_test) #classes predicted
#print model.predict_proba(x_test) #classes probability

pred = []
y_pred = model.predict_classes(x_test)
for i in range(len(x_test)):
	pred.append(y_pred[i])

print(confusion_matrix(label, pred))

print(history.history.keys())

#==========================================================================
# plot train and validation loss and accuracy

fig = plt.figure(figsize=(10, 5))

# sintaxe para add subplots
ax1 = fig.add_subplot(121)      #(linha, coluna, indice)
ax2 = fig.add_subplot(122)

# plot train and validation accuracy
ax1.plot(history.history['acc'])
ax1.plot(history.history['val_acc'])
#ax1.set_title('Model accuracy \n (units=300, epochs=200, softmax)')
ax1.set_title('Model accuracy \n (batch_size=128, epochs=20)')
ax1.set_ylabel('Accuracy')
ax1.set_xlabel('Epoch')
ax1.legend(['Train', 'Test'], loc='upper center')
#center right

# plot train and validation loss
ax2.plot(history.history['loss'])
ax2.plot(history.history['val_loss'])
#ax2.set_title('Model loss \n (units=300, epochs=200, softmax)')
ax2.set_title('Model loss \n (batch_size=128, epochs=20)')
ax2.set_ylabel('Loss')
ax2.set_xlabel('Epoch')
ax2.legend(['Train', 'Test'], loc='upper center')
plt.show()
