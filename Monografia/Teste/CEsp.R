##################################################################################################
#############CLASSIFICACAO DE ESPECIES USANDO IMAGENS MACROSCOPICAS DA MADEIRA####################
##################################################################################################

# Este script faz parte o processo de aprendizado para classificar espécies florestais a partir de
# imagens macrosopicas de madeira. Aqui, irei trabalhar com 3 espécies e 15 imagens, 5 de cada
# espécies. As imagens estao rotuladas por códigos de 4 digitos. Os dois primeiros digitos identificam
# a espécie e os dois últimos o número da imagem. Por exemplo, a imagem codificada como "0102",
# 01 = Acrocarpus e 02 = imagem 2 da espécie.

#Então, tem-se: 
# 01 = Acrocarpus (Acrocarpus fraxinifolius)
# 04 = Angelin (Hymenolobium petraeum)
# 07 = Bracatinga (Mimosa scabrella)
# 34 = Pinus (Pinus sp)

# Instalação da biblioteca "EBImage"
install.packages("BiocManager")
BiocManager::install("EBImage")

###################################################################################################
# Carregando pacotes
###################################################################################################
library(EBImage)   # trabalhar com imagens
library(keras)     # interface entre R e TensorFlow
library(caret)     # interface para treinamento de modelos
library(pbapply)   # loop com barras de progresso

###################################################################################################
# Diretório de trabalho
###################################################################################################
getwd()               # diretorio de trabalho atual?
setwd(choose.dir())   # Muda diretório
dir()                 # listar os arquivos existentes no diretório corrente

# Obs.: Os arquivos de imagem estao na pasta "Macro".

###################################################################################################
# Propriedades das Imagens - usando EBImage
###################################################################################################
# Lendo a imagem "0401" = Angelin
#----------------------------------------------------------------------
browseVignettes("EBImage")

path <- "C:/Users/Deivison/OneDrive/DS&BD/TCC_DSBD/Teste/Macro/0401.jpg"
?Image
img <- readImage(path)                      # ler a imagem inteira
img_rec <- readImage(path)[1:1623, 1:1224,] # ler apenas canto superior-esquerdo

# Obs.: EBImage suporta três formatos de arquivo de imagem: jpeg, png e tiff.
# Obs2.: Classe "Image"

#----------------------------------------------------------------------
# Pode-se visualizar a imagem original e recortada:
#----------------------------------------------------------------------
?display
display(img, method="browser")
text(x = 20, y = 20, label = "Angelim", adj = c(0,1), col = "white", cex = 2) # Inserir texto

display(img, method="raster")

display(img_rec, method="browser")
display(img_rec, method="raster", all=T)

#----------------------------------------------------------------------
# Visualizando dados da imagem - imageData()
#----------------------------------------------------------------------
imageData(img)[1:5,1:6,1]
display(imageData(img), method="raster", all=T)                   # imagem inteira
display(imageData(img)[1:1623, 1:1224, ], method="raster", all=T) #superior-esquerdo

#Obs.: argumento "all = TRUE" permite vizualizar cada canal em tons de cinza.

#----------------------------------------------------------------------
# Visualizando as 3 espécies
#----------------------------------------------------------------------
path2 <- "C:/Users/Deivison/OneDrive/DS&BD/TCC_DSBD/Teste/Macro/0101.jpg"
path3 <- "C:/Users/Deivison/OneDrive/DS&BD/TCC_DSBD/Teste/Macro/0701.jpg"

img_Acro <- readImage(path2)
img_Brac <- readImage(path3)

op <- par(mfrow=c(2,2), mar=c(8.1,1.1,2.1,1.1)) # mar(bottom, left, top, right)
display(img, method="raster", all=T)
text(x = 20, y = 20, label = "Angelim", adj = c(0,1), col = "white", cex = 3)

display(img_Acro, method="raster", all=T)
text(x = 20, y = 20, label = "Acrocarpus", adj = c(0,1), col = "white", cex = 3)

display(img_Brac, method="raster", all=T)
text(x = 20, y = 20, label = "Bracatinga", adj = c(0,1), col = "white", cex = 3)

# Salvando imagens
?dev.print
dev.print(jpeg, filename = "Prancha.jpeg", res = 300,
          width = dim(img)[1], height = dim(img)[2])

writeImage(img, "Prancha2.jpeg", quality = 100) # qualidade 1 a 100

#----------------------------------------------------------------------
# Histograma da distribuicao da intensidade de pixels por canal
#----------------------------------------------------------------------
hist(img)
hist(img_rec)

# Faixa de variação de pixels?
range(img)

#----------------------------------------------------------------------
# Quais as propriedades da imagem original?
#----------------------------------------------------------------------
print(img)           # dimensoes: largura: 3264, altura = 2448, 3 canais = RGB
print(img, short=TRUE)
# Obs1: É impresso uma pedaço da matriz de características. As cinco primeiras linhas,
# das 6 primeiras colunas, do canal 1 = Red.

# Obs2: As dimensoes podem ser consultadas tambem fazendo: dim(img)
# Obs3: 3264 e 2448 corresponde à largura e altura do pixel da imagem.

#----------------------------------------------------------------------
# Qual a estrutura do objeto "img" que contem informacoes da imagem "0401"?
#----------------------------------------------------------------------
str(img)

# Basicamente, tem dois elementos: @.Data e @.colormode

# @.Data = É um array que contém as informaçoes numericas dos pixels da imagem, para
# os 3 canais de cor (RGB) (Um array com dados de pixels!)

# Vamos salvar as informaçoes contidas em @.Data em um objeto:
data.img <- img@.Data     # um array

# Obs1.: No ambiente global é possivel verificar que o array possui: 23.970.816 elementos.
# Este valor é o produto: Dimensao*Largura*Canais = 3264*2448*3

3264*2448*3

# Obs2.: Os valores variam no intervalo de 0 e 1. Ambos os extremos desse 
# intervalo [0, 1] são cores preto e branco, respectivamente. Assim, espera-se que os 
# pixels com valores mais próximos de qualquer um desses pontos extremos sejam mais 
# escuros ou mais claros, respectivamente. E como os pixels estão contidos em uma 
# matriz grande, podemos fazer todas as manipulações de matriz disponíveis em R para 
# processamento.


# No futuro será importante dispor os dados como um vetor de características, então:

data.img <- img@.Data %>% as.vector() %>% print()
length(data.img)

#----------------------------------------------------------------------
# Acessando informacao de cada Canal:
#----------------------------------------------------------------------
CR <- as.vector(img@.Data[1:3264, 1:2448, 1]) # canal R
CG <- as.vector(img@.Data[1:3264, 1:2448, 2]) # canal G
CB <- as.vector(img@.Data[1:3264, 1:2448, 3]) # canal B

length(CR)*3


###################################################################################################
# Customizando o brilho e contraste
###################################################################################################
# Ajustando brilho
#----------------------------------------------------------------------
img2 <- img + 0.2
img3 <- img - 0.2

display(img2)   # mais claro
display(img3)   # mais escuro

# Ajustando contraste
#----------------------------------------------------------------------
img4 <- img*0.5
img5 <- img*2

display(img4)
display(img5)

###################################################################################################
# Fazendo Recorte
###################################################################################################
print(img)
display(img)

# Considerando a divisao da imagem em 4 partes:
display(img[1:1623, 1:1224,])       # recorte: superior-esquerdo
display(img[1624:3264, 1:1224,])    # recorte: superior-direito
display(img[1:1623, 1225:2448,])    # recorte: inferior-esquerdo
display(img[1624:3264, 1225:2448,]) # recorte: inferior-direito


###################################################################################################
# Transformacao espacial
###################################################################################################
display(img)

# rotaçao 90 - Ficar sentido dos raios
img6 <- rotate(img, 90) 
display(img6)

img6 <- rotate(img, 30, bg.col="white")
display(img6)

# Transpor
img_t <- transpose(img)
display(img_t)

###################################################################################################
# Redimensionando uma imagem - Função Resize
###################################################################################################
img_resize = resize(img, w=256, h=256)
display(img_resize)

# Funções flipe flop

img_flip = flip(img)
img_flop = flop(img)

display(combine(img_flip, img_flop), all=TRUE)


###################################################################################################
# Gerenciando a cor: Função colorMode
###################################################################################################
# A função colorMode pode ser usada para acessar e alterar, modificando
# o modo de renderização de uma imagem. Por exemplo, se pegarmos uma  Color imagem e 
# alterarmos seu modo para Grayscale, a imagem não será mais exibida como uma única 
# imagem colorida, mas sim como três quadros separados em escala de cinza correspondentes 
# aos canais vermelho, verde e azul. A função colorMode não altera o conteúdo real da 
# imagem, mas apenas altera a maneira como a imagem é renderizada pelo EBImage .
display(img, method="raster") 
display(imageData(img), method="raster", all=T) 

colorMode(img) <- Grayscale
display(img, method="raster") 
display(img, method="raster", all=T) 


display(img)                   # imagem original
display(img7[1:3264,1:2448,1]) # Canal R
display(img7[1:3264,1:2448,2]) # Canal G
display(img7[1:3264,1:2448,3]) # Canal B

###################################################################################################
# Manipulando Imagens
###################################################################################################
# Sendo matrizes numéricas, as imagens podem ser convenientemente manipuladas por qualquer um dos 
# operadores aritméticos de R. Por exemplo, podemos produzir uma imagem negativa simplesmente 
# subtraindo a imagem do seu valor máximo.
img <- readImage(path)
display(img)

# Imagem Negativa
#----------------------------------------------------------------
img_neg = max(img) - img
display(img_neg)

# Brilho, contraste, correcao gama
#----------------------------------------------------------------
# Podemos também aumentar o brilho de uma imagem através da adição, ajustar o contraste
# através da multiplicação e aplicar a correção gama através da exponenciação.

img_comb = combine(
  img,
  img+0.3, # brilho
  img*2,   # contraste
  img^0.5  # gama
)

display(img_comb, all=TRUE)


###################################################################################################
# Filtragem
###################################################################################################
# Filtros Lineares
#-----------------------------------------------
w = makeBrush(size = 31, shape = 'gaussian', sigma = 5)
img_flo = filter2(img, w)
display(img_flo)


fhi = matrix(1, nrow = 3, ncol = 3)
fhi[2, 2] = -8
img_fhi = filter2(img, fhi)
display(img_fhi)


# Filtro Mediano
#-----------------------------------------------
l = length(img)
n = l/10
pixels = sample(l, n)
img_noisy = img
img_noisy[pixels] = runif(n, min=0, max=1)
display(img_noisy)

img_median = medianFilter(img_noisy, 1)
display(img_median)

###################################################################################################
# Listar imagens no diretorio corrente
###################################################################################################
?list.files

# Antes, deve-se especificar a pasta onde estao as imagens. 
# Lembrar que as img estao na pasta "Macro/" dentro do diretorio corrente.

list.files(path = "Macro/", pattern = "*.JPG")
list.files(path = "Macro/", full.names = T) # full.names = retorna nome da pasta/nome dos arquivos

# Usar a função "dir" também funciona:
dir(path = "Macro/")
dir(path = "Macro/", full.names = T)

# Quantas imagens no diretorio?
length(list.files(path = "Macro/", pattern = "*.JPG"))

###################################################################################################
# Padrões binários locais (lbp) - Pacote "wvtool"
###################################################################################################
# wvtool: Ferramentas de Imagem para Identificação Automatizada de Madeira

# O operador LBP foi originalmente projetado para descrição de textura. O operador atribui 
# um rótulo a cada pixel de uma imagem, limitando a vizinhança de 3x3 de cada pixel com
# o valor de pixel central e considerando o resultado como um número binário (dá 0 se 
# cada pixel é menor que o centro, caso contrário 1). Então, o histograma dos rótulos 
# pode ser usado como um descritor de textura. As vizinhanças circulares (8, r = 1) e 
# (8, r = 2) são consideradas. A função assume uma imagem em tons de cinza de 8 bits 
# como entrada.

# Esta ferramenta, ferramenta de visão de madeira, destina-se a facilitar o pré-processamento 
# e a análise de imagens de madeira bidimensionais para o reconhecimento automático. O
#  primeiro inclui algumas noções básicas, como funções de RGB para tons de cinza, 
#  cinza para binário, corte, rotação (bilinear), mediana / média / filtro gaussiano 
#  e detecção de borda de Canny / Sobel. Este último inclui matriz de co-ocorrência de 
#  nível de cinza (GLCM), parâmetros de Haralick, padrão binário local (LBP), autocorrelação 
#  local de ordem superior (HLAC), transformada de Fourier (integração radial e 
#  azimutal) e filtragem Gabor. As funções são destinadas a ler dados usando 
#  'readTIFF (x, info = T)' do pacote 'tiff'. As funções neste pacote basicamente 
#  assumem as imagens em escala de cinza como dados de entrada, portanto, as imagens 
#  coloridas devem ser submetidas à função rgb2gray () antes de serem usadas para 
#  algumas outras funções.

#---------------------------------------------------------------------------------------------------
# Ferramentas do pacote "wvtool"
#---------------------------------------------------------------------------------------------------
rgb2gray # Converter imagem RGB em escala de cinza. Uma função retorna a imagem em tons de cinza 
#com coeficientes = c (0,3, 0,59, 0,11).

rot90c       # Transpoe a imagem 90 graus no sentido horário.

gabor.filter # Filtragem Gabor Bidimensional no Domínio de Frequência

gray2bin     # Conversão de Grayscale para Binary Image

lbp          # Padrões binários locais (lbp) - descritor de textura

glcm         # Matriz de co-ocorrência de nível cinza

#-------------------------------------------------------------------------------------
# Exemplo do pacote
#-------------------------------------------------------------------------------------
library(wvtool)
data(camphora)
display(camphora, method="raster") # a imagem está em tons de cinza
print(camphora)                    # a classe de imagem não é EBImage
hist(camphora)
range(camphora)

par(mfrow=c(2,2))
r1 <- lbp(camphora,1)
image(rot90c(r1$lbp.u2),col = gray((0:58)/58), main="lbp.u2 (r=1, 8 points)", useRaster=TRUE, 
      asp=1, axes=FALSE)
image(rot90c(r1$lbp.ori),col = gray((0:255)/255), main="lbp.ori (r=1, 8 points)", useRaster=TRUE, 
      asp=1, axes=FALSE)
hist(r1$lbp.u2,breaks=59, main="Histogram of lbp.u2")
hist(r1$lbp.ori,breaks=256, main="Histogram of lbp.ori")


#
path <- "C:/Users/Deivison/OneDrive/DS&BD/TCC_DSBD/Teste/Macro/0401.jpg"
img<-readImage(path)  # formato .jpeg
img_tiff <- writeImage(img, '0401.tiff', quality=100)

path_teste <- "C:/Users/Deivison/OneDrive/DS&BD/TCC_DSBD/Teste/0401.tiff"
img_teste<-readImage(path_teste,native=TRUE)
display(img_teste)
print(img_teste)



library(tiff)
path <- "C:/Users/Deivison/OneDrive/DS&BD/TCC_DSBD/Teste/Macro/images.tiff"
img_tiff <- readTIFF(path_teste,info=T)
display(img_tiff)

rgb2gray(img_tiff, coefs=c(0.30, 0.59, 0.11))





