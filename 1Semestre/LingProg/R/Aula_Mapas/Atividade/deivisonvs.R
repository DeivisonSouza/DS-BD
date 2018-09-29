#############################################################################################
#####------------------------ ELABORAÇÃO DE MAPA COM ATRIBUTO-------------------------- #####
##----------------------------Autor: Deivison Venicio Souza--------------------------------##
#############################################################################################

#--------------------------------------------------------------------------------------------
## OBTENDO O ARQUIVO SHAPEFILE
#--------------------------------------------------------------------------------------------

#O arquivo shapefile do Estado do Paraná foi adquirdo no link:
#http://geoftp.ibge.gov.br/organizacao_do_territorio/malhas_territoriais/malhas_de_setores_censitarios__divisoes_intramunicipais/censo_2010/setores_censitarios_shp/pa/
#No link acessei a pasta "pr/" e, em seguida, "pr_municipios.zip" para fazer o download da malha municipal
# do Estado do Paraná.
# 
#--------------------------------------------------------------------------------------------
## OBTENDO A TABELA DE ATRIBUTOS: NASCIDOS E VIVOS NO PARANÁ (ANO 2016)
#--------------------------------------------------------------------------------------------
# A tabela de atributos foi obtida no site do DATASUS (Estatísticas Vitais). No link abaixo:
# http://tabnet.datasus.gov.br/cgi/tabcgi.exe?sinasc/cnv/nvPR.def
# No site selecionei a opção "Nascidos Vivos - 1994 a 2016", escolhi o Estado do Paraná,
# ano 2016 (Nascim p/resid.mãe). Os dados de nascimento por município foram espacializados
# no mapa do Estado do Paraná.
#--------------------------------------------------------------------------------------------
## CARREGANDO OS PACOTES NECESSÁRIOS
#--------------------------------------------------------------------------------------------
# Carrega as bibliotecas para criação do mapa e espacializacão de atributo
library(foreign)
library(rgdal)
library(reshape2) #Para dividir uma string em duas colunas
library(maptools) #Aqui usei apenas para empregar a função "leglabs"
#--------------------------------------------------------------------------------------------
## IMPORTANDO SHAPE DO ESTADO DO PARANÁ COM DIVISÃO MUNICIPAL
#--------------------------------------------------------------------------------------------
map = readOGR("C:/Users/Deivison/OneDrive/DS&BD/Linguagem R/Aula_Mapas/Atividade/pr_municipios", "41MUE250GC_SIR")
View(map@data)         #399 municípios (PR)
names(map)             #3 atributos de dados
str(map@data)          #estrutura do objeto @data

#--------------------------------------------------------------------------------------------
# Plotando o Paraná e seus municípios
par(mar=c(0,0,0,0))    #configura margens
plot(map)              #visualiza a malha municipal do PR

#-------------------------------------------------------------------------------------------
# Uma inspeção do objeto "map"
head(map@data)         #imprime 6 primeiras linhas do objeto "map"
dim(map@data)          #dimensão do objeto @data
names(attributes(map))
map@class              #classe do objeto
length(map@polygons)   #Quantidade de polígonos
str(map@polygons[[1]])

#--------------------------------------------------------------------------------------------
## IMPORTANDO TABELA DE ATRIBUTOS
#--------------------------------------------------------------------------------------------
# Contém dados de pessoas nascidas e no Estado no Paraná e vivas no ano de 2016. O data set contém 
# informaçoes por município.

#Ler conjunto de dados baixado
natalidade1 <- read.csv2("C:/Users/Deivison/OneDrive/DS&BD/Linguagem R/Aula_Mapas/Atividade/A165211191_177_187_217.csv", header = TRUE, encoding='latin1', skip=3, na.string='-') #skip=3: ignora as linnhas que saõ cabecalhos

#Divide a coluna municipio em duas colunas: (codigo, Municipio) e adiciona ao objeto "natalidade2"
natalidade2 <-colsplit(string = natalidade1$Município, pattern = " ", names=c("CODIGO","NM_MUNICIP"))

#Adiciona coluna de "Nascidos2016" ao objeto "natalidade2"
natalidade2$NASCIDOS2016<-natalidade1$Nascim_p.resid.mãe

# Transforma os nomes dos municípios para caixa alta.
natalidade2["NM_MUNICIP"] <- toupper(natalidade2[,2])

# Inspeciona o objeto "natalidade2"
head(natalidade2)       #imprime as 6 primeiras linhas do dataset
dim(natalidade2)        #399 municípios e 3 atributos
str(natalidade2)        #estrutura do dataset
summary(natalidade2)    #summario do dataset

#--------------------------------------------------------------------------------------------
## FAZENDO UM JOIN (MERGE)
#--------------------------------------------------------------------------------------------
# Join (merge) entre map@data e natalidade2...
map@data <- merge(map@data, natalidade2, by="NM_MUNICIP", sort=FALSE)
View(map@data)
str(map@data)

# verificando a cidade com mais de 23000 nascidos-vivos em 2016...Será Curitiba?
map@data[map@data$NASCIDOS2016 > 23000, ]

# Criando um mapa com o atributo "NASCIDOS2016" espacializado...
spplot(map, "NASCIDOS2016")

#--------------------------------------------------------------------------------------------
## MELHORANDO O MAPA: CRIANDO CLASSES E CORES
#--------------------------------------------------------------------------------------------
# Criando classes para o atributo "NASCIDOS2016"
classes <- quantile(map$NASCIDOS2016, 0:10/10)
print(classes)

# Criando grid de cores rgb
color <- findInterval(map$NASCIDOS2016, classes, rightmost.closed=TRUE)
table(color)

RGBcolor <- rgb(0:9/10, 1-2*abs(0:9/10-0.45), 9:0/10)
print(RGBcolor)

# Plotando o novo mapa
tiff('Mapa.tiff', units="in", width=8, height=4, res=100)
par(mar=c(2,2,2,2))
plot(map, col=RGBcolor[color], 
     main="Nascido e Vivos por Município no Paraná \n (Ano 2016)", 
     lwd=1)

legend("topright", leglabs(format(classes, dig=2), '<', '>'),
       fill=RGBcolor, bty='n', title="Quantidade")
dev.off()

#--------------------------------------------------------------------------------------------
## END
#--------------------------------------------------------------------------------------------



