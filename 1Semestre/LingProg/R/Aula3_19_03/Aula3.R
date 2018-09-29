#---------------------------------------------------------------------------
#INDEXAÇÃO
#---------------------------------------------------------------------------
## Indexação e seleção
##Indexação de vetores
cont<-c(8,4,NA,9,6,1,7,9)
cont

cont[4]
cont[-4]

cont[c(1,4,8)]
cont[-c(1,4,8)]

cont[1:5]
cont[-(1:5)]

cont[seq(1,8,2)]

#---------------------------------------------------------------------------
#EXERCICIO
#---------------------------------------------------------------------------

#CRIE UM VETOR COM VALORES: 88, 5, 12 , 13
x<-c(88, 5, 12 , 13)

#Selecione o elemento na posicao 3..
x[3]

#Selecione o valor 88
x[1]

#Selecione os valores 13 e 5
x[c(4,2)] #Ver que preserva a ordem...

#Selecione todos os valores, menos 88 e 13..
x[-c(1,4)]

#Insira o valor 168 entre 12 e 3, criando um novo objeto
y<-c(x[1:3],168,x[4])
y

append(x, 168, after = 3)


#Identificar NA
is.na(cont)
cont[is.na(cont)]

#Substituir NA
cont[is.na(cont)]<-0 #Por zero...
cont

#Voltar com NA na posição 3...
cont[3] <- NA
cont

#Operacao contraria...
!is.na(cont)
cont[!is.na(cont)]

names(cont) <- letters[1:length(cont)]
cont

cont["d"]
cont[c("f","a")]

#--------------------------------------------
##Indexação de matrizes
#--------------------------------------------
m<-matrix(1:9,nrow=3)
m
m[2,3] #Por default, retorna vetores de uma dimensão...
m[3,]
m[,2]

#Então, como retornar uma matriz?
m[3, , drop=FALSE] #Retorna uma matriz...
m[, 2, drop=FALSE]

m[c(1,3),c(2,3)]

#Atribui nomes...
colnames(m)<- LETTERS[1:3]
rownames(m)<- LETTERS[24:26]
m

m["Z","A"]
m[,"B",drop=F]
m[1,"C"]

#-----------------------------------------
##LISTA
#-----------------------------------------
lis<-list(c(3,8,7,4),m,5:0) #Três componentes
lis

lis[1] #Retorna o primeiro componente, e mantem o formato de lista
class(lis[1])

lis[[1]] #Retorna só os valores que estavam dentro da lista...
class(lis[[1]]) #Veja que deixou de ser lista. Agora, posso calcular uma média
mean(lis[[1]])

lis[[2]]
lis[[1]][3] #Acessar valor da posicao 3 do componente 1...
lis[[2]][,2]

#Dar nomes aso componentes da lista...
names(lis)<-c("Vetor1","Matriz","Vetor2") #Quando os componentes estão nomesdos eu posso usar o dolar ($)..
lis$Vetor1
lis$Matriz[2,3]
lis[["Vetor2"]]

#----------------------------------------------
##DATAFRAME
#----------------------------------------------
da<-data.frame(A=4:1,B=c(2,NA,5,8))
da
da[2,1]
da[,2]
da[,"B"]

row.names(da)
da["1",]
da[1,]
da[,2]

#Preservar a estrutura de dataframe...
da[,2, drop=F]
class(da[,2, drop=F])

str(da)
da$A
da$B

da$A[c(1,3)]
