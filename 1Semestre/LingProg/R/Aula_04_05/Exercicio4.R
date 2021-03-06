#Exerc�cios 4
#Crie um objeto para armazenar a seguinte matriz
m<-m<-matrix(c(2,0,9,8,4,7,4,1,5), nrow=3, ncol=3, byrow=F)
m

#Atribua nomes para as linhas e colunas dessa matriz.
?rownames
rownames(m) <- c("A","B","C")
m
colnames(m) <- c("D","E","F")
m

#Crie uma lista (n�o nomeada) com dois componentes: (1) um vetor com as letras A, B, e C, repetidas 2, 5, e 4 vezes respectivamente; e (2) a matriz do exemplo anterior.
lista<-list(rep(c("A","B","C"),times=c(2,5,4)),m)
lista

#Atribua nomes para estes dois componentes da lista.
lista<-list(Vetor=rep(c("A","B","C"),times=c(2,5,4)),Matriz=m)
lista

lista<-list(rep(c("A","B","C"),times=c(2,5,4)),m)
lista
names(lista)<-c("Vetor","Matriz")
lista

#Inclua mais um componente nesta lista, com o nome de fator, e que seja um vetor da classe factor,id�ntico ao objeto caracter criado acima (que possui apenas os nomes brava, joaquina, arma��o).
lista<-list(Vetor=rep(c("A","B","C"),times=c(2,5,4)),Matriz=m)
lista
lista$caracter<-c("brava","joaquina","arma��o")
lista

#Crie um data frame para armazenar duas vari�veis: local (A, B, C, D), e contagem (42, 34, 59 e 18).
df<-data.frame(local= c("A", "B", "C", "D"), contagem=c(42,34,59,18))
df

#Crie um data frame com as seguintes colunas:
#Nome,
#Sobrenome
#Se possui animal de estima��o
#Caso possua, dizer o n�mero de animais (caso contr�rio, colocar 0)

#Para criar o data frame, a primeira linha deve ser preenchida com as suas pr�prias informa��o (use a fun��o  data.frame()). Depois, pergunte essas mesmas informa��es para dois colegas ao seu lado, e adicione as informa��es deles � esse data frame (use rbind()). Acresente mais uma coluna com o nome do time de futebol de cada um.

data.frame(Nome=c("Deivison","Marcio","Jayme"), 
           Sobrenome=c("Souza", "Liz", "Silva"), 
           Animal=c("Sim","N�o","N�o"), 
           N=c(1,0,0))

#Usando rbind...
df<-data.frame(Nome="Deivison", 
               Sobrenome="Souza", 
               Animal="Sim", 
               n=1)
df

df<-rbind(df,c("Marcio","Liz","N�o",0)) #N�o � poss�vel concatenar...
df<-rbind(df,list("Marcio","Liz","N�o",0)) #Alternativa, usar list. No entanto, observa que retorna NA. O que fazer?

#Criar os novos n�veis...
df<-data.frame(Nome="Deivison", 
               Sobrenome="Souza", 
               Animal="Sim", 
               n=1)
str(df)
levels(df$Nome)<-c("Deivison","Marcio")
levels(df$Sobrenome)<-c("Deivison","Liz")
levels(df$Animal)<-c("Sim","N�o")

df<-rbind(df,list("Marcio","Liz","N�o",0))
df



