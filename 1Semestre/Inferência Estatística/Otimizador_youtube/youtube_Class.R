#############################################################################################
#####-----------------CLASSIFICADOR P/ ACESSO A CANAIS DO YOUTUBE---------------------- #####
##----------------------------Autor: Deivison Venicio Souza--------------------------------##
#############################################################################################
#Avaliação: Ajustando modelos não-lineares

#O conjunto de dados youtube.csv apresenta o numero de views e inscritos desde o dia de sua 
#abertura para dois canais de sucesso do youtube.

#O objetivo é predizer o número acumulado de inscritos em cada um destes canais para o próximo 
#ano (365 dias).

#Para isto você decidiu emprestar um modelo biológico do crescimento de bactérias chamado 
#modelo logístico, dado pela seguinte equação:

#y = L/[1+exp(b(x-b0))]

#onde:
# L: é o valor máximo da curva;
# b0 (beta zero): o valor de x no ponto médio da curva; e
# b (beta): e a declividade da curva.

#--------------------------------------------------------------------------------------------
## QUESTÕES
#--------------------------------------------------------------------------------------------
#1) Proponha e descreva um algoritmo para ajustar este modelo aos dados disponíveis.
#2) Ajuste o modelo aos dados dos canais e reporte a sua predição de forma gráfica.

#############################################################################################
## CARREGANDO PACOTES NECESSÁRIOS
#############################################################################################
library(easypackages)
libraries("ggplot2","ggthemes","dplyr", "data.table", "cowplot")

#############################################################################################
## CARREGANDO OS DADOS
#############################################################################################
dados <- read.table("youtube.txt", header = T, sep="")

#apenas p/ tranformar p/ minúsculo
colnames(dados) <- tolower(colnames(dados))

#apenas customizando os nomes dos canais
dados$canal <- with(dados,ifelse((canal == "inventonahora"),"Canal-1" ,"Canal-2"))

#############################################################################################
# ANÁLISE EXPLORATÓRIA DOS DADOS
#############################################################################################
str(dados)          # estrutura dos dados
class(dados)        # classe do objeto dados
names(dados)        # atributos disponíveis
head(dados)         # 6 primeiras instâncias
tail(dados)         # 6 últimas instâncias
table(dados$canal)  # quantidade de observações por canal

## Frequências acumuladas de inscritos e views ao longo do tempo...

setDT(dados)[,`:=`(fcum.insc = cumsum(inscritos)/100000, 
                   fcum.views = cumsum(views)/100000),
             by=canal][]

#--------------------------------------------------------------------------------------------
# Visualização gráfica - Número de inscritos ao longo do tempo...
#--------------------------------------------------------------------------------------------
ggplot(data=dados,aes(x=dias,y=inscritos,colour=canal))+
  geom_point(aes(size=views))+
  ggtitle(label="")+ theme_few()+
  scale_color_manual(values=c("green","red"))+
  theme(axis.line.x=element_line(size=0.5,colour="black"),
        axis.line.y=element_line(size=0.5,colour="black"),
        axis.line=element_line(size=1,colour="black"),
        strip.text.x=element_text(colour="black",size=12,family="serif",face="bold"),
        strip.background = element_rect(colour="black", fill="snow2"),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        panel.border=element_rect(color="black"),
        panel.background=element_blank(),
        axis.text.x=element_text(colour="black",size=12,family="serif",angle=90),
        axis.text.y=element_text(colour="black",size=12,family="serif"),
        #legend.position="none",
        legend.position=c(0.2,0.9), 
        legend.box = "horizontal",
        legend.direction="vertical",
        legend.justification="center",
        legend.key=element_rect(fill="white",colour="white"),
        legend.text=element_text(size=12,colour="black",family="serif"),
        legend.key.size=unit(1,"cm"),
        legend.key.height=unit(0.8,"line"),
        legend.margin=margin(t=0,r=0,b=0,l=0,unit="cm"),
        legend.title=element_text(),
        plot.title=element_text(hjust=0.50,size=16,face="bold",family="serif"),
        plot.subtitle=element_text(hjust=0.50,size=12,face="italic",family="serif"))+
  scale_x_continuous(name="Dias da abertura",
                     breaks=c(seq(from=0,
                                  to=max(dados$dias),
                                  by=30)),
                     limits=c(0,max(dados$dias)))+
  scale_y_continuous(name="Número de inscritos",
                     breaks=c(seq(from=0,
                                  to=max(dados$inscritos),
                                  by=1000)),
                     limits=c(0,max(dados$inscritos)))

#--------------------------------------------------------------------------------------------
# Visualização gráfica - Curva acumulada de inscritos ao longo do tempo...
#--------------------------------------------------------------------------------------------
melt<-melt(dados,id.vars=c("canal", "dias"),
        measure.vars=list(c("fcum.insc")),
        value.factor=TRUE, variable.name = "variable", 
        value.name = "fcum", na.rm=TRUE)
#melt[, grp := .GRP, by=c("variable","canal")]

ggplot(melt, aes(x=dias, y=fcum)) + geom_step() + 
  geom_vline(data=filter(melt, canal=="Canal-1"), 
             aes(xintercept=which.max(subset(dados, canal == "Canal-1")$dias)),
             colour="red", linetype = "dashed") + 
  geom_vline(data=filter(melt, canal=="Canal-2"), 
               aes(xintercept=which.max(subset(dados, canal == "Canal-2")$dias)),
               colour="red", linetype = "dashed") +
  facet_wrap(~canal,scales="free") + theme_bw() +
  theme(axis.line.x=element_line(size=0.5,colour="black"),
        axis.line.y=element_line(size=0.5,colour="black"),
        axis.line=element_line(size=1,colour="black"),
        axis.text.x=element_text(colour="black",size=10,angle=90,family="serif"),
        axis.text.y=element_text(colour="black",size=10,family="serif"),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        panel.border=element_rect(),
        panel.background=element_blank(),
        plot.title=element_text(hjust=0.50,size=14,face="bold",family="serif"),
        plot.subtitle=element_text(hjust=0.50, size=12,face="italic",family="serif"),
        legend.text=element_text(size=12,colour="black",family="serif"),
        legend.position=c(0.80,0.1),
        legend.margin=margin(1,1,1,1),
        legend.title=element_blank(),
        legend.direction="horizontal")+
  scale_y_continuous(name="Número de inscritos (por 1e+5)",
                     breaks=c(seq(from=0,
                                  to=25,
                                  by=5)),
                     limits=c(0,25))+
  scale_x_continuous(name="Dias da abertura",
                     breaks=c(seq(from=0,
                                  to=max(dados$dias),
                                  by=50)),
                     limits=c(0,max(dados$dias)))

#############################################################################################
# DIVIDE CONJUNTO DE DADOS PARA MODELAGEM
#############################################################################################
Canal.1 <- subset(dados, canal == "Canal-1")
Canal.2 <- subset(dados, canal == "Canal-2")

#############################################################################################
# FUNÇÕES PERDA
#############################################################################################

# Modelo Linear (usando perda quadrática)
f_ols <- function(par, y, x1,...) {
  mu <- par[1] + par[2]*x1
  SQ <- sum((y - mu)^2)
  return(SQ)
}

#Em que: 
#par = parâmetros do modelo; 
#y = variável a ser predita (dado real); 
#x1 = variável preditora (dado real);
#mu = valor da predição de cada "y";
#SQ = Soma da perda quadrática.

# Função logística (exemplo)
#f_logit <- function(par, y, x1,...) {
#  mu <- 1/(1 + exp(- (par[1] + par[2]*x1)))
#  SQ_logit <- sum((y - mu)^2)
#  return(SQ_logit)
#}

# Modelo Logístico (usando perda quadrática)
f_logit2 <- function(par, y, x1,...) {
  mu <- par[1] / (1 + exp(par[2]*(x1 - par[3])))
  SQ_logit <- sum((y - mu)^2)
  return(SQ_logit)
}

# A função logit recebe os seguintes parâmetros: 
# L (valor máximo da curva); = par[1]
# beta (declividade da curva); = par[2]
# beta zero (valor de x no ponto médio da curva). = par[3]

#############################################################################################
# OTIMIZANDO
#############################################################################################
#--------------------------------------------------------------------------------------------
## CANAL-1
#--------------------------------------------------------------------------------------------
# Usando a função perda quadrática e a função optim p/ otimizar
fit_ols_C1 <- optim(par = c(0,0), 
                    fn = f_ols, 
                    y = Canal.1$fcum.insc, 
                    x1 = Canal.1$dias)
# parâmetros otimizados pelo optim, usando da função perda quadrática
fit_ols_C1$par   
fit_ols_C1$value #valor da perda quadrática
#--------------------------------------------------------------------------------------------
# Usando a função logit e a função optim p/ otimizar
fit_logit_C1 <- optim(par = c(25,-0.007,600), 
                      fn = f_logit2, 
                      y = Canal.1$fcum.insc, 
                      x1 = Canal.1$dias)
fit_logit_C1$par
fit_logit_C1$value #
#--------------------------------------------------------------------------------------------
## CANAL-2
#--------------------------------------------------------------------------------------------
# Usando a função perda quadrática e a função optim p/ otimizar
fit_ols_C2 <- optim(par = c(0,0), 
                    fn = f_ols, 
                    y = Canal.2$fcum.insc, 
                    x1 = Canal.2$dias)
# parâmetros otimizados pelo optim, usando da função perda quadrática
fit_ols_C2$par      

#--------------------------------------------------------------------------------------------
# Usando a função logit e a função optim p/ otimizar
fit_logit_C2 <- optim(par = c(20,-0.001,400), 
                      fn = f_logit2, 
                      y = Canal.2$fcum.insc, 
                      x1 = Canal.2$dias)
fit_logit_C2$par

#############################################################################################
## OBTENDO AS PREDIÇÕES E PROGNOSE PARA 365 DIAS
#############################################################################################
# Cria um data.table com a sequência de dias observados + 365 (p/ cada canal)....
DTpred_C1 <- data.table(dias = with(Canal.1,seq(1, max(dias) + 365)), 
                        pred_ols = NA, pred_logit = NA)

DTpred_C2 <- data.table(dias = with(Canal.2,seq(1, max(dias) + 365)), 
                        pred_ols = NA, pred_logit = NA)

# Predições para os dias observados e prognose p/ próximos 365 dias
# Canal 1
DTpred_C1$pred_ols <- fit_ols_C1$par[1] + fit_ols_C1$par[2]*DTpred_C1$dias

DTpred_C1$pred_logit <- fit_logit_C1$par[1]/(1 + exp(fit_logit_C1$par[2]*(DTpred_C1$dias - fit_logit_C1$par[3])))

# Canal 2
DTpred_C2$pred_ols <- fit_ols_C2$par[1] + fit_ols_C2$par[2]*DTpred_C2$dias

DTpred_C2$pred_logit <- fit_logit_C2$par[1]/(1 + exp(fit_logit_C2$par[2]*(DTpred_C2$dias - fit_logit_C2$par[3])))

#------------------------------------------------------------------------
# Curva ajustada: Real x Predito (Canal-1)
g1<-ggplot(Canal.1, aes(x=dias, y=fcum.insc)) + geom_step(linetype=1, lwd=0.8)+
  geom_vline(data=Canal.1,aes(xintercept=which.max(Canal.1$dias)),
             colour="blue", linetype = "dashed")+
  geom_line(data=DTpred_C1,aes(x=dias,y=pred_logit), colour="red",lwd=0.6,lty=1) +
  geom_hline(yintercept=fit_logit_C1$par[1],colour="green",lwd=0.5,lty="solid")+
  ggtitle(label="Curva ajustada x Curva real",subtitle="(Inscritos no Canal-1)")+
  theme_bw() +
  theme(axis.line.x=element_line(size=0.5,colour="black"),
        axis.line.y=element_line(size=0.5,colour="black"),
        axis.line=element_line(size=1,colour="black"),
        axis.text.x=element_text(colour="black",size=10,angle=90,family="serif"),
        axis.text.y=element_text(colour="black",size=10,family="serif"),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        panel.border=element_rect(),
        panel.background=element_blank(),
        legend.position="bottom",
        legend.direction="horizontal",
        legend.justification="center",
        legend.key=element_rect(fill="white",colour="white"),
        legend.text=element_text(size=12,colour="black",family="serif"),
        legend.key.size=unit(1,"cm"),
        legend.key.height=unit(0.8,"line"),
        legend.margin=margin(t=0,r=0,b=0,l=0,unit="cm"),
        legend.title=element_blank(),
        plot.title=element_text(hjust=0.50,size=16,face="bold",family="serif"),
        plot.subtitle=element_text(hjust=0.50,size=14,face="italic",family="serif"))+
  scale_y_continuous(name="Número de inscritos (por 1e+5)",
                     breaks=c(seq(from=0,
                                  to=25,
                                  by=5)),
                     limits=c(0,25))+
  scale_x_continuous(name="Dias da abertura",
                     breaks=c(seq(from=0,
                                  to=max(Canal.1$dias)+400,
                                  by=50)),
                     limits=c(0,max(Canal.1$dias)+400))+
  geom_text(position="identity",
            aes(x = 1150 , y = fit_logit_C1$par[1]+0.6, label = "L = 24.20"))

#------------------------------------------------------------------------
# Curva ajustada: Real x Predito (Canal-2)
g2<-ggplot(Canal.2, aes(x=dias, y=fcum.insc)) + geom_step(linetype=1, lwd=0.8)+
  geom_vline(data=Canal.2,aes(xintercept=which.max(Canal.2$dias)),
             colour="blue", linetype = "dashed")+
  geom_line(data=DTpred_C2,aes(x=dias,y=pred_logit), colour="red",lwd=0.6,lty=1) +
  geom_hline(yintercept=fit_logit_C2$par[1],colour="green",lwd=0.5,lty="solid")+
  ggtitle(label="Curva ajustada x Curva real",subtitle="(Inscritos no Canal-2)")+
  theme_bw() +
  theme(axis.line.x=element_line(size=0.5,colour="black"),
        axis.line.y=element_line(size=0.5,colour="black"),
        axis.line=element_line(size=1,colour="black"),
        axis.text.x=element_text(colour="black",size=10,angle=90,family="serif"),
        axis.text.y=element_text(colour="black",size=10,family="serif"),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        panel.border=element_rect(),
        panel.background=element_blank(),
        legend.position="bottom",
        legend.direction="horizontal",
        legend.justification="center",
        legend.key=element_rect(fill="white",colour="white"),
        legend.text=element_text(size=12,colour="black",family="serif"),
        legend.key.size=unit(1,"cm"),
        legend.key.height=unit(0.8,"line"),
        legend.margin=margin(t=0,r=0,b=0,l=0,unit="cm"),
        legend.title=element_blank(),
        plot.title=element_text(hjust=0.50,size=16,face="bold",family="serif"),
        plot.subtitle=element_text(hjust=0.50,size=14,face="italic",family="serif"))+
  scale_y_continuous(name="Número de inscritos (por 1e+5)",
                     breaks=c(seq(from=0,
                                  to=50,
                                  by=5)),
                     limits=c(0,50))+
  scale_x_continuous(name="Dias da abertura",
                     breaks=c(seq(from=0,
                                  to=max(Canal.2$dias)+400,
                                  by=50)),
                     limits=c(0,max(Canal.2$dias)+400))+
  geom_text(position="identity",
            aes(x = 950 , y = fit_logit_C2$par[1]+1, label = "L = 46.72"))

plot_grid(g1,g2 , labels = "", nrow = 1, align = 'h', label_fontfamily = "serif")


