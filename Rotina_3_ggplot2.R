#Efeito do Promalin sobre Frutos de Macieira

#Resultados de um experimento instalado na Fazenda 
#Chapadão, no município de Angatuba - SP. 
#O delineamento experimental foi o de blocos 
#casualizados, sendo as parcelas constituídas 
#de 4 plantas espaçadas de 6 x 7 metros, com 12 anos 
#de idade na época da instalação do experimento.

#Baixar dados
dados <- read.table("https://www.dropbox.com/s/9woiye3ce9twp78/BanzattoQd4.5.2.txt?dl=1")

#Verificar estrutura dos dados:
str(dados)

#Transformação categorica:
dados$promalin = as.factor(dados$promalin)
dados$bloco= as.factor(dados$bloco)

str(dados)

#Estatistísca descritiva:
summary(dados)

#Ativar o pacote ggplot:
library(ggplot2) #pacote para fazer grafico
library(tidyverse) #conjunto de pacotes
library(gridExtra) #pacote para unir graficos
#install.packages("gridExtra")

#Fazer o gráfico:
ggplot(dados,aes(x=promalin ,y=peso, fill=promalin)) + 
  geom_boxplot(size=0.55,shape=19, colour="black") + 
  theme(legend.position="top")  

#Inserindo legenda nos eixos:
ggplot(dados,aes(x=promalin,y=peso, fill=promalin)) + 
  geom_boxplot(size=0.55,shape=19, colour="black") + 
  theme(legend.position="top") + 
  xlab("Tratamentos") +  
  ylab("Peso médio dos frutos (g)")  
  
#Inserindo legenda nos eixos:
ggplot(dados,aes(x=promalin ,y=peso, fill=promalin)) + 
  geom_boxplot(size=0.55,shape=19, colour="black") + 
  theme(legend.position="top") + 
  stat_summary(fun.y=mean, geom="point",shape=1,size=2) + 
  xlab("Tratamentos") +  
  ylab("Peso médio dos frutos (g)")  +
  theme(panel.grid.minor = element_line(colour = "blue", linetype = "dotted"))
  
#Inserindo tema_bw preto e branco:
ggplot(dados,aes(x=promalin ,y=peso, fill=promalin)) + 
  geom_boxplot(size=0.55,shape=19, colour="black") + 
  stat_summary(fun.y=mean, geom="point",shape=1,size=2) + 
  xlab("Tratamentos") +  
  ylab("Peso médio dos frutos (g)")  +
  theme(legend.position="top") +
  theme_bw()

#Mudando escala do eixo y:
ggplot(dados,aes(x=promalin ,y=peso, fill=promalin)) + 
  geom_boxplot(size=0.55,shape=19, colour="black") + 
  stat_summary(fun.y=mean, geom="point",shape=1,size=2) + 
  xlab("Tratamentos") +  
  ylab("Peso médio dos frutos (g)")  +
  theme_bw() +
  theme(legend.position="top") +
  scale_y_continuous(breaks=seq(0, 180, 5)) +
  theme( axis.text.x  = element_text(angle=0, vjust=0, size=10)) 

## Construindo uma nova data.frame com a media e desvio
dados_res = summarise(group_by(dados, promalin),
                 media = mean(peso), desvio = sd(peso))

#grafico de barra:
ggplot(dados_res,aes(x=promalin ,y=media, fill=promalin)) + 
  geom_col()+
  xlab("Tratamentos") +  
  ylab("Peso médio dos frutos (g)")  +
  theme_classic()+
  theme(legend.position="top") +
  scale_y_continuous(breaks=seq(0, 160, 10)) +
  theme( axis.text.x  = element_text(angle=0, vjust=-2, size=10)) +
geom_text(label=paste(round(dados_res$media,1),c("c","b","d","a", "a")), vjust=-2)+
geom_errorbar(aes(ymax=media+desvio,ymin=media-desvio), width=0.25)
  
#juntar todos graficos

a = ggplot(dados_res,aes(x=promalin ,y=media, fill=promalin)) + 
  geom_col()+
  xlab("Tratamentos") +  
  ylab("Peso médio dos frutos (g)")  +
  theme_classic()+
  theme(legend.position="top") +
  scale_y_continuous(breaks=seq(0, 160, 10)) +
  theme( axis.text.x  = element_text(angle=0, vjust=-1, size=10)) +
  geom_text(label=paste(round(dados_res$media,1),c("c","b","d","a")), vjust=-1.5)+
  geom_errorbar(aes(ymax=media+desvio,ymin=media-desvio), width=0.25)+
  scale_fill_manual(values = c("#9932cc", "#9d60b7", "#9c84a1", "#94a48a", "#84c270"))
a

b = ggplot(dados,aes(x=promalin ,y=peso, fill=promalin)) + 
  geom_boxplot(size=0.55,shape=19, colour="black") + 
  stat_summary(fun.y=mean, geom="point",shape=1,size=2) + 
  xlab("Tratamentos") +  
  ylab("Peso médio dos frutos (g)")  +
  theme_bw() +
  theme(legend.position="top") +
  scale_y_continuous(breaks=seq(0, 180, 5)) 
b
c  = ggplot(dados_res,aes(x=promalin ,y=media, fill=promalin)) + 
  geom_col()+
  xlab("Tratamentos") +  
  ylab("Peso médio dos frutos (g)")  +
  theme_classic()+
  theme(legend.position="top") +
  scale_y_continuous(breaks=seq(0, 160, 10)) +
  geom_text(label=paste(round(dados_res$media,1),c("c","b","d","a")), vjust=-1.5)+
  geom_errorbar(aes(ymax=media+desvio,ymin=media-desvio), width=0.25)+
  scale_fill_manual(values = c("#9932cc", "#9d60b7", "#9c84a1", "#94a48a", "#84c270")) +
  coord_flip() 
c 

#Salvar
g = grid.arrange(a, b, c, ncol=2)

#Escolher local para salvar
#crtl + shift + h

ggsave("experimento.png", g, width=400, height=400, units = "mm", dpi=600)
  

setwd("C:/Users/User/Downloads")
  
