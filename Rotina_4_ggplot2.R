#Instalar pacotes
#install.packages("readxl") #pacote abrir excel
install.packages("ExpDes.pt") # pacote anova
#install.packages("ggplot2") # pacote grafico
#install.packages("tidyverse") #pacote manipulação dos dados

#Abrir pacotes
library("readxl") #abrir excel
library("ExpDes.pt") #anova
library("ggplot2") #grafico
library("tidyverse") #pacote manipulação

#ler dados via drobox
dados <- read.table("https://www.dropbox.com/s/jjyo8dhyy0qt3ft/BanzattoQd3.2.1.txt?dl=1") 

#teste estatístico
an=with(dados,dic(trat,pulgoes,
                    quali = TRUE, 
                    mcomp = "tukey",
                    hvar = "bartlett",
                    sigT = 0.05, sigF = 0.05))

#Grafico reposta

theme_set(
  theme_bw() +
    theme(legend.background = element_rect( colour ="black",size=0.5, linetype="solid")) +
    theme(legend.position = "right")
)
x11()#abrir uma janela suspensa
ggplot(dados , aes(x=trat, y=pulgoes)) +
  geom_boxplot(size=0.65,shape=19, fill = "grey90")+ #grafico boxplot
  stat_summary(fun.y=mean, geom="point",shape=1,size=1) + #colocar valor medio
  guides(fill=FALSE)+ #nao aparecer legenda
  scale_fill_brewer(palette = 1, type = "qual") + #escala de cor
  ylab (expression(paste('N. pulgões')))+
  xlab(expression(paste('Tratamentos')))+
  geom_hline(aes(yintercept=865.4), data = dados, linetype="dashed", colour="#BB0000", size=0.5) +
  theme(legend.position="top") +
  annotate("text", label="b", x=1, y=1650, size = 5)  +
  annotate("text", label="c", x=2, y=1650, size = 5)  +
  annotate("text", label="c", x=3, y=1650, size = 5)  +
  annotate("text", label="c", x=4, y=1650, size = 5)  +
  annotate("text", label="a", x=5, y=1650, size = 5)  +
    theme(legend.position="none") +
  labs(caption = "Médias seguidas de mesma letra indicam diferença nula a 5%")
#crt + shift + h
  ggsave("pulgoes.jpg", width=200, height=100, units = "mm", dpi=300)

summary(dados)

#grafico de coluna

    erro = summarise(group_by(dados, trat),
                     avg = mean(pulgoes), sd = sd(pulgoes))
    theme_set(
      theme_bw() +
        theme(legend.background = element_rect( colour ="black",size=0.5, linetype="solid")) +
        theme(legend.position = "right")
    )
    
    ggplot(erro, aes(trat, avg, fill=trat))+
      geom_bar(stat="identity")+
      geom_errorbar(aes(ymin=avg-sd, ymax =avg+sd), with=0.1, col="black") +
      stat_summary(fun.y=mean, geom="point",shape=1,size=1) +
      guides(fill=FALSE)+
      scale_fill_brewer(palette = 1, type = "qual") +
      ylab (expression(paste('Numero de pulgões')))+
      xlab(expression(paste('Cultivares')))+
      ylim(0, 3000) +
      geom_hline(aes(yintercept=865.4), data = dados, linetype="dashed", colour="#BB0000", size=0.5) +
      theme(legend.position="top") +
      annotate("text", label="b", x=1, y=1650, size = 5)  +
      annotate("text", label="c", x=2, y=1650, size = 5) +
      annotate("text", label="c", x=3, y=1650, size = 5)  +
      annotate("text", label="c", x=4, y=1650, size = 5)  +
      annotate("text", label="a", x=5, y=1650, size = 5)  +
      theme(legend.position="none") +
      labs(caption = "Médias seguidas de mesma letra indicam diferença nula a 5%") 
    
    #crtl+shit+h
    ggsave("pulgoes_coluna.jpg", width=180, height=90, units = "mm", dpi=300)
    
    summary(dados) #resumo estastico
















