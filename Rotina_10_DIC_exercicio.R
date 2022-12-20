#DIC

#Abrir pacotes####
library(readxl) #abrir excel
library(ExpDes.pt) #anova
library(ggplot2) #grafico
library(tidyverse) #pacote manipulação
library(agricolae) #anova
library(gridExtra) #grafico
library(grid) #grafico
library(knitr) #gerar relatorio
library(lmtest) #teste 

#____________________________________________________________________

#Dados de um experimento visando controle de pulgão (Aphis gossypii Glover)
#em cultura de pepino, instalado em delineamento inteiramente casualizado 
#com 6 repetições. A resposta observada foi o número de pulgões após a 
#aplicação de produtos indicados para seu controle. 

#ler dados via drobox
dados <- read.table("https://www.dropbox.com/s/jjyo8dhyy0qt3ft/BanzattoQd3.2.1.txt?dl=1") 
head(dados)

#____________________________________________________________________
#Croqui DIC####
croqui=function(trat,r){
  sort=design.crd(trat,r,serie=0)
  sort$book[,3]=as.factor(matrix(sort$book[,3],r,,T))
  ncol=r
  gs <- lapply(sort$book[,3], function(ii)
    grobTree(rectGrob(gp=gpar(fill=ii, alpha=0.5)),textGrob(ii)))
  grid.arrange(grobs=gs, ncol=ncol)}

#Vetor de tratamentos
trat=c("Testemunha","Azinfos etilico","Supracid 40CE dose 1 ","Supracid 40CE dose 2",
       "Diazinon 60CE" )

#Usando a função
croqui(trat,r=6)
#____________________________________________________________________
#Analise descritiva geral####

  Media=mean(dados$pulgoes)
  Desvio=sd(dados$pulgoes)
  Variancia=var(dados$pulgoes)
  Maximo=max(dados$pulgoes)
  Minimo=min(dados$pulgoes)
  Mediana=median(dados$pulgoes)
  
  descritiva=cbind(Media,
                 Desvio, 
                 Variancia, 
                 Maximo, 
                 Minimo, 
                 Mediana)
  kable(descritiva)
  
  
  #teste estatístico
  an=with(dados,dic(trat,pulgoes,
                    quali = TRUE, 
                    mcomp = "tukey",
                    hvar = "bartlett",
                    sigT = 0.05, sigF = 0.05))

#Analise descritiva por tratamento

  Media=tapply(dados$pulgoes,dados$trat, mean)
  Desvio=tapply(dados$pulgoes,dados$trat,sd)
  Variancia=tapply(dados$pulgoes,dados$trat, var)
  Maximo=tapply(dados$pulgoes,dados$trat,max)
  Minimo=tapply(dados$pulgoes,dados$trat, min)
  Mediana=tapply(dados$pulgoes,dados$trat,median)
  
  descritiva=cbind(Media,
                   Desvio, 
                   Variancia, 
                   Maximo, 
                   Minimo, 
                   Mediana)
  kable(descritiva, 2)
#____________________________________________________________________
#Pressuposições da Análise####
#Anova#
m0 <- lm (dados$pulgoes ~ dados$trat)

#Normalidade dos erros
shapiro.test(m0$res)

  #Como p-valor calculado (p= 0.006742) é MENOR que o nível de significância 
  #adotado ( α = 0,05), rejeita H0. 
  #Logo, os erros NÃO seguem distribuição normal.

#Homogeneidade de variâncias
bartlett.test(m0$res, dados$trat)

  #Como p-valor calculado (p= 5.942e-06) é MENOR que o nível de significância 
  #adotado ( α = 0,05), rejeita H0. 
  #Logo, as variâncias NAO são homogêneas.

#Independências dos erros
ind=dwtest(m0)
ind

  #Como p-valor calculado (p=0.9943) é maior que o nível de significância
  #adotado (α=0,05), não se rejeita H0. Logo, os erros são independentes.

  plot(m0$res, col="blue",
     las=1, pch=16,
     ylab="Residuos brutos")
  abline(h=0, col="red", lwd=2)

  #A Figura apresenta o gráfico dos resíduos brutos. 
  #Percebe-se que os resíduos estão distribuídos de forma totalmente 
  #aleatório, evidenciando a independência dos erros.

#____________________________________________________________________
#Transformação de dados#######  
 
  require(MASS) 

  #Usando o comando boxcox e conferindo visualmente um 
  #valor aproximado de λ
    boxcox((aov(dados$pulgoes+0.000001~dados$trat)))
  
  #Descobrindo o valor exato de  
    bc=boxcox(aov(dados$pulgoes+0.000001~dados$trat))
    bc$x[which.max(bc$y)]
  
#____________________________________________________________________
#13.12 Dados transformados#######     
    #Anova#
    m_trans <- lm (dados$pulgoes^0.18 ~ dados$trat)
    
    #Normalidade dos erros
    shapiro.test(m_trans$res)
    
    #Como p-valor calculado (p= 0.5652) é MAIOR que o nível de significância 
    #adotado ( α = 0,05), ACEITA H0. 
    #Logo, os erros seguem distribuição normal.
    
    #Homogeneidade de variâncias
    bartlett.test(m_trans$res, dados$trat)
    
    #Como p-valor calculado (p= 0.5868) é MAIOR que o nível de significância 
    #adotado ( α = 0,05), ACEITA H0. 
    #Logo, as variâncias são homogêneas.
    
    #Independências dos erros
    ind=dwtest(m_trans)
    ind
    
    #Como p-valor calculado (p=0.9544) é maior que o nível de significância
    #adotado (α=0,05), não se rejeita H0. Logo, os erros são independentes. 
  
#____________________________________________________________________
#Análise de variância - Ajuste com a variável transformada####    
    
    an=with(dados,dic(trat,pulgoes^0.18,   #TRANSFORMAÇÃO
                      quali = TRUE, 
                      mcomp = "tukey",
                      hvar = "bartlett",
                      sigT = 0.05, sigF = 0.05))
 
#____________________________________________________________________
##Grafico reposta box plot####
  
  theme_set(
    theme_bw() +
      theme(legend.background = element_rect( colour ="black",size=0.5, linetype="solid")) +
      theme(legend.position = "right")
  )
  #x11()#abrir uma janela suspensa
  ggplot(dados , aes(x=trat, y=pulgoes)) +
    geom_boxplot(size=0.65,shape=19, fill = "grey90")+ #grafico boxplot
    stat_summary(fun.y=mean, geom="point",shape=1,size=1) + #colocar valor medio
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
    
  #crt + shift + h #ESCOLHER A PASTA
    ggsave("pulgoes_box_plot.jpg", width=200, height=100, units = "mm", dpi=300)
  
  
##grafico de coluna####
  
  dados_2 = summarise(group_by(dados, trat),
                      media = mean(pulgoes), sd = sd(pulgoes))
    
  theme_set(
    theme_bw() +
      theme(legend.background = element_rect( colour ="black",size=0.5, linetype="solid")) +
      theme(legend.position = "right")
  )
  
  ggplot(dados_2, aes(trat, media, fill=trat))+
    geom_bar(stat="identity")+
    geom_errorbar(aes(ymin=media-sd, ymax =media+sd), with=0.1, col="black") +
    stat_summary(fun.y=mean, geom="point",shape=1,size=1) +
    guides(fill=FALSE)+
    scale_fill_brewer(palette = 1, type = "qual") +
    ylab (expression(paste('Numero de pulgões')))+
    xlab(expression(paste('Tratamentos')))+
    ylim(0, 3000) +
    geom_hline(aes(yintercept=865.4), data = dados, linetype="dashed", colour="#BB0000", size=0.5) +
    theme(legend.position="top") +
    annotate("text", label="b", x=1, y=1650, size = 5)  +
    annotate("text", label="c", x=2, y=1650, size = 5)  +
    annotate("text", label="c", x=3, y=1650, size = 5)  +
    annotate("text", label="c", x=4, y=1650, size = 5)  +
    annotate("text", label="a", x=5, y=1650, size = 5)  +
    theme(legend.position="none") +
    labs(caption = "Médias seguidas de mesma letra indicam diferença nula a 5%")
  
  summary(dados) #verficar o valor medio e inserir na linha geom_hline
  
  #crtl+shit+h #ESCOLHER A PASTA
  ggsave("pulgoes_coluna.jpg", width=180, height=90, units = "mm", dpi=300)

  
##grafico de coluna####
  ggplot(dados_2, aes(x=trat,y=media))+
    geom_col(fill=c(1,2,3,4,5),col="black")+
    geom_text(label=paste(round(dados_2$media,1),c("b","c","c","c","a")), vjust=-1)+
    theme_bw()+
    theme_classic()+
    ylab("Numero de pulgões")+
    xlab(" ")+
    geom_errorbar(aes(ymin=media-sd, ymax =media+sd), width=0.25) # Width é a largura da barra
  
  #crtl+shit+h #ESCOLHER A PASTA
  ggsave("pulgoes_coluna_dois.jpg", width=180, height=90, units = "mm", dpi=300)
  #____________________________________________________________________
