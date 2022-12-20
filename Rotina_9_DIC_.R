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
install.packages("grid")
#____________________________________________________________________

#Num experimento inteiramente casualizado, de competição da cultivares
#de mandioca, realizado numa área perfeitamente homogênea quanto às condições experimentais,
#foram utilizados 5 cultivares e 5 repetições. Os cultivares utilizados foram:
#A-IAC 5
#D-IRACEMA
#B-IAC 7
#E- MANTIQUEIRA

#ler dados via drobox
dados <- read.table("https://www.dropbox.com/s/yv5clm6qljurzbw/BanzattoQd3.4.1.txt?dl=1") 
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
trat=c("IAC 5","IRACEMA","IAC 7","IAC 11","MANTIQUEIRA")

#Usando a função
croqui(trat,r=5)
#____________________________________________________________________
#Analise descritiva geral####

  Media=mean(dados$prod)
  Desvio=sd(dados$prod)
  Variancia=var(dados$prod)
  Maximo=max(dados$prod)
  Minimo=min(dados$prod)
  Mediana=median(dados$prod)
  
  descritiva=cbind(Media,
                 Desvio, 
                 Variancia, 
                 Maximo, 
                 Minimo, 
                 Mediana)
  kable(descritiva)
  
#Analise descritiva por tratamento

  Media=tapply(dados$prod,dados$cult, mean)
  Desvio=tapply(dados$prod,dados$cult,sd)
  Variancia=tapply(dados$prod,dados$cult, var)
  Maximo=tapply(dados$prod,dados$cult,max)
  Minimo=tapply(dados$prod,dados$cult, min)
  Mediana=tapply(dados$prod,dados$cult,median)
  
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
m0 <- lm (dados$prod ~ dados$cult)

#Normalidade dos erros
shapiro.test(m0$res)

  #Como p-valor calculado (p= 0.2156) é MAIOR que o nível de significância 
  #adotado ( α = 0,05), ACEITA H0. 
  #Logo, os erros  seguem distribuição normal.

#Homogeneidade de variâncias
bartlett.test(m0$res, dados$cult)

  #Como p-valor calculado (p= 0.1116) é MAIOR que o nível de significância 
  #adotado ( α = 0,05), ACEITA H0. 
  #Logo, as variâncias são homogêneas.

#Independências dos erros
ind=dwtest(m0)
ind

  #Como p-valor calculado (p=0.6881) é maior que o nível de significância
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
    boxcox((aov(dados$prod+0.000001~dados$cult)))
  
  #Descobrindo o valor exato de  
    bc=boxcox(aov(dados$prod+0.000001~dados$cult))
    bc$x[which.max(bc$y)]
  
#____________________________________________________________________
#Dados transformados#######     
    #Anova#
    m_trans <- lm (dados$prod^1.07 ~ dados$cult)
    
    #Normalidade dos erros
    shapiro.test(m_trans$res)
    
    #Como p-valor calculado (p= 0.1996) é MAIOR que o nível de significância 
    #adotado ( α = 0,05), ACEITA H0. 
    #Logo, os erros seguem distribuição normal.
    
    #Homogeneidade de variâncias
    bartlett.test(m_trans$res, dados$cult)
    
    #Como p-valor calculado (p= 0.1134) é MAIOR que o nível de significância 
    #adotado ( α = 0,05), ACEITA H0. 
    #Logo, as variâncias são homogêneas.
    
    #Independências dos erros
    ind=dwtest(m_trans)
    ind
    
    #Como p-valor calculado (p=0.6679) é maior que o nível de significância
    #adotado (α=0,05), não se rejeita H0. Logo, os erros são independentes. 
  
#____________________________________________________________________
#Análise de variância    
    
    an=with(dados,dic(cult,prod,   #NAO PRECISA DE TRANSFORMAÇÃO
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
  ggplot(dados , aes(x=cult, y=prod)) +
    geom_boxplot(size=0.65,shape=19, fill = "grey90")+ #grafico boxplot
    stat_summary(fun.y=mean, geom="point",shape=1,size=1) + #colocar valor medio
    scale_fill_brewer(palette = 1, type = "qual") + #escala de cor
    ylab (expression(paste('Produtividade (t/ha)')))+
    xlab(expression(paste('Cultivares')))+
    geom_hline(aes(yintercept=34.25), data = dados, linetype="dashed", colour="#BB0000", size=0.5) +
    theme(legend.position="top") +
    annotate("text", label="c", x=1, y=35, size = 5)  +
    annotate("text", label="c", x=2, y=35, size = 5)  +
    annotate("text", label="c", x=3, y=35, size = 5)  +
    annotate("text", label="b", x=4, y=35, size = 5)  +
    annotate("text", label="a", x=5, y=35, size = 5)  +
    theme(legend.position="none") +
    labs(caption = "Médias seguidas de mesma letra indicam diferença nula a 5%")
    
  #crt + shift + h #ESCOLHER A PASTA
    ggsave("prod_box_plot.jpg", width=200, height=100, units = "mm", dpi=600)
    
    summary(dados) #verficar o valor medio e inserir na linha geom_hline
    
  
##grafico de coluna####
  
  dados_2 = summarise(group_by(dados, cult),
                      media = mean(prod), sd = sd(prod))
    
  theme_set(
    theme_bw() +
      theme(legend.background = element_rect( colour ="black",size=0.5, linetype="solid")) +
      theme(legend.position = "right")
  )
  
  ggplot(dados_2, aes(cult, media, fill=cult))+
    geom_bar(stat="identity")+
    geom_errorbar(aes(ymin=media-sd, ymax =media+sd), with=0.1, col="black") +
    stat_summary(fun.y=mean, geom="point",shape=1,size=1) +
    guides(fill=FALSE)+
    scale_fill_brewer(palette = 5, type = "qual") +
    ylab("Produtividade (t/ha)")+
    xlab("Cultivares")+
    ylim(0, 60) +
    geom_hline(aes(yintercept=34.25), data = dados, linetype="dashed", colour="#BB0000", size=0.5) +
    theme(legend.position="top") +
    annotate("text", label="c", x=1, y=60, size = 5)  +
    annotate("text", label="c", x=2, y=60, size = 5)  +
    annotate("text", label="c", x=3, y=60, size = 5)  +
    annotate("text", label="b", x=4, y=60, size = 5)  +
    annotate("text", label="a", x=5, y=60, size = 5)  +
    theme(legend.position="none") +
    labs(caption = "Médias seguidas de mesma letra indicam diferença nula a 5%")
  
  summary(dados) #verificar o valor medio e inserir na linha geom_hline
  
  #crtl+shit+h #ESCOLHER A PASTA
  ggsave("prod_coluna.jpg", width=180, height=90, units = "mm", dpi=300)

  
##grafico de coluna####
  ggplot(dados_2, aes(x=cult,y=media))+
    geom_col(fill=c(8,2,3,4,5),col="black")+
    geom_text(label=paste(round(dados_2$media,1),c("b","c","c","c","a")), vjust=-3.3)+
    theme_bw()+
    ylim(0, 60) +
    ylab("Produtividade t/ha")+
    xlab("Cultivares")+
    geom_errorbar(aes(ymin=media-sd, ymax =media+sd), width=0.25) # Width é a largura da barra
  
  #crtl+shit+h #ESCOLHER A PASTA
  ggsave("prod_coluna_dois.jpg", width=180, height=90, units = "mm", dpi=300)
  
#____________________________________________________________________
