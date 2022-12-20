#DBC

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
#No trabalho "Estudo comparativo entre diferentes métodos de semeadura
#na cultura do mamoeiro", realizado em Jaboticabal- SP, Ruiz (1977) utilizou os tratamentos:
#A - Semeadura direta no campo;
#B - Semeadura em recipientes a pleno sol;
#C - Semeadura em recipientes no ripado.

#ler dados via drobox
dados <- read.table("https://www.dropbox.com/s/40m95attfw2fdh2/BanzattoQd4.7.1.txt?dl=1") 
head(dados)

#____________________________________________________________________
#Croqui DBC####

croqui=function(trat,r){
  sort=design.rcbd(trat,r,serie=0)
  sort$book[,3]=as.factor(matrix(sort$book[,3],r,,T))
  ncol=r
  gs <- lapply(sort$book[,3], function(ii)
    grobTree(rectGrob(gp=gpar(fill=ii, alpha=0.5)),textGrob(ii)))
  grid.arrange(grobs=gs, ncol=ncol)}

#Vetor de tratamentos
trat=c("Direta no campo", "Recip. ao sol", "Recip. ripado")

#Usando a função
croqui(trat,r=4)
#____________________________________________________________________
#Analise descritiva geral####

  Media=mean(dados$altura)
  Desvio=sd(dados$altura)
  Variancia=var(dados$altura)
  Maximo=max(dados$altura)
  Minimo=min(dados$altura)
  Mediana=median(dados$altura)
  
  descritiva=cbind(Media,
                 Desvio, 
                 Variancia, 
                 Maximo, 
                 Minimo, 
                 Mediana)
  kable(descritiva)
  
#Analise descritiva por tratamento

  Media=tapply(dados$altura,dados$semead  , mean)
  Desvio=tapply(dados$altura,dados$semead  ,sd)
  Variancia=tapply(dados$altura,dados$semead  , var)
  Maximo=tapply(dados$altura,dados$semead  ,max)
  Minimo=tapply(dados$altura,dados$semead  , min)
  Mediana=tapply(dados$altura,dados$semead  ,median)
  
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
m0 <- lm (dados$altura ~ dados$semead  )

#Normalidade dos erros
shapiro.test(m0$res)

  #Como p-valor calculado (p= 0.7932) é MAIOR que o nível de significância 
  #adotado ( α = 0,05), ACEITA H0. 
  #Logo, os erros  seguem distribuição normal.

#Homogeneidade de variâncias
bartlett.test(m0$res, dados$semead  )

  #Como p-valor calculado (p= 0.2221) é MAIOR que o nível de significância 
  #adotado ( α = 0,05), ACEITA H0. 
  #Logo, as variâncias são homogêneas.

#Independências dos erros
ind=dwtest(m0)
ind

  #Como p-valor calculado (p=0.08259) é maior que o nível de significância
  #adotado (α=0,05), não se rejeita H0. Logo, os erros são independentes.

  plot(m0$res, col="blue",
     las=1, pch=16,
     ylab="Residuos brutos")
  abline(h=0, col="red", lwd=2)

  #A Figura apresenta o gráfico dos resíduos brutos. 
  #Percebe-se que os resíduos estão distribuídos de forma totalmente 
  #aleatório, evidenciando a independência dos erros.

#____________________________________________________________________
#Transformação de dados#######  Não houve necessidade
 
  require(MASS)

  #Usando o comando boxcox e conferindo visualmente um 
  #valor aproximado de λ
    boxcox((aov(dados$altura+0.000001~dados$semead  )))
  
  #Descobrindo o valor exato de  
    bc=boxcox(aov(dados$altura+0.000001~dados$semead  ))
    bc$x[which.max(bc$y)]
  
#____________________________________________________________________
#Dados transformados####### Não houve necessidade    
    #Anova#
    m_trans <- lm (dados$altura^-0.54 ~ dados$semead  )
    
    #Normalidade dos erros
    shapiro.test(m_trans$res)
    
    #Homogeneidade de variâncias
    bartlett.test(m_trans$res, dados$semead  )
    
    #Independências dos erros
    ind=dwtest(m_trans)
    ind
    
#____________________________________________________________________
#Análise de variância    
    
    an=with(dados,dbc(semead  , bloco, altura,   #NAO PRECISA DE TRANSFORMAÇÃO
                      quali = TRUE, 
                      mcomp = "duncan",
                      sigT = 0.05, sigF = 0.05))
 
#____________________________________________________________________
##Grafico reposta box plot####
  
  theme_set(
    theme_bw() +
      theme(legend.background = element_rect( colour ="black",size=0.5, linetype="solid")) +
      theme(legend.position = "right")
  )
  #x11()#abrir uma janela suspensa
  ggplot(dados , aes(x=semead  , y=altura)) +
    geom_boxplot(size=0.65,shape=19, fill = "grey90")+ #grafico boxplot
    stat_summary(fun.y=mean, geom="point",shape=1,size=1) + #colocar valor medio
    scale_fill_brewer(palette = 1, type = "qual") + #escala de cor
    ylab (expression(paste('Alturas médias  do mamoeiro em cm')))+
    xlab(expression(paste('Métodos de semeadura')))+
    geom_hline(aes(yintercept=74.55), data = dados, linetype="dashed", colour="#BB0000", size=0.5) +
    theme(legend.position="top") +
    annotate("text", label="a", x=1, y=130, size = 5)  +
    annotate("text", label="b", x=2, y=130, size = 5)  +
    annotate("text", label="b", x=3, y=130, size = 5)  +
     theme(legend.position="none") +
    labs(caption = "Médias seguidas de mesma letra indicam diferença nula a 5%")
    
  #crt + shift + h #ESCOLHER A PASTA
    ggsave("altura_box_plot.jpg", width=200, height=100, units = "mm", dpi=300)
    
    summary(dados) #verficar o valor medio e inserir na linha geom_hline
    
  
##grafico de coluna####
  
  dados_2 = summarise(group_by(dados, semead  ),
                      media = mean(altura), sd = sd(altura))
    
  theme_set(
    theme_bw() +
      theme(legend.background = element_rect( colour ="black",size=0.5, linetype="solid")) +
      theme(legend.position = "right")
  )
  
  ggplot(dados_2, aes(semead  , media, fill=semead  ))+
    geom_bar(stat="identity")+
    geom_errorbar(aes(ymin=media-sd, ymax =media+sd), with=0.1, col="black") +
    stat_summary(fun.y=mean, geom="point",shape=1,size=1) +
    guides(fill=FALSE)+
    scale_fill_brewer(palette = 1, type = "qual") +
    ylab (expression(paste('Alturas médias  do mamoeiro em cm')))+
    xlab(expression(paste('Métodos de semeadura')))+
    ylim(0, 150) +
    geom_hline(aes(yintercept=74.55), data = dados, linetype="dashed", colour="#BB0000", size=0.5) +
    theme(legend.position="top") +
    annotate("text", label="a", x=1, y=130, size = 5)  +
    annotate("text", label="b", x=2, y=130, size = 5)  +
    annotate("text", label="b", x=3, y=130, size = 5)  +
    
    theme(legend.position="none") +
    labs(caption = "Médias seguidas de mesma letra indicam diferença nula a 5%")
  
  summary(dados) #verficar o valor medio e inserir na linha geom_hline
  
  #crtl+shit+h #ESCOLHER A PASTA
  ggsave("altura_coluna.jpg", width=180, height=90, units = "mm", dpi=300)

  
##grafico de coluna####
  ggplot(dados_2, aes(x=semead  ,y=media))+
    geom_col(fill=c(8,2,3),col="black")+
    geom_text(label=paste(round(dados_2$media,1),c("b","c","c")), vjust=-3)+
    theme_bw()+
    theme_classic()+
    ylim(0, 200) +
    ylab (expression(paste('Alturas médias  do mamoeiro em cm')))+
    xlab(expression(paste('Métodos de semeadura')))+
    geom_errorbar(aes(ymin=media-sd, ymax =media+sd), width=0.25) # Width é a largura da barra
  
  #crtl+shit+h #ESCOLHER A PASTA
  ggsave("altura_coluna_dois.jpg", width=180, height=90, units = "mm", dpi=300)
  
#____________________________________________________________________
