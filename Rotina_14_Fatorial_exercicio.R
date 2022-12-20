#Fatorial DBC 2x2x2

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
#EXEMPLO 5.3 - Vamos considerar os dados de um experimento para a cultura do cafeeiro,
#no delineamento em blocos casualizados com 6 repetições, no esquema fatorial 2 x 2 x 2, com os
#fatores: Nitrogênio (N), Fósforo (P) e Potássio (K), cada um deles nos níveis O (ausência) e 1
#(presença).

#ler dados via drobox
dados <- read.table("https://www.dropbox.com/s/jjdor9q4gako60w/BanzattoQd5.3.1.txt?dl=1", header=T) 
head(dados)

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
  
#Analise descritiva por por fator 1

  Media=tapply(dados$prod,dados$N  , mean)
  Desvio=tapply(dados$prod,dados$N  ,sd)
  Variancia=tapply(dados$prod,dados$N  , var)
  Maximo=tapply(dados$prod,dados$N  ,max)
  Minimo=tapply(dados$prod,dados$N  , min)
  Mediana=tapply(dados$prod,dados$N  ,median)
  
  descritiva=cbind(Media,
                   Desvio, 
                   Variancia, 
                   Maximo, 
                   Minimo, 
                   Mediana)
  kable(descritiva, 2)
  
  
  #Analise descritiva por  fator 2
  
  Media=tapply(dados$prod,dados$P  , mean)
  Desvio=tapply(dados$prod,dados$P  ,sd)
  Variancia=tapply(dados$prod,dados$P  , var)
  Maximo=tapply(dados$prod,dados$P  ,max)
  Minimo=tapply(dados$prod,dados$P  , min)
  Mediana=tapply(dados$prod,dados$P  ,median)
  
  descritiva=cbind(Media,
                   Desvio, 
                   Variancia, 
                   Maximo, 
                   Minimo, 
                   Mediana)
  kable(descritiva, 2)
  
  
  #Analise descritiva por fator 3
  
  Media=tapply(dados$prod,dados$K  , mean)
  Desvio=tapply(dados$prod,dados$K  ,sd)
  Variancia=tapply(dados$prod,dados$K  , var)
  Maximo=tapply(dados$prod,dados$K  ,max)
  Minimo=tapply(dados$prod,dados$K  , min)
  Mediana=tapply(dados$prod,dados$K  ,median)
  
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
m0 <- lm (dados$prod ~ dados$N * dados$P * dados$K  )

#Normalidade dos erros
shapiro.test(m0$res)

  #Como p-valor calculado (p= 0.4068) é MAIOR que o nível de significância 
  #adotado ( α = 0,05), ACEITA H0. 
  #Logo, os erros  seguem distribuição normal.

#Homogeneidade de variâncias
bartlett.test(m0$res ~ interaction (P, N, K), data = dados)
  
#Como p-valor calculado (p= 0.2249) é MAIOR que o nível de significância 
  #adotado ( α = 0,05), ACEITA H0. 
  #Logo, as variâncias são homogêneas.

#Independências dos erros
ind=dwtest(m0)
ind

  #Como p-valor calculado (p=0.726) é maior que o nível de significância
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
    boxcox((aov(dados$prod+0.000001~dados$N*dados$P*dados$K)))
  
  #Descobrindo o valor exato de  
    bc=boxcox((aov(dados$prod+0.000001~dados$N*dados$P*dados$K)))
    bc$x[which.max(bc$y)]
  
#____________________________________________________________________
#Dados transformados####### Não houve necessidade    
    #Anova#
    m_trans <- lm (dados$prod ~ dados$N*dados$P*dados$K)
    
    #Normalidade dos erros
    shapiro.test(m_trans$res)
    
    #Homogeneidade de variâncias
    bartlett.test(m0$res ~ interaction (N, P, K), data = dados)
    
    #Independências dos erros
    ind=dwtest(m_trans)
    ind
    
#____________________________________________________________________
#Análise de variância    
    
    an=with(dados,fat3.dbc (N, P, K, bloco, prod,   #NAO PRECISA DE TRANSFORMAÇÃO
                      quali = c(TRUE, TRUE, TRUE), 
                      mcomp = "tukey",
                      fac.names = c("N", "P", "K"),
                      sigT = 0.05, sigF = 0.05))
 
#Conclusões:
#O teste foi significativo (P<O,O 1) para Nitrogênio (N) e Potássio (K). Mas, como a Interação
#N x K também foi significativa (P<O,O 1 ), não podemos tirar conclusões para os efeitos principais
#de N e K, pois os efeitos de Nitrogênio e Potássio são dependentes.
#Então, devemos proceder ao desdobramento da Interação N x K, o que pode ser feito de duas maneiras:
#a) Para estudar o efeito do N, na ausência e na presença de K;
#b) Para estudar o efeito de K, na ausência e na presença de N.

#Conclusões:
#1 a) O Nitrogênio não possui efeito significativo (P > 0,05), quando na ausência de Potássio.
#2 a) O Nitrogênio possui efeito significativo (P <O,O 1 ) , quando na presença de Potássio, ou
#seja, a presença do Potássio estimula o efeito do Nitrogênio.    
#____________________________________________________________________

    dados2 = dados %>% 
      group_by(N, K) %>% summarise("media" = round(mean(prod),1), 
                                 "desvio" = round(sd(prod),1))
#____________________________________________________________________
    ##grafico de coluna####
    
    labels <- c('0' = "Ausente", "1" = "Presente")

    ggplot(dados2, aes(factor(N), media, fill=N))+
      geom_bar(stat="identity") +
            facet_wrap(~ K, nrow=1, labeller=labeller(K = labels))+
      guides(fill=FALSE) +
      ggtitle("Potássio") +
      ylab (expression(paste('Produções de café coco, em kg/ha'))) +
      xlab(expression(paste('Nitrogênio'))) +
      scale_x_discrete(breaks=c("0", "1"),
                       labels=c("Ausente","Presente")) +
      theme_bw()
    



