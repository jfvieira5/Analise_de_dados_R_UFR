#Fatorial DIC

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
#Vamos considerar os dados de um experimento inteiramente casualizado,
#com 4 repetições, no esquema fatorial 3 x 2, para testar os efeitos de 3 Recipientes ( R 1 , R 2 e R 3 )
#para produção de mudas e 2 Espécies de eucaliptos ( E 1 e E 2 ), quanto ao desenvolvimento das mudas.
#Os Recipientes e as Espécies testados foram:
#SPP = saco plástico pequeno
#SPG = saco plástico grande
#Lam = laminado
#E1 = Eucalyptus citriodora
#E2 = Eucalyptus grandis

#ler dados via drobox
dados <- read.table("https://www.dropbox.com/s/sahc5n80rlkcfx4/BanzattoQd5.2.4.txt?dl=1") 
head(dados)

#____________________________________________________________________
#Analise descritiva geral####

  Media=mean(dados$alt)
  Desvio=sd(dados$alt)
  Variancia=var(dados$alt)
  Maximo=max(dados$alt)
  Minimo=min(dados$alt)
  Mediana=median(dados$alt)
  
  descritiva=cbind(Media,
                 Desvio, 
                 Variancia, 
                 Maximo, 
                 Minimo, 
                 Mediana)
  kable(descritiva)
  
#Analise descritiva por fator 1

  Media=tapply(dados$alt,dados$especie  , mean)
  Desvio=tapply(dados$alt,dados$especie  ,sd)
  Variancia=tapply(dados$alt,dados$especie  , var)
  Maximo=tapply(dados$alt,dados$especie  ,max)
  Minimo=tapply(dados$alt,dados$especie  , min)
  Mediana=tapply(dados$alt,dados$especie  ,median)
  
  descritiva=cbind(Media,
                   Desvio, 
                   Variancia, 
                   Maximo, 
                   Minimo, 
                   Mediana)
  kable(descritiva, 2)
  
  
  #Analise descritiva por por fator 2
  
  Media=tapply(dados$alt,dados$recipie  , mean)
  Desvio=tapply(dados$alt,dados$recipie  ,sd)
  Variancia=tapply(dados$alt,dados$recipie  , var)
  Maximo=tapply(dados$alt,dados$recipie  ,max)
  Minimo=tapply(dados$alt,dados$recipie  , min)
  Mediana=tapply(dados$alt,dados$recipie  ,median)
  
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
m0 <- lm (dados$alt ~ dados$especie * dados$recipie  )  #arrrumar os fatores

#Normalidade dos erros
shapiro.test(m0$res)

  #Como p-valor calculado (p= 0.09402) é MAIOR que o nível de significância 
  #adotado ( α = 0,05), ACEITA H0. 
  #Logo, os erros  seguem distribuição normal.

#Homogeneidade de variâncias
bartlett.test(m0$res ~ interaction (recipie, especie), data = dados) #arrrumar os fatores
  
#Como p-valor calculado (p= 0.3845) é MAIOR que o nível de significância 
  #adotado ( α = 0,05), ACEITA H0. 
  #Logo, as variâncias são homogêneas.

#Independências dos erros
ind=dwtest(m0)
ind

  #Como p-valor calculado (p=0.9665) é maior que o nível de significância
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
    boxcox((aov(dados$alt+0.000001~dados$recipie*dados$especie))) #arrumar variavel resposta e os fatores
  
  #Descobrindo o valor exato de  
    bc=boxcox((aov(dados$alt+0.000001~dados$recipie*dados$especie))) #arrumar variavel resposta e os fatores
    bc$x[which.max(bc$y)]
  
#____________________________________________________________________
#Dados transformados####### Não houve necessidade    
    #Anova#
    m_trans <- lm (dados$alt ~ dados$recipie*dados$especie)
    
    #Normalidade dos erros
    shapiro.test(m_trans$res)
    
    #Homogeneidade de variâncias
    bartlett.test(m0$res ~ interaction (recipie, especie), data = dados)
    
    #Independências dos erros
    ind=dwtest(m_trans)
    ind
    
#____________________________________________________________________
#Análise de variância    
    
    an=with(dados,fat2.dic (recipie, especie, alt,   #
                      quali = c(TRUE, TRUE), 
                      mcomp = "duncan",
                      fac.names = c("Recipientes", "Especíes"),
                      sigT = 0.05, sigF = 0.05))
 
#Conclusões:
#a) Quando se utiliza o Recipiente SPP (saco plástico pequeno); não há diferença significativa (P>0,05) no desenvolvimento das mudas das 2 Espécies;
#b) Quando se utiliza o Recipiente SPG (saco plástico grande), há diferença significativa (P<0,01) no desenvolvimento das mudas das 2 Espécies, sendo melhor para a espécie EI (Eucalyptus                                                                              citriodora);
#c) Quando se utiliza o Recipiente lam (laminado), não há diferença significativa (P>0,05) no desenvolvimento das mudas das 2 Espécies.
    
#Conclusões:
#a) Os 3 Recipientes têm efeitos diferentes (P<0,01) sobre o desenvolvimento de mudas da Espécie Eucaliptus citriodora.
#b) Os 3 Recipientes têm efeitos diferentes (P<0,01) sobre o desenvolvimento de mudas da Espécie Eucaliptus grandis.

#Para a Espécie Eucalyptus citriodora, os melhores Recipientes foram: SPP (saco plástico pequeno) e SPG (saco plástico grande), que determinaram desenvolvimento de mudas
#significativamente maiores que Lam (laminado ), sem diferirem entre si.    
    
#Para a Espécie Eucalyptus grandis, o melhor Recipiente foi SPP (saco plástico pequeno),  que determinou desenvolvimento de mudas significativamente maior que SPG (saco plástico grande)
#e que Lam (laminado ). As médias de SPP e Lam (dentro da espéci 2) não diferiram significativamente entre si    

#____________________________________________________________________

##grafico de coluna####
  
  dados_2 = summarise(group_by(dados, recipie  ), #trocar variavel resposta
                      avg = mean(alt), sd = sd(alt)) #trocar o fator
    
##grafico de coluna####
    ggplot(dados_2, aes(recipie, avg, fill=recipie))+
      geom_bar(stat="identity")+
      geom_errorbar(aes(ymin=avg-sd, ymax =avg+sd), with=0.1, col="black") +
            stat_summary(fun.y=mean, geom="point",shape=1,size=1) +
        guides(fill=FALSE)+
        scale_fill_brewer(palette = 8, type = "qual") +
        ylab (expression(paste('Altura de mudas, em cm')))+
        xlab(expression(paste('Recipientes')))+
        ylim(0, 30) +
      geom_hline(aes(yintercept=381.6), data = dados, linetype="dashed", colour="#BB0000", size=0.5) +
          theme(legend.position="none") +
          labs(caption = "a, b - Para cada Espécie, médias de Recipientes seguidas de mesma letra minúscula não
diferem significativamente entre si;
A, B - Para cada Recipiente, médias de Espécies seguidas de mesma letra maiúscula não
diferem significativamente entre si.") +
      facet_wrap(~ especie, scales = "free", nrow=1)+
      theme_bw()
      
  #crtl+shit+h #ESCOLHER A PASTA
  ggsave("alt_coluna_dois.jpg", width=180, height=90, units = "mm", dpi=300)
  
#____________________________________________________________________

