#Regressao DIC

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
library("ggpmisc") #regressão


#____________________________________________________________________
#No trabalho: "Efeito de doses de gesso na cultura do feijoeiro (Phaseolus vulgaris L.)", Ragazzi
#(1979) utilizou um experimento inteiramente casualizado com 4 repetições, para estudar os efeitos
#de 7 doses de gesso (Tratamentos): O, 50, 100, 150, 200,250 e 300 kg/ha sobre diversas características
#do feijoeiro.
#Para a característica: peso de 1.000 sementes, os resultados obtidos, em gramas,

#ler dados via drobox
dados <- read.table("https://www.dropbox.com/s/r6jz7mrktbgnbnx/BanzattoQd7.2.1.txt?dl=1", header=T) 
head(dados)

#____________________________________________________________________
#Analise descritiva geral####

  Media=mean(dados$peso)
  Desvio=sd(dados$peso)
  Variancia=var(dados$peso)
  Maximo=max(dados$peso)
  Minimo=min(dados$peso)
  Mediana=median(dados$peso)
  
  descritiva=cbind(Media,
                 Desvio, 
                 Variancia, 
                 Maximo, 
                 Minimo, 
                 Mediana)
  kable(descritiva)
  
#Analise descritiva por por fator 1

  Media=tapply(dados$peso,dados$gesso  , mean)
  Desvio=tapply(dados$peso,dados$gesso  ,sd)
  Variancia=tapply(dados$peso,dados$gesso  , var)
  Maximo=tapply(dados$peso,dados$gesso  ,max)
  Minimo=tapply(dados$peso,dados$gesso  , min)
  Mediana=tapply(dados$peso,dados$gesso  ,median)
  
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
m0 <- lm (dados$peso ~ dados$gesso  )

#Normalidade dos erros
shapiro.test(m0$res)

  #Como p-valor calculado (p= 0.8599) é MAIOR que o nível de significância 
  #adotado ( α = 0,05), ACEITA H0. 
  #Logo, os erros  seguem distribuição normal.

#Homogeneidade de variâncias
bartlett.test(m0$res ~ gesso, data = dados)
  
#Como p-valor calculado (p= 0.9359) é MAIOR que o nível de significância 
  #adotado ( α = 0,05), ACEITA H0. 
  #Logo, as variâncias são homogêneas.

  #Como p-valor calculado (p= 0.00469) é menor que o nível de significância
  #adotado (α=0,05), se rejeita H0. Logo, os erros são dependentes.

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
    boxcox((aov(dados$peso+0.000001~dados$gesso)))
  
  #Descobrindo o valor exato de  
    bc=boxcox((aov(dados$peso+0.000001~dados$gesso)))
    bc$x[which.max(bc$y)]
  
#____________________________________________________________________
#Dados transformados####### Não houve necessidade    
    #Anova#
    m_trans <- lm (dados$peso ~ dados$gesso)
    
    #Normalidade dos erros
    shapiro.test(m_trans$res)
    
    #Homogeneidade de variâncias
    bartlett.test(m0$res ~ gesso, data = dados)
    
#____________________________________________________________________
#Análise de variância    
    
    an=with(dados,dic (gesso, peso,   #NAO PRECISA DE TRANSFORMAÇÃO
                      quali = FALSE, #os tratamento é quantitativo
                      mcomp = "tukey",
                      sigT = 0.05, sigF = 0.05))
#Conclusões:
#Verificamos que a regressão linear e a regressão quadrática foram significativas (P<0,01),
#indicando que é possível estabelecer uma relação funcional entre a dose de gesso colocada (X) e o
#peso de 1.000 sementes do feijoeiro (Y).

### Para encontrar o ponto de máximo ou mínimo em equação quadrática, fazer derivada primeira de Y=0
    
    (x=-an$reg$`Coeficientes parabola`[2]/(2*an$reg$`Coeficientes parabola`[3]))

#Então, o máximo da função é dado por:   
    (y=an$reg$`Coeficientes parabola`[1]+an$reg$`Coeficientes parabola`[2]*x+an$reg$`Coeficientes parabola`[3]*x^2)
   
#____________________________________________________________________

dados2 = dados %>% 
  group_by(gesso) %>% summarise("media" = round(mean(peso),1), 
                                 "desvio" = round(sd(peso),1))
#____________________________________________________________________

formula <- y ~ poly(x, 2, raw = T) #quadratica

ggplot(dados2, 
       aes(x=gesso, y=media)) + 
  geom_point(colour="black", size=2, shape=1)+
  geom_smooth(method="lm", se = F, formula = formula, show.legend = T) +
  stat_poly_eq(aes(label =  paste(..eq.label..,..rr.label.., sep = "*\",\"~~")), eq.with.lhs = "italic(hat(y))~`=`~", 
               formula = formula, parse = T, label.y.npc =0.2, size= 3) + 
    labs(title = "Regressão do peso de 1.000 sementes em função das doses de gesso",
       y = "Peso de 1.000 sementes (g)", 
       x = expression(Dose~de~gesso~~(Kg~ha^-1)),
       caption = "Ponto máximo 175 kg/ha produz 164,70 g") +
  ylim(100, 170) +
  theme_classic()+
  theme(axis.title = element_text(size = 12),
        axis.text = element_text(size = 12)) +
  geom_vline(xintercept = x, colour="red", linetype="dotted", size=1)+
  geom_hline(yintercept = y,colour='red', linetype='dotted', size=1)

ggsave("gesso.jpg", width=120, height=90, units = "mm", dpi=300)

