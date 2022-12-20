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
#Vamos considerar os dados de um experimento fatorial 3 x 3, com os fatores: Diâmetro (D) e
#Altura (A) dos tubetes de papel, utilizados na formação de mudas de eucalipto.
#Os Diâmetros utilizados foram: 
#D1 = 3,5 cm; 
#D2 = 5,0 cm e 
#D3 = 6,5 cm e 
#as Alturas foram: A 1 =  10cm 
#em; A 2 = 12 cm e A 3 = 14 cm.
#Os tratamentos foram distribuídos em 3 blocos casualizados.
#Os resultados obtidos para alturas médias (em) das mudas, 75 dias após a semeadura, 

#ler dados via drobox
dados <- read.table("https://www.dropbox.com/s/3n6d52as841a55x/BanzattoQd7.3.1.txt?dl=1", header=T) 
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

Media=tapply(dados$alt,dados$dt  , mean)
Desvio=tapply(dados$alt,dados$dt  ,sd)
Variancia=tapply(dados$alt,dados$dt  , var)
Maximo=tapply(dados$alt,dados$dt  ,max)
Minimo=tapply(dados$alt,dados$dt  , min)
Mediana=tapply(dados$alt,dados$dt  ,median)

descritiva=cbind(Media,
                 Desvio, 
                 Variancia, 
                 Maximo, 
                 Minimo, 
                 Mediana)
kable(descritiva, 2)


#Analise descritiva por por fator 2

Media=tapply(dados$alt,dados$at  , mean)
Desvio=tapply(dados$alt,dados$at  ,sd)
Variancia=tapply(dados$alt,dados$at  , var)
Maximo=tapply(dados$alt,dados$at  ,max)
Minimo=tapply(dados$alt,dados$at  , min)
Mediana=tapply(dados$alt,dados$at  ,median)

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
m0 <- lm (dados$alt ~ dados$dt * dados$at  )

#Normalidade dos erros
shapiro.test(m0$res)

#Como p-valor calculado (p= 0.1901) é MAIOR que o nível de significância 
#adotado ( α = 0,05), ACEITA H0. 
#Logo, os erros  seguem distribuição normal.

#Homogeneidade de variâncias
bartlett.test(m0$res ~ interaction (at, dt), data = dados)

#Como p-valor calculado (p= 0.08507) é MAIOR que o nível de significância 
#adotado ( α = 0,05), ACEITA H0. 
#Logo, as variâncias são homogêneas.

#Independências dos erros
ind=dwtest(m0)
ind

#Como p-valor calculado (p=0.3773) é maior que o nível de significância
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
boxcox((aov(dados$alt+0.000001~dados$at*dados$dt)))

#Descobrindo o valor exato de  
bc=boxcox((aov(dados$alt+0.000001~dados$at*dados$dt)))
bc$x[which.max(bc$y)]

#____________________________________________________________________
#Dados transformados####### Não houve necessidade    
#Anova#
m_trans <- lm (dados$alt ~ dados$at*dados$dt)

#Normalidade dos erros
shapiro.test(m_trans$res)

#Homogeneidade de variâncias
bartlett.test(m0$res ~ interaction (at, dt), data = dados)

#Independências dos erros
ind=dwtest(m_trans)
ind

#____________________________________________________________________
#Análise de variância    

an=with(dados,fat2.dbc (at, dt, bloco, alt,    #NAO PRECISA DE TRANSFORMAÇÃO
               quali = c(FALSE, FALSE), #é qualitativo?
                        mcomp = "duncan",
                        fac.names = c("Alturas", "Diametros"),
                        sigT = 0.05, sigF = 0.05))

#Conclusões:
#Verificamos que a Interação A x D não foi significativa (P~0,05), o que indica que os efeitos
#dos fatores Diâmetro do tubete (D) e Altura do tubete (A) agem de modo independente sobre a
#altura média das mudas.

#____________________________________________________________________

dados2 = dados %>% 
  group_by(dt) %>% summarise("media" = round(mean(alt),1), 
                                 "desvio" = round(sd(alt),1))
#____________________________________________________________________

formula <- y ~ poly(x, 1, raw = T)

a= ggplot(dados2, 
       aes(x=dt, y=media)) + 
  geom_point(colour="black", size=4, shape=1)+
  geom_smooth(method="lm", se = F, formula = formula, show.legend = T) +
  stat_poly_eq(aes(label =  paste(..eq.label..,..rr.label.., sep = "*\",\"~~")), eq.with.lhs = "italic(hat(y))~`=`~", 
               formula = formula, parse = T, label.y.npc =1, size= 5) + 
    labs(title = " ",
       y = "Altura médias das mudas (cm)", 
       x = "Diâmetro dos Tubetes (cm)",
       caption = " ") +
  xlim(3,7) +
  ylim(0, 25) +
  theme_classic()+
  theme(axis.title = element_text(size = 12),
      axis.text = element_text(size = 12)) 

a 
ggsave("FatoA_mudas.jpg",a, width=120, height=90, units = "mm", dpi=300)

#____________________________________________________________________

dados3 = dados %>% 
  group_by(at) %>% summarise("media" = round(mean(alt),1), 
                             "desvio" = round(sd(alt),1))

b = ggplot(dados3, 
       aes(x=at, y=media)) + 
  #geom_point(colour="black", size=4, shape=1)+
  #geom_smooth(method="lm", se = F, formula = formula, show.legend = T) +
  stat_poly_eq(aes(label =  paste(..eq.label..,..rr.label.., sep = "*\",\"~~")), eq.with.lhs = "italic(hat(y))~`=`~", 
               formula = formula, parse = T, label.y.npc =1, size= 5) + 
  geom_hline(aes(yintercept=12.37), data = dados, linetype="dashed", colour="blue", size=0.5) +
  
  labs(title = " ",
       y = "Altura médias das mudas (cm)", 
       x = "Altura dos Tubetes (cm)",
       caption = " ") +
  xlim(10,14) +
  ylim(0, 25) +
  theme_classic()+
  theme(axis.title = element_text(size = 12),
        axis.text = element_text(size = 12)) 
 
b
ggsave("Fator B_mudas.jpg",b, width=120, height=90, units = "mm", dpi=300)

g = grid.arrange(a, b, ncol=1)

#Escolher local para salvar
#crtl + shift + h

ggsave("fatorial_regress.png", g, width=120, height=180, units = "mm", dpi=600)

