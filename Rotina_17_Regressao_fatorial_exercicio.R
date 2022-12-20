#Regressao DBC Fatorial

#Abrir pacotes####
library(readxl) #abrir excel
library(ExpDes.pt) #anova
library(ggplot2) #grafico
library(tidyverse) #pacote manipulação
library(agricolae) #anova
library(gridExtra) #grafico
library(grid) #grafico
library(knitr) #gerar reladuborio
library(lmtest) #teste 
library(ggpmisc) #regressão

#____________________________________________________________________
#Vamos considerar os dados (fictícios) de um experimento faduborial2 x 4, com 2 Variedades (V 1 e V 2 )
#e 4 níveis de Adubação (A 1 =O kglha ; A 2 = 100 kglha ; A 3 = 200 kglha ; A 4 = 300 kglha ), num
#experimento com 4 blocos casualizados, cujos resultados são apresentados no Quadro 7.3.3

#ler dados via drobox
dados <- read.table("https://www.dropbox.com/s/uvj6mk4i73tona1/BanzadubtoQd7.3.3.txt?dl=1", header=T) 
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

#Analise descritiva por fator 1

Media=tapply(dados$prod,dados$varied  , mean)
Desvio=tapply(dados$prod,dados$varied  ,sd)
Variancia=tapply(dados$prod,dados$varied  , var)
Maximo=tapply(dados$prod,dados$varied  ,max)
Minimo=tapply(dados$prod,dados$varied  , min)
Mediana=tapply(dados$prod,dados$varied  ,median)

descritiva=cbind(Media,
                 Desvio, 
                 Variancia, 
                 Maximo, 
                 Minimo, 
                 Mediana)
kable(descritiva, 2)


#Analise descritiva por por fator 2

Media=tapply(dados$prod,dados$adub  , mean)
Desvio=tapply(dados$prod,dados$adub  ,sd)
Variancia=tapply(dados$prod,dados$adub  , var)
Maximo=tapply(dados$prod,dados$adub  ,max)
Minimo=tapply(dados$prod,dados$adub  , min)
Mediana=tapply(dados$prod,dados$adub  ,median)

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
m0 <- lm (dados$prod ~ dados$varied * dados$adub  )

#Normalidade dos erros
shapiro.test(m0$res)

#Como p-valor calculado (p= 0.1842) é MAIOR que o nível de significância 
#adotado ( α = 0,05), ACEITA H0. 
#Logo, os erros  seguem distribuição normal.

#Homogeneidade de variâncias
bartlett.test(m0$res ~ interaction (adub, varied), dados)

#Como p-valor calculado (p= 0.1917) é MAIOR que o nível de significância 
#adotado ( α = 0,05), ACEITA H0. 
#Logo, as variâncias são homogêneas.

#Independências dos erros
ind=dwtest(m0)
ind

#Como p-valor calculado (p=0.0575) é maior que o nível de significância
#adotado (α=0,05), não se rejeita H0. Logo, os erros são independentes.

plot(m0$res, col="blue",
     las=1, pch=16,
     ylab="Residuos brutos")
abline(h=0, col="red", lwd=2)

#A Figura apresenta o gráfico dos resíduos brutos. 
#Percebe-se que os resíduos estão distribuídos de forma totalmente 
#aleadubório, evidenciando a independência dos erros.

#____________________________________________________________________
#Transformação de dados#######  Não houve necessidade

require(MASS)

#Usando o comando boxcox e conferindo visualmente um 
#valor aproximado de λ
boxcox((aov(dados$prod+0.000001~dados$adub*dados$varied)))

#Descobrindo o valor exadubo de  
bc=boxcox((aov(dados$prod+0.000001~dados$adub*dados$varied)))
bc$x[which.max(bc$y)]

#____________________________________________________________________
#Dados transformados####### Não houve necessidade    
#Anova#
m_trans <- lm (dados$prod ~ dados$adub*dados$varied)

#Normalidade dos erros
shapiro.test(m_trans$res)

#Homogeneidade de variâncias
bartlett.test(m0$res ~ interaction (adub, varied), dados)

#Independências dos erros
ind=dwtest(m_trans)
ind

#____________________________________________________________________
#Análise de variância####    

an=with(dados,fat2.dbc (adub, varied, bloco, prod,    #NAO PRECISA DE TRANSFORMAÇÃO
                        quali = c(FALSE, TRUE), 
                        mcomp = "duncan",
                        fac.names = c("Adubo", "Variedade"),
                        sigT = 0.05, sigF = 0.05))

#Conclusões:
#Verificamos que a Interação V x A foi significativa (P50,0 1 ), indicando que os fatores
#Variedades e Adubações agem de modo dependente sobre a produção.
#Então, a Interação V x A deve ser desdobrada, o que pode ser feito de duas maneiras:
  #a) Para estudar os efeitos das Variedades em cada Adubação;
  #b) Para estudar os efeitos das Adubações em cada Variedade.

#____________________________________________________________________

dados2 = dados %>% 
  group_by(adub, varied) %>% 
  filter(varied == "B") %>%  #escolhero nivel
  summarise("media" = round(mean(prod),1), 
            "desvio" = round(sd(prod),1))
#____________________________________________________________________

formula <- y ~ poly(x, 2, raw = T)
a = ggplot(dados2, 
       aes(x=adub, y=media)) + 
  geom_point(colour="black", size=4, shape=1)+
  geom_smooth(method="lm", se = F, formula = formula, show.legend = T) +
  stat_poly_eq(aes(label =  paste(..eq.label..,..rr.label.., sep = "*\",\"~~")), eq.with.lhs = "italic(hat(y))~`=`~", 
               formula = formula, parse = T, label.y.npc =1, size= 2) +
    labs(title = " ",
       y = "produção (t/ha)", 
       x = "Doses de adubo (kg/ha)",
       caption = " ") +
  xlim(0,300) +
  ylim(1, 4) +
  theme_classic()+
  theme(axis.title = element_text(size = 12),
      axis.text = element_text(size = 12)) 

a 
ggsave("doses_var_B.jpg",a, width=120, height=90, units = "mm", dpi=300)

#Verificamos que, para a Variedade 1, existe uma relação linear entre a quar1tidade de Adubação
#(X) e a produção (Y).
#____________________________________________________________________

dados3 = dados %>% 
  group_by(adub, varied) %>% 
  filter(varied == "A") %>%  #escolhero nivel
  summarise("media" = round(mean(prod),1), 
            "desvio" = round(sd(prod),1))
#____________________________________________________________________
formula <- y ~ poly(x, 1, raw = T)


b = ggplot(dados3, 
       aes(x=adub, y=media)) + 
  geom_point(colour="black", size=4, shape=1)+
  geom_smooth(method="lm", se = F, formula = formula, show.legend = T) +
  stat_poly_eq(aes(label =  paste(..eq.label..,..rr.label.., sep = "*\",\"~~")), eq.with.lhs = "italic(hat(y))~`=`~", 
               formula = formula, parse = T, label.y.npc =1, size= 2) + 
  geom_hline(aes(yintercept=12.37), dados, linetype="dashed", colour="blue", size=0.5) +
  
  labs(title = " ",
       y = "produção (t/ha)", 
       x = "Doses de adubo (kg/ha)",
       caption = " ") +
  xlim(0,300) +
  ylim(1, 4) +
  theme_classic()+
  theme(axis.title = element_text(size = 12),
        axis.text = element_text(size = 12)) 
 
b
ggsave("Dose_var_B.jpg",b, width=120, height=90, units = "mm", dpi=300)

g = grid.arrange(a, b, ncol=1)

#Escolher local para salvar
#crtl + shift + h

ggsave("fat_regress.png", g, width= 90, height=180, units = "mm", dpi=600)
