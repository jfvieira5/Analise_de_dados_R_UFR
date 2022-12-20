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
#No trabalho "Estudos dos efeitos do Promalin sobre frutos de macieiras
#(Malus spp) promalin ivares Brasil e Rainha", Mestriner (1980) utilizou 4 repetições dos seguintes
#tratamentos:
#1 - 12,5 ppm de Promalin em plena floração
#2- 25,0 ppm de Promalin em plena floração
#3 - 50,0 ppm de Promalin em plena floração
#4- 12,5 ppm de Promalin em plena floração+ 12,5 ppm de Promalin no início dafrutificaç~
#5- Testemunha.
#O experimento foi instalado na Fazenda Chapadão, no município de Angatuba- SP. O
#delineamento experimental foi o de blocos casualizados, sendo as parcelas constituídas de 4 plantas
#espaçadas de 6 x 7 m, com 12 anos de idade na época de instalação do experimento.
#A designação dos tratamentos às parcelas e os pesos médios dos frutos, expressos em gramas,
#obtidos pela pesagem de 250 frutos por parcela, são apresentados no Quadro 4.5.1.

#ler dados via drobox
dados <- read.table("https://www.dropbox.com/s/9woiye3ce9twp78/BanzattoQd4.5.2.txt?dl=1") 
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
trat=c("promalin","12,5","25.0","50.0", "12.5+12.5", "Testemunha")

#Usando a função
croqui(trat,r=4)
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
  
#Analise descritiva por tratamento

  Media=tapply(dados$peso,dados$promalin , mean)
  Desvio=tapply(dados$peso,dados$promalin ,sd)
  Variancia=tapply(dados$peso,dados$promalin , var)
  Maximo=tapply(dados$peso,dados$promalin ,max)
  Minimo=tapply(dados$peso,dados$promalin , min)
  Mediana=tapply(dados$peso,dados$promalin ,median)
  
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
m0 <- lm (dados$peso ~ dados$promalin )

#Normalidade dos erros
shapiro.test(m0$res)

  #Como p-valor calculado (p= 0.1293) é MAIOR que o nível de significância 
  #adotado ( α = 0,05), ACEITA H0. 
  #Logo, os erros  seguem distribuição normal.

#Homogeneidade de variâncias
bartlett.test(m0$res, dados$promalin )

  #Como p-valor calculado (p= 0.6624) é MAIOR que o nível de significância 
  #adotado ( α = 0,05), ACEITA H0. 
  #Logo, as variâncias são homogêneas.

#Independências dos erros
ind=dwtest(m0)
ind

  #Como p-valor calculado (p=0.4669) é maior que o nível de significância
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
    boxcox((aov(dados$peso+0.000001~dados$promalin )))
  
  #Descobrindo o valor exato de  
    bc=boxcox(aov(dados$peso+0.000001~dados$promalin ))
    bc$x[which.max(bc$y)]
  
#____________________________________________________________________
#Dados transformados####### Não houve necessidade    
    #Anova#
    m_trans <- lm (dados$peso^-2 ~ dados$promalin )
    
    #Normalidade dos erros
    shapiro.test(m_trans$res)
    
    #Homogeneidade de variâncias
    bartlett.test(m_trans$res, dados$promalin )
    
    #Independências dos erros
    ind=dwtest(m_trans)
    ind
    
#____________________________________________________________________
#Análise de variância    
    
    an=with(dados,dbc(promalin , bloco, peso,   #NAO PRECISA DE TRANSFORMAÇÃO
                      quali = TRUE, 
                      mcomp = "sk",
                      sigT = 0.05, sigF = 0.05))
 
#____________________________________________________________________
##Grafico reposta box plot####
  
  theme_set(
    theme_bw() +
      theme(legend.background = element_rect( colour ="black",size=0.5, linetype="solid")) +
      theme(legend.position = "right")
  )
  #x11()#abrir uma janela suspensa
  ggplot(dados , aes(x=promalin , y=peso)) +
    geom_boxplot(size=0.65,shape=19, fill = "grey90")+ #grafico boxplot
    stat_summary(fun.y=mean, geom="point",shape=1,size=1) + #colocar valor medio
    scale_fill_brewer(palette = 1, type = "qual") + #escala de cor
    ylab (expression(paste('Pesos frutos da macieira, em gramas')))+
    xlab(expression(paste('Promalin')))+
    geom_hline(aes(yintercept=143.0), data = dados, linetype="dashed", colour="#BB0000", size=0.5) +
    theme(legend.position="top") +
    annotate("text", label="b", x=1, y=160, size = 5)  +
    annotate("text", label="b", x=2, y=160, size = 5)  +
    annotate("text", label="b", x=3, y=160, size = 5)  +
    annotate("text", label="b", x=4, y=160, size = 5)  +
    annotate("text", label="a", x=5, y=160, size = 5)  +
    theme(legend.position="none") +
    labs(caption = "Médias seguidas de mesma letra indicam diferença nula a 5%")
    
  #crt + shift + h #ESCOLHER A PASTA
    ggsave("peso_box_plot.jpg", width=200, height=100, units = "mm", dpi=300)
    
    summary(dados) #verficar o valor medio e inserir na linha geom_hline
    
  
##grafico de coluna####
  
  dados_2 = summarise(group_by(dados, promalin ),
                      media = mean(peso), sd = sd(peso))
    
  theme_set(
    theme_bw() +
      theme(legend.background = element_rect( colour ="black",size=0.5, linetype="solid")) +
      theme(legend.position = "right")
  )
  
  ggplot(dados_2, aes(promalin , media, fill=promalin ))+
    geom_bar(stat="identity")+
    geom_errorbar(aes(ymin=media-sd, ymax =media+sd), with=0.1, col="black") +
    stat_summary(fun.y=mean, geom="point",shape=1,size=1) +
    guides(fill=FALSE)+
    scale_fill_brewer(palette = 1, type = "qual") +
    ylab (expression(paste('Pesos frutos da macieira, em gramas')))+
    xlab(expression(paste('Promalin')))+
    ylim(0, 180) +
    geom_hline(aes(yintercept=34.25), data = dados, linetype="dashed", colour="#BB0000", size=0.5) +
    theme(legend.position="top") +
    annotate("text", label="b", x=1, y=170, size = 5)  +
    annotate("text", label="b", x=2, y=170, size = 5)  +
    annotate("text", label="b", x=3, y=170, size = 5)  +
    annotate("text", label="b", x=4, y=170, size = 5)  +
    annotate("text", label="a", x=5, y=170, size = 5)  +
    theme(legend.position="none") +
    labs(caption = "Médias seguidas de mesma letra indicam diferença nula a 5%")
  
  summary(dados) #verficar o valor medio e inserir na linha geom_hline
  
  #crtl+shit+h #ESCOLHER A PASTA
  ggsave("peso_coluna.jpg", width=180, height=90, units = "mm", dpi=300)

  
##grafico de coluna####
  ggplot(dados_2, aes(x=promalin ,y=media))+
    geom_col(fill=c(8,2,3,4,5),col="black")+
    geom_text(label=paste(round(dados_2$media,1),c("b","c","c","c","a")), vjust=-2)+
    theme_bw()+
    theme_classic()+
    ylim(0, 200) +
    ylab (expression(paste('Pesos frutos da macieira, em gramas')))+
    xlab(expression(paste('Promalin')))+
    geom_errorbar(aes(ymin=media-sd, ymax =media+sd), width=0.25) # Width é a largura da barra
  
  #crtl+shit+h #ESCOLHER A PASTA
  ggsave("peso_coluna_dois.jpg", width=180, height=90, units = "mm", dpi=300)
  
#____________________________________________________________________
