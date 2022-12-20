#Utilize o código abaixo para instalar o pacote ggplot2:
install.packages("ggplot2")
#Sempre carregue o pacote antes de utilizá-lo:
library(ggplot2)

#Utilizaremos o banco de dados: dadosfisio
#Baixar os dados:
fisio <- read.csv2("D:/curso_r/dadosfisio.csv")

#Veja as primeiras linhas:
head(fisio)  

#O código abaixo é um exemplo de um gráfico bem simples, construído a partir das duas principais camadas. O eixo y representa a densidade do solo e ao eixo x a variável capacidade de campo:  

ggplot(data = fisio, aes(x = ds, y = cc)) +
  geom_point() 

#Outro aspecto que pode ser mapeado nesse gráfico é a cor dos ponto
ggplot(data = fisio, aes(x = ds, y = cc, colour = as.factor(z))) +
  geom_point()

#No entanto, também podemos mapear uma variável contínua, a cor dos pontos:
ggplot(data = fisio, aes(x = ds, y = cc, colour = ptotal)) +
  geom_point()

#Também podemos mapear o tamanho dos pontos a uma variável de interesse:
ggplot(data = fisio, aes(x = ds, y = cc, colour = ptotal, size = ma)) +
  geom_point()

#Outros geoms bastante utilizados:
#geom_line: para retas definidas por pares (x,y)
#geom_abline: para retas definidas por um intercepto e uma inclinação
#geom_hline: para retas horizontais
#geom_boxplot: para boxplots
#geom_histogram: para histogramas
#geom_density: para densidades
#geom_area: para áreas
#geom_bar: para barras

ggplot(fisio, aes(cota)) + geom_bar() #grafico de barra

ggplot(fisio, aes(cota)) + geom_histogram() #histograma

ggplot(fisio, aes(cota)) + geom_histogram(binwidth = 0.1)

ggplot(fisio, aes(cota)) + geom_density() #grafico de densidade (estimativa de densidade de Kernel)

ggplot(fisio, aes(cota)) + geom_freqpoly() #poligno de frequencia

ggplot(fisio, aes(cota)) + geom_dotplot() #grafico de pontos

#mais complexos

ggplot(fisio, aes(ds, fill = factor(z))) + 
  geom_boxplot(alpha = 0.5, colour="black")

#Veja a seguir como é fácil gerar diversos gráficos diferentes utilizando a mesma estrutura do gráfico de dispersão acima:



#6.1.1 Cores
#Usando colour:#

#Usando fill:

#Mude a cor dos objetos sem atribuir a uma variável. Para isso, observe que os aspectos colour e fill são especificados fora do aes():


#6.1.2 Eixos
#Para alterar os rótulos dos eixos acrescentamos as funções xlab() ou ylab():








#6.2.3 Dados Climáticos
#Baixar dados no banco de dados o arquivo: roo
roo <- read.csv2("https://www.dropbox.com/s/1ajoi1c8pla3yk6/roo.csv?dl=1")

str(roo)
#Boxplot para tempearatura mínima:

ggplot(data = roo, aes(x = factor(mm),y = (Tmin)))+
  geom_boxplot() +
  scale_x_discrete(breaks=c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11","12"),
                   labels=c("Jan","Fev", "Mar", "Abr", "Mai", "Jun", "Jul", "Ago", "Set", "Out", "Nov", "Dez")) +
  xlab("Mês") +
  ylab("Temperatura Min.") +
  theme_minimal()
  

#Gráfico de distribuição de temperatura mínima total:
ggplot(data = roo, aes(x = (Tmin)))+
  geom_density() +
  theme_light()

#Gráfico de distribuição de temperatura mínima para cada mês:
ggplot(data = roo, aes(x = (Tmin), fill=factor(mm)))+
  geom_density() 
  
  

