#7 Testes Estatísticos

#O R inclui em sua gama de utilidades uma poderosa ferramenta da estatástica contemporânea: 
#os testes estatísticos. Dentre esses, podemos destacar os testes de média, amplamente 
#usados em várias áreas do conhecimento.

#7.1 Teste t de Student

#O teste t é bastante usado em várias situações do cotidiano quando se deseja fazer 
#comparações entre uma ou mais médias, sejam elas dependentes ou não. Abaixo estão 
#exemplos de vários modos de realizarmos o teste t.

#Dados referentes a temperatura média do ar em duas condições: dentro de uma casa de vegetação e no campo:

pira_tem <- read.csv2 ("https://www.dropbox.com/s/zvp5iftcpb6bdpe/pira_tem.csv?dl=1",
                       dec=".")
str(pira_tem)

#Apresentação dos dados em forma de gráfico:

library(ggplot2)
ggplot(data= pira_tem, aes (x = hora, y = temp, colour =periodo)) +
  geom_point(size=2,shape=19) +
  geom_line() +
  facet_grid(.~local) +
  xlab("Horas") +
  ylab("Temperatura ºC") + 
    ggtitle("Variação da temperatura mediana\n nas quatro efemêrides") +
    theme(plot.title=element_text(face="bold", size=12, hjust = 0.5))  +
  theme_bw()


#t.test() Realiza o teste t-Student para uma ou duas amostras.

#sintaxe: t.test(amostra1, amostra2, opções)

#Parâmetros
#amostra1: Vetor contendo a amostra da qual se quer testar a média populacional, ou comparar a média populacional com a média populacional da amostra 2.
#amostra2: Vetor contendo a amostra 2 para comparação da média populacional com a média populacional da amostra 1.

#Opções
#alternative: string indicando a hipótese alternativa desejada. Valores possíveis: “two-sided”, “less” ou “greater”.
#mu: valor indicando o verdadeiro valor da média populacional para o caso de uma amostra, ou a diferença entre as médias para o caso de duas amostras.
#paired: - TRUE - realiza o teste t pareado. - FALSE - realiza o teste t não pareado.
#var.equal: - TRUE - indica que a variância populacional é igual nas duas amostras. - FALSE - indica que a variância populacional de cada amostra é diferente.
#conf.level: coeficiênte de confiança do intervalo.


#7.1.1 Para uma média

#Vamos testar se a temperatura horária do solsticio de verão no campo tem média igual ou maior que 21 ºC na cidade de Piracicaba-SP.

#H0: mu >= 21
#IC 95 para mu

#1.0 Passo filtrar os dados pelo fator “período” com o nível sol_verao (solsticio de verão):
#Dividir os dados - subset()
sol_verao_amb <- subset(pira_tem, periodo == "sol_verao")

#2.0 Passo filtrar os dados pelo fator “local” com o nível campo:
sol_verao_camp <- subset(sol_verao_amb, local == "campo")

#3.0 Verificar dados graficamente:
attach(pira_tem)
boxplot(temp)

#4.0 Usar o teste T:
t.test(sol_verao_camp$temp,                     #amostra a ser testada
       mu=21,                                   #hipótese de nulidade
       alternative="greater",                   #teste unilateral pela direita
       conf.level = 0.95 )                         #Intervalo de confiancia de 95%  
  
#Como a hipótese alternativa foi a aceita isso implica que a temperatura do ar no 
#solsticio de verão possui média estatisticamente diferente do valor 21ºC a um nível de significância de 5%.
  

#Exercicio 1____________________________________________________________________________________________________________________
#Um pesquisador afirmou que a temperatura média de solstício de verão medido na 
#casa de vegetação em Piracicaba-SP tem média 22,2 ºC. Desconfiando desse resultado, 
#um outro pesquisador com dados provinientes da mesma estação climatológicas em 
#períodos diferentes encontrou os seguintes resultados:

#H0: mu = 22,2

  sol_verao_amb <- subset(pira_tem, periodo == "sol_verao")
  
  sol_verao_est <- subset(sol_verao_amb, local == "estufa")
    boxplot(sol_verao_est$temp)
  
#Essa afirmação é verdadeira?
  t.test(sol_verao_est$temp,            #amostra a ser testada
       mu=22.2,                              #hipótese de nulidade
       alternative="two.sided",              #teste bilateral não considera se é maior ou menor
       conf.level = 0.99)                    #significância de 1%        

#Exercicio 2___________________________________________________________________________________________________________________
  #Vamos testar se X tem média estatiscamente igual a 35 ou maior H0: mu =>35:
  x <-c (30.5,35.3,33.2,40.8,42.3,41.5,36.3,43.2,34.6,38.5)
  boxplot(x)
  
  #Teste t:
  t.test(x,
         mu=35,
         alternative = "greater")
  #Como foi significativo, admitimos que a amostra x é oriunda de um população com 
  #média maior que o valor de 35, com nivel de 5% de significância.
#_______________________________________________________________________________________________________________________________
  
#7.1.2 Para duas médias independentes
#Para a realização do teste t pressupõe-se que as amostras possuem variâncias iguais além de seguirem distribuição normal.
  
 # Vamos a um exemplo:
    
#Suponha dois conjuntos de dados de temperatura de média do ar de dois ambientes 
#(casa de vegetação e campo). Verifique se as temperaturas dos dois ambientes 
#são estatisticamente diferentes usando 5% de significância. 
  #H0: mu da temp da casa de vegetação = mu da temp do campo
  
  boxplot(sol_verao_camp$temp, sol_verao_est$temp)
  
  #test t
  t.test(sol_verao_camp$temp, sol_verao_est$temp, #amostras a serem testadas
         alternative = "greater",                  #unilateral a direita 
         var.equal = T )                            #variância homogênea
  
#Uma vez que o p-value foi maior que 0,05, podemos concluir que as médias de temperatura
#dos dois ambientes não são diferentes, estatisticamente, a 5% de significância. 
#Veja que o resultado desta análise mostra o valor de t (estatística do teste), 
#os graus de liberdade (df) e o valor de p (significância). 
#Além disso, o resultado do teste ainda mostra as médias para cada grupo.
  
#_______________________________________________________________________________________________________________________________

#7.1.3 Para duas médias dependentes
#Neste caso vamos usar o mesmo nível de significância do exemplo das amostras independentes. 
#As hipóteses se mantêm. Agora basta adicionar o argumento paired=T, informando que as amostras são dependentes:  
 
  t.test(sol_verao_camp$temp, sol_verao_est$temp, #amostras a serem testadas
         conf.level=0.99,                          #nível de confiança
         paired=T,                                 #indica dependência entre as amostras
         var.equal = T )                           #variância homogênea       

  #Note que a estatística do teste-t pareado não é baseada na média dos tratamentos, e sim na diferença entre os pares de tratamentos.
  
#_______________________________________________________________________________________________________________________________
#7.2 Teste de variância
#7.2.1 Usando o teste de F
  #H0: a variancias das amostras são homogeneas
  
  var.test (sol_verao_camp$temp, sol_verao_est$temp)
  
#As variâncias não são homogeneas.
  
 #Vamos resolver novamente o exercicio anterior, modificando o argumento var.equal: 
  
  t.test(sol_verao_camp$temp, sol_verao_est$temp, #amostras a serem testadas
         conf.level=0.99,                          #nível de confiança
         paired=T,                                 #indica dependência entre as amostras
         var.equal = F )                           #variância homogênea 
  
#_______________________________________________________________________________________________________________________________
#7.3 Teste para a normalidade - shapiro.test()
  
  #Por vezes temos necessidade de identificar com certa confiança se uma amostra ou 
  #conjunto de dados segue a distribuição normal. Isso e possível, no R, com o uso do comando shapiro.test()
  #Verifique normalidade dos dados:
  
  shapiro.test(sol_verao_camp$temp)
  shapiro.test(sol_verao_est$temp)

#_______________________________________________________________________________________________________________________________
#7.4 Teste U de Mann-Whitney
 # H0: mu da temp da casa de vegetação = mu da temp do campo:
    
   wilcox.test(sol_verao_camp$temp,sol_verao_est$temp,
              alternative = "two.side")
#_______________________________________________________________________________________________________________________________
#7.5 Covariância e Correlação
  #A correlação de Pearson é uma medida paramétrica de associação linear entre duas variáveis.
  #A correlação de ordem de Sperman é uma medida não paramétrica de associação entre duas variáveis
  #A correlação de ordem de Kendall é outra medida não paramétrica da associação, baseada na concordância ou discordância dos pares x-y
   
   plot(sol_verao_camp$temp,sol_verao_est$temp, las=2)
   
   #Teste de correlação de Pearson:
     
   cor(sol_verao_camp$temp,sol_verao_est$temp, 
       method = "pearson" )
   
   #Teste de correlação de Pearson trocando o X e Y:
   cor(sol_verao_est$temp, sol_verao_camp$temp)
   
   #Teste de correlação de Spearman:
   cor(sol_verao_camp$temp,sol_verao_est$temp, 
       method = "spearman")
   
   #Teste de correlação de Kendall:
   cor(sol_verao_camp$temp,sol_verao_est$temp, 
       method = "kendall")
#_______________________________________________________________________________________________________________________________
# Outros testes  
   #Utilizaremos o banco de dados: dadosfisio
      fisio <- read.csv2("https://www.dropbox.com/s/zg7fyg1iewtji49/dadosfisio.csv?dl=1")
   attach(fisio)
   
  #Visualização gráfica
#A seguinte função proporciona uma visualização gráfica de todos os pares de correlação possíveis (scatter-plot)
   pairs(fisio[,4:10])
   
   #Teste de Spearman
    cor(fisio[,3:8],method = "spearman")
#_______________________________________________________________________________________________________________________________
#Carregando pacote metan
    library(metan)
    corr_plot(fisio) 
    
#A função corr_plot() pode ser utilizada com o operador %>%. Em adição, é possível escolher as variáveis a serem plotadas. Para isto, basta digitar o nome das variáveis.  
    fisio %>%
      corr_plot(ds, cc, ma, ptotal, tibe,
                shape.point = 19,
                size.point = 2,
                alpha.point = 0.5,
                alpha.diag = 0,
                pan.spacing = 0,
                col.sign = "gray",
                alpha.sign = 0.3,
                axis.labels = TRUE,
                progress = FALSE)
    
    fisio %>%
      corr_plot(ds, cc, ma, ptotal, tibe,
                shape.point = 21,
                col.point = "black",
                fill.point = "orange",
                size.point = 2,
                alpha.point = 0.6,
                maxsize = 4,
                minsize = 2,
                smooth = TRUE,
                col.smooth = "black",
                col.sign = "cyan",
                upper = "scatter",
                lower = "corr",
                diag.type = "density",
                col.diag = "cyan",
                pan.spacing = 0,
                lab.position = "bl")  
    
#_______________________________________________________________________________________________________________________________
#Combinando visualização gráfica e numérica 
    #Carregando pacote  corrplot
    library(corrplot)
    
    cor1 <- cor(fisio)
    corrplot.mixed(cor1,
                   upper = "ellipse",
                   lower = "number",
                   number.digits = 2)
  
#_______________________________________________________________________________________________________________________________
 #Carregando a biblioteca hydroGOF, que contém dados e funções usadas nesta análise:
     library(hydroGOF)
   
#Cálculo das medidas numéricas de qualidade do ajuste para o “melhor” caso (inatingível):
     gof(sim = fisio$ds, obs= fisio$cc)
   
#_______________________________________________________________________________________________________________________________
#Carregando a biblioteca GGally
     library(GGally)
     ggpairs(fisio, lower = list(continuous = "smooth"))     
     
#Ou podemos comparar as medidas entre solos com os valores das correlações, histogramas e gráficos de dispersão para cada solo
     ggpairs(fisio, columns = 4:9, ggplot2::aes(colour=factor(z)))
     
