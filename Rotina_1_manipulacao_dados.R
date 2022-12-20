#Vamos baixar os dados climáticos de Rondonópolis-MT
dados <- read.csv2("D:/1-WORK/1-ENSINO/POS-GRADUACAO/MCA106 - 2022- Estatística Aplicada/Rotina/roo.csv")
#Podemos verificar o cabeçario do conjunto de dados:
head(dados)

#Baixando os pacotes necessários:
library(tidyverse)
#dplyr

#Selecionando algumas colunas pelo nome com a função select:
dados_2 = select(dados, ano, Prec, Tmax)

#Retirar colunas com a função select:
dados_3 = select(dados, -ano, -dd)

#Filtrar os dados com a função filter Apresentar somente os dias com temperatura mínima do ar menor que 20ºC:
dados_tmin = filter(dados, Tmin <20)
dados_tmax = filter(dados, Tmax >30)

dados_criticos = filter(dados, Tmin<20, UR <30)

#Quando precisamos adicionar uma coluna usamos a função mutate:
dados = mutate(dados, Tmd = ((Tmax + Tmin)/2))


#Resumir e agrupar os dados usando a função summarise
#Qual o valor mínimo de umidade relativa?
summarise(dados, min(UR, na.rm = TRUE))

#Qual o valor médio de velocidade do vento por mês?
summarise(group_by(dados, mm), mean(Vvento, na.rm =TRUE))

#Exploração de dados: gerar um resumo estatístico com o comando pipe
dados %>% 
  summarise(med = mean(UR,na.rm=TRUE), 
            min = min(UR,na.rm=TRUE),
            max = max(UR,na.rm=TRUE),
            total = n())

#Caso necessitamos retirar o mês de outubro:
dados %>% 
  filter(mm != 10) %>% 
  group_by(mm) %>% 
  summarise(mean(UR, na.rm = TRUE))

#Com os dados dados vamos filtrar os valores mensais da UR e Tmax, com uma condição de apresentar somente os valores de Tmax maiores que 40. Apresentação com a função head
dados %>%
  select(mm, UR, Tmax)%>%
  filter(Tmax > 40)  %>% head




  