#Limpar
rm(list = ls())

# preparando o r -------------------------------------------------------------
library(hydroTSM) #hidrologia
library(readxl) #abrir excel
require(lattice) #fazer graficos

# buscar banco de dados
dados <- read_excel("planilha_balanco_hidrico.xlsx", 
                    sheet = "Planilha1")
View(dados)

# transformar em data
dados$DATE = as.Date(dados$DATE)

# transformar em classe zoo
ad = read.zoo(dados)
head(ad)
class(ad)#verificar classes

# escolher a variavel -------------------------------------------------------------

variavel=ad$RAIN #chuva
head(variavel)

#Selecionar somente 5 anos da serie: Arrumar a data do inicio e final
var <- window(variavel, start=as.Date("1980-01-01"), end=c("2016-01-31"))

#Valores mensal:
 m <- daily2monthly(var, FUN=sum) #se fosse temperatura colocar FUN=mean
 m

## An?lise explorat?rios da variavel -------------------------------------------------------------

  #Resumo estatistico:
  smry(var)

### grafico hydroplot --------------------------------------------------------------
 
  x11()
  hydroplot(var, var.type="Precipitation", main="Cruzeiro do Sul-Acre",
            pfreq = "dma", from="2000-01-01")
  
### grafico matrixplot - lattice -------------------------------------------------------------
  x11()
  # Daily zoo to monthly zoo
  m <- daily2monthly(var, FUN=sum, na.rm=TRUE)
  
  # Creating a matrix with monthly values per year in each column
  M <- matrix(m, ncol=12, byrow=TRUE)
  colnames(M) <- month.abb
  rownames(M) <- unique(format(time(m), "%Y"))

  print(matrixplot(M, ColorRamp="Precipitation", main="Precipitação mensal de Cruzeiro do Sul- Ac (mm/mês)"))

## An?lise mensal dos dados - BOXPLOT ---------------------------------------------
  
  monthlyfunction(m, FUN=median, na.rm=TRUE)

  #Vetor com as abreviaturas de tres letras para os nomes dos meses:
  cmonth <- format(time(m), "%B") #%b nome do m?s abreviado
  
  #Criando fatores mensais ordenados:
  months <- factor(cmonth, levels=unique(cmonth), ordered=TRUE)

x11()

  #Boxplot dos valores mensais:
  boxplot( coredata(m) ~ months,
           col="lightblue",
           main="Cruzeiro do Sul - Acre",
           ylab="Precipitação pluvial (mm)", xlab="Mensal")

## Analise sazonal -------------------------

  #Dez, Jan, Fev
  ( DJF <- dm2seasonal(var, season="DJF", FUN=sum) )
  
  #Mar, Abr, Maio
  ( MAM <- dm2seasonal(m, season="MAM", FUN=sum) )
  
  #Jun, Jul, Ago
  ( JJA <- dm2seasonal(m, season="JJA", FUN=sum) )
  
  #Set, Out, Nov
  ( SON <- dm2seasonal(m, season="SON", FUN=sum) )
  
  
## grafico climograph -------------- 

# extracting individual ts of precipitation, maximum and minimum temperature
pcp <- ad$RAIN 
tmx <- ad$TMAX
tmn <- ad$TMIN

x11()

# Plotting the climograph
m <- climograph(pcp=pcp, tmx=tmx, tmn=tmn, na.rm=F, pcp.label="Chuva, [mm]", 
                tmean.label="Temperatura, [\U00B0 C]",
                pcp.col="lightblue", 
                tmean.col="red")
dev.off()


