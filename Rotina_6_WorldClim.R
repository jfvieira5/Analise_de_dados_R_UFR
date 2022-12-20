rm(list=ls())

# preparando o r -------------------------------------------------------------
library(raster) # Geographic Data Analysis and Modeling
#install.packages("raster")
## download de limites ------------------------------------------------

#limites adm do brasil
Brasil <- getData("GADM", country = "BR", level = 0)
  plot(Brasil, main="Adm Fronterias do Brasil, nivel = 0") #plotar brasil

#limites adm Estados
Brasil_UF <- getData("GADM", country = "BR", level = 1)
  plot(Brasil_UF, main="Adm Fronterias Estaduais do Brasil, nivel = 1") #plotar brasil

#limites adm municipal
Brasil_mun <- getData("GADM", country = "BR", level = 2)
  plot(Brasil_mun, main="Adm Fronterias municipais do Brasil, nivel = 2") #plotar brasil

## filtrar dados espaciais ------------------------------------------------
  
Brasil_UF@data #verificar tabela de atributos
#Filtar os valores da tabela de atributos nivel 1
  AC = subset (Brasil_UF,NAME_1 == "Acre") 
  plot(AC, main="Estado do Acre")

Brasil_mun@data #verificar tabela de
#Filtar os valores da tabela de atributos nivel 2
  AC_mun = subset (Brasil_mun,NAME_1=="Acre") 
  plot(AC_mun) 
  text(coordinates(AC_mun), as.character(AC_mun$NAME_2), cex = 0.6)

#Plotar as esta??es
  pontos <- SpatialPoints(coords = data.frame(c(-71.949720, -72.681), #long
                                           c(-9.186933,  -7.611))) #lat=y, 
  AC=subset(Brasil_mun,NAME_1=="Acre") 
  
  plot(AC) 
  plot(pontos, add = TRUE, col = "red", pch = 21 ) 
  
#colocar os nomes dos pontos 
  text(coordinates(pontos),c("Jordão", "Cruzeiro do Sul"), col = "red")
  

# Worldclim.org download de dados climaticos ------------------------------------------------

  #https://www.worldclim.org/data/worldclim21.html
  
  #Baixar os dados clim?ticos
  climate <- getData('worldclim', var='bio', res=10)
  
  #transforma??o para celsius
  gain(climate)=0.1
  
  #Vamos tra?ar o primeiro indicador "Temperatura M?dia Anual"
  plot(climate$bio1, main="Temperatura média anual")

  #Plotar dados corrigidos
  plot(climate[[1:3]])

##Dados de temperatura do ar mensal para o Estado do Acre ---------------------------------------
  tmax=getData('worldclim', var='tmax', res=10)
  gain(tmax)=0.1 #transforma??o para celsius
  
  #Organizar os nomes
  sort(names(tmax))
  names(tmax)
  
  #Mudar os nomes dos meses
  names(tmax)=sprintf("%02d",1:12) #numeral com 2 digitos
  names(tmax) = month.name
  
  #exemplos de nomes do m?s
  month.name # nome completo do m?s
  month.abb #nome abreviado do m?s
  sprintf("%02d",1:12)#numeral com 2 digitos
  sprintf("%04d",1:12) #numeral com 4 digitos
  
  #Cortar os valores de Temperatura no vetor do ACRE
  tmax_crop = crop (tmax, AC)
  
  #Plotar mapas dos meses
  plot(tmax_crop)

  #Plotar somente o ms de janeiro
  tmax_crop = crop(tmax$January,AC)
  plot(tmax_crop,  main="Temperatura máxima do ar no Acre - Janeiro (1970-2000) - WordlClim") 
  plot(AC, add=TRUE)
  
  #Plotar somente o m?s de fev
  tmax_crop = crop(tmax$February,AC)
  plot(tmax_crop,  main="Temperatura m?xima do ar no Acre - fev (1970-2000) - WordlClim") 
  plot(AC, add=TRUE)
  
  #Plotar somente o m?s de mar
  tmax_crop = crop(tmax$March,AC)
  plot(tmax_crop,  main="Temperatura m?xima do ar no Acre - mar (1970-2000) - WordlClim") 
  plot(AC, add=TRUE)
  
  #Plotar somente o m?s de abr
  tmax_crop = crop(tmax$April,AC)
  plot(tmax_crop,  main="Temperatura m?xima do ar no Acre - abr (1970-2000) - WordlClim") 
  plot(AC, add=TRUE)
  
  #Plotar somente o m?s de maio
  tmax_crop = crop(tmax$May,AC)
  plot(tmax_crop,  main="Temperatura m?xima do ar no Acre - maio (1970-2000) - WordlClim") 
  plot(AC, add=TRUE)

  #Plotar somente o m?s de set
  tmax_crop = crop(tmax$September,AC)
  plot(tmax_crop,  main="Temperatura m?xima do ar no Acre - set (1970-2000) - WordlClim") 
  plot(AC, add=TRUE)
  
  #Plotar somente o m?s de dez
  tmax_crop = crop(tmax$December,AC)
  plot(tmax_crop,  main="Temperatura m?xima do ar no Acre - set (1970-2000) - WordlClim") 
  plot(AC, add=TRUE)
  
##Dados de chuva mensal para o Estado do Acre ---------------------------------------
  prec=getData('worldclim', var='prec', res=10)

  #Organizar os nomes
  sort(names(prec))
  names(prec)
  
  #Mudar os nomes dos meses
  names(prec)=sprintf("%02d",1:12)
  names(prec) = month.name
  
  #Cortar os valores de Temperatura no vetor do ACRE
  prec_crop = crop (prec, AC)
  
  #Plotar mapas dos meses
  plot(prec_crop)
 
  #Plotar somente o m?s de janeiro
  prec_crop = crop(prec$January,AC)
  plot(prec_crop,  main="Precipitação pluvial no Acre - Janeiro (1970-2000) - WordlClim") 
  plot(AC, add=TRUE)
  
  #Plotar somente o m?s de agosto
  prec_crop = crop(prec$August,AC)
  plot(prec_crop,  main="Precipita??o pluvial no Acre - Agosto (1970-2000) - WordlClim") 
  plot(AC, add=TRUE)
  
  #Plotar somente o mês de dezembro
  prec_crop = crop(prec$December,AC)
  plot(prec_crop,  main="Precipitação pluvial no Acre - Dezembro (1970-2000) - WordlClim") 
  plot(AC, add=TRUE)
  
  
  