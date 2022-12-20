# preparando o r -------------------------------------------------------------
# packages
library(readxl) #abrir excel
library(dplyr)  #manipulacao dados
library(geobr) #vetores brasil
library(ggplot2) #grafico e mapas
library(ggspatial) # elementos espacias no ggplot2
library(RColorBrewer) #paletas de cores

#Instalacao
#devtools::install_github("ipeaGIT/geobr", subdir = "r-package")

# diretorio
setwd("D:/1-WORK/1-ENSINO/POS-GRADUACAO/Analise de dados R_UFR/2022-2/dados") 
# define o diretorio crtl + shit + h
getwd() # mostrar diretorio

# dado vetorial ------------------------------------------------
#poligno
conj_dados = list_geobr() #listar variaveis geograficas

  #limite brasil
        bra = read_state(code_state = "all",
                        year = 2010)
        plot (bra)
        
  #limite acre      
        ac = read_state(code_state = "AC",
                         year = 2010,
                         simplified = TRUE,
                         showProgress = TRUE)
        plot (ac)
        
  #limite acre cidade      
        muni_ac = read_municipality(code_muni = "AC",
                                    year = 2010,
                                    simplified = TRUE,
                                    showProgress = TRUE)
        plot(muni_ac)
  
  #limite cruzeiro do sul      
     
        muni_czs = read_municipality(code_muni = 1200203,
                                    year = 2010,
                                    simplified = TRUE,
                                    showProgress = TRUE)
        plot(muni_czs)
    
  #limite rodrigues alves     
        
        muni_ra = read_municipality(code_muni = 1200427,
                                     year = 2010,
                                     simplified = TRUE,
                                     showProgress = TRUE)
        plot(muni_ra)    
  
  #limite rio branco
        
        muni_rb = read_municipality(code_muni = 1200401,
                                    year = 2010,
                                    simplified = TRUE,
                                    showProgress = TRUE)
        plot(muni_rb)        
        
#pontos
        city = c("ponto 1", "ponto 2", "ponto 3")
        lat = c(-9.186933, -7.611, -8.62)
        lon = c(-71.949720,-72.681, -73.4)
    
    dat = data.frame(city, lat, lon) #criar data frame

# mapas --------------------------------------------------------------------
    
# limite do brasil
    ggplot()+
    geom_sf(data=bra, fill="white")

# limite do brasil  + acre  
    ggplot()+
      geom_sf(data=bra, fill="white") +
      geom_sf(data=ac, color= "red", fill = "gray")
      
# limite do brasil  + acre  + pontos
    ggplot()+
      geom_sf(data=bra, fill="white") +
      geom_sf(data=ac, color= "black", fill = "gray") +
      geom_point(data=dat,
               aes(x=lon, y=lat, color = city)) 
    
# limite do brasil  + acre  + pontos + tema
    ggplot()+
      geom_sf(data=bra, fill="white") +
      geom_sf(data=ac, color= "black", fill = "gray") +
      geom_point(data=dat,
                 aes(x=lon, y=lat, color = city)) +
      theme_linedraw()
    
# limite do brasil  + acre  + pontos + tema + titulo/eixos 
    ggplot()+
      geom_sf(data=bra, fill="white") +
      geom_sf(data=ac, color= "black", fill = "gray") +
      geom_point(data=dat,
                 aes(x=lon, y=lat, color = city)) +
      theme_light() +
      labs(x = "Longitude", 
           y = "Latitude",
           color = "Cidade")
    
# limite do brasil  + acre  + pontos + tema + titulo/eixos + zoom
    ggplot()+
      geom_sf(data=bra, fill="white") +
      geom_sf(data=ac, color= "black", fill = "gray") +
      geom_point(data=dat,
                 aes(x=lon, y=lat, color = city)) +
      theme_light() +
      labs(x = "Longitude", 
           y = "Latitude",
           color = "Cidade")  +
      xlim(-75, -65) +
      ylim (-12, -6)

# limite do brasil  + acre  + pontos + tema + titulo/eixos + zoom + salvar
    ggplot()+
      geom_sf(data=bra, fill="white") +
      geom_sf(data=ac, color= "black", fill = "gray") +
      geom_point(data=dat,
                     aes(x=lon, y=lat, color = city)) +
      theme_light() +
        labs(x = "Longitude", 
             y = "Latitude",
             color = "Cidade")+
      xlim(-75, -65) +
      ylim (-12, -7)        

    ggsave(filename = "localizacao.png")


# limite do brasil  + acre  + pontos + tema + titulo/eixos + zoom + salvar + cidades
    ggplot()+
      geom_sf(data=bra, fill="white") +
      geom_sf(data=ac, color= "black", fill = NA) +
      geom_sf(data=muni_ac, color= "black", fill = NA) +
      geom_sf(data=muni_ra, color= "black", fill = "black") +
      geom_sf(data=muni_czs, color= "black", fill ="blue") +
      geom_sf(data=muni_rb, color = "black", fill = "red") +
      
      geom_point(data=dat,
                 aes(x=lon, y=lat, color = city)) +
      theme_light() +
      labs(x = "Longitude", 
           y = "Latitude",
           color = "Cidade") +
     xlim(-75, -65) + #
     ylim (-12, -7)      
    
    ggsave(filename = "localizacao.png")

