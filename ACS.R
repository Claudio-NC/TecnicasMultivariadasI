################################################################
#                                                              #
#              ANALISIS DE CORRESPONDENCIA SIMPLE              #
#                                                              #
################################################################


# Cambiar el directorio
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
dir()
#---------------------------------------------------------------
# Limpiar workspace y gráficos
rm(list=ls())
dev.off()
graphics.off()

#---------------------------------------------------------------
# Eliminar la notación científica
options(scipen=999)      
options(digits = 7)

#---------------------------------------------------------------
# Paquetes
library(pacman)
p_load(MASS, ca, anacor,FactoMineR,vegan,
       gplots,vcd,graphics,factoextra,foreign)



#####################################
#                                   #
#          I. INTRODUCCION          #
#                                   #
#####################################

#----------------------------------------------#
#     Prueba de Independencia Chi cuadrado     #
#----------------------------------------------#

# Ho: Las categorias de las variable 1 son independientes de las categorias de la variable 2
# H1; Las categorias de las variable 1 son dependientes de las categorias de la variable 2

# Perfil fila = Perfil medio
niv.ingles<-matrix(c(100,200,200,300,600,600),ncol=3,byrow = T)
colnames(niv.ingles)<-c("Clase A","Clase B","Clase C")
row.names(niv.ingles)<-c("Hombres","Mujeres")
niv.ingles
chisq.test(niv.ingles)  
# NRHo. Las categorias del nivel de ingles son independientes de las categorias del género

# Perfil fila != Perfil medio
niv.ingles2<-matrix(c(150,50,300,250,750,500),ncol=3,byrow = T)
colnames(niv.ingles2)<-c("Clase A","Clase B","Clase C")
row.names(niv.ingles2)<-c("Hombres","Mujeres")
niv.ingles2
chisq.test(niv.ingles2)  
# RHo. Las categorias del nivel de ingles son dependientes de las categorias del género


######################################
#                                    #
#          II. APLICACION 1          #
#                                    #
######################################


#---------------------------------------#
#       1.- Tabla de contingencia       #
#---------------------------------------#

# Descricpion de la data
#-----------------------
# Data de opinion sobre opinion de sistema sanitario publico y nivel de ingresos
# Se busca relacion entre categorias de opinion e ingresos

# Ingreso de data (forma ideal)
#-----------------------------
# 1.- lectura de data
# Encuestado  Renta Opinion
#    1         Medio  Malo
#    .
#    .
#    n         Alto   Bueno
# 2.- tabla de contigencia

# Ingresando la tabla de contingencia
#------------------------------------
datos.acs <- matrix(c(75,40,35,
                      60,50,70,
                      20,40,30,
                      15,40,25),nrow=4,byrow=T) #ncol = 3
#datos.acs<-matrix(c(30,30,155,30,130,30,80,30,30,80,30,5),ncol=3,byrow=T)
datos.acs

# Asignación de nombres a las filas y columnas de la tabla
dimnames(datos.acs)<-list(renta=c("Bajo", "Medio", "Alto", "Muy Alto")
                          ,opinion=c("Bueno","Malo","Regular"))
# dimnames(datos.acs)<-list(Marcas=c("A","B","C","D")
#                     ,opinion=c("Segmento 1","Segmento 2","Segmento 3"))

datos.acs
datos.acs[2,1]

# Totales fila y totales columna
addmargins(datos.acs)


#--------------------------------------#
#       2.- Graficos de perfiles       #
#--------------------------------------#

# Primera forma Ballon plots
#---------------------------
library(gplots)
# Convertir los datos en una tabla 
dt <- as.table(datos.acs)
dt
str(dt)
# Para graficarlo con % fila (perfiles fila)
dt <- prop.table(dt,margin=1)  # 1: porcentaje fila, 2: porcentaje columna, null: porcentaje tabla
dt
balloonplot(t(dt),                  # Transpuesta
            main ="Gráfico Opinión Renta", 
            xlab ="Opinión", 
            ylab="Renta",
            label = F, cum.margins=F, 
            label.lines=F, show.margins = FALSE)

# Segunda forma - Mosaicos
#-------------------------
library(graphics)
mosaicplot(t(dt), shade = F, 
           main ="Gráfico Opinión Renta", 
           xlab ="Opinión", 
           ylab="Renta",
           las=2)   # Mientras mas grande el valor, mas grande es el area del rectangulo


#--------------------------------------------------#
#     3.- Prueba Chi cuadrado de independencia     #
#--------------------------------------------------#

# Prueba de independencia
# Ho: La opinion sobre el sistema sanitario 
#     es independiente del nivel de ingresos de los contribuyentes
# Ho: La opinion sobre el sistema sanitario 
#     es dependiente del nivel de ingresos de los contribuyentes
prueba <- chisq.test(datos.acs)
prueba

# La probabilidad de que esta muestra haya sido tomada de una población
# donde haya independencia entre categorias es casi nula

# Valores observados y valores esperados
#---------------------------------------
prueba$observed
prueba$expected
# addmargins(datos.acs)
# 51.0=170*150/500
# P(renta baja y opinion Bueno) = P(Renta Baja)*P(Opinion Bueno)
#       500*(150/500)*(170/500) = 150*170/500

# Frecuencia Relativa (fij)
prop.table(datos.acs) 
#datos.acs/660

# Perfiles Fila
prop.table(datos.acs, 1) 

# Perfiles Columna
prop.table(datos.acs, 2) 

# Tabla con el paquete gmodels y función CrossTable()
library(gmodels)
CrossTable(datos.acs,
           prop.t=F,  # Frecuencia Relativa
           prop.r=F,  # Perfil Fila
           prop.c=F,  # Perfil Columna
           prop.chisq=T)
# prop.chis=T muestra la contribucion de la combinacion de cada fila y columna al Chi cuadrado=40


#-------------------------------------------------------------------#
#       4.- ANALISIS DE CORRESPONDENCIA SIMPLE CON FACTOMINER       #
#-------------------------------------------------------------------#

# El 100% de la Inercia Total, se puede descomponer en
# min(número de filas,número de columnas) -1 componentes o
# dimensiones

library(FactoMineR) 
res.ca <- CA(datos.acs,ncp=2,graph=FALSE)
res.ca


#-----------------------------------------------#
#       5.- Scree Plot de los Autovalores       #
#-----------------------------------------------#

# Extrayendo autovalores
res.ca$eig
eig.val <- get_eigenvalue(res.ca)
eig.val

# Grafico de sedimentacion
#--------------------------
fviz_screeplot(res.ca,choice = c( "variance"))   # choice(eigenvalue) muestra el autovalor en eje Y
# fviz_screeplot(fit) + geom_hline(yintercept=33.33, linetype=2, color="red")
fviz_screeplot(res.ca, addlabels = TRUE) + ylim(0, 100)
#--------------------------------------------------------------


#------------------------------------------------------#
#       6.- Interpretacion de Indicadores de ACS       #
#------------------------------------------------------#

summary(res.ca,nb.dec = 3, ncp = 2)    # nb.dec=numero de decimales, ncp=numero de componentes

# Rows
#             Iner*1000    Dim.1   ctr   cos2    Dim.2    ctr   cos2  
# Bajo     |    34.375 | -0.322 47.655  0.903 | -0.106 22.341  0.097 |
# Medio    |     9.485 | -0.027  0.391  0.027 |  0.160 61.558  0.973 |
# Alto     |    13.219 |  0.268 19.803  0.975 | -0.043  2.178  0.025 |
# Muy Alto |    23.019 |  0.362 32.151  0.909 | -0.114 13.923  0.091 |
  
#   Columns
#            Iner*1000    Dim.1    ctr   cos2    Dim.2    ctr   cos2  
# Bueno    |    40.923 | -0.344 61.919  0.985 | -0.042  4.081  0.015 |
# Malo     |    26.667 |  0.253 33.467  0.817 | -0.120 32.533  0.183 |
# Regular  |    12.509 |  0.097  4.614  0.240 |  0.172 63.386  0.760 |


# Resultados extendidos, tomar las coordenadas, contribuciones 
# absolutas y relativas.
# En summary se tienen dos indicadores importantes: 
# Contribución Absoluta (ctr)  # x100
# Contribución Relativa (cos2) 
# Inercia                      # x1000

# Interpretación de la Contribución Absoluta (ctr)
#-------------------------------------------------
# Por ejemplo: para la fila BAJO y la dimensión 1 se tiene 
# una ctr = 47.655
# El 47,65% de la inercia de la dimensión 1 es explicada 
# por la fila BAJO

# Interpretación de la Contribución Relativa (cos2)
#-------------------------------------------------
# Por ejemplo: para la fila BAJO y la dimensión 1 se tiene 
# una cos2 = 0.903
# El 90,3% de la inercia de la fila BAJO es explicada 
# por la dimensión 1

# Contribucion Relativa 1 + Contribucion Relativa 2 = 100% (analogia: comunalidad en AF)

# Con la funcion plot() sobre un objeto de clase ca se 
# obtiene el biplot.


#----------------------------#
#       Interpretacion       #
#----------------------------#

#  Categorias  |  Componente 1  | Componente 2
#----------------------------------------------
#  Bajo        |  CA (-)        |
#  Medio       |                |  CA (+)        
#  Alto        |  CR (+)        |
#  Muy alto    |  CA (+)        |
#----------------------------------------------
#  Bueno       |  CA (-)        |
#  Malo        |  CA (+)        |
#  Regular     |                |  CA (+)  

# Las personas de ingreso BAJO tienen una opinion BUENA del sistema sanitario
# Las personas de ingreso ALTO y MUY ALTO tienen una opinion MALA del sistema sanitario
# Las personas de ingreso MEDIO tienen una opinion REGULAR del sistema sanitario


#---------------------------------#
#       7.- Gráficos Biplot       #
#---------------------------------#

#Representación gráfica de los datos en las dos dimensiones

# Primera forma - usando plot.CA de FactoMineR
#---------------------------------------------
plot.CA(res.ca) # Mapa Simétrico
