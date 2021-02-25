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
plot.CA(res.ca) # Mapa Simétrico
plot.CA(res.ca, axes = c(1,2), col.row = "darkgreen", col.col = "red")
plot.CA(res.ca,mass=c(T,T))

# Segunda forma - usando fviz_ca_biplot de factoextra
fviz_ca_biplot(res.ca, repel = T)
fviz_ca_biplot(res.ca, repel = T) + theme_minimal()
fviz_ca_biplot(res.ca, repel = T) + theme_light()
fviz_ca_biplot(res.ca, repel = T) + theme_void()
fviz_ca_biplot(res.ca, repel = T) + theme_test()



#--------------------------------------------#
#       8.- Gráficos de contribuciones       #
#--------------------------------------------#

# Contribuciones absolutas
#-------------------------
row <- get_ca_row(res.ca); row
str(row)
col <- get_ca_col(res.ca); col

head(row$contrib)
head(col$contrib)
# grafico de contribuciones absolutas por filas
fviz_contrib(res.ca, choice = "row", axes = 1)
fviz_contrib(res.ca, choice = "row", axes = 2)
# grafico de contribuciones absolutas por columnas
fviz_contrib(res.ca, choice = "col", axes = 1)
fviz_contrib(res.ca, choice = "col", axes = 2)

# Contribuciones relativas de cada dimensión
#-------------------------------------------
head(row$cos2)
head(col$cos2)
# Grafico de contribuciones relativas por fila
fviz_cos2(res.ca, choice = "row", axes = 1)
fviz_cos2(res.ca, choice = "row", axes = 2)
# Grafico de contribuciones relativas por columna
fviz_cos2(res.ca, choice = "col", axes = 1)
fviz_cos2(res.ca, choice = "col", axes = 2)

# Coordenadas de las Dimensiones para filas y columnas 
#-----------------------------------------------------
head(row$coord) 
head(col$coord)


#--------------------------------------------------------------#
#       9.- Significancia de asociacion filas y columnas       #
#--------------------------------------------------------------#

# Significancia de la asociación entre filas y columnas
eig <- factoextra::get_eigenvalue(res.ca)
eig

trace <- sum(eig[,1])  ; trace  # trace = Inercia Total
cor.coef <- sqrt(trace)
cor.coef
# Como regla, un valor por encima de 0.2 indica 
# una correlación que puede ser considerada importante
# (Bendixen 1995, 576; Healey 2013, 289-290).


# Relacion Chi Cuadrado - Inercia
#--------------------------------
# ChiCuadrado = Traza * (Total de Tabla)
chi = trace*500
chi

prueba<-chisq.test(datos.acs)
prueba
# IT= chi /500; IT


#-----------------------------------------------------------------#
#       10.- ANALISIS DE CORRESPONDENCIA SIMPLE CON PAQUETES      #
#-----------------------------------------------------------------#

# PAQUETE VEGAN
#--------------
library(vegan)
corres2 <- cca(datos.acs)   
summary(corres2)

# PAQUETE ANACOR
#--------------
library(anacor)
fit2 <- anacor(datos.acs,ndim=2)
str(fit2)

summary(fit2)
plot(fit2,plot.type="jointplot")  # Grafico Biplot
plot(fit2)
fit2$row.scores  # coordenadas fila

# Autovectores
#-------------
fit2$left.singvec
fit2$right.singvec



#--------------------------------------------------------------------------#
#       11.- ANALISIS DE CORRESPONDENCIA SIMPLE CON UNA BASE DE DATOS      #
#--------------------------------------------------------------------------#

# Lectura de datos
#-----------------
library(foreign)
datos <- read.spss("Riesgo_morosidad.sav", 
                   use.value.labels = T,  
                   to.data.frame=TRUE)
str(datos)
attach(datos)   

# Tabla de contingencia
#----------------------
table(nrodepen)
table(dpto)

addmargins(table(dpto,nrodepen))

datos.acs.b <- as.matrix(table(dpto,nrodepen))
datos.acs.b

# ACS
#----
library(FactoMineR) 
res.ca.b <- CA(datos.acs.b,ncp=2,graph=FALSE)

eig.val <- get_eigenvalue(res.ca.b)
eig.val

# Grafico de sedimentacion
#-------------------------
fviz_screeplot(res.ca.b)

# Resumen de indicadores
summary(res.ca.b,nb.dec = 3, ncp = 2) 

# INERTIA
# Lima Trujillo y Arequipa estan explicando la inercia----------> alejado del origen
# La distribucion de la fila Piura es similar al perfil medio --> cercano al origen   
# La distribucion de las columna 0 es similar al perfil medio --> cercano al origen   

# CONTRIBUCION RELATIVA
# No en todo los casos sumará 1, porque se estan reteniendo solo 2 dimensiones. 
# Con 5 componentes si sumaría 1

# CONCLUSION
# Piura y 0 estarán cercano al origen

# BIPLOT
fviz_ca_biplot(res.ca.b, repel = T)


#------------------------------------------#
#       12.- VARIABLES SUPLEMENTARIAS      #
#------------------------------------------#


#             E1	E2	E3	E4	E5	E6	E7	E8	E9	Ideal
# Precios	    16	17	18	19	16	45	15	19	18	45
# Variedad	   8	15	18	17	27	20	 2	14	53	53
# Rapidez	    20	20	23	21	29	20	18	19	25	29
# Información	11	13	12	17	20	16	15	10	44	44
# Trato	      28	25	25	22	30	26	24	22	26	30
# Condiciones	21	21	20	24	27	22	18	21	24	27
# Acceso	    21	21	21	23	26	15	16	18	21	26

# Ideal es una variable creada con los valores mas altos por fila

# Tabla de contigencia
#---------------------
datos_s.acs <- matrix(c(16,17,18,19,16,45,15,19,18,45,
                        8,15,18,17,27,20, 2,14,53,53,
                        20,20,23,21,29,20,18,19,25,29,
                        11,13,12,17,20,16,15,10,44,44,
                        28,25,25,22,30,26,24,22,26,30,
                        21,21,20,24,27,22,18,21,24,27,
                        21,21,21,23,26,15,16,18,21,26),
                      nrow=7,byrow=T)
datos_s.acs

# Asignación de nombres a las filas y columnas de la tabla
dimnames(datos_s.acs)<-list(Atributos=c("Precios", "Variedad", "Rapidez", 
                                        "Información","Trato","Condiciones","Acceso")
                            ,Empresa=c("Empresa 1","Empresa 2","Empresa 3",
                                       "Empresa 4","Empresa 5","Empresa 6",
                                       "Empresa 7","Empresa 8","Empresa 9",
                                       "Ideal"))

datos_s.acs
addmargins(datos_s.acs)

# Prueba de Independencia Chi-Cuadrado  
chisq.test(datos_s.acs[,-10]) # Se trabaja con todas las columnas menos la suplementaria
# Se Rechaza Ho

# Grados de libertad de la Chi-cuadrado
gl<-(7-1)*(9-1); gl


# ACS con el paquete FactoMiner   
#------------------------------
library(FactoMineR) 
res.ca.s <- CA(datos_s.acs,
               ncp=2,
               graph=FALSE,
               col.sup = 10)

# Scree Plot de los Autovalores
#------------------------------
get_eigenvalue(res.ca.s)
# Con dos dimensiones se explica el 91.1 % de la inercia
fviz_screeplot(res.ca.s, addlabels = TRUE, ylim = c(0, 80))


# Interpretación de los Indicadores del ACS
#------------------------------------------
summary(res.ca.s,nb.dec = 3, ncp = 2) 
# Aparece las coordenadas y contribucion relativa de la variable suplementaria

# Biplot filas, columnas y columna suplementaria
#-----------------------------------------------
fviz_ca_biplot(res.ca.s, repel = T) + theme_light()


# VARIABLES SUPLEMENTARIA CON LA DATA LUQUE
#------------------------------------------
datos<-matrix(c(30,30,155,
                30,130,30,
                80,30,30,
                80,30,5,
                80,130,155),ncol=3,byrow=T,
              dimnames=list(c("A","B","C","D","Ideal"),
                            c("Segmento 1","Segmento 2","Segmento 3")))
datos

# ACS con FactoMineR
#-------------------
# Prueba de Independencia
chisq.test(datos[-5,])
# Los segmentos son dependientes de las marcas (Pvalor<alfa)

# ACS (2 dimensiones)
library(FactoMineR)
datos.acs.s <-CA(X=datos,ncp=2,row.sup=5,graph=F)

summary(datos.acs.s)

                            
                            
                            

