#Código para el Trabajo Fin de Máster: Propuesta del Plan de Contingencia ante el Cambio Climático en México.
#Tania Reyes Léon

#4.1.3	Datos de temperatura y precipitación fluvial:
temp<-read.table("Temperatura_promedio_Mex_1990_2012.csv", sep=",", header=T, na.strings=c("?"))
str(temp)#vemos el tipo de datos y una muestra de cada variable:
names(temp)# Verificamos el nombre de las columnas
dim(temp) # Revisamos él número de registros
tail(temp) # Observamos el final del dataset
temperatura<-temp$Temperature..C
precipitacion<-rain$Rainfall...#variable de precipitación fluvial
anio<-temp$X.Year
mes<-temp$Month
pais<-temp$Country
temp_precip<-data.frame(temperatura,precipitacion,anio,mes)#un sólo dataset
 View(temp_precip)
#4.1.4	Indicadores mundiales en cambio climático: México
ind_en_columnas<-as.data.frame(read.csv(file="Indicadores_bmd.csv",header=F))
# cargamos los datos sin cabecera
ind<-t(ind_en_columnas)#con esta instrucción transpongo el dataframe(tabla)
#Realizamos lo vectores que nos interesan para posteriormente crear el dataframe completo:
ANIO<-ind$V1
POBLACION_TOTAL<-ind$V2
EXT_AGUA_DULCE_BMC<-ind$V3
EMI_CO2_TMP<-ind$V4
EMI_CO2_KT<-ind$V5
USO_ENERGIA_KGPET<-ind$V6
ELEC_FTE_RENOVABLE<-ind$V7
ELEC_FTE_PETROLE<-ind$V8
ELEC_FTE_NUCLEAR<-ind$V9
ELEC_FTE_GAS_NAT<-ind$V10
ELEC_FTE_HIDRO<-ind$V11
ELEC_FTE_CARBON<-ind$V12
AREA_SEV_KM2<-ind$V13
ind<-data.frame(ANIO, POBLACION_TOTAL, EXT_AGUA_DULCE_BMC, EMI_CO2_TMP, EMI_CO2_KT, USO_ENERGIA_KGPET, ELEC_FTE_RENOVABLE, ELEC_FTE_PETROLE, ELEC_FTE_NUCLEAR, ELEC_FTE_GAS_NAT, ELEC_FTE_HIDRO, ELEC_FTE_CARBON, AREA_SEV_KM2)
# Exploramos el tipo de datos
str(ind)
names(ind)# nombre de las columnas
dim(ind) # de registros
View(ind)

#4.1.5	Datos globales de derrumbes en México desde enero 2007 a febrero 2016

derr<-read.csv(file="Global_Landslide_Catalog_Export.csv",header=T) 
str(derr) #exploramos los datos
dim(derr) # de registros
#seleccionamos sólo los datos de México
der_mex<-which(derr$"country"=="Mexico", arr.ind=T)
derrumbes_mex<-derr[der_mex,]
derrumbes_mex
#Renombramos columnas
FECHA_DERR<-derrumbes_mex$date
HORA_DERR<-derrumbes_mex$time
DESENCADENO_DERR<-derrumbes_mex$trigger
NOMBRE_TORMENTA<-derrumbes_mex$storm_name
MUERTES_DERR<-derrumbes_mex$fatalities
TAMAÑO_DERR<-derrumbes_mex$landslide_size
ESTADO_POLITICO<-derrumbes_mex$adminname1
POBLACION<-derrumbes_mex$population
LATITUD<-derrumbes_mex$latitude	
LONGITUD<-derrumbes_mex$longitude
derrumbes<-#creamos dataframe de interés
  data.frame(FECHA_DERR,HORA_DERR,DESENCADENO_DERR,NOMBRE_TORMENTA,MUERTES_DERR,TAMAÑO_DERR,ESTADO_POLITICO,POBLACION,LATITUD,LONGITUD)
dim(derrumbes)# de registros
View(derrumbes)#exploramos el dataset final:

#4.1.6	Índices de producción Agricultura y de Alimentación
#Primero cargamos el archivo correspondiente a agricultura
ag<-read.csv(file="UNdata_Export_20170222_184527309_Agriculture.csv",header=T)
agr<-which(ag$"Element"=="Net Production 1999-2001 (1000 I$)", arr.ind=T)
agri<-ag[agr,]
#Luego lo filtramos por el elemento de interés producción neta de 1991-2001: Net Production 1999-2001 (1000 I$)

#creamos las variables cod_element y year

cod_element<-agri$Element.Code
year<-agri$Year

#creamos la variable referida a la métrica que nos proporciona este archivo en esta caso agricultura
agricultura<-agri$Value
agricultura
#Cargamos el archivo correspondiente a semillas
ag<-read.csv(file="UNdata_Export_20170222_184734444_Semillas.csv",header=T)
agr<-which(ag$"Element"=="Net Production 1999-2001 (1000 I$)", arr.ind=T)
agri<-ag[agr,]

semillas<-agri$Value
semillas

Cargamos el archivo correspondiente a alimentación
ag<-read.csv(file="UNdata_Export_20170222_185018865_Alimetacion.csv",header=T)
agr<-which(ag$"Element"=="Net Production 1999-2001 (1000 I$)", arr.ind=T)
agri<-ag[agr,]
#creamos la variable referida a la métrica que nos proporciona este archivo
alimentacion<-agri$Value
alimentacion
#Cargamos el archivo correspondiente a ganado
ag<-read.csv(file="UNdata_Export_20170222_185152538_Ganado.csv",header=T)
agr<-which(ag$"Element"=="Net Production 1999-2001 (1000 I$)", arr.ind=T)
agri<-ag[agr,]
#creamos la variable referida a la métrica que nos proporciona este archivo
ganado<-agri$Value
ganado

#Cargamos el archivo correspondiente a hambre
ag<-read.csv(file="UNdata_Export_20170222_185257541_Hambre.csv",header=T)
agr<-which(ag$"Element"=="Net Production 1999-2001 (1000 I$)", arr.ind=T)
agri<-ag[agr,]
#creamos la variable referida a la métrica que nos proporciona este archivo
hambre<-agri$Value
hambre
#creamos un sólo dataframe con las variable de interés:
ind_agricultura<-data.frame(cod_element,year,agricultura,semillas,cultivo,alimentacion,ganado,hambre)
dim(ind_agricultura)
View(ind_agricultura)

#4.1.7	Información referente a las ventas y suministro de energía eléctrica a usuarios de cada municipio por entidad federativa en México.
cfe<-read.csv(file="UsuariosEnConsumoElectricoPorMunicipio.csv",header=T) 
# carga con cabecera
str(cfe) #exploramos los datos
dim(cfe) # de registros
#seleccionamos sólo los datos de México
cfe_tot<-which(cfe$"Tipo"=="TOTAL", arr.ind=T)
cfe_tot_mex<-cfe[cfe_tot,]
cfe_tot_mex #Renombramos columnas
ENTIDAD <- cfe_tot_mex $Entidad_Federativa
MUNICIPIO<- cfe_tot_mex $Municipio 
ANUAL<- cfe_tot_mex $Anual
#creamos dataframe de interés
cfe<-data.frame(ENTIDAD,MUNICIPIO,ANUAL)
dim(cfe)# de registros
View(cfe)

#4.2	Análisis de los Datos:
#4.2.1.1	Temperatura y precipitación fluvial:

hist(temp_precip$temperatura, breaks=10)
hist(temp_precip$precipitacion, breaks=30)
#seleccionando Agosto
temp_precip_agos<-temp_precip[(temp_precip$"mes"=="August"),]
View(temp_precip_agos)
#Dibujamos el histograma de temperatura para el mes de Agosto
hist(temp_precip_agos$temperatura, breaks=10)
#Dibujamos el histograma de precipitación para el mes de Agosto
hist(temp_precip_agos$precipitacion, breaks=10)
#Código utilizado para graficar el diagrama de caja y brazos
boxplot(temp_precip_agos$temperatura,col = "orange")
text(25, , "min=25.1")
text(25.2, , "min=25.1")
text(26.6, , "max=26.7")
text(25.6, ,"Q1=25.5")
text(25.8, , "M=25.7")
text(26, , "Q3=26")
boxplot(temp_precip_agos$precipitacion,col = "cyan")
text(113.1, ,"Q1=113.22")
text(131, , "M=131.08")
text(148.1, , "Q3=148.26")
text(97.5, , "min=97.4")
text(180.7, , "max=180.8")

#Temperatura a través del tiempo. 
install.packages("plotly") 
library(plotly)
install.packages('DBI', dependencies = TRUE)
ggplot(subset(temp_precip, mes %in% c("August")),
        aes(x=anio, y=temperatura, colour=mes)) +
  geom_line()


ggplot(subset(temp_precip, mes %in% c("August")),
        aes(x=anio, y=temperatura, colour=mes)) +
  geom_line()
#precipitación fluvial a través del tiempo: 

ggplot(subset(temp_precip, mes %in% c("August")),
        aes(x=anio, y=precipitacion, colour=mes)) +
  geom_line()

#4.2.1.2	Indicadores mundiales en cambio climático: México
#Código utilizado para graficar la tendencia histórica
ggplot(ind, aes(ANIO, POBLACION_TOTAL)) + geom_boxplot()
ggplot(ind, aes(ANIO, EMI_CO2_TMP)) + geom_boxplot()
ggplot(ind, aes(ANIO, EMI_CO2_KT)) + geom_boxplot()
ggplot(ind, aes(ANIO, USO_ENERGIA_KGPET)) + geom_boxplot()
ggplot(ind, aes(ANIO, ELEC_FTE_RENOVABLE)) + geom_boxplot()
ggplot(ind, aes(ANIO, ELEC_FTE_PETROLE)) + geom_boxplot()
ggplot(ind, aes(ANIO, ELEC_FTE_NUCLEAR)) + geom_boxplot()
ggplot(ind, aes(ANIO, ELEC_FTE_GAS_NAT)) + geom_boxplot()
ggplot(ind, aes(ANIO, ELEC_FTE_HIDRO)) + geom_boxplot()
ggplot(ind, aes(ANIO, ELEC_FTE_CARBON)) + geom_boxplot()
ggplot(ind, aes(ANIO, AREA_SEV_KM2)) + geom_boxplot()

#4.2.1.3	Datos de globales de derrumbes en México:
#Histograma derrumbes por tamaño
ggplot(derrumbes, aes(reorder_size(FECHA_DERR))) + geom_bar(aes(fill = TAMAÑO_DERR))
#Derrumbes distribución por Estado y tamaño
ggplot(derrumbes, aes(reorder_size(ESTADO_POLITICO))) + geom_bar(aes(fill = TAMAÑO_DERR))
#Distribución derrumbes por Estado y Tipo que lo desencadeno
ggplot(derrumbes, aes(reorder_size(ESTADO_POLITICO))) + geom_bar(aes(fill = DESENCADENO_DERR))
#Distribución derrumbes por tamaño y motivo que los desencadeno
ggplot(derrumbes, aes(reorder_size(DESENCADENO_DERR))) + geom_bar(aes(fill = TAMAÑO_DERR))

#4.2.1.5	Información suministros de energía eléctrica en México
#DG barras para combinar una variable discreta (Estado político)con una variable continua (#suministros):
ggplot(cfe_tot_mex2, aes(ENTIDAD, ANUAL)) + geom_bar(stat = "identity")

#4.2.4	Relaciones entre variables de interés.
library(dplyr)#. Para filtros, cÃ¡lculos y agregaciÃ³n de datos.
library(tidyr)#. ManipulaciÃ³n de datos
#Independencia entre variables:
#Primero creamos la tabla de contingencia:
  # Creamos la tabla de contingencia
tabla<-table(temp_precip$temperatura, temp_precip$precipitacion)
chisq.test(tabla)

#Correlación 
cor.test(data1$temperatura, data1$precipitacion)
#Regresión lineal
modelo<-lm(precipitacion  ~ temperatura, data=temp_precip)
summary(modelo)
modelo
plot(temp_precip$temperatura, temp_precip$precipitacion )
abline(modelo)

#Regresión lineal para predecir valores
cor.test(resumen3$poblacion, resumen3$anio)
modelo_pob<-lm(poblacion ~ anio, data=resumen3)
summary(modelo_pob)
modelo_pob

#4.3	Determinación de la tendencia de los principales KPI's:
#4.3.1	Población Total

nuevas.anio<- data.frame(anio = seq(2013, 2018))
ic_pob <- predict(modelo_pob, nuevas.anio, interval = "prediction")
poblacion_pred<-  ic_pob[, 1]
plot(seq(1990, 2018), append(poblacion,poblacion_pred))
lines(nuevas.prueba$anio, ic_pob[, 1], lty = 2,col = "green")
lines(nuevas.anio$anio, ic_pob[, 1], lty = 2,col = "green")
lines(nuevas.anio$anio, ic_pob[, 2], lty = 2, col = "red")
lines(nuevas.anio$anio, ic_pob[, 3], lty = 2, col = "red")
#El pronóstico de la población se encuentra en la parte verde de la gráfica mientras que las #líneas punteadas rojas muestran el intervalo de confianza para el modelo utilizado.

#######4.4	Evaluación de los Resultados
#4.4.1	Análisis de residuales para el modelo de regresión lineal

modelo_pob<-lm(poblacion ~ anio, data=resumen3)
summary(modelo_pob)
confint(modelo_pob)
confint(modelo_pob, level = 0.9)
anova(modelo_pob)

residuos <- rstandard(modelo_pob)
valores.ajustados <- fitted(modelo_pob)
plot(valores.ajustados, modelo_pob)
qqnorm(residuos)
qqline(residuos)

#4.4.2	Evaluación de los modelos utilizados para predecir valores futuros

plot(resumen3$anio, resumen3$poblacion)
nuevas.prueba<- data.frame(anio = seq(1990, 2012))
ic_prueba_pob <- predict(modelo_pob, nuevas.prueba, interval = "confidence")
lines(nuevas.prueba$anio, ic_prueba_pob[, 1], lty = 2)

