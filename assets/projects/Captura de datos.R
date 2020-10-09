library(foreign)#para DBF
library(rgdal)#Para Shp

location=getwd()
#location="/home/victor/Documentos/Carrera/Maestría/UNAM/Investigaciones/Visualizador/"
setwd(location)
#Constantes
PrimeraFechaRegistrada="2020-03-17"#Se suma 1
UltimaFechaRegistrada="2020-10-06"

#  Descarga de datos
##  Los que ya estaban colapsados
url <- "https://github.com/ykidch/covid19_mex/archive/master.zip"
download.file(url, "covid19_mex-master.zip")
unzip("covid19_mex-master.zip")
#  Funciones

dates<-function(inicial,final)
{
  while (as.double(inicial)<=as.double(final))
  {
    if(as.double(inicial)<10)#Forza fechas en caracteres completas
      inicial=paste("0", as.double(inicial), sep = "")
    month=substr(inicial, 4, 5)
    #La siguiente condicional corrige un error de redondeo para octubre.
    if(month==1)
    {
      month=10
      inicial=paste(inicial, sep = "0", "")
    }
    urlAux<-paste(url, month, "/datos_abiertos_covid19_", inicial, ".2020.zip", sep = "")
    destAux<-paste(inicial, ".2020.zip")
    download.file(urlAux, destAux)
    unzip(destAux)
    inicial=as.character(as.double(inicial)+1)
  }
}

downloader<-function(url, final)
{
  dates("19.04", "30.04")
  dates("01.05", "31.05")
#  dates("01.06", "30.06")
#  dates("01.07", "31.07")
#  dates("01.08", "31.08")
#  dates("01.09", "30.09")
#  dates("01.10", final)
}
AjustaOrden<-function(DataFrame)
{
  ValorAuxiliar=DataFrame[5,2]
  DataFrame[5,2]=DataFrame[7,2]
  DataFrame[7,2]=DataFrame[9,2]
  DataFrame[9,2]=DataFrame[6,2]
  DataFrame[6,2]=DataFrame[8,2]
  DataFrame[8,2]=ValorAuxiliar
  return(DataFrame)
}
url<-"http://datosabiertos.salud.gob.mx/gobmx/salud/datos_abiertos/historicos/"

FormatUlFecha=paste(substr(UltimaFechaRegistrada,9,10), substr(UltimaFechaRegistrada,6,7), sep=".")
downloader(url, FormatUlFecha)
#url<- "http://187.191.75.115/gobmx/salud/datos_abiertos/historicos/datos_abiertos_covid19_20.04.2020.zip"
#url<- "http://187.191.75.115/gobmx/salud/datos_abiertos/historicos/datos_abiertos_covid19_01.05.2020.zip"
# Combina en un solo DataFrame
paths=list.files(path="covid19_mex-master/data")
n=length(paths)
Datos=read.csv(paste("covid19_mex-master/data/", paths[1],"/positivos_",paths[1], ".csv", sep = ""))
for (i in 2:n)
  Datos=merge(Datos, read.csv(paste("covid19_mex-master/data/", paths[i],"/positivos_",paths[i], ".csv", sep = "")))

m=length(list.files(pattern = "*.csv"))
print(m)
#DatosConfirmados
for (i in 1:m)
{
  Aux=read.csv(list.files(pattern = "*.csv")[i])
  Confirmados=Aux[which(Aux$RESULTADO==1),]

  DatosConfirmados= Datos[,1:2]
  colnames(DatosConfirmados)[2]=paste("X2020.", substr(list.files(pattern = "*.csv")[i], 3,4), ".", substr(list.files(pattern = "*.csv")[i], 5,6), sep="")
  for (j in 1:32)
    DatosConfirmados[j, 2]= length(which(Confirmados$ENTIDAD_UM==j))
  DatosConfirmados=AjustaOrden(DatosConfirmados)
  DatosConfirmados=merge(Datos, DatosConfirmados)
}
#DatosNegativos
for (i in 1:m)
{
  Aux=read.csv(list.files(pattern = "*.csv")[i])
  Negativos=Aux[which(Aux$RESULTADO==2),]
  
  DatosNegativos= Datos[,1:2]
  colnames(DatosNegativos)[2]=paste("X2020.", substr(list.files(pattern = "*.csv")[i], 3,4), ".", substr(list.files(pattern = "*.csv")[i], 5,6), sep="")
  for (j in 1:32)
    DatosNegativos[j, 2]= length(which(Negativos$ENTIDAD_UM==j))
  DatosNegativos=AjustaOrden(DatosNegativos)
  DatosNegativos=merge(Datos, DatosNegativos)
}
#DatosSospechosos
for (i in 1:m)
{
  Aux=read.csv(list.files(pattern = "*.csv")[i])
  Sospechosos=Aux[which(Aux$RESULTADO==3),]
  
  DatosSospechosos= Datos[,1:2]
  colnames(DatosSospechosos)[2]=paste("X2020.", substr(list.files(pattern = "*.csv")[i], 3,4), ".", substr(list.files(pattern = "*.csv")[i], 5,6), sep="")
  for (j in 1:32)
    DatosSospechosos[j, 2]= length(which(Sospechosos$ENTIDAD_UM==j))
  DatosSospechosos=AjustaOrden(DatosSospechosos)
  DatosSospechosos=merge(Datos, DatosSospechosos)
}

unlink("*.zip")
unlink("*.csv")
unlink("covid19_mex-master", recursive=TRUE)
write.csv(x = DatosConfirmados, file = "Confirmados.csv")
write.csv(x = DatosNegativos, file = "Negativos.csv")
write.csv(x = DatosSospechosos, file = "Sospechosos.csv")

### Incrementos
DatosIncrementos<-DatosConfirmados[,-n]
for (i in 2:(n-1))
  DatosIncrementos[,i]=DatosConfirmados[,i+1]-DatosConfirmados[,i]
#######Derivada<-Perfiles(DatosIncrementos, FALSE)+theme(legend.position="right")
#ggsave("Derivada.png" ,Derivada)

#DatosConfirmados=read.csv("Confirmados.csv")[,-1]
n=ncol(DatosConfirmados)
#Lectura de información del MAPA
download.file("https://tapiquen-sig.jimdofree.com/app/download/5497303759/Mexico_States.rar?t=1455822276", "States")
system("unrar e States")
DatosGeo=read.dbf("Mexico_States.dbf")
#Las siguientes 2 lineas ajustan las columnas para
#que la base coincida con el shape
a=order(order(DatosGeo$NAME))
#a[c(10,13,12,25,9,11)]=a[c(9,10,11,12,13,25)]
a[c(12,25,11)]=a[c(11,12,25)]

DatosConfirmados=DatosConfirmados[a,-n]### ¿Quita la ultima columna?
DatosNegativos=DatosNegativos[a,-n]### ¿Quita la ultima columna?
DatosSospechosos=DatosSospechosos[a,-n]### ¿Quita la ultima columna?

write.dbf(DatosConfirmados,"Mexico_States.dbf")

#Visualización del mapa
ConfirmadosOGR<-readOGR(dsn = location, layer = "Mexico_States")
ConfirmadosOGR@data=DatosConfirmados

NegativosOGR<-readOGR(dsn = location, layer = "Mexico_States")
NegativosOGR@data=DatosNegativos

SospechososOGR<-readOGR(dsn = location, layer = "Mexico_States")
SospechososOGR@data=DatosSospechosos
#####Contagios <- spTransform(Contagios, CRS("+proj=longlat +init=epsg:3857"))
