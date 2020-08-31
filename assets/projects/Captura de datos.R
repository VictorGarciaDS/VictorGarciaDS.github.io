library(foreign)#para DBF
library(rgdal)#Para Shp

location=getwd()
location="/home/victor/Documentos/Carrera/Maestría/UNAM/Investigaciones/Visualizador/"
setwd(location)
#Constantes
PrimeraFechaRegistrada="2020-03-17"#Se suma 1
UltimaFechaRegistrada="2020-08-26"

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
  dates("01.06", "30.06")
  dates("01.07", "31.07")
  dates("01.08", final)
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
for (i in 1:m)
{
  Aux=read.csv(list.files(pattern = "*.csv")[i])
  Aux=Aux[which(Aux$RESULTADO==1),]#1 ES CONFIRMADOS
  A=Datos[,1:2]
  colnames(A)[2]=paste("X2020.", substr(list.files(pattern = "*.csv")[i], 3,4), ".", substr(list.files(pattern = "*.csv")[i], 5,6), sep="")
  for (j in 1:32)
    A[j, 2]=length(which(Aux$ENTIDAD_UM==j))
  A=AjustaOrden(A)
  Datos=merge(Datos, A)
}
unlink("*.zip")
unlink("*.csv")
write.csv(x = Datos, file = "aux.csv")

### Incrementos
DatosIncrementos<-Datos[,-n]
for (i in 2:(n-1))
  DatosIncrementos[,i]=Datos[,i+1]-Datos[,i]
Derivada<-Perfiles(DatosIncrementos, FALSE)+theme(legend.position="right")
#ggsave("Derivada.png" ,Derivada)

#Datos=read.csv("aux.csv")[,-1]
n=ncol(Datos)
#Lectura de información del MAPA
download.file("https://tapiquen-sig.jimdofree.com/app/download/5497303759/Mexico_States.rar?t=1455822276", "States")
system("unrar e States")
Datos2=read.dbf("Mexico_States.dbf")
#Las siguientes 2 lineas ajustan las columnas para
#que la base coincida con el shape
a=order(order(Datos2$NAME))
#a[c(10,13,12,25,9,11)]=a[c(9,10,11,12,13,25)]
a[c(12,25,11)]=a[c(11,12,25)]

Datos=Datos[a,-n]
write.dbf(Datos,"Mexico_States.dbf")

#Visualización del mapa
Contagios<-readOGR(dsn = location, layer = "Mexico_States")
Contagios@data=Datos
Contagios <- spTransform(Contagios, CRS("+proj=longlat +init=epsg:3857"))