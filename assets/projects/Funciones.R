library(reshape2)#para perfiles
library(mapview)
library(sf)
library(leaflet)
library(leaftime)
library(geojsonio)
library(ggplot2)#Para perfiles
library(htmlwidgets)
library(geojsonlint)
library(htmltools)

Perfiles<-function(Datos, logscale)
{
  n=ncol(Datos)
  m=nrow(Datos)
  data_long<-cbind(melt(Datos, id.vars="Estado"), rep(1:(n-1), each=m))
  colnames(data_long)[c(3,4)]=c("Contagiados", "Días transcurridos")
  p <- ggplot ( data = data_long , aes ( x = `Días transcurridos` , y = Contagiados , group = Estado, col=Estado))
  p <-p + geom_line()+theme(legend.position = "none")
  if(logscale==TRUE)
  {
    data_long$value=data_long$value+1
    p<-p+coord_trans(y="log")
  }
  return(p)
}
MiPaleta<-function(Valores)
{
  FactorZonaVe=1
  FactorZonaAm=2
  FactorZonaRo=2
  SegundoMayor=Valores[order(Valores)][length(Valores)-1]
  #569 es el máximo número de colores sin repetir
  Escalado=floor(SegundoMayor/569)+1
  Colores=rainbow(1530*Escalado*FactorZonaVe)[(569*Escalado*FactorZonaVe):(290*Escalado*FactorZonaVe)]
  Colores=c(Colores, rainbow(1530*Escalado*FactorZonaAm)[(290*Escalado*FactorZonaAm):(236*Escalado*FactorZonaAm)])
  Colores=c(Colores, rainbow(1530*Escalado*FactorZonaRo)[(236*Escalado*FactorZonaRo):1])
  #Se corta
  n=length(Colores)-SegundoMayor-1
  return(Colores[n:length(Colores)])#Toma solo los últimos
}
ContagiosAColores<-function(Estado)
{
  Niveles=as.numeric(unlist(as.vector(Estado[-1])))+1
  Colores=rep(" ",n)
  TamPaleta=length(Paleta)
  for (i in 1:(n-1))
  {
    if (Niveles[i]>TamPaleta) {
      Colores[i]=Paleta[TamPaleta]
    } else { 
      Colores[i]=Paleta[Niveles[i]]
    }
  }
  Colores=substr(Colores,1,7)
  return(Colores)
}

ColorMatrix<-function(Mapa)
{
  CM<-ContagiosAColores(Mapa[1,]@data)
  m=nrow(Mapa@data)
  for (i in 2:m)
    CM<-rbind(CM, ContagiosAColores(Mapa[i,]@data))
  return(CM)
}

PerfilesPNG<-function(Datos, views)
{
  m=nrow(Datos)
  for (i in 1:m)
    ggsave(Perfiles(Datos[i,], FALSE)+ggtitle(Datos$Estado[i]),
           filename = paste(views, i,".png", sep=""),
           bg="transparent", width = 3, height = 3)
}

MapaDeContagios<-function(Shape, views)
{
  n=ncol(Shape@data)
  # add some fake start and end dates
  breweries$start <- rep(seq.Date(as.Date(PrimeraFechaRegistrada), by="days", length.out = n),n)[1:224]
  breweries$end <- breweries$start - 1
  # convert to geojson
  brew_gj <- geojsonio::geojson_json(breweries)
  bbox <- as.vector(st_bbox(breweries))
  #Extender manualmente a todos
  #Explicar que al intentar compactar usando arreglos
  #hay operadores que no se pueden sobrecargar
  #Al intentar usando listas, el mapa queda estático
  #//A MANO
  Baja_Califirnia_Sur=Shape[2,]
  Nayarit=Shape[3,]
  Jalisco=Shape[4,]
  Aguascalientes=Shape[5,]
  Guanajuato=Shape[6,]
  Queretaro=Shape[7,]
  Hidalgo=Shape[8,]
  Michoacan=Shape[9,]
  Mexico=Shape[10,]
  CDMX=Shape[11,]#Aquí es el nombre
  Colima=Shape[12,]
  Morelos=Shape[13,]
  Yucatan=Shape[14,]
  Campeche=Shape[15,]
  Puebla=Shape[16,]
  Quintana_Roo=Shape[17,]
  Tlaxcala=Shape[18,]
  Guerrero=Shape[19,]
  Oaxaca=Shape[20,]
  Tabasco=Shape[21,]
  Chiapas=Shape[22,]
  Sonora=Shape[23,]
  Chihuahua=Shape[24,]
  Coahuila=Shape[25,]
  Sinaloa=Shape[26,]
  Durango=Shape[27,]
  Zacatecas=Shape[28,]
  San_Luis_Potosi=Shape[29,]
  Nuevo_Leon=Shape[30,]
  Tamaulipas=Shape[31,]
  Veracruz=Shape[32,]
  
  #//A MANO
i=1
Estado=Shape[i,]
  m1<-mapview(Estado, col.regions = "#00FF00",
              label=sprintf(paste("<img src=\"", views, i,
                                  ".png\" style=\"width:300px;height:300px;\">", sep=""))%>%
                lapply(HTML), alpha.regions = 0.5, legend=FALSE,
              map.types="Esri.WorldImagery")
  CodigoHTML="this.layerManager.getLayerGroup('Estado')"
  
  
  
  m2<-mapview(Baja_Califirnia_Sur, col.regions = "#00FF00",
              label=sprintf(
                "<img src=\"img/2.png\" style=\"width:300px;height:300px;\">")%>%
                lapply(HTML),
              alpha.regions = 0.5, legend=FALSE)
  m3<-mapview(Nayarit, col.regions = "#00FF00",
              label=sprintf(
                "<img src=\"img/3.png\" style=\"width:300px;height:300px;\">")%>%
                lapply(HTML),
              alpha.regions = 0.5, legend=FALSE)
  m4<-mapview(Jalisco, col.regions = "#00FF00",
              label=sprintf(
                "<img src=\"img/4.png\" style=\"width:300px;height:300px;\">")%>%
                lapply(HTML),
              alpha.regions = 0.5, legend=FALSE)
  m5<-mapview(Aguascalientes, col.regions = "#00FF00",
              label=sprintf(
                "<img src=\"img/5.png\" style=\"width:300px;height:300px;\">")%>%
                lapply(HTML),
              alpha.regions = 0.5, legend=FALSE)
  m6<-mapview(Guanajuato, col.regions = "#00FF00",
              label=sprintf(
                "<img src=\"img/6.png\" style=\"width:300px;height:300px;\">")%>%
                lapply(HTML),
              alpha.regions = 0.5, legend=FALSE)
  m7<-mapview(Queretaro, col.regions = "#00FF00",
              label=sprintf(
                "<img src=\"img/7.png\" style=\"width:300px;height:300px;\">")%>%
                lapply(HTML),
              alpha.regions = 0.5, legend=FALSE)
  m8<-mapview(Hidalgo, col.regions = "#00FF00",
              label=sprintf(
                "<img src=\"img/8.png\" style=\"width:300px;height:300px;\">")%>%
                lapply(HTML),
              alpha.regions = 0.5, legend=FALSE)
  m9<-mapview(Michoacan, col.regions = "#00FF00",
              label=sprintf(
                "<img src=\"img/9.png\" style=\"width:300px;height:300px;\">")%>%
                lapply(HTML),
              alpha.regions = 0.5, legend=FALSE)
  m10<-mapview(Mexico, col.regions = "#00FF00",
               label=sprintf(
                 "<img src=\"img/10.png\" style=\"width:300px;height:300pxpx;\">")%>%
                 lapply(HTML),
               alpha.regions = 0.5, legend=FALSE)
  m11<-mapview(CDMX, col.regions = "#00FF00",
               label=sprintf(
                 "<img src=\"img/11.png\" style=\"width:300px;height:300px;\">")%>%
                 lapply(HTML),
               alpha.regions = 0.5, legend=FALSE)
  m12<-mapview(Colima, col.regions = "#00FF00",
               label=sprintf(
                 "<img src=\"img/12.png\" style=\"width:300px;height:300px;\">")%>%
                 lapply(HTML),
               alpha.regions = 0.5, legend=FALSE)
  m13<-mapview(Morelos, col.regions = "#00FF00",
               label=sprintf(
                 "<img src=\"img/13.png\" style=\"width:300px;height:300px;\">")%>%
                 lapply(HTML),
               alpha.regions = 0.5, legend=FALSE)
  m14<-mapview(Yucatan, col.regions = "#00FF00",
               label=sprintf(
                 "<img src=\"img/14.png\" style=\"width:300px;height:300px;\">")%>%
                 lapply(HTML),
               alpha.regions = 0.5, legend=FALSE)
  m15<-mapview(Campeche, col.regions = "#00FF00",
               label=sprintf(
                 "<img src=\"img/15.png\" style=\"width:300px;height:300px;\">")%>%
                 lapply(HTML),
               alpha.regions = 0.5, legend=FALSE)
  m16<-mapview(Puebla, col.regions = "#00FF00",
               label=sprintf(
                 "<img src=\"img/16.png\" style=\"width:300px;height:300px;\">")%>%
                 lapply(HTML),
               alpha.regions = 0.5, legend=FALSE)
  m17<-mapview(Quintana_Roo, col.regions = "#00FF00",
               label=sprintf(
                 "<img src=\"img/17.png\" style=\"width:300px;height:300px;\">")%>%
                 lapply(HTML),
               alpha.regions = 0.5, legend=FALSE)
  m18<-mapview(Tlaxcala, col.regions = "#00FF00",
               label=sprintf(
                 "<img src=\"img/18.png\" style=\"width:300px;height:300px;\">")%>%
                 lapply(HTML),
               alpha.regions = 0.5, legend=FALSE)
  m19<-mapview(Guerrero, col.regions = "#00FF00",
               label=sprintf(
                 "<img src=\"img/19.png\" style=\"width:300px;height:300px;\">")%>%
                 lapply(HTML),
               alpha.regions = 0.5, legend=FALSE)
  m20<-mapview(Oaxaca, col.regions = "#00FF00",
               label=sprintf(
                 "<img src=\"img/20.png\" style=\"width:300px;height:300px;\">")%>%
                 lapply(HTML),
               alpha.regions = 0.5, legend=FALSE)
  m21<-mapview(Tabasco, col.regions = "#00FF00",
               label=sprintf(
                 "<img src=\"img/21.png\" style=\"width:300px;height:300px;\">")%>%
                 lapply(HTML),
               alpha.regions = 0.5, legend=FALSE)
  m22<-mapview(Chiapas, col.regions = "#00FF00",
               label=sprintf(
                 "<img src=\"img/22.png\" style=\"width:300px;height:300px;\">")%>%
                 lapply(HTML),
               alpha.regions = 0.5, legend=FALSE)
  m23<-mapview(Sonora, col.regions = "#00FF00",
               label=sprintf(
                 "<img src=\"img/23.png\" style=\"width:300px;height:300px;\">")%>%
                 lapply(HTML),
               alpha.regions = 0.5, legend=FALSE)
  m24<-mapview(Chihuahua, col.regions = "#00FF00",
               label=sprintf(
                 "<img src=\"img/24.png\" style=\"width:300px;height:300px;\">")%>%
                 lapply(HTML),
               alpha.regions = 0.5, legend=FALSE)
  m25<-mapview(Coahuila, col.regions = "#00FF00",
               label=sprintf(
                 "<img src=\"img/25.png\" style=\"width:300px;height:300px;\">")%>%
                 lapply(HTML),
               alpha.regions = 0.5, legend=FALSE)
  m26<-mapview(Sinaloa, col.regions = "#00FF00",
               label=sprintf(
                 "<img src=\"img/26.png\" style=\"width:300px;height:300px;\">")%>%
                 lapply(HTML),
               alpha.regions = 0.5, legend=FALSE)
  m27<-mapview(Durango, col.regions = "#00FF00",
               label=sprintf(
                 "<img src=\"img/27.png\" style=\"width:300px;height:300px;\">")%>%
                 lapply(HTML),
               alpha.regions = 0.5, legend=FALSE)
  m28<-mapview(Zacatecas, col.regions = "#00FF00",
               label=sprintf(
                 "<img src=\"img/28.png\" style=\"width:300px;height:300px;\">")%>%
                 lapply(HTML),
               alpha.regions = 0.5, legend=FALSE)
  m29<-mapview(San_Luis_Potosi, col.regions = "#00FF00",
               label=sprintf(
                 "<img src=\"img/29.png\" style=\"width:300px;height:300px;\">")%>%
                 lapply(HTML),
               alpha.regions = 0.5, legend=FALSE)
  m30<-mapview(Nuevo_Leon, col.regions = "#00FF00",
               label=sprintf(
                 "<img src=\"img/30.png\" style=\"width:300px;height:300px;\">")%>%
                 lapply(HTML),
               alpha.regions = 0.5, legend=FALSE)
  m31<-mapview(Tamaulipas, col.regions = "#00FF00",
               label=sprintf(
                 "<img src=\"img/31.png\" style=\"width:300px;height:300px;\">")%>%
                 lapply(HTML),
               alpha.regions = 0.5, legend=FALSE)
  m32<-mapview(Veracruz, col.regions = "#00FF00",
               label=sprintf(
                 "<img src=\"img/32.png\" style=\"width:300px;height:300px;\">")%>%
                 lapply(HTML),
               alpha.regions = 0.5, legend=FALSE)
  
  M<-m1+m2+m3+m4+m5+m6+m7+m8
  M<-M+m9+m10+m11+m12+m13
  M<-M+m14+m15+m16+m17+m18
  M<-M+m19+m20+m21+m22+m23
  M<-M+m24+m25+m26+m27+m28
  M<-M+m29+m30+m31+m32
  M<-M@map%>%#, map.types="Esri.WorldImagery"
    #M<-M@map%>%#, map.types="Esri.WorldImagery"
    addTimeline(
      brew_gj
    ) %>%
    htmlwidgets::onRender(sprintf(paste(
      "
      function(el,x) {
      var colors = %s;
      var map = this;
      // get the timeline control
      var timeline = map.layerManager._byCategory.timeline.getLayers()[1];
      
      // use R leaflet layerManager to get Zone polygon layer group
      var Zone = [", CodigoHTML ,"]
      var Zone2 = this.layerManager.getLayerGroup('Baja_Califirnia_Sur')
      var Zone3 = this.layerManager.getLayerGroup('Nayarit')
      var Zone4 = this.layerManager.getLayerGroup('Jalisco')
      var Zone5 = this.layerManager.getLayerGroup('Aguascalientes')
      var Zone6 = this.layerManager.getLayerGroup('Guanajuato')
      var Zone7 = this.layerManager.getLayerGroup('Queretaro')
      var Zone8 = this.layerManager.getLayerGroup('Hidalgo')
      var Zone9 = this.layerManager.getLayerGroup('Michoacan')
      var Zone10 = this.layerManager.getLayerGroup('Mexico')
      var Zone11 = this.layerManager.getLayerGroup('CDMX')
      var Zone12 = this.layerManager.getLayerGroup('Colima')
      var Zone13 = this.layerManager.getLayerGroup('Morelos')
      var Zone14 = this.layerManager.getLayerGroup('Yucatan')
      var Zone15 = this.layerManager.getLayerGroup('Campeche')
      var Zone16 = this.layerManager.getLayerGroup('Puebla')
      var Zone17 = this.layerManager.getLayerGroup('Quintana_Roo')
      var Zone18 = this.layerManager.getLayerGroup('Tlaxcala')
      var Zone19 = this.layerManager.getLayerGroup('Guerrero')
      var Zone20 = this.layerManager.getLayerGroup('Oaxaca')
      var Zone21 = this.layerManager.getLayerGroup('Tabasco')
      var Zone22 = this.layerManager.getLayerGroup('Chiapas')
      var Zone23 = this.layerManager.getLayerGroup('Sonora')
      var Zone24 = this.layerManager.getLayerGroup('Chihuahua')
      var Zone25 = this.layerManager.getLayerGroup('Coahuila')
      var Zone26 = this.layerManager.getLayerGroup('Sinaloa')
      var Zone27 = this.layerManager.getLayerGroup('Durango')
      var Zone28 = this.layerManager.getLayerGroup('Zacatecas')
      var Zone29 = this.layerManager.getLayerGroup('San_Luis_Potosi')
      var Zone30 = this.layerManager.getLayerGroup('Nuevo_Leon')
      var Zone31 = this.layerManager.getLayerGroup('Tamaulipas')
      var Zone32 = this.layerManager.getLayerGroup('Veracruz')
      
      
      //A MANO
      
      timeline.on('change', function() {
      // figure out what time is current selected on timeline and select that color
      var time_selected = this.time;
      var idx = this.times.indexOf(time_selected);
      // but when playing instead of stepping times will not match exactly so in this case we will
      //   crudely bisect the array in a very inefficient way; easy to optimize if there is a need
      if(idx === -1) {
      this.times.forEach(function(d,i) {
      d <= time_selected ? idx = (i+1) : idx = idx;
      })
      }
      // A MANO
      Zone[0].setStyle({fillColor: colors[0][idx-1]});
      Zone2.setStyle({fillColor: colors[1][idx-1]});
      Zone3.setStyle({fillColor: colors[2][idx-1]});
      Zone4.setStyle({fillColor: colors[3][idx-1]});
      Zone5.setStyle({fillColor: colors[4][idx-1]});
      Zone6.setStyle({fillColor: colors[5][idx-1]});
      Zone7.setStyle({fillColor: colors[6][idx-1]});
      Zone8.setStyle({fillColor: colors[7][idx-1]});
      Zone9.setStyle({fillColor: colors[8][idx-1]});
      Zone10.setStyle({fillColor: colors[9][idx-1]});
      Zone11.setStyle({fillColor: colors[10][idx-1]});
      Zone12.setStyle({fillColor: colors[11][idx-1]});
      Zone13.setStyle({fillColor: colors[12][idx-1]});
      Zone14.setStyle({fillColor: colors[13][idx-1]});
      Zone15.setStyle({fillColor: colors[14][idx-1]});
      Zone16.setStyle({fillColor: colors[15][idx-1]});
      Zone17.setStyle({fillColor: colors[16][idx-1]});
      Zone18.setStyle({fillColor: colors[17][idx-1]});
      Zone19.setStyle({fillColor: colors[18][idx-1]});
      Zone20.setStyle({fillColor: colors[19][idx-1]});
      Zone21.setStyle({fillColor: colors[20][idx-1]});
      Zone22.setStyle({fillColor: colors[21][idx-1]});
      Zone23.setStyle({fillColor: colors[22][idx-1]});
      Zone24.setStyle({fillColor: colors[23][idx-1]});
      Zone25.setStyle({fillColor: colors[24][idx-1]});
      Zone26.setStyle({fillColor: colors[25][idx-1]});
      Zone27.setStyle({fillColor: colors[26][idx-1]});
      Zone28.setStyle({fillColor: colors[27][idx-1]});
      Zone29.setStyle({fillColor: colors[28][idx-1]});
      Zone30.setStyle({fillColor: colors[29][idx-1]});
      Zone31.setStyle({fillColor: colors[30][idx-1]});
      Zone32.setStyle({fillColor: colors[31][idx-1]});
      
      
      // could also send to Shiny here if helpful
      })
      }
      ", sep=""),
      jsonlite::toJSON(ColorMatrix(Shape), auto_unbox=TRUE)
    ))
  M<-M%>% addLegend("bottomleft",
                    labels= c(Cantidades),
                    colors =c(substr(Paleta[Cantidades],1,7)),
                    title= "Contagios confirmados de COVID 19",
                    opacity = 1)
  return(M)
}
PrediccionDeDatos<-function(Data, l)#Data es la base y l los días a futuro
{
  A=matrix(0,ncol = l-1, nrow = 32)
  m=nrow(Data)
  for (i in 1:m)
  {
    x=1:(n-11)
    y1=as.numeric(Datos[i,2:(n-10)])+1
    data=data.frame(x,y1, group="Observado")
    mod1<-glm(y1~x, data=data, family = Gamma(link="log"))
    x=1:(n-1)
    y1=as.numeric(Datos[i,-1])+1
    data=data.frame(x,y1, group="Observado")
    newdata<-data.frame(x=70:(80+l), y1=exp(predict(mod1,
              newdata = data.frame(x=70:(80+l)), interval="prediction")),
              group="Estimado")
    newdata=rbind(data, newdata)
    colnames(newdata)=c("Días transcurridos","Contagiados","CantidadContagios")
    
    PLOT<-ggplot(dat=newdata, aes(`Días transcurridos`,Contagiados, col=CantidadContagios))+
      geom_point()+
      geom_smooth(method="glm", method.args=list(family=Gamma(link = "log")), 
                  fullrange=TRUE, level=0.99, col="red") +
      xlim(1, (n+l))+theme(legend.position = c(0.4,0.7))
    #Aquí exportar la imagen
    ggsave(PLOT+ggtitle(Datos$Estado[i]),
           filename = paste("img/confirmados/predicciones/", i,".png", sep=""),
           bg="transparent", width = 3, height = 3)
    A[i,]=t(newdata[172:(n+2*l),2])#172??? de donde salió??
  }
  NewData=cbind(Data, A)
}
