n=ncol(Datos)

Cantidades=c(100,1000,10000,40000)#Cantidades de la etiqueta
Paleta=MiPaleta(as.vector(unlist(Contagios@data[n])))
PerfilesPNG(Datos, "img/confirmados/actuales/")
Mapa=MapaDeContagios(Contagios, "img/confirmados/actuales/")
saveWidget(Mapa, file = "MapaDeContagios.html", selfcontained = F)

ContagiosCPrediccion=Contagios
ContagiosCPrediccion@data=PrediccionDeDatos(Datos, 10)
Mapa3=MapaDeContagios(ContagiosCPrediccion, 10)
saveWidget(Mapa3, file = "ContagiosConPrediccion.html", selfcontained = F)

CIncrementos=Contagios
CIncrementos@data=cbind(DatosIncrementos[,1],
                        rep(-30,1), DatosIncrementos[-1])
CIncrementos@data[,-1]=CIncrementos@data[,-1]+31
Mapa2=MapaDeContagios(CIncrementos)
Mapa2
saveWidget(Mapa2, file = "DinamicaIncrementos.html", selfcontained = T)
Perfiles(DatosIncrementos, FALSE)+theme(legend.position = "right")