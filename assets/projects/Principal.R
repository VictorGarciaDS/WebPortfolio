            ### Confirmados
Confirmados=Confirmados[a,-ncol(Confirmados)]
write.dbf(Confirmados,"Mexico_States.dbf")
#Visualización del mapa
ConfirmadosOGR<-readOGR(dsn = location, layer = "Mexico_States")
ConfirmadosOGR@data=Confirmados
n=ncol(Confirmados)
Cantidades=c(100,1000,10000,60000)#Cantidades de la etiqueta
Paleta=MiPaleta(ConfirmadosOGR@data)
PerfilesPNG(Confirmados, "assets/img/confirmados/actuales/", "Confirmados")
MapaConfirmados=MapaDeContagios(ConfirmadosOGR, "assets/img/confirmados/actuales/", "Contagios confirmados de COVID 19")
saveWidget(MapaConfirmados, file = "Confirmados.html", selfcontained = F, title="Confirmados")

###Para las demás variables se cambia la fecha inicial###
PrimeraFechaRegistrada="2020-04-20"#Se suma 1

            ### Negativos
Negativos=Negativos[a,-ncol(Negativos)]
write.dbf(Negativos,"Mexico_States.dbf")
#Visualización del mapa
NegativosOGR<-readOGR(dsn = location, layer = "Mexico_States")
NegativosOGR@data=Negativos
n=ncol(Negativos)
Cantidades=c(1000,10000,40000,70000)#Cantidades de la etiqueta
Paleta=MiPaleta(NegativosOGR@data)
Paleta=rev(Paleta)
PerfilesPNG(Negativos, "assets/img/negativos/actuales/", "Negativos")
MapaNegativos=MapaDeContagios(NegativosOGR, "assets/img/negativos/actuales/", "Negativos a contagio de COVID 19")
saveWidget(MapaNegativos, file = "Negativos.html", selfcontained = F, title="Negativos")

            ### Sospechosos
Sospechosos=Sospechosos[a,-ncol(Sospechosos)]
write.dbf(Sospechosos,"Mexico_States.dbf")
#Visualización del mapa
SospechososOGR<-readOGR(dsn = location, layer = "Mexico_States")
SospechososOGR@data=Sospechosos
n=ncol(Sospechosos)
Cantidades=c(100,1000,10000,19000)#Cantidades de la etiqueta
Paleta=MiPaleta(SospechososOGR@data)
PerfilesPNG(Sospechosos, "assets/img/sospechosos/actuales/", "Sospechosos")
MapaSospechosos=MapaDeContagios(SospechososOGR, "assets/img/sospechosos/actuales/", "Sospechosos de contagio de COVID 19")
saveWidget(MapaSospechosos, file = "Sospechosos.html", selfcontained = F, title="Sospechosos")

            ### Defunciones
Defunciones=Defunciones[a,-ncol(Defunciones)]
write.dbf(Defunciones,"Mexico_States.dbf")
#Visualización del mapa
DefuncionesOGR<-readOGR(dsn = location, layer = "Mexico_States")
DefuncionesOGR@data=Defunciones
n=ncol(Defunciones)
Cantidades=c(100,1000,4000,9000)#Cantidades de la etiqueta
Paleta=MiPaleta(DefuncionesOGR@data)
PerfilesPNG(Defunciones, "assets/img/defunciones/actuales/", "Defunciones")
MapaDefunciones=MapaDeContagios(DefuncionesOGR, "assets/img/defunciones/actuales/", "Defunciones por COVID 19")
saveWidget(MapaDefunciones, file = "Defunciones.html", selfcontained = F, title="Defunciones")

            ### Recuperados
Recuperados=Recuperados[a,-ncol(Recuperados)]
write.dbf(Recuperados,"Mexico_States.dbf")
#Visualización del mapa
RecuperadosOGR<-readOGR(dsn = location, layer = "Mexico_States")
RecuperadosOGR@data=Recuperados
n=ncol(Recuperados)
Cantidades=c(100,1000,10000,30000)#Cantidades de la etiqueta
Paleta=MiPaleta(RecuperadosOGR@data)
Paleta=rev(Paleta)
PerfilesPNG(Recuperados, "assets/img/recuperados/actuales/", "Recuperados")
MapaRecuperados=MapaDeContagios(RecuperadosOGR, "assets/img/recuperados/actuales/", "Recuperados de COVID 19")
saveWidget(MapaRecuperados, file = "Recuperados.html", selfcontained = F, title="Recuperados")

            ### Activos
Activos=Activos[a,-ncol(Activos)]
write.dbf(Activos,"Mexico_States.dbf")
#Visualización del mapa
ActivosOGR<-readOGR(dsn = location, layer = "Mexico_States")
ActivosOGR@data=Activos
n=ncol(Activos)
Paleta=MiPaleta(ActivosOGR@data)
Cantidades=c(100,1000,2000,3000)#Cantidades de la etiqueta
PerfilesPNG(Activos, "assets/img/activos/actuales/", "Activos")
MapaActivos=MapaDeContagios(ActivosOGR, "assets/img/activos/actuales/", "Contagios activos de COVID 19")
saveWidget(MapaActivos, file = "Activos.html", selfcontained = F, title="Activos")







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