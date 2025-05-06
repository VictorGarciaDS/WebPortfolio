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
library(stringr) 


Perfiles<-function(Datos, logscale, ylabel)
{
  n=ncol(Datos)
  m=nrow(Datos)
  data_long<-cbind(melt(Datos, id.vars="Estado"), rep(1:(n-1), each=m))
  colnames(data_long)[c(3,4)]=c("Contagiados", "Días transcurridos")
  p <- ggplot ( data = data_long , aes ( x = `Días transcurridos` , y = Contagiados , group = Estado, col=Estado))
  p <-p + geom_line()+theme(legend.position = "none")+ylab(ylabel)
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
  FactorZonaAm=0.7
  FactorZonaRo=2
  Valores=Valores[-11,-1]
  SegundoMayor=max(Valores)#Valores[order(Valores)][length(Valores)-1]
  #569 es el máximo número de colores sin repetir
  Escalado=floor(SegundoMayor/569)+1
  Colores=rainbow(floor(1530*Escalado*FactorZonaVe))[(floor(569*Escalado*FactorZonaVe)):(floor(290*Escalado*FactorZonaVe))]
  Colores=c(Colores, rainbow(floor(1530*Escalado*FactorZonaAm))[(floor(290*Escalado*FactorZonaAm)):(floor(236*Escalado*FactorZonaAm))])
  Colores=c(Colores, rainbow(floor(1530*Escalado*FactorZonaRo))[(floor(236*Escalado*FactorZonaRo)):1])
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

PerfilesPNG<-function(Datos, views, ylabel)
{
  m=nrow(Datos)
  for (i in 1:m)
    ggsave(Perfiles(Datos[i,], FALSE, ylabel)+ggtitle(Datos$Estado[i]),
           filename = paste(views, i,".png", sep=""),
           bg="transparent", width = 3, height = 3)
}

MapaDeContagios<-function(Shape, views, titleLegend)
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
  #Se eliminan acentos y espacios para nombrar los mapas
  Estado=c()
  CodigoHTML=c()
  for (i in 1:32)
  {
    Estado[i]=iconv(gsub(" ", "_", str_to_title(Shape$Estado[i])), from="UTF-8",to="ASCII//TRANSLIT")
    assign(Estado[i], Shape[i,])
    CodigoHTML=paste(CodigoHTML, "this.layerManager.getLayerGroup('", Estado[i],"'), ", sep="")
  }
  lenght=str_length(CodigoHTML)
  CodigoHTML=substr(CodigoHTML, 1, lenght-2)
  
  #//El nombre del layer del mapview y el LayerGroup en html deben coincidir,
  #no solo como variable, ser el mismo objeto
  #data[2] es porque la primera columna es el nombre del estado
  m1<-mapview(Baja_California,
              col.regions = Paleta[as.integer(Baja_California@data[2])+1],
              label=sprintf(paste("<img src=\"", views,
              "1.png\" style=\"width:300px;height:300px;\">", sep=""))%>%
              lapply(HTML), alpha.regions = 0.5, legend=FALSE,
              map.types="Esri.WorldImagery")
  m2<-mapview(Baja_California_Sur,
              col.regions = Paleta[as.integer(Baja_California_Sur@data[2])+1],
              label=sprintf(paste("<img src=\"", views,
              "2.png\" style=\"width:300px;height:300px;\">", sep=""))%>%
              lapply(HTML), alpha.regions = 0.5, legend=FALSE,
              map.types="Esri.WorldImagery")
  m3<-mapview(Nayarit,
              col.regions = Paleta[as.integer(Nayarit@data[2])+1],
              label=sprintf(paste("<img src=\"", views,
              "3.png\" style=\"width:300px;height:300px;\">", sep=""))%>%
              lapply(HTML), alpha.regions = 0.5, legend=FALSE,
              map.types="Esri.WorldImagery")
  m4<-mapview(Jalisco,
              col.regions = Paleta[as.integer(Jalisco@data[2])+1],
              label=sprintf(paste("<img src=\"", views,
              "4.png\" style=\"width:300px;height:300px;\">", sep=""))%>%
              lapply(HTML), alpha.regions = 0.5, legend=FALSE,
              map.types="Esri.WorldImagery")
  m5<-mapview(Aguascalientes,
              col.regions = Paleta[as.integer(Aguascalientes@data[2])+1],
              label=sprintf(paste("<img src=\"", views,
              "5.png\" style=\"width:300px;height:300px;\">", sep=""))%>%
              lapply(HTML), alpha.regions = 0.5, legend=FALSE,
              map.types="Esri.WorldImagery")
  m6<-mapview(Guanajuato,
              col.regions = Paleta[as.integer(Guanajuato@data[2])+1],
              label=sprintf(paste("<img src=\"", views,
              "6.png\" style=\"width:300px;height:300px;\">", sep=""))%>%
              lapply(HTML), alpha.regions = 0.5, legend=FALSE,
              map.types="Esri.WorldImagery")
  m7<-mapview(Queretaro,
              col.regions = Paleta[as.integer(Queretaro@data[2])+1],
              label=sprintf(paste("<img src=\"", views,
              "7.png\" style=\"width:300px;height:300px;\">", sep=""))%>%
              lapply(HTML), alpha.regions = 0.5, legend=FALSE,
              map.types="Esri.WorldImagery")
  m8<-mapview(Hidalgo,
              col.regions = Paleta[as.integer(Hidalgo@data[2])+1],
              label=sprintf(paste("<img src=\"", views,
              "8.png\" style=\"width:300px;height:300px;\">", sep=""))%>%
              lapply(HTML), alpha.regions = 0.5, legend=FALSE,
              map.types="Esri.WorldImagery")
  m9<-mapview(Michoacan,
              col.regions = Paleta[as.integer(Michoacan@data[2])+1],
              label=sprintf(paste("<img src=\"", views,
              "9.png\" style=\"width:300px;height:300px;\">", sep=""))%>%
              lapply(HTML), alpha.regions = 0.5, legend=FALSE,
              map.types="Esri.WorldImagery")
  m10<-mapview(Mexico,
               col.regions = Paleta[as.integer(Mexico@data[2])+1],
              label=sprintf(paste("<img src=\"", views,
              "10.png\" style=\"width:300px;height:300px;\">", sep=""))%>%
              lapply(HTML), alpha.regions = 0.5, legend=FALSE,
              map.types="Esri.WorldImagery")
  m11<-mapview(Ciudad_De_Mexico,
               col.regions = Paleta[as.integer(Ciudad_De_Mexico@data[2])+1],
               label=sprintf(paste("<img src=\"", views,
               "11.png\" style=\"width:300px;height:300px;\">", sep=""))%>%
               lapply(HTML), alpha.regions = 0.5, legend=FALSE,
               map.types="Esri.WorldImagery")
  m12<-mapview(Colima,
               col.regions = Paleta[as.integer(Colima@data[2])+1],
               label=sprintf(paste("<img src=\"", views,
               "12.png\" style=\"width:300px;height:300px;\">", sep=""))%>%
               lapply(HTML), alpha.regions = 0.5, legend=FALSE,
               map.types="Esri.WorldImagery")
  m13<-mapview(Morelos,
               col.regions = Paleta[as.integer(Morelos@data[2])+1],
               label=sprintf(paste("<img src=\"", views,
               "13.png\" style=\"width:300px;height:300px;\">", sep=""))%>%
               lapply(HTML), alpha.regions = 0.5, legend=FALSE,
               map.types="Esri.WorldImagery")
  m14<-mapview(Yucatan,
               col.regions = Paleta[as.integer(Yucatan@data[2])+1],
               label=sprintf(paste("<img src=\"", views,
               "14.png\" style=\"width:300px;height:300px;\">", sep=""))%>%
               lapply(HTML), alpha.regions = 0.5, legend=FALSE,
               map.types="Esri.WorldImagery")
  m15<-mapview(Campeche,
               col.regions = Paleta[as.integer(Campeche@data[2])+1],
               label=sprintf(paste("<img src=\"", views,
               "15.png\" style=\"width:300px;height:300px;\">", sep=""))%>%
               lapply(HTML), alpha.regions = 0.5, legend=FALSE,
               map.types="Esri.WorldImagery")
  m16<-mapview(Puebla,
               col.regions = Paleta[as.integer(Puebla@data[2])+1],
               label=sprintf(paste("<img src=\"", views,
               "16.png\" style=\"width:300px;height:300px;\">", sep=""))%>%
               lapply(HTML), alpha.regions = 0.5, legend=FALSE,
               map.types="Esri.WorldImagery")
  m17<-mapview(Quintana_Roo,
               col.regions = Paleta[as.integer(Quintana_Roo@data[2])+1],
               label=sprintf(paste("<img src=\"", views,
               "17.png\" style=\"width:300px;height:300px;\">", sep=""))%>%
               lapply(HTML), alpha.regions = 0.5, legend=FALSE,
               map.types="Esri.WorldImagery")
  m18<-mapview(Tlaxcala,
               col.regions = Paleta[as.integer(Tlaxcala@data[2])+1],
               label=sprintf(paste("<img src=\"", views,
               "18.png\" style=\"width:300px;height:300px;\">", sep=""))%>%
               lapply(HTML), alpha.regions = 0.5, legend=FALSE,
               map.types="Esri.WorldImagery")
  m19<-mapview(Guerrero,
               col.regions = Paleta[as.integer(Guerrero@data[2])+1],
               label=sprintf(paste("<img src=\"", views,
               "19.png\" style=\"width:300px;height:300px;\">", sep=""))%>%
               lapply(HTML), alpha.regions = 0.5, legend=FALSE,
               map.types="Esri.WorldImagery")
  m20<-mapview(Oaxaca,
               col.regions = Paleta[as.integer(Oaxaca@data[2])+1],
               label=sprintf(paste("<img src=\"", views,
               "20.png\" style=\"width:300px;height:300px;\">", sep=""))%>%
               lapply(HTML), alpha.regions = 0.5, legend=FALSE,
               map.types="Esri.WorldImagery")
  m21<-mapview(Tabasco,
               col.regions = Paleta[as.integer(Tabasco@data[2])+1],
               label=sprintf(paste("<img src=\"", views,
               "21.png\" style=\"width:300px;height:300px;\">", sep=""))%>%
               lapply(HTML), alpha.regions = 0.5, legend=FALSE,
               map.types="Esri.WorldImagery")
  m22<-mapview(Chiapas,
               col.regions = Paleta[as.integer(Chiapas@data[2])+1],
               label=sprintf(paste("<img src=\"", views,
               "22.png\" style=\"width:300px;height:300px;\">", sep=""))%>%
               lapply(HTML), alpha.regions = 0.5, legend=FALSE,
               map.types="Esri.WorldImagery")
  m23<-mapview(Sonora,
               col.regions = Paleta[as.integer(Sonora@data[2])+1],
               label=sprintf(paste("<img src=\"", views,
               "23.png\" style=\"width:300px;height:300px;\">", sep=""))%>%
               lapply(HTML), alpha.regions = 0.5, legend=FALSE,
               map.types="Esri.WorldImagery")
  m24<-mapview(Chihuahua,
               col.regions = Paleta[as.integer(Chihuahua@data[2])+1],
               label=sprintf(paste("<img src=\"", views,
               "24.png\" style=\"width:300px;height:300px;\">", sep=""))%>%
               lapply(HTML), alpha.regions = 0.5, legend=FALSE,
               map.types="Esri.WorldImagery")
  m25<-mapview(Coahuila,
               col.regions = Paleta[as.integer(Coahuila@data[2])+1],
               label=sprintf(paste("<img src=\"", views,
               "25.png\" style=\"width:300px;height:300px;\">", sep=""))%>%
               lapply(HTML), alpha.regions = 0.5, legend=FALSE,
               map.types="Esri.WorldImagery")
  m26<-mapview(Sinaloa,
               col.regions = Paleta[as.integer(Sinaloa@data[2])+1],
               label=sprintf(paste("<img src=\"", views,
               "26.png\" style=\"width:300px;height:300px;\">", sep=""))%>%
               lapply(HTML), alpha.regions = 0.5, legend=FALSE,
               map.types="Esri.WorldImagery")
  m27<-mapview(Durango,
               col.regions = Paleta[as.integer(Durango@data[2])+1],
               label=sprintf(paste("<img src=\"", views,
               "27.png\" style=\"width:300px;height:300px;\">", sep=""))%>%
               lapply(HTML), alpha.regions = 0.5, legend=FALSE,
               map.types="Esri.WorldImagery")
  m28<-mapview(Zacatecas,
               col.regions = Paleta[as.integer(Zacatecas@data[2])+1],
               label=sprintf(paste("<img src=\"", views,
               "28.png\" style=\"width:300px;height:300px;\">", sep=""))%>%
               lapply(HTML), alpha.regions = 0.5, legend=FALSE,
               map.types="Esri.WorldImagery")
  m29<-mapview(San_Luis_Potosi,
               col.regions = Paleta[as.integer(San_Luis_Potosi@data[2])+1],
               label=sprintf(paste("<img src=\"", views,
               "29.png\" style=\"width:300px;height:300px;\">", sep=""))%>%
               lapply(HTML), alpha.regions = 0.5, legend=FALSE,
               map.types="Esri.WorldImagery")
  m30<-mapview(Nuevo_Leon,
               col.regions = Paleta[as.integer(Nuevo_Leon@data[2])+1],
               label=sprintf(paste("<img src=\"", views,
               "30.png\" style=\"width:300px;height:300px;\">", sep=""))%>%
               lapply(HTML), alpha.regions = 0.5, legend=FALSE,
               map.types="Esri.WorldImagery")
  m31<-mapview(Tamaulipas,
               col.regions = Paleta[as.integer(Tamaulipas@data[2])+1],
               label=sprintf(paste("<img src=\"", views,
               "31.png\" style=\"width:300px;height:300px;\">", sep=""))%>%
               lapply(HTML), alpha.regions = 0.5, legend=FALSE,
               map.types="Esri.WorldImagery")
  m32<-mapview(Veracruz,
               col.regions = Paleta[as.integer(Veracruz@data[2])+1],
               label=sprintf(paste("<img src=\"", views,
               "32.png\" style=\"width:300px;height:300px;\">", sep=""))%>%
               lapply(HTML), alpha.regions = 0.5, legend=FALSE,
               map.types="Esri.WorldImagery")
  
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
      for(i=0; i<32; i++)
      {
        Zone[i].setStyle({fillColor: colors[i][idx-1]});
      }
      
      // could also send to Shiny here if helpful
      })
      }
      ", sep=""),
      jsonlite::toJSON(ColorMatrix(Shape), auto_unbox=TRUE)
    ))
  M<-M%>% addLegend("bottomleft",
                    labels= c(Cantidades),
                    colors =c(substr(Paleta[Cantidades],1,7)),
                    title= titleLegend,
                    opacity = 1)%>%
    addLogo("assets/img/Logo_Claro_Horizontal.png", src = "local",
            position = "topleft", alpha = 1, width=250, height = 69,
            url="index.html#MapasEvolucionCOVID")
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

#Función en construcción, al intentar empaquetar, no corre bien las comparaciones de MiPaleta
CreateMap<-function(BaseDatos, views, titleLegend, file, titleFile)
{
  n=ncol(BaseDatos)
  BaseDatos=BaseDatos[a,-n]
  write.dbf(BaseDatos,"Mexico_States.dbf")
  #Visualización del mapa
  OGR<-readOGR(dsn = location, layer = "Mexico_States")
  OGR@data=BaseDatos

  n=ncol(BaseDatos)
  Paleta=MiPaleta(as.vector(unlist(OGR@data[n])))
  PerfilesPNG(BaseDatos, views)
  MapaConfirmados=MapaDeContagios(OGR, views, titleLegend)
  saveWidget(MapaConfirmados, file=file, selfcontained = F, title=titleFile)
}