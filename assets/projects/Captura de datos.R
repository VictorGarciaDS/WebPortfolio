library(foreign)#para DBF
library(rgdal)#Para Shp

location=getwd()
location="/home/victor/Documentos/Carrera/Maestría/PaginaPersonal/VictorGarciaDS.github.io/"
setwd(location)
#Constantes
PrimeraFechaRegistrada="2020-03-17"#Se suma 1
UltimaFechaRegistrada="2020-12-31"

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
  dates("01.06", "30.06")
  dates("01.07", "31.07")
  dates("01.08", "31.08")
  dates("01.09", "30.09")
  dates("01.10", "31.10")
  dates("01.11", "30.11")
  dates("01.12", final)
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
Confirmados=read.csv(paste("covid19_mex-master/data/", paths[1],"/positivos_",paths[1], ".csv", sep = ""))
for (i in 2:n)
  Confirmados=merge(Confirmados, read.csv(paste("covid19_mex-master/data/", paths[i],"/positivos_",paths[i], ".csv", sep = "")))

Activos=Recuperados=Defunciones=Negativos=Sospechosos=Confirmados

m=length(list.files(pattern = "*.csv"))
for (i in 1:m)
{
  Aux=read.csv(list.files(pattern = "*.csv")[i])
  Actividad=as.Date(Aux$FECHA_ACTUALIZACION[1])-14
  print(paste("Leyendo", Actividad+14))

  AuxConfirmados=Aux[which(Aux$RESULTADO==1),]
  AuxNegativos=Aux[which(Aux$RESULTADO==2),]
  AuxSospechosos=Aux[which(Aux$RESULTADO==3),]
  AuxDefunciones=AuxConfirmados[which(AuxConfirmados$FECHA_DEF!="9999-99-99"),]
  AuxRecuperados=AuxConfirmados[which(AuxConfirmados$TIPO_PACIENTE==1),]
  AuxRecuperados=AuxRecuperados[which(AuxRecuperados$FECHA_DEF=="9999-99-99"),]
  AuxRecuperados=AuxRecuperados[which(as.Date(AuxRecuperados$FECHA_SINTOMAS)<(Actividad+1)),]
  AuxActivos=AuxConfirmados[which(as.Date(AuxConfirmados$FECHA_SINTOMAS)>Actividad),]
  if (Actividad>as.Date("2020-11-12"))
  {
    AuxConfirmados=Aux[which(Aux$RESULTADO_LAB==1),]
    AuxNegativos=Aux[which(Aux$RESULTADO_LAB==2),]
    AuxSospechosos=Aux[which(Aux$RESULTADO_LAB==3),]
    AuxDefunciones=AuxConfirmados[which(AuxConfirmados$FECHA_DEF!="9999-99-99"),]
    AuxRecuperados=AuxConfirmados[which(AuxConfirmados$TIPO_PACIENTE==1),]
    AuxRecuperados=AuxRecuperados[which(AuxRecuperados$FECHA_DEF=="9999-99-99"),]
    AuxRecuperados=AuxRecuperados[which(as.Date(AuxRecuperados$FECHA_SINTOMAS)<(Actividad+1)),]
    AuxActivos=AuxConfirmados[which(as.Date(AuxConfirmados$FECHA_SINTOMAS)>Actividad),]
  }
#    AuxActivos=AuxActivos[which(AuxActivos$TIPO_PACIENTE==1),]

  CargaActivos=CargaRecuperados=CargaDefunciones=CargaConfirmados=CargaNegativos=CargaSospechosos=Confirmados[,1:2]

  colnames(CargaConfirmados)[2]=paste("X2020.", substr(list.files(pattern = "*.csv")[i], 3,4), ".", substr(list.files(pattern = "*.csv")[i], 5,6), sep="")
  colnames(CargaNegativos)[2]=paste("X2020.", substr(list.files(pattern = "*.csv")[i], 3,4), ".", substr(list.files(pattern = "*.csv")[i], 5,6), sep="")
  colnames(CargaSospechosos)[2]=paste("X2020.", substr(list.files(pattern = "*.csv")[i], 3,4), ".", substr(list.files(pattern = "*.csv")[i], 5,6), sep="")
  colnames(CargaDefunciones)[2]=paste("X2020.", substr(list.files(pattern = "*.csv")[i], 3,4), ".", substr(list.files(pattern = "*.csv")[i], 5,6), sep="")
  colnames(CargaRecuperados)[2]=paste("X2020.", substr(list.files(pattern = "*.csv")[i], 3,4), ".", substr(list.files(pattern = "*.csv")[i], 5,6), sep="")
  colnames(CargaActivos)[2]=paste("X2020.", substr(list.files(pattern = "*.csv")[i], 3,4), ".", substr(list.files(pattern = "*.csv")[i], 5,6), sep="")

  for (j in 1:32)
  {
    CargaConfirmados[j, 2]=length(which(AuxConfirmados$ENTIDAD_UM==j))
    CargaNegativos[j, 2]=length(which(AuxNegativos$ENTIDAD_UM==j))
    CargaSospechosos[j, 2]=length(which(AuxSospechosos$ENTIDAD_UM==j))
    CargaDefunciones[j, 2]=length(which(AuxDefunciones$ENTIDAD_UM==j))
    CargaRecuperados[j, 2]=length(which(AuxRecuperados$ENTIDAD_UM==j))
    CargaActivos[j, 2]=length(which(AuxActivos$ENTIDAD_UM==j))
  }
  CargaConfirmados=AjustaOrden(CargaConfirmados)
  CargaNegativos=AjustaOrden(CargaNegativos)
  CargaSospechosos=AjustaOrden(CargaSospechosos)
  CargaSospechosos=AjustaOrden(CargaSospechosos)
  CargaDefunciones=AjustaOrden(CargaDefunciones)
  CargaRecuperados=AjustaOrden(CargaRecuperados)
  CargaActivos=AjustaOrden(CargaActivos)

  Confirmados=merge(Confirmados, CargaConfirmados)
  Negativos=merge(Negativos, CargaNegativos)
  Sospechosos=merge(Sospechosos, CargaSospechosos)
  Defunciones=merge(Defunciones, CargaDefunciones)
  Recuperados=merge(Recuperados, CargaRecuperados)
  Activos=merge(Activos, CargaActivos)
}
Negativos=Negativos[,-(2:35)]#
Sospechosos=Sospechosos[,-(2:35)]#
Defunciones=Defunciones[,-(2:35)]#
Recuperados=Recuperados[,-(2:35)]#
Activos=Activos[,-(2:35)]#

unlink("*.zip")
unlink("*.csv")
unlink("covid19_mex-master", recursive=TRUE)
write.csv(x = Confirmados, file = "Confirmados.csv")
write.csv(x = Negativos, file = "Negativos.csv")
write.csv(x = Sospechosos, file = "Sospechosos.csv")
write.csv(x = Defunciones, file = "Defunciones.csv")
write.csv(x = Recuperados, file = "Recuperados.csv")
write.csv(x = Activos, file = "Activos.csv")

### Incrementos
#DatosIncrementos<-Confirmados[,-n]
#for (i in 2:(n-1))
#  DatosIncrementos[,i]=Confirmados[,i+1]-Confirmados[,i]
#######Derivada<-Perfiles(DatosIncrementos, FALSE)+theme(legend.position="right")
#ggsave("Derivada.png" ,Derivada)

#Datos=read.csv("aux.csv")[,-1]
#Lectura de información del MAPA
download.file("https://tapiquen-sig.jimdofree.com/app/download/5497303759/Mexico_States.rar?t=1455822276", "States")
system("unrar e States")
Datos2=read.dbf("Mexico_States.dbf")
#Las siguientes 2 lineas ajustan las columnas para
#que la base coincida con el shape
a=order(order(Datos2$NAME))
#a[c(10,13,12,25,9,11)]=a[c(9,10,11,12,13,25)]
a[c(12,25,11)]=a[c(11,12,25)]
