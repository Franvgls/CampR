#' Mapas de presentación de datos de una campaña comparando dos sectores/lances
#'
#' Crea mapas con la distribucion en biomasa o numero para distintas zonasPorcupine (dns="Pnew"), el Cantábrico (dns=Cant)
#' @param gr Grupo de la especie: 1 peces, 2 crustáceos 3 moluscos 4 equinodermos 5 invertebrados
#' @param esp Código de la especie numérico o carácter con tres espacios. 999 para todas las especies del grupo
#' @param camps Campaña a representar en el mapa de un año concreto (XX): Demersales "NXX", Porcupine "PXX", Arsa primavera "1XX" y Arsa otoño "2XX"
#' @param dns Elige el origen de las bases de datos: Porcupine "Pnew", Cantábrico "Cant", Golfo de Cadiz "Arsa" (proporciona los datos para Medits pero no saca mapas)
#' @param bw Gráfico en blanco en negro si T o en color si F
#' @param ti Añade el nombre de la especie en latín sin T, si F no añade titulo
#' @param plot Saca el gráfico (T) o lo guarda como objeto para componer con otros gráficos (F)
#' @param out.dat Si T el resultado final de la función es la figura en pantalla, pero los datos en objeto
#' @param ind Parámetro a representar saca los datos en "p"eso o "n"úmero
#' @param idi Nombre científico de la especie ("l") o nombre común ("e")
#' @param layout Organización de gráficos en filas ó columnas c(r,c)
#' @param leg Si T añade la leyenda
#' @param pts Saca los puntos de los datos
#' @param ceros Añade puntos para los datos igual a 0 si T, si F no incluye x en los valores 0
#' @param escmult Varía la relación de tamaño de los puntos con la leyenda y el máximo en los datos
#' @param cexleg Varía el tamaño de letra de los ejes y del número de la leyenda
#' @param years Si T saca los años como nombre de campaña en los paneles lattice de campañas
#' @export
mapcomp<-function(gr,esp,camp="N21",dns="Cant",lance=66,bw=T,ti=T,plot=T,out.dat=F,ind="p",idi="l",
                  layout=NA,leg=T,pts=F,ceros=T,escmult=.25,cexleg=1,years=F) {
  dat<-maphist(gr,esp,camp,dns,plot=F,out.dat=T)
  dat$barco<-cut(dat$lan,c(0,66,150),c("29MO","29VE"))
  escmult<- .05
  leyenda<-signif(max(dat$peso.gr)*10^c(-3)*.9,1)
  leyenda<-signif(c(1,.5,.25)*leyenda,1)
  escala<-signif(max(dat$peso.gr*10^c(-3)),1)*escmult
  #windows()
  MapNort(places=T,bw=F)
  points(lat~long,dat,cex=sqrt(peso.gr*10^c(-3)/escala),subset=barco=="29VE",pch=21,bg="blue")
  points(lat~long,dat,cex=sqrt(peso.gr*10^c(-3)/escala),subset=barco=="29MO",pch=21,bg="green")
  points(rep(-7,3),c(43,42.6,42.2),cex=sqrt((leyenda)/escala),pch=21,col=1,bg="darkgrey")
  text(rep(-7,3),c(43,42.6,42.2),label=paste(leyenda,"kg"),pos=4,offset = 1.1,cex=1)
  }

