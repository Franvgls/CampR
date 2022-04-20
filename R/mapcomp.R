#' Mapas de presentación de datos de una campaña comparando dos sectores/lances, creada para la campaña N21 con dos barcos, el lance 66 es el último que se hizo con el Miguel Oliver
#'
#' Crea un mapa con la distribucion en biomasa o numero para distintas el Cantábrico (dns=Cant)
#' @param gr Grupo de la especie: 1 peces, 2 crustáceos 3 moluscos 4 equinodermos 5 invertebrados
#' @param esp Código de la especie numérico o carácter con tres espacios. 999 para todas las especies del grupo
#' @param camp Campaña a representar en el mapa de un año concreto (XX): Demersales "NXX", Porcupine "PXX", Arsa primavera "1XX" y Arsa otoño "2XX"
#' @param dns Elige el origen de las bases de datos: Porcupine "Pnew", Cantábrico "Cant", Golfo de Cadiz "Arsa" (proporciona los datos para Medits pero no saca mapas)
#' @param lance Lance a partir del cual se utiliza el otro barco en principio solo para usar con la campaña de 2021
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
#' @param years Si T saca los años como nombre de campaña
#' @export
MapComp<-function(gr,esp,camp="N21",dns="Cant",lance=66,ti=T,plot=T,out.dat=F,ind="p",idi="l",
                  layout=NA,leg=T,pts=F,ceros=T,escmult=.25,cexleg=1,years=F,bw=FALSE) {
  dat<-maphist(gr,esp,camp,dns,plot=F,out.dat=T)
#  dat$peso<-dat$peso.gr/1000
  dat$barco<-cut(dat$lan,c(0,66,150),c("29MO","29VE"))
  escmult<- .05
  if (ind=="p") {
    dat$peso<-dat$peso.gr/1000
    leyenda<-signif(max(dat$peso)*.9,1)
    escala<-signif(max(dat$peso),1)*escmult }
  else {
    leyenda<-signif(max(dat$numero)*.9,1)
    escala<-signif(max(dat$numero),1)*escmult }
  # leyenda<-signif(max(dat$peso.gr)*10^c(-3)*.9,1)
  # leyenda<-signif(c(1,.5,.25)*leyenda,1)
  # escala<-signif(max(dat$peso.gr*10^c(-3)),1)*escmult
  par(mar=c(1,4,4, 2) + 0.1,oma=c(1,1,1,1))
  # nf <- layout( matrix(c(1,2), ncol =2),widths = c(3,1),heights = c(1)) #,heights=c(2.5,1)
  nf <- layout(
    matrix(c(1,2), nrow=2, byrow=TRUE),
    widths=c(1),
    heights=c(1,1)
  )
  MapNort(places=T,bw=bw)
  if (ind=="p") {
    leyenda<-signif(c(1,.5,.25)*leyenda,1)
    points(lat~long,dat,cex=sqrt(peso.gr*10^c(-3)/escala),subset=barco=="29MO",pch=21,bg="blue")
    points(lat~long,dat,cex=sqrt(peso.gr*10^c(-3)/escala),subset=barco=="29VE",pch=21,bg="green")
    points(rep(-7,3),c(43,42.6,42.2),cex=sqrt((leyenda)/escala),pch=21,col=1,bg="darkgrey")
    text(rep(-7,3),c(43,42.6,42.2),label=paste(leyenda,"kg"),pos=4,offset = 1.1,cex=1)
    }
  else {
    leyenda<-signif(c(1,.5,.25)*leyenda,1)
    points(lat~long,dat,cex=sqrt(numero/escala),subset=barco=="29MO",pch=21,bg="blue")
    points(lat~long,dat,cex=sqrt(numero/escala),subset=barco=="29VE",pch=21,bg="green")
    points(rep(-7,3),c(43,42.6,42.2),cex=sqrt((leyenda)/escala),pch=21,col=1,bg="darkgrey")
    text(rep(-7,3),c(43,42.6,42.2),label=paste(leyenda,"ind"),pos=4,offset = 1.1,cex=1)
  }
  if (leg) legend("bottomright",c("B/O Miguel Oliver","B/O Vizconde de eza"),pch=21,pt.bg=c("blue","green"),inset = c(.02,.03),bg="white")
  if (ti) title(main=buscaesp(gr,esp),cex=1,font.main=4)
  #par(mar=c(5, 4, 4, 2) + 0.1)
  if(ind=="p") {boxplot(peso~barco,dat,notch=T,outline=F,col=c("blue","green"),varwidth=T,xlab=NA,ylab="kg");title("Biomass")}
  else {boxplot(numero~barco,dat,notch=T,outline=F,col=c("blue","green"),varwidth=T,xlab=NA,ylab="Number");title("Abundance in number",line=1)}
  }

# layout(matrix(c(0,0,0,0,
#                 0,1,2,2,
#                 0,0,0,0), nc = 4, byrow = TRUE),
#        widths = c(lcm(2), 1, 1, lcm(2)),
#        heights = c(lcm(2), 1, lcm(2)))
# layout.show(2)
# box("outer", lty = "dotted")
