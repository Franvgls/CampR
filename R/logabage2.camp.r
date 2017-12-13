#' Crea gráficos de descenso de abundancias con la edad 
#' 
#' Tipico gráfico en escala logarítimica para el seguimiento de la mortalidad por cohortes Todas las cohortes en un mísmo gráfico
#' @param gr Grupo de la especie: 1 peces sólo hay claves de talla para peces y cigala?
#' @param esp Código de la especie numérico o carácter con tres espacios. Sólo admite una especie por gráfica
#' @param camps Serie historica de campañas de la que se extraen los datos. Todas las campañas han de tener ALKs para las especie en cuestión
#' @param dns Elige el origen de las bases de datos: Porcupine "Porc" o "Pnew", Cantábrico "Cant", Golfo de Cádiz "Arsa" (proporciona los datos para Medits pero no saca mapas)
#' @param plus Edad plus: Edad considerada como plus, todas las edades mayores se suman como edad +
#' @param cor.time Si T corrige las abundancias en función de la duración del lance
#' @param ti Si T muestra el título con el nombre de la especie (y el sector si se elige sect o excl.sect si falso no muestra nada. También se puede dar un valor y aparece el texto incluido o dar un list(label=(título), font, cex)
#' @param layout parámetro con dos valores, filas y columnas del gráfico de lattice, si NA lattice rellena la pantalla para optimizar la vista l*c
#' @seealso {\link{logabage.camp}}, {\link{edadstr.camp}}, {\link{bubbage.camp}}, {\link{logabage.camp}}
#' @examples logabage2.camp("1"," 43",Nsh,"Cant",8)
#' @return Saca gráficos de descenso de la abundancia a lo largo de la edad con la mortalidad total
#' @family edades
#' @export
logabage2.camp<-function(gr,esp,camps,dns="Porc",plus=8,cor.time=TRUE,ti=FALSE,layout=NA) {
  if (length(esp)>1) {
    stop("Sólo se puede incluir una especie en esta función")
  }
  esp<-format(esp,width=3,justify="r")
  ndat<-length(camps)
  cohts<-data.frame(age=NULL,year=NULL,abund=NULL)
  for (i in 1:length(camps)) {
    if (camps[i]=="N87") cohts<-rbind(cohts,data.frame(age=0:plus,year=1987,abund=rep(NA,plus+1)))
    else {			
      anyo<-ifelse(as.numeric(substr(camps[i],2,3))>50,1900,2000)+as.numeric(substr(camps[i],2,3))
      cohts<-rbind(cohts,data.frame(age=0:plus,year=anyo,abund=edadstr.camp(gr,esp,camps[i],dns,plus,cor.time=cor.time)$total))
    }
  }
  cohts$cohort<-cohts$year-cohts$age
  if (ti) {
    especie<-buscaesp(gr,esp)
  }
  #tapply(cohts$abund,cohts[,c("year","age")],log)
  plot(log(abund)~year,cohts,xlim=c(range(year)[1]-1,range(year)[2]),type="p",pch=16)
  if (ti) title(main=especie,font.main=4)
  for (i in levels(factor(cohts$cohort))) {
    lines(cohts$year[cohts$cohort==i],log(cohts$abund[cohts$cohort==i]),type="o",pch=21,col=1,bg=i)
    lines(cohts$year[cohts$cohort==i],log(cohts$abund[cohts$cohort==i]),type="l",lwd=2,col=i)
  }
  text(cohts$year[cohts$year==1983],log(cohts$abund[cohts$year==1983]),label=cohts$cohort[cohts$year==1983],cex=.7,font=2,pos=2)
}
