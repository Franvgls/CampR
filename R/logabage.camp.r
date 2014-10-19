#' Crea gráficos de descenso de abundancias con la edad 
#' 
#' gráficos en lattice (uno por cohorte en la serie) en escala logarítimica para el seguimiento de la mortalidad por cohortes
#' @param gr Grupo de la especie: 1 peces sólo hay claves de talla para peces y cigala?
#' @param esp Código de la especie numérico o carácter con tres espacios. Sólo admite una especie por gráfica
#' @param camps Serie historica de campañas de la que se extraen los datos. Todas las campañas han de tener ALKs para las especie en cuestión
#' @param dns Elige el origen de las bases de datos: Porcupine "Pnew", Cantábrico "Cant", Golfo de Cádiz "Arsa" (proporciona los datos para Medits pero no saca mapas)
#' @param plus Edad plus: Edad considerada como plus, todas las edades mayores se suman como edad +
#' @param cor.time Si T corrige las abundancias en función de la duración del lance
#' @param clms Número de columnas del gráfico
#' @param layout parámetro con dos valores, filas y columnas del gráfico de lattice
#' @seealso logabage2.camp {\link{logabage2.camp.r}}  edadstr.camp {\link{edadstr.camp}} bubbage.camp {\link{bubbage.camp.r}}
#' @examples logabage.camp("1"," 43",Nsh,"Cant",8,0)
#' @return Saca gráficos de descenso de la abundancia a lo largo de la edad con la mortalidad total
#' @export
logabage.camp<-function(gr,esp,camps,dns="Pnew",plus=8,cor.time=T,clms=2,layout=NA) {
  library(lattice)
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
  library(lattice)
  trellis.par.set(col.whitebg())
  trellis.par.set("strip.background",list(col=c(gray(.80))))
  ndat<-length(levels(factor(cohts$cohort)))
  orden<-NULL
  ylims<-ceiling(max(abs(log(cohts$abund[which(cohts$abund>0)]))))
  if (any(is.na(layout))) {	
    ndats<-ndat+plus+1
    floorndats<-floor(sqrt(ndats))
    if (ndats-floorndats^2>floorndats) layout<-c(ceiling(sqrt(ndats)),ceiling(sqrt(ndats)))
    else layout<-c(floorndats,floorndats)
  }
  print(xyplot(log(abund)~age|factor(cohort),cohts,subset=(cohts$abund!=0),
               as.table=F,layout=layout,ylim=c(-ylims,ylims),main=list(label="Abundance along age by cohort",cex=.8),
               xlab=list(label="Age",cex=.7),ylab=list(label="Log(Abundance)",cex=.7),
               par.strip.text=list(cex=.7,font=2),scales=list(alternating=F,tck=c(1,0),cex=.6,
                                                              x=list(at=c(0,seq(1,plus,by=1))),y=list(at=seq(-ylims+1,ylims-1,by=2))),
               panel=function(x,y,...) {
                 #if (length(x)>1) panel.abline(lm(y~x,na.action=na.omit),col=4,lty=1)
                 panel.xyplot(x,y,pch=20,col=gray(.3),type="o")
                 ltext(plus-1,ylims-1,label=round(coef(lm(y~x,na.action=na.omit))[2],2),cex=.7)
                 panel.abline(lm(log(abund)~age,cohts,subset=(cohts$abund!=0))$coef,col=2,lty=2)
               }))
  print(tapply(cohts$abund,cohts[c(1,2)],sum))	
  #print(tapply(cohts$abund,cohts[c(1,4)],sum))
}
