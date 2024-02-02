#' Información anual sobre cigala en la FU31 en el Cantábrico
#'
#' Función de acceso a datos:
#' Extrae las características de los lances para una campaña determinada y las capturas de cigala por unidad funcional
#'
#' Un problema que ocurre al utilizar el CampR con ficheros dbf de las primeras campañas
#' puede ser que al fichero lanceXXX.dbf le falte algún campo, habitualmente
#' el campo **ESTN** utilizado en las últimas versiones del **CAMP** para ligar lances con las estaciones de CTD.
#' El error usual es **$ operator is invalid for atomic vectors**
#' Si se detecta este error revisar la estructura de lanceXXX.dbf con la de
#' otros ficheros de lances de los últimos años
#'
#' @param camp Campaña de la que se extraen los datos: año comcreto (XX): Demersales "NXX"
#' @param dns Sólo disponible para el Cantábrico "Cant", combinados con "dnsred" busca los datos en el servidor de Santander si se han creado las RODBCs
#' @param year si T incluye una columna con el año al final de los datos
#' @param plot Saca el gráfico (T) o lo omite para dejar sólo los datos (F)
#' @param es si T letreros en español, si F en inglés (F por defecto)
#' @param plotnep si T presenta todos los lances en los que ha habido cigala en la campaña, además de los lances en cada FU, con o sín captura. Si F sólo saca los lances en cada FU sin marcar si ha habido cigala o no.
#' @param ICESrect Si T saca los rectangulos ices de 1 grado de latitud por medio de longitud
#' @param icesrectcol Color para los rectángulos ICES
#' @param places Si T saca etiquetas de principales ciudades en el mapa, si F se omiten los letreros
#' @param ICESlab Si T incluye las etiquetas de los rectángulos ICES
#' @param ICESlabcex tamaño del ICESlab en cex, .5 por defecto subirlo si se quiere más grande
#' @param FU pinta la FU31
#' @param ColFU color de relleno de la FU, blanco por defecto, con dens=0 se omite el relleno.
#' @param dens valores de 20 hace que el color permita ver el fondo con los ICES rects y la batimetría, más lo deja en color sólido, 0 quita el relleno del todo.
#' @param out.dat Si T el resultado final de la función es la figura en pantalla, pero los datos de abundancia de cigala en cada FU con datos de biomasa, numero, desviaciones estándar y número de lances en cada FU.
#' @return Produce un gráfico con los lances en los que ha habido cigala en el lance y especialmente los lances en cada FU dentro de Demersales FU25,FU26 y FU31
#' @family mapas, NEP
#' @examples
#'   NepFU31.camp("N21")
#' @export
NepFU31.camp<-function(camp=camp,dns="Cant",plot=TRUE,es=FALSE,ti=TRUE,ICESlab=FALSE,
                      ICESrectcol=1,ICESrect=TRUE,FU="FU31",ColFU="white",dens=20,places=TRUE,out.dat=TRUE,bw=FALSE,escmult=.25,leg=TRUE,cexleg=.9,cor.time=TRUE) {
  Nep<-maphist(2,19,camp,"Cant",plot=F,cor.time=cor.time,out.dat=T)
  Nep_31<-subset(Nep,c(long>c(-8) & long<c(-2) & lat <c(44.5) & lat>43))
  #lans_FU31<-dplyr::filter(datlan.camp(Nsh,"Cant",redux=T,incl2=T),c(long>c(-8) & long<c(-2) & lat >c(43) & lat<(44.5)))
  lans_FU31<-filter(datlan.camp(camp,"Cant",redux=T,incl2=T,incl0 = F),c(long>c(-8) & long<c(-2) & lat >c(43) & lat<(44.5)))
  leyenda<-signif(max(Nep$numero)*.9,1)
  leyenda<-signif(c(1,.5,.25)*leyenda,1)
  escala<-signif(max(Nep$numero),1)*escmult
  MapNort(ICESrect = ICESrect,ICESlab = ICESlab,ICESrectcol = ICESrectcol,ylims=c(42.5,44.5),xlims=c(-8.2,-1.8),FU="FU31",ColFU = ColFU,dens=dens,bw=bw)
  title(main=camptoyear(camp),line=1.5,sub=paste("FU 31 Nep Catch (n)= ",
                                                   sum(Nep_31[Nep_31$camp==camp,"numero"])),cex.sub=1.2,cex.main=2)
  points(lat~long,Nep,subset=c(peso.gr>0 & camp==camp),cex=sqrt(Nep$numero/escala),pch=21,col=2,bg=2)
  points(lat~long,Nep,subset=c(peso.gr==0 & camp==camp),cex=.7,pch=21,col=1,bg=1)
  legend("bottomright",legend=c("0 catch hauls"),pch=21,pt.bg=1,pt.cex=.7,inset=.01,bty="n")
  if (leg & max(Nep$numero,na.rm=TRUE)>0) {
    points(rep(-7,3),c(43.2,42.85,42.60),cex=sqrt(leyenda/escala),pch=21,bg="grey")
    text(rep(-7,3),c(43.2,42.85,42.60),labels=paste(leyenda,"ind."),pos=4,offset=3,cex=cexleg)
  }
  if (out.dat) {data.frame(camp=camp,Wgh=sum(Nep_31$peso.gr/1000),Nb=sum(Nep_31$numero),MeanWg=mean(Nep_31$peso.gr/1000),
                  SDwg=sd(Nep_31$peso.gr/1000),MeanNb=mean(Nep_31$numero),sdNb=sd(Nep_31$numero),Nlans=nrow(Nep_31))}
  }

