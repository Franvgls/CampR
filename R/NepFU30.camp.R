#' Información anual sobre cigala para la FU 30 en el Golfo de Cádiz
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
#' @param camp Campaña de la que se extraen los datos: año concreto (XX): Arsa "1XX"
#' @param dns Sólo disponible para el Golfo de Cádiz "Cant" combinados con "dnsred" busca los datos en el servidor de Santander si se han creado las RODBCs
#' @param year si T incluye una columna con el año al final de los datos
#' @param plot Saca el gráfico (T) o lo omite para dejar sólo los datos (F)
#' @param es si T letreros en español, si F en inglés (F por defecto)
#' @param plotnep si T presenta todos los lances en los que ha habido cigala en la campaña, además de los lances en cada FU, con o sín captura. Si F sólo saca los lances en cada FU sin marcar si ha habido cigala o no.
#' @param ICESrect Si T saca los rectangulos ices de 1 grado de latitud por medio de longitud
#' @param icesrectcol Color para los rectángulos ICES
#' @param places Si T saca etiquetas de principales ciudades en el mapa, si F se omiten los letreros
#' @param ICESlab Si T incluye las etiquetas de los rectángulos ICES
#' @param ICESlabcex tamaño del ICESlab en cex, .5 por defecto subirlo si se quiere más grande
#' @param FU pinta una o varias unidades funcionales, a elegir FU30, FU25 o FU30 con grueso lwd=2 y color rojo
#' @param out.dat Si T el resultado final de la función es la figura en pantalla, pero los datos de abundancia de cigala en cada FU con datos de biomasa, numero, desviaciones estándar y número de lances en cada FU.
#' @return Produce un gráfico con los lances en los que ha habido cigala en el lance y especialmente los lances en cada FU dentro de Demersales FU25,FU30 y FU31
#' @family mapas, NEP
#' @examples
#'   NepFU30.camp("N21")
#' @export
NepFU30.camp<-function(camp=camp,dns="Arsa",trimes=4,plot=TRUE,es=FALSE,ti=TRUE,ICESlab=FALSE,
                      ICESrectcol=1,ICESrect=TRUE,FU=30,places=TRUE,out.dat=TRUE,bw=FALSE) {
  Nep<-maphist(2,19,camp,"Arsa",plot=F,out.dat=T)
  Nep_30<-subset(Nep,c(long>c(-10) & long<c(-8.5) & lat<c(43.005) & lat>42.005))
  lans_FU30<-datlan.camp(camp,"Arsa",redux=T,incl2=T)
  #lans_FU30<-rbind(lans_FU30,dplyr::filter(datlan.camp(Nsh,"Arsa",redux=T,incl2=T),c(long>c(-3) & long<c(-2) & lat >c(43) & lat<(44))))
  MapArsa(ICESrect = ICESrect,bw=bw,ICESlab = ICESlab,ICESrectcol = ICESrectcol,xlims = c(-7.7,-6),ylims = c(36,37.3))
  title(main=paste(camptoyear(camp),ifelse(substr(camp,1,1)==1,"Q1","Q4")),line=1.5,sub=paste("FU 30 Nep Catch (n)= ",
                                                   sum(Nep_30[Nep_30$camp==camp,"numero"])),cex.sub=1.2,cex.main=2)
  points(lat~long,Nep,subset=c(peso.gr>0 & camp==camp),cex=sqrt(Nep$numero/5),pch=21,col=2,bg=2)
  points(lat~long,Nep,subset=c(peso.gr==0 & camp==camp),cex=.7,pch=21,col=1,bg=1)
  legend("bottomright",legend=c("0 catch hauls"),pch=21,pt.bg=1,pt.cex=.7,inset=.01,bty="n")
}

