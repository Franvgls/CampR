#' Información anual sobre cigala por FUs en el Cantábrico y Galicia
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
#' @param camp Campaña de la que se extraen los datos: año comcreto (XX): Demersales "NXX", Porcupine "PXX", Arsa primavera "1XX" y Arsa otoño "2XX"
#' @param dns Sólo disponible para el Cantábrico "Cant", Golfo de Cádiz "Arsa", combinados con "dnsred" busca los datos en el servidor de Santander si se han creado las odbcs
#' @param year si T incluye una columna con el año al final de los datos
#' @param plot Saca el gráfico (T) o lo omite para dejar sólo los datos (F)
#' @param es si T letreros en español, si F en inglés (F por defecto)
#' @param plotnep si T presenta todos los lances en los que ha habido cigala en la campaña, además de los lances en cada FU, con o sín captura. Si F sólo saca los lances en cada FU sin marcar si ha habido cigala o no.
#' @param ICESrect Si T saca los rectangulos ices de 1 grado de latitud por medio de longitud
#' @param icesrectcol Color para los rectángulos ICES
#' @param places Si T saca etiquetas de principales ciudades en el mapa, si F se omiten los letreros
#' @param country si T saca el país
#' @param ICESlab Si T incluye las etiquetas de los rectángulos ICES
#' @param ICESlabcex tamaño del ICESlab en cex, .5 por defecto subirlo si se quiere más grande
#' @param FU pinta una o varias unidades funcionales, a elegir FU26, FU25 o FU31 con grueso lwd=2 y color rojo
#' @param out.dat Si T el resultado final de la función es la figura en pantalla, pero los datos de abundancia de cigala en cada FU con datos de biomasa, numero, desviaciones estándar y número de lances en cada FU.
#' @return Produce un gráfico con los lances en los que ha habido cigala en el lance y especialmente los lances en cada FU dentro de Demersales FU25,FU26 y FU31
#' @family mapas
#' @examples
#'   NepFUs.camp(camp="N20",dns="Cant",plot=T,plotnep=T)
#' @export
NepFUs.camp<-function(camp=Nsh[length(Nsh)],dns="Cant",plot=TRUE,plotnep=TRUE,es=FALSE,ti=TRUE,ICESlab=TRUE,
                      ICESrectcol=2,ICESrect=TRUE,FU=NA,places=TRUE,country=TRUE,out.dat=TRUE) {
  if (!substr(dns,1,4) %in% c("Cant","Arsa")) {stop("Esta función sólo permite sacar resultados DEMERSALES o ARSA *dns* debe ser Xxxx o Xxxxred")}
  if (length(camp)>1) {warning("Si escoge mas de una camp los resultados son para el total de campañas, para ver resultados último año coja sólo ese año")}
  Nep<-maphist(2,19,camp,dns,plot=F,out.dat=T)
  Nep$year<-camptoyear(Nep$camp)
  if (substr(dns,1,4)=="Cant") {
  Nep_17E1<-subset(Nep,c(long<c(-7.9) &lat>c(44)))
  Nep_31<-subset(Nep,c(c(long>c(-8) & long<c(-2) & lat>43.5 &  lat <c(44.5) & prof<650) | c(c(long>c(-8) & long<c(-2) & lat> 43.5 & lat<44.5))))
  Nep_25<-subset(Nep,c(long<c(-7.99) & lat>c(43) & lat<(44.5) & prof<600))
  Nep_26<-subset(Nep,c(long>c(-10) & long<c(-8.5) & lat<c(43.005) & lat>42.005))
  if (plot) {
    MapNort(places=places,country=country,ICESlab = ICESlab,ICESrect = ICESrect,ICESrectcol = ICESrectcol,FU=FU)
    if (ti) {title(main=paste0("Demersales ",ifelse(length(camp)==1,camptoyear(camp),paste0(range(camptoyear(camp))[1],"-",range(camptoyear(camp))[2]))))}
  if (plotnep) {points(lat~long,Nep,subset=numero>0,pch=21,bg="grey",cex=1.5)}
  points(lat~long,Nep_26,pch=21,bg="red")
  points(lat~long,Nep_25,pch=21,bg="green")
  points(lat~long,Nep_31,pch=21,bg="blue")
  if (plotnep){
  legend("bottomright",legend=c("Nep+","FU26","FU25","FU31"),pch=21,
         pt.bg=c("grey","red","green","blue"),inset=c(.02,.02),bg="white",
         ncol = 4)}
  else legend("bottomright",legend=c("FU26","FU25","FU31"),pch=21,
              pt.bg=c("grey","red","green","blue"),inset=c(.02,.02),bg="white",
              ncol =3)
  }
  }
  if (substr(dns,1,4)=="Arsa") {
  if (plot) {
    MapArsa(places=places,ICESlab = ICESlab,ICESrect = ICESrect,ICESrectcol = ICESrectcol)
    if (ti) {title(main=paste0("ARSA",ifelse(substr(camp[1],1,1)==1,"Q1 ","Q4 "),
                  ifelse(length(camp)==1,camptoyear(camp),
                  paste(range(camptoyear(camp))[1],range(camptoyear(camp))[2],sep="-"))),line=1.3)}
    if (plotnep) {points(lat~long,Nep,subset=numero>0,pch=21,bg="grey",cex=1.8)
    legend("bottomleft",legend=c("Nep+","FU30"),pch=21,
           pt.bg=c("grey","red"),pt.cex=c(1.8,1.1),inset=c(.13,.2),bg="white",ncol = 2)}
    points(lat~long,Nep,pch=21,cex=1.1,bg="red")
  }
  }
  if (out.dat & substr(dns,1,4)=="Cant") {
  FU31.B<-mean((Nep_31$peso.gr+0)/1000)
  FU31.Bsd<-sd(Nep_31$peso.gr/1000)
  FU31.BS<-sum(Nep_31$peso.gr/1000)
  FU31.N<-mean(Nep_31$numero)
  FU31.Nsd<-sd(Nep_31$numero)
  FU31.NS<-sum(Nep_31$numero)
  FU31.L<-length(Nep_31$lan)
  FU26.B<-mean(Nep_26$peso.gr/1000)
  FU26.Bsd<-sd(Nep_26$peso.gr/1000)
  FU26.BS<-sum(Nep_26$peso.gr/1000)
  FU26.N<-mean(Nep_26$numero)
  FU26.Nsd<-sd(Nep_26$numero)
  FU26.NS<-sum(Nep_26$numero)
  FU26.L<-length(Nep_26$lan)
  FU25.B<-mean(Nep_25$peso.gr/1000)
  FU25.Bsd<-sd(Nep_25$peso.gr/1000)
  FU25.BS<-sum(Nep_25$peso.gr/1000)
  FU25.N<-mean(Nep_25$numero)
  FU25.Nsd<-sd(Nep_25$numero)
  FU25.NS<-sum(Nep_25$numero)
  FU25.L<-length(Nep_25$lan)
  data.frame(FU31=c(B=FU31.B,Bsd=FU31.Bsd,BS=FU31.BS,N=FU31.N,Nsd=FU31.Nsd,NS=FU31.NS,L=FU31.L),
             FU25=c(B=FU25.B,Bsd=FU25.Bsd,BS=FU25.BS,N=FU25.N,Nsd=FU25.Nsd,NS=FU25.NS,L=FU25.L),
             FU26=c(B=FU26.B,Bsd=FU26.Bsd,BS=FU26.BS,N=FU26.N,Nsd=FU26.Nsd,NS=FU26.NS,L=FU26.L))
  }}

