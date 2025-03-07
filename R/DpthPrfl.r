#' Perfil de distribución por profundidad
#'
#' Crea un gráfico de perfil de distribución por profundidad de la biomasa o número de una especie o grupo de especies a partir de los datos de peso y número de la faunística tomados en una campaña
#' @param gr Grupo de la especie: 1 peces, 2 crustaceos 3 moluscos 4 equinodermos 5 invertebrados
#' @param esp Código de la especie numerico o caracter con tres espacios. 999 para todas las especies del grupo
#' @param camps Campañas de las que se obtiene la distribución de profundidades (XX): Demersales "NXX", Porcupine "PXX", Arsa primavera "1XX" y Arsa otoño "2XX"
#' @param dns Elige el origen de las bases de datos: Porcupine "Porc" or "Pnew", Cantábrico "Cant", Golfo de Cádiz "Arsa" (únicamente para sacar datos al IBTS, no gráficos)
#' @param cor.time Si T corrige las abundancias en función de la duración del lance
#' @param ind Parámetro a representar, saca los datos en "p"eso o "n"úmero
#' @param es Si T gráfico en castellano, si F gráfico en inglés
#' @param ti Si T añade título al gráfico, el nombre de la especie en latín
#' @param idi Nombre científico de la especie ("l") o nombre común ("e")
#' @param xmax Valor máximo del eje x
#' @param nlans Si T añade el número de lances en cada rango de profundidad
#' @param spl Si T incluye una curva spline en el gráfico
#' @param brks Especifica los rangos de profundidad: "norte" estratificación de Demersales, "porcupine" estratificación de Porcupine, las opciones "FD", "Sturges" y "scott" también son válidas como están implenmentadas en {\link{hist}}. También se pueden fijar unos límites a voluntad, si da error de hay datos fuera de los rangos (de norte o porcupine, se cogen los límites de la campaña y se fija el último rango a la prof máxima)
#' @param overdpth En el caso de que, con brks=norte o porcupine de un aviso de que hay profundidades fuera de la estratificación fija el límite del estrato máximo al de la profundidad máxima detectada más 10 m, se aconseja tenerlo en T, y da error si hay profundidades fuera de brks .
#' @param tabres Muestra una tabla resumen del total de lances, media y total de biomasa o número y frecuencia de la especie por estrato según el brks especificado
#' @examples DpthPrfl(1, 50, "N08", "Cant",brks = "norte",tabres=TRUE,ind="p")
#' @examples DpthPrfl(1,50,"P08","Porc",brks="porcupine",ti=TRUE)
#' @seealso {\link{DpthPrflTals}}
#' @export
DpthPrfl<-function(gr,esp,camps,dns="Porc",cor.time=TRUE,incl2=TRUE,ind="p",es=TRUE,ti=TRUE,idi="l",xmax=NA,nlans=TRUE,spl=FALSE,brks="Sturges",overdpth=TRUE,tabres=TRUE) {
    esp<-format(esp,width=3,justify="r")
    if (length(gr)>1) stop("No se pueden mezclar datos de grupos distintos, se pueden mezclar todos menos 6, utilizando 9 como grupo")
    #  if (chpar)  opar<-par(no.readonly=TRUE)
    #  if (length(wghts)>1) par(mfrow=c(2,2))
    #  par(mar=c(3,3,3,1))
    options(scipen=2)
    K=F
    values<-c("norte","porcupine","Sturges","scott","FD")
    #browser()
    if (!is.numeric(brks) & any(!brks %in% values)) stop("brks tiene que ser norte, porcupine, valores numéricos o Sturges como forma de determinar los rangos de profundidad")
    dumb<-maphist(gr,esp,camps,dns,cor.time=cor.time,incl2=incl2,plot=FALSE,out.dat=TRUE,ind=ind)
    if (sum(dumb$numero)==0) {
      stop(paste("La especie",buscaesp(gr,esp),"no tiene datos de capturas en las campañas seleccionadas, revise por favor"))
    }
    #print(dumb)
    if (length(brks)>1) {
      dumb<-dumb[dumb$prof>brks[1] & dumb$prof<brks[length(brks)],]
      ylims<-c(brks[length(brks)],brks[1])*c(-1)
    }
    else {
      brks<-values[match(brks,values)]
      ylims<-c(-850,0)
    }
    if (any(brks=="porcupine")) {
      brks=c(150,300,450,800)
      if (min(dumb$prof)<brks[1] | max(dumb$prof)>brks[4]) k=T
      if (max(dumb$prof)>brks[4] & overdpth) {
        brks=c(brks,max(dumb$prof)+10)
        ylims<-c(brks[length(brks)],brks[1])*c(-1)
        }
    }
    if (any(brks=="norte")) {
      brks=c(0,70,120,200,500,1000)
      if (min(dumb$prof)<brks[1] | max(dumb$prof)>brks[6]) message("Existen lances fuera de los rangos de la campa?a, revise los datos")
      if (max(dumb$prof)>brks[6] & overdpth) {
        brks[6]=max(dumb$prof)+10
        ylims<-c(brks[length(brks)],brks[1])*c(-1)
      }
      }
    dumbDpth<-hist(dumb$prof,plot=FALSE,breaks=brks)
    if (ind=="n") {dumbDatDpth<-hist(rep(dumb$prof,dumb$numero),plot=FALSE,breaks=dumbDpth$breaks)}
    else {dumbDatDpth<-hist(rep(dumb$prof,dumb$peso.gr/1000),plot=FALSE,breaks=dumbDpth$breaks)}
    spln<-spline(-dumbDpth$mids,c(dumbDatDpth$counts/dumbDpth$counts),n=201)
    if (all(par("mfrow")==c(1,1))) cex.mn=.8
    else cex.mn=1.1
    if (is.logical(ti)) {
      if (ti) {
        titulo1<-list(buscaesp(gr,esp,id=idi),font=ifelse(idi==idi,4,2),cex=cex.mn)
      }
    }
    else {
      titulo1=ti
    }
    # browser()
    dumbDpth$counts1<-dumbDpth$counts
    if (any(dumbDpth$counts==0)) {
      count0<-which(dumbDpth$counts==0)
      dumbDpth$counts[count0]<-1
    }
    #  browser()
    par(mar=c(4,4,4,2))
    plot(c(-dumbDpth$breaks,-ylims[2])~c(0,c(dumbDatDpth$counts/dumbDpth$counts),0),type=c("s"),xlim=c(0,ifelse(is.na(xmax),max(spln$y)*1.05,xmax*1.05)),
         ylim=ylims,xlab=NA,ylab=ifelse(es,"Prof (m)","Depth (m)"),axes=FALSE,
         pch=21,bg="white",cex.lab=cex.mn)
    if (ti) title(main=titulo1,line=1.8)
    points(c(-dumbDpth$mids)~c(dumbDatDpth$counts/dumbDpth$counts),type="p",pch=21)
    ceros<-rep(0,length(dumbDpth$mids)+1)
    segments(ceros,c(-dumbDpth$breaks,-ylims[2]),c(0,c(dumbDatDpth$counts/dumbDpth$counts),0),c(-dumbDpth$breaks,-ylims[2]))
    if (ind=="n") {title(xlab=ifelse(es,expression("ind."%*%"lan"^-1),expression("ind"%*%"haul "^-1)),cex.lab=cex.mn)}
    else{title(xlab=ifelse(es,expression("Yst"("kg"%*%"lan"^-1)),expression("kg"%*%"haul "^-1)),cex.lab=cex.mn)}
    #	print(spln)
    if (spl) lines(spln$y,spln$x, col = 2,lty=2,lwd=.1)
    #  browser()
    if (nlans) {
      text(c(-dumbDpth$mids)~c(dumbDatDpth$counts/dumbDpth$counts),
           labels=dumbDpth$counts1,cex=.7,font=2,pos=4)
    }
    axis(1,cex.axis=cex.mn-.1)
    if (any(brks %in% values)) axis(2,at=seq(-min(brks),-max(brks),by=-100),seq(min(brks),max(brks),100),las=2,cex.axis=cex.mn-.1)
    else axis(2,at=-brks,labels=brks,las=2,cex.axis=cex.mn-.1)
    box()
    dumb<-cbind(dumb,strat=cut(dumb$prof,dumbDpth$breaks))
    if (tabres) {
      if (ind=="n") {
        dumb0<-dumb[dumb$numero>0,]
        nlans<-tapply(dumb$numero,dumb$strat,length)
        dlans<-tapply(dumb0$numero,dumb0$strat,length)
        totstr<-tapply(dumb0$numero,dumb0$strat,sum,na.rm=TRUE)
        avgstr<-tapply(dumb$numero,dumb$strat,mean,na.rm=TRUE)
      }
      if (ind=="p") {
        dumb0<-dumb[dumb$peso>0,]
        nlans<-tapply(dumb$peso,dumb$strat,length)
        dlans<-tapply(dumb0$peso,dumb0$strat,length)
        totstr<-tapply(dumb0$peso,dumb0$strat,sum,na.rm=TRUE)
        avgstr<-tapply(dumb$peso,dumb$strat,mean,na.rm=TRUE)
      }
      #    par(opar)
      if (K) message("Existen lances fuera de los rangos de la campaña, revise los datos")
      resumen<-data.frame(lans=nlans,totstr=totstr,meanstr=avgstr,frecuencia=dlans)
      resumen
    }
  # DpthPrfl(1,50,"N08","Cant",brks=c(0,70,100,130,160,190,220))
  # DpthPrfl(1,50,"N08","Cant",brks="FD")
  }
