#'  Histograma de distribución de tallas con edades 
#'  
#'  Función gráfica a partir de los ficheros del camp. Histograma distribución de tallas estratificada por edad
#' @param gr Grupo de la especie: 1 peces sólo hay claves de talla para peces y cigala?
#' @param esp Código de la especie numérico o carácter con tres espacios. Sólo admite una especie por gráfica
#' @param camp Campaña a representar en el mapa de un año concreto (XX): Demersales "NXX", Porcupine "PXX", Arsa primavera "1XX" y Arsa otoño "2XX"
#' @param dns Elige el origen de las bases de datos: Porcupine "Pnew", Cantábrico "Cant", Golfo de Cádiz "Arsa" (proporciona los datos para Medits pero no saca mapas)
#' @param plus Edad plus: Edad considerada como plus, todas las edades mayores se suman como edad +
#' @param cor.time Si T corrige las abundancias en función de la duración del lance
#' @param excl.sect Sectores a excluir como carácter, se pueden elegir tanto los sectores como estratos
#' @param ti Si T añade título al gráfico, el nombre de la especie en latín
#' @param leg Si T añade leyenda
#' @param bw Gráfico en blanco en negro si T o en color si F
#' @param es Si T gráfico en castellano, si F gráfico en inglés
#' @param plot Saca el gráfico (T) o lo guarda como objeto para componer con otros gráficos (F)
#' @param ymax Valor máximo del eje y
#' @param out.dat Si T el resultado final de la función es la figura en pantalla, pero los datos en objeto
#' @return Saca gráfica con distribución de tallas y la distribución de las edades en cada talla. Si out.dat=T saca un data.frame con columnas n(valor del número de la distribución estratifcada para la talla y la edad),talla,edad,camp. Da error si no existe ALK para la especie en la campaña
#' @examples grafedtal.camp(1,43,"P09","Pnew",es=F,out.dat=T)
#' @export
grafedtal.camp <- function(gr,esp,camp,dns="Pnew",plus=8,cor.time=T,excl.sect=NA,ti=F,leg=T,bw=T,es=T,plot=T,ymax=NA,out.dat=F) {
  if (length(camp)>1) {stop("seleccionadas más de una campaña, no se pueden sacar resultados de más de una")}
  if (length(esp)>1) {
    stop("Sólo se puede incluir una especie en esta función")
  }
  require(RODBC)
  esp<-format(esp,width=3,justify="r")
  ch1<-odbcConnect(dns)
  odbcSetAutoCommit(ch1, FALSE)
  edad<-sqlQuery(ch1,paste("select * from EDAD",camp," where grupo='",
                           gr,"' and esp='",esp,"'",sep=""))
  odbcClose(ch1)
  if (nrow(edad)==0) stop(paste("no existe clave talla edad para la especie",buscaesp(gr,esp),"en la campaña",camp))
  require(lattice)
  if (ti) tit<-list(label=paste(buscaesp(gr,esp),camp),font=4,cex=1)
  else tit<-NULL
  dtall<-dattal.camp(gr,esp,camp,dns,excl.sect=excl.sect,cor.time=cor.time)
  if (ncol(dtall)>2) dtall<-data.frame(talla=dtall[,1],n=rowSums(dtall[,-1])) else names(dtall)<-c("talla","n")
  edad<-edad[which(rowSums(edad[5:20],na.rm=T)>0),]
  edad<-edad[,-c(1:3,21)]
  for (i in 1:nrow(edad)) edad[i,which(is.na(edad[i,]))]<-0
  if (plus<15) edadedad<-data.frame(edad[,1:(plus+1)],plus=rowSums(edad[,(plus+2):length(edad[1,])]))
  edad<-data.frame(edad[,1],edad[,-1]/rowSums(edad[,-1]))
  names(edad)<-c("talla",paste("E",0:(plus-1),sep=""),paste("E",plus,"+",sep=""))
  tedad<-data.frame(talla=dtall$talla)
  tedad<-merge(tedad,edad,by.x="talla",by.y="talla",all=T)
  a<-dtall$talla[which(dtall$n>0)]
  b<-which((match(a,edad$talla,nomatch=0)==0),T)
  if (length(b)>0) {
    print("Las tallas: ",quote=F)
    print(a[b])
    print("no estan en la clave talla edad",quote=F)
  }
  else {
    for (i in 1:nrow(tedad)) tedad[i,which(is.na(tedad[i,]))]<-0
    odbcCloseAll()
    b<-which((match(tedad$talla,dtall$talla,nomatch=0)==0),T)
    if (length(b)>0) {
      print("Las tallas: ",quote=F)
      print(tedad$talla[b])
      print("aparecen en la clave y no en la distribución de tallas",quote=F)
    }
    if (length(b)>0) tedad<-tedad[-which(tedad$talla==b),]
    tedad[,2:ncol(tedad)]<-tedad[,2:ncol(tedad)]*dtall[,2]
    edadtal<-data.frame(n=tedad[,2])
    for (i in 3:ncol(tedad)) {edadtal<-rbind(edadtal,data.frame(n=tedad[,i]))}	
    edadtal$talla<-rep(tedad$talla,ncol(tedad)-1)
    edadtal$edad<-rep(names(tedad)[-1],rep(nrow(tedad),ncol(tedad)-1))
    edadtal$n[which(is.na(edadtal$n))]<-0
    trellis.par.set(col.whitebg())
    colo<-rainbow(plus+1)
    cols<-ifelse((plus+1)/2-trunc((plus+1)/2)==0,(plus+1)/2,trunc((plus+1)/2)+1)
    if (is.na(ymax)) ylim=c(0,max(dtall$n)*1.1)
    else ylim=c(0,ymax)
    if (leg) {leg<-list(columns=cols,space="top",rect=list(T,col=colo),
                           text=list(labels=names(edad)[-1],col="black",cex=.7))}
    else {leg<-NULL}
    xlimi<-c(min(dtall$talla)*(.95-1),max(dtall$talla)*1.05)
    foo<-barchart(n~talla,edadtal,groups=factor(edadtal$edad),col=colo,main=tit,xlim=xlimi,ylim=ylim,
                  scales=list(alternating=F,tck=c(1,0),x=list(tick.number=10)),box.ratio=1000,h=F,stack=T,
                  key=leg,xlab=paste(ifelse(es,"talla","length"),"(cm)"),
                  panel=function(x,y,...){
                    panel.grid(-1,0,lty=3,col="black")
                    panel.barchart(x,y,...)
                  }
    )
    if (plot) print(foo)
    else foo
  }
  if (out.dat) cbind(edadtal,camp=camp)
}
