#' Funcion auxiliar para el PescaWin
#'
#' Analiza los datos del fichero de arte de una campaña a partir de fichero arte_XXX.csv en el directorio del pescawin (c:/gps/mrk/XXXX/arte_aXX.csv)
#' @param camp Campaña de las que se cogen los resultados un año concreto (XX): Demersales "NXX", Porcupine "PXX", Arsa primavera "1XX" y Arsa otoño "2XX"
#' @param lan Elique el lance del que se quieren comprobar los datos, si no existe dice que no  se encuentra. Si se deja en NA se recoge el lance o los lances seleccionados
#' @param dns Elige el origen de las bases de datos: Porcupine "Porc", Cantábrico "Cant", Golfo de Cadiz "Arsa" (proporciona los datos para Medits pero no saca mapas)
#' @param gps Si T exporta el fichero directamente como camp.lan al directorio de lances del PescaWin
#' @param valid Si T incluye sólo los lances válidos (>0) y excluye los nulos (=0)
#' @return Devuelve un data.frame con datos long_l,lat_l,prof_l,long_v,lat_v,prof_v en grados decimales con signo
#' @family PescaWin
#' @export
pasa.arte<-function(camp,lan=NA,FileArte="Arte_",folder="c:/gps/mrk/",gps=FALSE,graphs=TRUE) {
  require(grid)
  # load file, change accordingly to your pescawin config
  DatArte<-read.csv(paste0(folder,FileArte,camp,".csv"),sep=",",head=T)
  if (!any(is.na(lan))) {DatArte<-dplyr::filter(DatArte,Lance %in% lan)}
  # Convert factor fields into numeric or character fields (get rid of pescawin headers per haul)
  DatArte$Tiempo<-as.character(DatArte$Tiempo)
  DatArte$Tiempo[which(nchar(DatArte$Tiempo)==8)]<-substr(DatArte$Tiempo[which(nchar(DatArte$Tiempo)==8)],1,5)
  for (i in c(1,4:11)) {
    DatArte[,i]<-as.numeric(as.character(DatArte[,i]))
  }
  rm(i)
  # Create a time field for trawling time
  DatArte$t.arr.m<-as.numeric(paste(substr(as.character(DatArte$Tiempo),1,nchar(as.character(DatArte$Tiempo))-3),
                                    ".",substr(as.character(DatArte$Tiempo),nchar(as.character(DatArte$Tiempo))-1,
                                               nchar(as.character(DatArte$Tiempo))),sep=""))
  DatArte$t.arr<-trunc(DatArte$t.arr.m)+(DatArte$t.arr.m-trunc(DatArte$t.arr.m))/.6
  # get rid of intermediate headers
  DatArte<-DatArte[-which(DatArte$Maniobra!="FIRME" & DatArte$Maniobra!="Fin lance"),]
  # filter for clear outsiders or nul values
  #DatArte$Calones<-DatArte$Puertas
  DatArte$Puertas[DatArte$Puertas==0]<-NA
  DatArte$Calones[which(DatArte$Calones==0)]<-NA
  DatArte$Vertical[DatArte$Vertical==0 & DatArte$Maniobra=="FIRME"]<-NA
  DatArte$Vertical[which(DatArte$Esp.libre > 0 & !is.na(DatArte$Esp.libre) & DatArte$Tiempo<6 )]<-NA
  DatArte$Vertical[DatArte$Vertical==0 & DatArte$Maniobra=="Fin lance"]<-NA
  DatArte$t.arr[nchar(as.character(DatArte$Tiempo))==2]<-as.numeric(as.character(DatArte$Tiempo[nchar(as.character(DatArte$Tiempo))==2]))
  DatArte$t.arr[nchar(as.character(DatArte$Tiempo))==1]<-as.numeric(as.character(DatArte$Tiempo[nchar(as.character(DatArte$Tiempo))==1]))
  Nlances<-length(unique(DatArte$Lance))
  if(Nlances==0) {stop("Los lances selecciondos no existen o no tienen datos válidos")}
  if (!is.na(lan)) DatArte<-dplyr::filter(DatArte,Maniobra=="FIRME" , Lance %in% lan,Val==1)
  # Calculo de abertura puertas a partir de calones
  # DatArte$Calones[is.na(DatArte$Calones) & !is.na(DatArte$Puertas)] <- DatArte$Puertas[is.na(DatArte$Calones) & !is.na(DatArte$Puertas)]*1/5
  # DatArte$Puertas[is.na(DatArte$Puertas) & !is.na(DatArte$Calones)] <- DatArte$Calones[is.na(DatArte$Puertas) & !is.na(DatArte$Calones)]*5
  #par(ask=T)
  if (graphs) {
    trellis.par.set(col.whitebg())
    #grafs for check, can be reduced to less hauls setting subset to lance>X
    #par(ask=T)
    a<-(xyplot(Vertical~t.arr.m|factor(Lance),DatArte,subset=(Maniobra=="FIRME"&is.na(Vertical)==F & t.arr.m<40),
               as.table=T,ylim=c(1,9.2),main=list(label="Vertical opening",cex=1),xlab=list(label="Trawling time (min.)",cex=.7),
               par.strip.text=list(cex=.7,font=2),scales=list(alternating=F,tck=c(1,0),cex=.6,
                                                              x=list(at=c(0,seq(5,25,by=10))),y=list(at=seq(2,4,by=1))),ylab=list(label="Meters",cex=.7),drop.unused.levels=F,
               panel=function(x,y,...) {
                 panel.abline(h=seq(2,6,by=1),col=gray(.5),lty=3)
                 panel.abline(h=median(y),col=4,lty=1)
                 panel.abline(v=trunc(min(x[y<=median(y,na.rm=T)*1.1])),col=4,lty=3)
                 #	panel.abline(v=trunc(min(x[y<=3])),col=4,lty=3)
                 panel.xyplot(x,y,pch=20,col=gray(.3))
                 ltext(20,6.5,label=ifelse(is.na(median(y)),"NA",paste("med=",round(median(y),1))),cex=.7,font=1)
                 #	ltext(20,5.5,label=paste("Bott=",trunc(min(x[y<=median(y,na.rm=T)*1.1])),"'"),cex=.7)
                 if (length(x)>3) panel.loess(x,y,span=.5,col=2,family="gaussian")}
    ))
    b<-(xyplot(Puertas~t.arr.m|factor(Lance),DatArte,subset=(Maniobra=="FIRME"&is.na(Puertas)==F & Puertas<150 & t.arr.m<40),as.table=T,
               main=list(label="Door spread",cex=1),xlab=list(label="Trawling time (min.)",cex=.7),ylim=c(0,130),
               par.strip.text=list(cex=.7,font=2),scales=list(alternating=F,tck=c(1,0),cex=.6,
                                                              x=list(at=c(0,seq(5,25,by=10)))),ylab=list(label="Meters",cex=.7),drop.unused.levels=F,
               panel=function(x,y,...) {
                 panel.grid(h=-1,v=0,col=gray(.4),lty=2)
                 panel.abline(h=median(y),col=4,lty=1)
                 panel.xyplot(x,y,pch=20,col=gray(.3))
                 ltext(25,45,label=ifelse(is.na(median(y)),"NA",paste("med=",round(median(y),0))),cex=.7)
                 if (length(x)>3) panel.loess(x,y,span=.3,col=2,family="gaussian")}
    ))
    c<-(xyplot(Calones~t.arr.m|factor(Lance),DatArte,subset=(Maniobra=="FIRME"&is.na(Calones)==F & Calones<150 & t.arr.m<40),as.table=T,
               main=list(label="Wing spread",cex=1),xlab=list(label="Trawling time (min.)",cex=.7),ylim=c(0,50),
               par.strip.text=list(cex=.7,font=2),scales=list(alternating=F,tck=c(1,0),cex=.6,
                                                              x=list(at=c(0,seq(5,25,by=10)))),ylab=list(label="Meters",cex=.7),drop.unused.levels=F,
               panel=function(x,y,...) {
                 panel.grid(h=-1,v=0,col=gray(.4),lty=2)
                 panel.abline(h=median(y),col=4,lty=1)
                 panel.xyplot(x,y,pch=20,col=gray(.3))
                 ltext(25,35,label=ifelse(is.na(median(y)),"NA",paste("med=",round(median(y),0))),cex=.7)
                 if (length(x)>3) panel.loess(x,y,span=.3,col=2,family="gaussian")}
    ))
    if (is.na(lan)) {
    windows()
    print(a)
    windows()
    print(b)
    windows()
    print(c)
    }
    else {gridExtra::grid.arrange(a,b,c,ncol=3)}
  }
  DatArte$lan<-as.factor(DatArte$Lance)
  if (any(is.na(lan))|length(lan)>1) {
      dumb<-c(Lance=1,robustbase::colMedians(as.matrix(filter(DatArte,Lance==lan[1],Val==1,Maniobra=="FIRME")[,c("Puertas","Calones","Vertical")]),na.rm=T))
      for (i in unique(DatArte$Lance)[c(2:Nlances)]) {
        if (nrow(filter(DatArte,
                    Lance==i,Val==1,Maniobra=="FIRME")[,c("Calones","Puertas","Vertical")]) > 2) {
        dumb<-rbind(dumb,c(Lance=i,robustbase::colMedians(as.matrix(filter(DatArte,
                                                                         Lance==i,Val==1,Maniobra=="FIRME")[,c("Puertas","Calones","Vertical")]),na.rm=T)))
        }}}
  else {dumb<-c(Lance=lan,robustbase::colMedians(as.matrix(filter(DatArte,Lance%in%lan,Val==1,Maniobra=="FIRME")[,c("Puertas","Calones","Vertical")]),na.rm=T))}
  print(dumb)
  if (gps) {write.table(dumb,paste0(folder,"lan",camp,".csv",row.names = F,col.names = F,sep=","))}
  }
