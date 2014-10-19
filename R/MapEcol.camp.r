#' Mapa de índices ecológicos para un grupo en una campaña
#' 
#' Utiliza los datos del Camp representar la variación geográfica de los índices ecológicos
#' @param gr Grupo de la especie: 1 peces, 2 crustáceos 3 moluscos 4 equinodermos 5 invertebrados
#' @param esp ha de ser 999 cuando se quiere incluir todas las especies del grupo, o elegir todas las especies deseadas con los codigos de las especies
#' @param camp Campaña de la que se extraen los datos: un año comcreto (XX): Demersales "NXX", Porcupine "PXX", Arsa primavera "1XX" y Arsa otoño "2XX"
#' @param dns Elige el origen de las bases de datos: Porcupine "Pnew", Cantábrico "Cant, Golfo de Cádiz "Arsa" (únicamente para sacar datos al IBTS, no gráficos)
#' @param ind Elige el valor (n)úmero o (p)eso sobre el que se calculan los índices de diversidad, dominancia....
#' @param indec Elige el índice ecológico a representar: opciones disponibles: 'div' 'simp', 'domsimp' y 'nesp'
#' @param plot Si T saca un gráfico en pantalla
#' @param bw Gráfico en blanco en negro si T o en color si F
#' @param ti Añade el nombre de la especie en latín sin T, si F no añade titulo
#' @param idi Nombre científico de la especie ("l") o nombre común ("e")
#' @param out.dat Si T el resultado final de la función es la figura en pantalla, pero los datos en objeto
#' @param es Si T saca los titulos y rotulos en español, si F en inglés
#' @param layout Organización de gráficos en filas ó columnas c(r,c)
#' @return Saca el mapa de diversidad en la campaña seleccionada.
#' @seealso MapEcol.camp {\link{MapEcol.camp.r}}
#' @examples ecolgr.camp(1,999,"P08","Pnew",ind="n")
#' @examples MapEcol.camp(1,999,"P08","Pnew",ind="n")
#' @export
MapEcol.camp<-function(gr,esp="999",camp,dns="Pnew",ind="n",indec="div",plot=T,bw=F,
                       ti=T,idi="l",es=T,out.dat=F,layout=NA) {
  if (!(indec %in% c("simp","div","domsimp","nesp"))) {
    stop(paste("el índice",indec,"no está implementado, índices disponibles: 'div' 'simp', 'domsimp' y 'nesp'"))
  }
  require(lattice)
  require(grid)
  require(maps)
  trellis.par.set(col.whitebg())
  ndat<-length(camp)
  dumb<-NULL
  for (i in 1:ndat) {
    dumb<-rbind(dumb,cbind(ecolgr.camp(gr,esp,camp[i],dns,ind=ind),camp=camp[i]))
  }
  dumb$camp<-factor(dumb$camp)
  if (indec=="div") {
    leyenda<-signif(max(dumb$div)*.9,1)
    escala<-signif(max(dumb$div),1)*50/70}
  if (!es) sub<-paste("Shannon Wiener",ifelse(ind=="p","- Biomass","- Number"))
  else sub<-paste("Shannon Wiener",ifelse(ind=="p","- Biomasa","- Número"))
  if (indec=="nesp") {
    leyenda<-signif(max(dumb$numbesp)*.9,1)
    escala<-signif(max(dumb$numbesp),1)*50/70
    sub<-ifelse(es,"Número de especies","Species number")
  }
  if (indec=="simp") {
    leyenda<-signif(max(dumb$simp)*.9,1)
    escala<-signif(max(dumb$simp),1)*50/70
    if (!es) sub<-paste("Simpson's Diversity",ifelse(ind=="p","- Biomass","- Number"))
    else sub<-paste("Diversidad de Simpson",ifelse(ind=="p","- Biomasa","- Número"))
  }
  if (indec=="domsimp") {
    leyenda<-signif(max(dumb$domsimp)*.9,1)
    escala<-signif(max(dumb$domsimp),1)*1/2
    if (!es) sub<-paste("Simpson's dominance",ifelse(ind=="p","- Biomass","- Number"))
    else sub<-paste("Dominancia Simpson",ifelse(ind=="p","- Biomasa","- Número"))
  }
  if (ti) titulo<-list(label=buscaesp(gr,esp,id=idi),font=ifelse((idi=="l" & gr!="9" & esp!="999"),4,2))
  else titulo<-NULL
  if (bw) colo=gray(.2)
  else colo=4
  if (ndat<4) layout=c(1,ndat)
  if (ndat==4) layout=c(2,2)
  if (substr(dns,1,4)=="Pnew" | substr(dns,1,4)=="Porc") {
    asp<-diff(c(50.5,54.5))/(diff(c(-15.5,-10.5))*cos(mean(c(50.5,54.5))*pi/180))
    leyenda<-signif(c(1,.5)*leyenda,1)
    mapdist<-xyplot(lat~long|camp,dumb,layout=layout,xlim=c(-15.5,-10.5),main=titulo,xlab=NULL,ylab=NULL,
                    ylim=c(50.5,54.5),aspect=asp,par.strip.text=list(cex=.8,font=2),scales=list(alternating=F,tck=c(1,0),cex=.7,
                                                                                                x=list(at=c(-15:-11),labels=as.character(abs(-15:11))),y=list(at=(51:54),rot=90)),as.table=T,sub=sub,
                    panel=function(x,y,subscripts) {
                      panel.xyplot(Porc.map$x,Porc.map$y,type="l",lty=3,col=gray(.2))
                      grid.polygon(map(Porc.map,"narr",plot=F)[[1]],map(Porc.map,"narr",plot=F)[[2]],
                                   default.units = "native",gp=gpar(fill=gray(.7)))
                      panel.xyplot(c(-12.5,-12.5),c(51.3,51),cex=sqrt((leyenda)/escala),pch=16,col=colo)
                      ltext(c(-12.5,-12.5),c(51.3,51),labels=leyenda,pos=4,offset=1.1,cex=.7)
                      if (indec=="div") {panel.xyplot(x,y,cex=ifelse(dumb$div[subscripts]>0,sqrt((dumb$div[subscripts])/escala),.35),
                                                      pch=ifelse(dumb$div[subscripts]>0,16,20),col=colo)}
                      if (indec=="simp") {panel.xyplot(x,y,cex=ifelse(dumb$simp[subscripts]>0,sqrt((dumb$simp[subscripts])/escala),.35),
                                                       pch=ifelse(dumb$simp[subscripts]>0,16,20),col=colo)}
                      if (indec=="nesp") {panel.xyplot(x,y,cex=ifelse(dumb$numbesp[subscripts]>0,sqrt((dumb$numbesp[subscripts])/escala),.35),
                                                       pch=ifelse(dumb$numbesp[subscripts]>0,16,20),col=colo)}
                      if (indec=="domsimp") {panel.xyplot(x,y,cex=ifelse(dumb$domsimp[subscripts]>0,sqrt((dumb$domsimp[subscripts])/escala),.35),
                                                          pch=ifelse(dumb$domsimp[subscripts]>0,16,20),col=colo)}
                    }) }
  if (substr(dns,1,4)=="Cant") {
    asp<-diff(c(41.82,44.3))/(diff(c(-10.25,-1.4))*cos(mean(c(41.82,44.3))*pi/180))
    leyenda<-signif(c(1,.5,.25)*leyenda,1)
    mapdist<-xyplot(lat~long|camp,dumb,layout=layout,xlim=c(-10.25,-1.4),main=titulo,xlab=NULL,ylab=NULL,
                    ylim=c(41.82,44.3),aspect=asp,par.strip.text=list(cex=.8,font=2),scales=list(alternating=F,tck=c(1,0),cex=.7,
                                                                                                 x=list(at=c(-10:-2),labels=as.character(abs(-10:-2))),y=list(at=(42:44),rot=90)),as.table=T,sub=sub,
                    panel=function(x,y,subscripts) {
                      panel.xyplot(Nort.str$x,Nort.str$y,type="l",lty=3,col=gray(.2))
                      grid.polygon(map(Nort.map,"Costa",plot=F)[[1]],map(Nort.map,"Costa",plot=F)[[2]],
                                   default.units = "native",gp=gpar(fill=gray(.7)))
                      panel.xyplot(rep(-7,3),c(43.,42.60,42.20),cex=sqrt((leyenda)/escala),pch=16,col=colo)
                      ltext(rep(-7,3),c(43.,42.60,42.20),labels=leyenda,pos=4,offset=1.1,cex=.7)
                      if (indec=="div") {panel.xyplot(x,y,cex=ifelse(dumb$div[subscripts]>0,sqrt((dumb$div[subscripts])/escala),.35),
                                                      pch=ifelse(dumb$div[subscripts]>0,16,20),col=colo)}
                      if (indec=="simp") {panel.xyplot(x,y,cex=ifelse(dumb$simp[subscripts]>0,sqrt((dumb$simp[subscripts])/escala),.35),
                                                       pch=ifelse(dumb$simp[subscripts]>0,16,20),col=colo)}
                      if (indec=="domsimp") {panel.xyplot(x,y,cex=ifelse(dumb$domsimp[subscripts]>0,sqrt((dumb$domsimp[subscripts])/escala),.35),
                                                          pch=ifelse(dumb$domsimp[subscripts]>0,16,20),col=colo)}
                      if (indec=="nesp") {panel.xyplot(x,y,cex=ifelse(dumb$numbesp[subscripts]>0,sqrt((dumb$numbesp[subscripts])/escala),.35),
                                                       pch=ifelse(dumb$numbesp[subscripts]>0,16,20),col=colo)}
                    })}
  if (plot) {print(mapdist)}
  if (out.dat) dumb
  else mapdist
}
