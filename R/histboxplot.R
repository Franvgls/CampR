#' Gráficos de boxplot para la serie histórica incluyendo lances especiales o no y con rangos de profundidad
#'
#' @description
#' Crea box plots con la distribución de la abundancia, en biomasa o número, no teniendo
#' en cuenta la ponderación al área (_see details_) para distintas zonas: Porcupine
#' (dns="Porc"), el Cantábrico (dns=Cant), Cádiz= (dns=Arsa), y el Mediterráneo
#' (dns=Medi).
#'
#' @details
#' Estos boxplots pueden ser engañosos y hay que explicar bien en la gráfica sus resultados. Sí se decide eliminar los ceros (=F) hay que tener cuidado porque los
#' resultados pueden depender mucho del número de lances sin captura y dar resultados muy engañosos al poder compararse datos con distinto numero de lances. Ver en
#' examples ambas opciones
#'
#' @param gr Grupo de la especie: 1 peces, 2 crustáceos 3 moluscos 4 equinodermos 5 invertebrados 6 desechos y otros, 9 escoge todos los orgánicos pero excluye desechos
#' @param esp Código de la especie numérico o carácter con tres espacios. 999 para todas las especies del grupo
#' @param camps Campaña a representar en el mapa de un año concreto (XX): Demersales "NXX", Porcupine "PXX", Arsa primavera "1XX" y Arsa otoño "2XX"
#' @param dns Elige el origen de las bases de datos: Porcupine "Porc" o  "Pnew", Cantábrico "Cant", Golfo de Cadiz "Arsa" (proporciona los datos para Medits pero no saca mapas)
#' @param cor.time Si T corrige las abundancias en función de la duración del lance
#' @param incl2 Si F no tiene en cuenta los lances especiales, si T si los tiene en cuenta, pero da problemas por que no puede calcular las abundancias estratificadas
#' @param bw Gráfico en blanco en negro si T o en color si F
#' @param ti Añade el nombre de la especie en latín sin T, si F no añade titulo
#' @param sub Añade un subtítulo debajo del gráfico, sin texto por defecto.
#' @param plot Saca el gráfico (T) o lo guarda como objeto para componer con otros gráficos (F)
#' @param out.dat Si T el resultado final de la función es la figura en pantalla, pero los datos en objeto
#' @param ind Parámetro a representar saca los datos en "p"eso o "n"úmero
#' @param es Si T rotulos gráfico en español, si F en inglés
#' @param profrange Si c(profmin,profmax) filtra por ese rango de profundidad por defecto NA no filtra profundidades
#' @param ceros por defecto incluye los valores de 0 al calcular los rangos y medianas, si T los quita, Es importante avisarlo en el la explicación de la gráfica
#' @param nlans T por defecto presenta el número de lances en la campaña por encima del eje x
#' @param lan.cex tamaño de las etiquetas del numero de lances por campaña
#' @param years Si T saca los años como nombre de campaña en los paneles lattice de campañas
#' @param idi Nombre científico de la especie ("l") o nombre común ("e")
#' @param escmult Varía la relación de tamaño de los puntos con la leyenda y el máximo en los datos
#' @param cex.leg Varía el tamaño de letra de los ejes y del número de la leyenda
#' @return Si out.dat=TRUE devuelve un data.frame con columnas: lan,lat,long,prof,peso.gr,numero (de individuos entre tmin y tmax),camp, si out.dat=F saca el gráfico en pantalla o como objeto para combinar con otros gráficos con print.trellis
#' @examples
#' histboxplot(1,50,Nsh[7:27],"Cant",years=TRUE)
#' histboxplot(1,50,Nsh[7:27],"Cant",years=TRUE,ind="n")
#' histboxplot(1,50,Nsh[7:27],"Cant",years=TRUE,ind="n",ceros=FALSE)
#' @family abunds
#' @export
histboxplot<-function(gr,esp,camps,dns="Porc",cor.time=TRUE,incl2=TRUE,es=T,bw=TRUE,ti=TRUE,sub=NULL,out.dat=FALSE,ind="p",idi="l",
  ceros=TRUE,cex.leg=1.1,years=TRUE,profrange=NA,proflab=F,nlans=TRUE,lan.cex=.6) {
  options(scipen=2)
  esp<-format(esp,width=3,justify="r")
  especie<-buscaesp(gr,esp,idi)
  colo<-ifelse(bw,gray(1),"lightblue")
  ndat<-length(camps)
	dumb<-NULL
	for (i in 1:ndat) {
    tempdumb<-datgr.camp(gr,esp,camps[i],dns,cor.time=cor.time,incl2=incl2)
		if (!is.null(tempdumb)) dumb<-rbind(dumb,cbind(tempdumb,camp=camps[i]))
	}
	if (years) {
    dumbcamp<-dumb
    dumb$camp<-camptoyear(dumb$camp)
    }
	dumb$camp<-factor(dumb$camp)
	if (out.dat) print(dumb[dumb[,5]>0,])
	if (!ceros) dumb<-filter(dumb,numero>0)
	if (any(!is.na(profrange))) {
	  dumb<-filter(dumb,prof>min(profrange) & prof<max(profrange))
	    if (min(profrange)==0) prang<-bquote("Depth range" <=.(format(paste0(max(profrange),"m")))) #list(label=bquote(" "<=.(format(paste0(max(profrange),"m")))),font.sub=2,cex=cex.leg*.9)
	    if (max(profrange)==9999) prang<-bquote("Depth range">=.(format(paste0(min(profrange),"m"))))
	    if (min(profrange)!=0 & max(profrange)!=9999) prang<-paste("Depth range:",min(profrange),"-",max(profrange),"m")
	    if (min(profrange)==0 & max(profrange)==9999) prang<-paste("Depth range:",min(profrange),"-",max(profrange),"m")
	}
#  op<-par(no.readonly=T)
#  par(mgp=c(2.5,.8,0))
	if (ind=="p") {
	    dumb$peso<-dumb$peso.gr/1000
	    boxplot(peso~camp,dumb,outline=F,varwidth=T,col=colo,ylab=ifelse(es,expression("kg"%*%"lance"^-1),expression("kg"%*%"haul"^-1)),
	     xlab=ifelse(es,"Año","Year"),las=2,cex.axis=cex.leg*.8)
	}
  if (ind=="n") {
    boxplot(numero~camp,dumb,outline=F,varwidth=T,col=colo,ylab=ifelse(es,expression("ind"%*%"lance"^-1),expression("ind"%*%"haul"^-1)),
      xlab=ifelse(es,"Año","Year"),las=2,cex.axis=cex.leg*.8)
  }
	if (!ceros) mtext(ifelse(es,"Lances sin captura excluidos","0 catches hauls excluded"),
	                  side=3,0,font=2,cex=.8,adj=ifelse(ceros,0,1))
	if (any(!is.na(profrange))) mtext(prang,   #paste(ifelse(es,"Rango prof: ","Depth range: "),expression(prang))
	                  side=3,font=2,cex=.8,adj=0)
	if (nlans) mtext(side=1,at=1:ndat,line=-1,text=tapply(dumb$numero,dumb$camp,length),cex=lan.cex,font=2)
	if (is.logical(ti)) {
	   if (ti) {title(main=especie,cex.main=1.1*cex.leg,
	                  font.main=ifelse((idi!="l" | any(esp=="999")),2,4),line=ifelse(any(is.character(sub),sub),1.5,1))}
	}
	else {title(main=buscaesp(gr,esp),font.main=4,line=1.3,cex.main=1.1*cex.leg)}
	if (is.logical(sub)) {
	  if (sub) {title(main=ifelse(ind=="p",ifelse(es,"Biomasa","Biomass"),ifelse(es,"Número","Number")),
	                  font.main=2,line=.5,cex.main=cex.leg*.9)}
	}
	else title(main=sub,line=.3,font.main=2,cex.main=cex.leg*.9)
	#if(any(!is.na(profrange)) & proflab) mtext(paste(ifelse(es,"Rango prof:","Depth range:"),min(profrange),"-",max(profrange),"m",collapse=" "),side=3,cex=.7,font=2,adj=ifelse(ceros,1,0))
	if (out.dat) {
    dumb$peso<-round(dumb$peso,3)
    if (years) dumb<-dumbcamp
    if (!ceros) dumb<-dumb[dumb$numero>0,]
    print(dumb)
    }
#  par(op)
}
