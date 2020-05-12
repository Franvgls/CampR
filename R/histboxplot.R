#' Gráficos de boxplot para la serie histórica incluyendo lances especiales o no y con rangosx de profundidad
#'
#' Crea mapas con la distribución en biomasa o numero para distintas zonas: Porcupine (dns="Pnew"), el Cantábrico (dns=Cant), Cádiz= (dns=Arsa), y el Mediterráneo (dns=Medi)
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
#' @param idi Nombre científico de la especie ("l") o nombre común ("e")
#' @param es Si T rotulos gráfico en español, si F en inglés
#' @param profrange Si c(profmin,profmax) filtra por ese rango de profundidad
#' @param layout Organización de gráficos en filas ó columnas c(r,c)
#' @param ceros por defecto incluye los valores de 0 al calcular los rangos y medianas, si T los quita, reflejarlo en el pie del gráfico
#' @param escmult Varía la relación de tamaño de los puntos con la leyenda y el máximo en los datos
#' @param cexleg Varía el tamaño de letra de los ejes y del número de la leyenda
#' @param years Si T saca los años como nombre de campaña en los paneles lattice de campañas
#' @return Si out.dat=TRUE devuelve un data.frame con columnas: lan,lat,long,prof,peso.gr,numero (de individuos entre tmin y tmax),camp, si out.dat=F saca el gráfico en pantalla o como objeto para combinar con otros gráficos con print.trellis
#' @examples
#' histboxplot(1,50,Nsh[7:27],"Cant",years=TRUE)
#' histboxplot(1,50,Nsh[7:27],"Cant",years=TRUE,ind="n")
#' @family abunds
#' @export
histboxplot<-function(gr,esp,camps,dns="Porc",cor.time=TRUE,incl2=TRUE,es=T,bw=TRUE,ti=TRUE,sub=NULL,out.dat=FALSE,ind="p",idi="l",
  ceros=TRUE,cex.leg=1.1,years=TRUE,profrange=NA) {
  options(scipen=2)
  esp<-format(esp,width=3,justify="r")
  especie<-buscaesp(gr,esp,idi)
  colo<-ifelse(bw,gray(.8),"lightblue")
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
	if (any(!is.na(profrange))) dumb<-filter(dumb,prof>min(profrange) & prof<max(profrange))
	if (ind=="p") {
	    dumb$peso<-dumb$peso.gr/1000
	    boxplot(peso~camp,dumb,outline=F,varwidth=T,col=colo,ylab=ifelse(es,expression("kg"%*%"lance"^-1),expression("kg"%*%"haul"^-1)),
	     xlab=ifelse(es,"Año","Year"),las=2)
	}
  if (ind=="n") {
    boxplot(numero~camp,dumb,outline=F,varwidth=T,col=colo,ylab=ifelse(es,expression("ind"%*%"lance"^-1),expression("ind"%*%"haul"^-1)),
      xlab=ifelse(es,"Año","Year"),las=2)
  }
	if (is.logical(ti)) {
	  if (ti) {title(main=especie,cex.main=1.1*cex.leg,
	                 font.main=ifelse((idi!="l" | any(esp=="999")),2,4),line=ifelse(any(is.character(sub),sub),1.5,1))}
	}
	else {title(main=ti,font.main=4,line=1.3,cex.main=1.1*cex.leg)}
	if (is.logical(sub)) {
	  if (sub) {title(main=ifelse(ind=="p",ifelse(es,"Biomasa","Biomass"),ifelse(es,"Número","Number")),
	                  font.main=2,line=.3,cex.main=cex.leg*.9)}
	}
	else title(main=sub,line=.3,font.main=2,cex.main=cex.leg*.9)
	if(any(!is.na(profrange))) mtext(paste(ifelse(es,"Rango prof:","Depth range:"),min(profrange),"-",max(profrange),"m",collapse=" "),side=3,line=.1,cex=.7,font=2,adj=1)
	if (out.dat) {
    dumb$peso<-round(dumb$peso,3)
    if (years) dumb<-dumbcamp
    if (!ceros) dumb<-dumb[dumb$numero>0,]
    print(dumb)
    }
	}
