#' Mapa distribución entre tallas tmin y tmax uniendo distintas campañas en una sola gráfica
#'
#' Saca mapas de distribución de una especie entre tallas tmin y tmax para varias campañas en una sola gráfica
#' @param gr Grupo de la especie: 1 peces, 2 crustaceos 3 moluscos 4 equinodermos 5 invertebrados. 6 Desechos y otros inorgánicos no tiene sentido sacar tallas, sólo recogidas en peces, crustáceos decápodos y algunos moluscos
#' @param esp Código de la especie numerico o caracter con tres espacios. 999 para todas las especies del grupo
#' @param camps Campaña a representar en el mapa de un año comcreto (XX): Demersales "NXX", Porcupine "PXX", Arsa primavera "1XX" y Arsa otoño "2XX"
#' @param dns Elige el origen de las bases de datos: Porcupine "Porc", Cantábrico "Cant", Golfo de Cádiz "Arsa" (únicamente para sacar datos al IBTS, no gráficos)
#' @param tmin Talla mínima del intervalo de tallas a incluir, si 0 el subtítulo sale =< tmax
#' @param tmax Talla máxima del intervalo de tallas a incluir, si 999 el subtítulo sale >= tmin
#' @param cor.time Si T corrige las abundancias en función de la duración del lance
#' @param incl2 Si F no presenta los lances especiales, si T si los tiene en cuenta, pero puede dar problemas por que no puede calcular las abundancias estratificadas
#' @param ind Permite elegir entre número "n" o peso "p" peso sólo funciona cuando existen a y b en especies.dbf y se elige sólo una especie
#' @param sex Permite elegir entre machos(1), hembras(2) o indeterminados(3), NA escoge sin tener en cuenta el sexo
#' @param bw gráfico en blanco en negro si T o en color si F
#' @param ICESrect Si T saca los rectangulos ices de 1 grado de latitud por medio de longitud
#' @param ti Si T añade titulo al mapa, el nombre de la especie en latín. También se puede poner como list con parámetros **main**: el título, **font** y demás.
#' @param subti ti T añade un subtítulo bajo la gráfica con el rango de tallas seleccionado.
#' @param plot Saca el gráfico (T) o si se salva como objeto se puede componer para componer con otros gráficos de lattice (F)
#' @param out.dat Si T el resultado final de la funcion es la figura en pantalla, pero los datos en objeto
#' @param idi Nombre científico de la especie ("l") o nombre común ("e")
#' @param layout Organización de gráficos en filas ó columnas c(r,c)
#' @param leg Si T añade la leyenda
#' @param ceros Añade puntos para los datos igual a 0 si T, si F no incluye x en los valores 0
#' @param escmult Varía la relación de tamaño de los puntos con la leyenda y el máximo en los datos
#' @param cexleg Varía el tamaño de letra de los ejes y del número de la leyenda
#' @param years Si T saca los años como nombre de campaña en los paneles lattice de campañas
#' @param graf Si F no saca nada, si pones el nombre de un gráfico lo saca saca como archivo png y al final del proceso dice dónde está el mapa con ese nombre:
#' @param xpng width archivo png si graf es el nombre del fichero
#' @param ypng height archivo png si graf es el nombre del fichero
#' @param ppng points png archivo si graf es el nombre del fichero
#' @return Si out.dat=TRUE devuelve un data.frame con columnas: lan,lat,long,prof,numero (de individuos entre tmin y tmax),camp, si out.dat=F saca el gráfico en pantalla o como objeto para combinar con otros gráficos con print.trellis
#' @examples maphistal.one(1,50,Nsh[32:41],"Cant",1,23)
#' @family mapas
#' @family tallas
#' @export
maphistal.one<-function(gr,esp,camps,dns="Porc",tmin=0,tmax=999,cor.time=TRUE,incl2=TRUE,ind="n",ICESrect=FALSE,
                    sex=NA,bw=FALSE,ti=TRUE,subti=TRUE,plot=TRUE,out.dat=FALSE,idi="l",leg=TRUE,ceros=TRUE,
                    escmult=.01,cexleg=1,years=TRUE,graf=FALSE,xpng=1000,ypng=500,ppng=15) {
  options(scipen=2)
  colo<-ifelse(bw,gray(.1),4)
  if (plot) {
    if (bw) {
      colo=gray(.2)
    }
    else {
      colo=4
    }
    }
  if (length(esp)>1 | any(esp=="999")) {
    message("Distintas especies pueden estar medidas en distintas unidades (mm y cm) o a la aleta anal")
    if (!is.na(sex)) {
      stop("No se pueden seleccionar sexo para más de una especie")
      }
    if (ind=="p") {
      stop("No se pueden calcular capturas en peso de un rango de tallas para más de una especie")
      }
    medida<-c("cm")
    }
  else { medida<-ifelse(unid.camp(gr,esp)["MED"]==1,"cm","mm") }
  esp<-format(esp,width=3,justify="r")
	ndat<-length(camps)
	dumb<-data.frame(camp=camps[1],dattalgr.camp(gr,esp,camps[1],dns,tmin,tmax,cor.time=cor.time,incl2=incl2,sex,ind))
	for (i in 2:ndat) {
	    dumb<-rbind(dumb,cbind(camp=camps[i],dattalgr.camp(gr,esp,camps[i],dns,tmin,tmax,cor.time=cor.time,incl2=incl2,sex,ind)))
	  }
	if (years) {
    dumbcamp<-dumb
    dumb$camp<-camptoyear(dumb$camp)
    }
	dumb$camp<-factor(dumb$camp)
	if (ind=="n") {
	  leyenda<-signif(max(dumb$numero,na.rm=TRUE)*.9,1)
	  escala<-signif(max(dumb$numero,na.rm=TRUE),1)
	}
	else {
	  leyenda<-signif(max(dumb$peso,na.rm=TRUE)*.9,1)
	  escala<-signif(max(dumb$peso,na.rm=TRUE),1)
	}
	vals_leg <- leyenda * c(1, 0.5, 0.1)
	pt_sizes <- sqrt(vals_leg / escala) * escmult
	if (is.logical(ti)) {
	  if (ti) {titulo<-list(label=buscaesp(gr,esp,id=idi),font=ifelse((idi=="l" & gr!="9" & esp!="999"),4,2))
	  }
	  else {titulo<-NULL}
	}
	else {
	  if(is.list(ti)) titulo<-ti
	  else titulo<-list(label=ti)
	}
	if (subti) {
	if (tmin==0) sub<-list(label=bquote(" "<=.(format(paste0(tmax,ifelse(unid.camp(gr,esp)$MED==2," mm"," cm"))))),font.sub=2,cex=cexleg*.9)
	if (tmax==999) sub<-list(font.sub=2,label=bquote(" ">=.(format(paste0(tmin,ifelse(unid.camp(gr,esp)$MED==2," mm"," cm"))))),cex=cexleg*.9)
	if (tmin!=0 & tmax!=999) sub<-list(font.sub=2,label=paste(tmin,"-",tmax,ifelse(unid.camp(gr,esp)$MED==2,"mm","cm")),cex=cexleg*.9)
	if (tmin==0 & tmax==999) sub<-list(font.sub=2,label=paste(tmin,"-",tmax,ifelse(unid.camp(gr,esp)$MED==2,"mm","cm")),cex=cexleg*.9)
	}
	if (!is.logical(graf)) png(filename=paste0(graf,".png"),width=xpng,height=ypng,pointsize=ppng)
	par(mar=c(3.5,4,3.2,3.5)+0.1)
	if (substr(dns,1,4)=="Pnew" | substr(dns,1,4)=="Porc") {
    mapporco()
	  if (ind=="n") {points(lat~long,dumb,pch=21,lwd=2,col="navy",cex=sqrt(numero/escala)*escmult,subset = numero>0)}
	  if (ind=="p") {points(lat~long,dumb,pch=21,lwd=2,col="navy",cex=sqrt(peso/escala)*escmult,subset = peso>0)}
	  legend("bottomright",
	         legend = paste0(format(vals_leg, big.mark = ""), ifelse(ind=="n"," ind."," g")),
	         pch = 21,
	         pt.cex = pt_sizes,
	         pt.lwd = 2,
	         col = "navy",
	         bg = "white",        # Fondo blanco, bordes limpios
	         box.col = "black",   # Marco negro
	         y.intersp = 2,     # Más espacio vertical (ajusta según necesites)
	         x.intersp = 1.2,     # Espacio entre símbolo y texto
	         inset = 0.02
	  )
	  # legend("bottomright",legend = as.character(valores_ley),pch = 21,pt.cex = pt_sizes,pt.lwd = 2,col = "navy",
	  #        bty = "o",bg="white",inset = 0.02,y.intersp = 3,x.intersp = 2,text.font = 2,cex = cexleg)
	  # legend("bottomright",legend = as.character(leyenda*c(1,.5,.1)),pch=21,pt.cex=sqrt(leyenda*c(1,.5,.1)/escala)*escmult,pt.lwd = 2,
	  #        col="navy",bg="white",inset=.02)
	  }
	if (substr(dns,1,4)=="Cant" | substr(dns,1,4)=="Cnew") {
	  MapNort(places=T)
	  if (ind=="n") {points(lat~long,dumb,pch=21,lwd=2,col="navy",cex=sqrt(numero/escala)*escmult,subset = numero>0)}
	  if (ind=="p") {points(lat~long,dumb,pch=21,lwd=2,col="navy",cex=sqrt(peso/escala)*escmult,subset = peso>0)}
	  legend("bottomright",
	         legend = paste0(format(vals_leg, big.mark = ""), ifelse(ind=="n"," ind."," g")),
	         pch = 21,
	         pt.cex = pt_sizes,
	         pt.lwd = 2,
	         col = "navy",
	         bg = "white",        # Fondo blanco, bordes limpios
	         box.col = "black",   # Marco negro
	         y.intersp = 1,     # Más espacio vertical (ajusta según necesites)
	         x.intersp = 1,     # Espacio entre símbolo y texto
	         inset = 0.02
	  )
	  # legend("bottomright",legend = as.character(valores_ley),pch = 21,pt.cex = pt_sizes,pt.lwd = 2,col = "navy",
	  #        bty = "o",bg="white",inset = 0.02,y.intersp = 3,x.intersp = 2,text.font = 2,cex = cexleg)
	  # legend("bottomright",legend=as.character(leyenda*c(1,.5,.1)),pch=21,pt.lwd=2,pt.cex = sqrt(leyenda*c(1,.5,.1)/escala)*escmult,
	  #        bg="white",col="navy",inset=.02,y.intersp = 1.5)
	}
	if (dns=="Arsa") {
	  MapArsa(places=T)
	  if (ind=="n") {points(lat~long,dumb,pch=21,lwd=2,col="navy",cex=sqrt(numero/escala)*escmult,subset = numero>0)}
	  if (ind=="p") {points(lat~long,dumb,pch=21,lwd=2,col="navy",cex=sqrt(peso/escala)*escmult,subset = peso>0)}
	  legend("topright",
	         legend = paste0(format(vals_leg, big.mark = ""), ifelse(ind=="n"," ind."," g")),
	         pch = 21,
	         pt.cex = pt_sizes,
	         pt.lwd = 2,
	         col = "navy",
	         bg = "white",        # Fondo blanco, bordes limpios
	         box.col = "black",   # Marco negro
	         y.intersp = 2,     # Más espacio vertical (ajusta según necesites)
	         x.intersp = 1.2,     # Espacio entre símbolo y texto
	         inset = 0.02,
	         cex=1.5
	  )
	  # legend("topright",legend=as.character(leyenda*c(1,.5,.1)),pch=21,pt.lwd=2,pt.cex = sqrt(leyenda*c(1,.5,.1)/escala)*escmult,
	  #        bg="white",col="navy",inset=.02,y.intersp = 1.5)
	}
	if (dns=="Medi") {
	  MapMedit(places=T)
	  if (ind=="n") {points(lat~long,dumb,pch=21,lwd=2,col="navy",cex=sqrt(numero/escala)*escmult,subset = numero>0)}
	  if (ind=="p") {points(lat~long,dumb,pch=21,lwd=2,col="navy",cex=sqrt(peso/escala)*escmult,subset = peso>0)}
	  legend("bottomright",
	         legend = paste0(format(vals_leg, big.mark = ""), ifelse(ind=="n"," ind."," g")),
	         pch = 21,
	         pt.cex = pt_sizes,
	         pt.lwd = 2,
	         col = "navy",
	         bg = "white",        # Fondo blanco, bordes limpios
	         box.col = "black",   # Marco negro
	         y.intersp = 2,     # Más espacio vertical (ajusta según necesites)
	         x.intersp = 1.2,     # Espacio entre símbolo y texto
	         inset = 0.02
	  )
	}
	if (ti) {
	  title(main=titulo$label,font=4,line=2)
	  mtext(paste0(camptoyear(camps[1]),"-",camptoyear(camps[length(camps)])),side = 3,line = 2,adj = 0,font=2,cex =.9)
	  }
	if (subti) {mtext(sub$label,side = 1,line = 2,font=2,cex =.9)}
	if (!is.logical(graf)) {
	  dev.off()}
	# if (plot) #{print(mapdist)}
	# if (!plot) #return(mapdist)
  if (!is.logical(graf)) {
	  dev.off()
	  message(paste0("figura: ",getwd(),"/",graf,".png"))
	}
	if (!is.logical(graf)) par(mar=c(5, 4, 4, 2) + 0.1)
	if (out.dat) {
	  if (ind=="n") dumb$numero<-round(dumb$numero,1)
    if (ind=="p") dumb$peso<-round(dumb$peso,2)
	  if (years) dumb<-dumbcamp
    if (!ceros) dumb<-dumb[dumb$numero>0,]
    print(dumb)
    }
}
