#' Da el percentil .95 de la distribucion de tallas estratificada de un grupo de especies
#' 
#' Funcion indicador MSFD Da el percentil perc (por defecto 95, en decimal) de la distribución de tallas de las especies en las serie historica campañas seleccionadas.  
#' @param gr Grupo de la especie: 1 peces, 2 crustáceos 3 moluscos (4 equinodermos 5 invertebrados habitualmente no medidos)
#' @param esps El grupo de códigos de la especies seleccionadas, no mezclar grupos distintos
#' @param camps Campañas de las que se extraen los datos: Demersales Nsh, Porcupine Psh, Arsa primavera Ash y Arsa otoño Ash
#' @param dns Elige el origen de las bases de datos: Porcupine "Pnew", Cantábrico "Cant, Golfo de Cádiz "Arsa", Medits: "Medi"
#' @param excl.sect Excluye el los sectores y estratos en cuestion, si NA usa toda el area.
#' @param perc El percentil de la distribucion de tallas a calcular, .95 es el valor utilizado habitualmente como indicador del maximo de la distribucione de tallas
#' @param graf Si T saca el gráfico de evolucion del p95 en la serie historica buscada para cada especie en lattice, si F ignora el gráfico
#' @param pch El simbolo a utilizar {\link{pchShow.r}}
#' @param lty Ancho de la linea que une los puntos
#' @param layout Configuracion del lattice en filas y columnas c(r,c)
#' @param bw Si T saca un gráfico en blanco y negro, si F los rotulos de los lattice van en color
#' @param cexleg Tamaño de la fuente de los nombres de las especies
#' @param years Si T en vez de campañas la serie historica se marca con los años en lugar del codigo de la campaña
#' @param es Si T los rotulos de los ejes se muestran en español, si F en ingles
#' @param ylim Valor maximo del eje de ordenadas, las tallas
#' @return Devuelve un data.frame con columnas especie, camp y p95tal y year. Ademas crea un gráfico de lattice con la evolucion del p95 para cada especie en un panel distinto
#' @examples P95talesps.camp(1,c(5,8,9,14,105),Nsh[7:27],"Cnew",layout=NA,graf=T,years=T,es=F)
#' @export
P95talesps.camp<-function(gr=1,esps,camps,dns="Cant",excl.sect=NA,perc=.95,graf=T,pch=20,lty=1,layout=NA,bw=F,cexleg=.8,years=T,es=T,ylim=NA) {
  esps<-format(esps,width=3,justify="r")
  dumbp95<-cbind(especie=buscaesp(gr,esps[1]),p95tal.camp(gr,esps[1],camps,dns,excl.sect=excl.sect,perc=perc))
  for (i in esps[2:length(esps)]) {
    dumbp95<-rbind(dumbp95,cbind(especie=buscaesp(gr,i),p95tal.camp(gr,i,camps,dns,excl.sect=excl.sect,perc=perc)))
  }
  if (graf) {
    dumbp95$year<-camptoyear(as.character(dumbp95$camp))
    if (years) labx<-as.character(dumbp95$year) else labx<-as.character(dumbp95$camp)      
    require(lattice)
    trellis.par.set(col.whitebg())
    if (bw) trellis.par.set("strip.background",list(col=c(gray(.80))))
    if (any(is.na(layout))) if (length(esps)<4) {layout<-c(1,length(esps))} else layout<- rep(ceiling(sqrt(length(esps))),2)
    if (is.na(ylim)) ylim<-c(0,max(dumbp95$p95tal)*1.1)
    print(xyplot(p95tal~camp|especie,dumbp95,type="o",pch=pch,lty=lty,layout=layout,par.strip.text=list(cex=cexleg+.1,font=4),
                 xlab=ifelse(years,ifelse(es,"Año","Year"),ifelse("Campaña","Survey")),ylab=ifelse(es,"Talla (cm)","Length (cm)"),as.table=T,
                 scales=list(alternating=F,tck=c(1,0),cex=cexleg,x=list(labels=labx,rot=90),
                             y=list(limits=ylim,rot=90)))) 
  }
  dumbp95
}
