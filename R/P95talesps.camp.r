#' Da el percentil .95 de la distribucion de tallas estratificada de un grupo de especies
#'
#' Funcion indicador MSFD Da el percentil perc (por defecto 95, en decimal) de la distribución de tallas de las especies en las serie historica campañas seleccionadas.
#' @param gr Grupo de la especie: 1 peces, 2 crustáceos 3 moluscos (4 equinodermos 5 invertebrados habitualmente no medidos)
#' @param esps El grupo de códigos de la especies seleccionadas, no mezclar grupos distintos
#' @param camps Campañas de las que se extraen los datos: Demersales Nsh, Porcupine Psh, Arsa primavera As1 y Arsa otoño As2
#' @param dns Elige el origen de las bases de datos: Porcupine "Pnew", Cantábrico "Cant, Golfo de Cádiz "Arsa", Medits: "Medi"
#' @param excl.sect Excluye el los sectores y estratos en cuestion, si NA usa toda el area.
#' @param perc El percentil de la distribucion de tallas a calcular, .95 es el valor utilizado habitualmente como indicador del maximo de la distribucione de tallas
#' @param graf Si T saca el gráfico de evolucion del p95 en la serie historica buscada para cada especie en lattice, si F ignora el gráfico
#' @param pch El simbolo a utilizar
#' @param lty Ancho de la linea que une los puntos
#' @param layout Configuracion del lattice en filas y columnas c(r,c)
#' @param bw Si T saca un gráfico en blanco y negro, si F los rotulos de los lattice van en color
#' @param cexleg Tamaño de la fuente de los nombres de las especies
#' @param years Si T en vez de campañas la serie historica se marca con los años en lugar del codigo de la campaña
#' @param es Si T los rotulos de los ejes se muestran en español, si F en ingles
#' @param ylim Valor maximo del eje de ordenadas, las tallas
#' @param out.dat si T saca los datos, en si F da el objeto lattice
#' @family MSFD
#' @return Devuelve un data.frame con columnas especie, camp y p95tal y year. Ademas crea un gráfico de lattice con la evolucion del p95 para cada especie en un panel distinto
#' @examples P95talesps.camp(1,c(5,8,9,14,105),Nsh[7:27],"Cnew",layout=NA,graf=TRUE,years=TRUE,es=FALSE)
#' @export
P95talesps.camp<-function(gr=1,esps,camps,dns="Cant",excl.sect=NA,perc=.95,graf=TRUE,pch=20,lty=1,layout=NA,bw=FALSE,cexleg=.8,years=TRUE,es=TRUE,ylim=NA,out.dat=TRUE) {
  esps<-format(esps,width=3,justify="r")
  dumbp95<-cbind(especie=buscaesp(gr,esps[1]),p95tal.camp(gr,esps[1],camps,dns,excl.sect=excl.sect,perc=perc))
  for (i in esps[2:length(esps)]) {
    dumbp95<-rbind(dumbp95,cbind(especie=buscaesp(gr,i),p95tal.camp(gr,i,camps,dns,excl.sect=excl.sect,perc=perc)))
  }
  dumbp95$year<-camptoyear(as.character(dumbp95$camp))
  if (years) labx<-as.character(dumbp95$year) else labx<-as.character(dumbp95$camp)
  lattice::trellis.par.set(lattice::col.whitebg())
  if (bw) lattice::trellis.par.set("strip.background",list(col=c(gray(.80))))
  if (any(is.na(layout))) if (length(esps)<4) {layout<-c(1,length(esps))} else layout<- rep(ceiling(sqrt(length(esps))),2)
  if (is.na(ylim)) ylim<-c(0,max(dumbp95$p95tal)*1.1)
  if (graf) {
      print(lattice::xyplot(p95tal~camp|especie,dumbp95,type="o",pch=pch,lty=lty,layout=layout,par.strip.text=list(cex=cexleg+.1,font=4),
                 xlab=ifelse(years,ifelse(es,"Año","Year"),ifelse("Campaña","Survey")),ylab=ifelse(es,"Talla (cm)","Length (cm)"),as.table=TRUE,
                 scales=list(alternating=FALSE,tck=c(1,0),cex=cexleg,x=list(labels=labx,rot=90),
                             y=list(limits=ylim,rot=90))))
    add_axes()
  }
  else {lattice::xyplot(p95tal~camp|especie,dumbp95,type="o",pch=pch,lty=lty,layout=layout,par.strip.text=list(cex=cexleg+.1,font=4),
                       xlab=ifelse(years,ifelse(es,"Año","Year"),ifelse("Campaña","Survey")),ylab=ifelse(es,"Talla (cm)","Length (cm)"),as.table=TRUE,
                       scales=list(alternating=FALSE,tck=c(1,1),cex=cexleg,x=list(labels=labx,rot=90),
                                   y=list(limits=ylim,rot=90)))
    add_axes()
  }
if (out.dat) dumbp95
}

add_axes <- function() {
  library(grid)
  library(lattice)
  l <- trellis.currentLayout()
  pan <- which(l[nrow(l), ]==0)
  if(length(pan) > 0) {
    g <- grid.ls(print=FALSE)
    # use an existing panel as a template for ticks
    ticks <- grid.get(g$name[grep("ticks.bottom.panel", g$name)][[1]])
    # use an existing panel as a template for labels
    labels <- grid.get(g$name[grep("ticklabels.bottom.panel", g$name)][[1]])
    ax <- grobTree(ticks, labels)
    invisible(sapply(pan, function(x) {
      trellis.focus("panel", x, nrow(l)-1, clip.off=TRUE)
      grid.draw(ax)
      trellis.unfocus()
    }))
  }
}
