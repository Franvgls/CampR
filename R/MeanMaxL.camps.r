#' Talla máxima media del grupo de especies elegidas
#'
#' Debería de usar especies de un ecotipo similar (mismo gr) y sobre todo medidas en la misma unidad, en caso contrario da un error y avisa del problema.
#' Para cada especie calcula la abundancia estratificada y la talla máxima, con ello hace una media ponderada, la salida
#' @param gr Grupo de la especie: 1 peces, 2 crustáceos 3 moluscos (4 equinodermos 5 invertebrados habitualmente no medidos)
#' @param esps Código de las especies seleccionadas. Si se mezclan grupos hay que introducir el mismo numero de elementos en gr que en esps
#' @param camps Campaña de las que se extraen los datos: Demersales NYY, Porcupine PYY, Arsa primavera 1YY y Arsa otoño 2YY, Medits MYY
#' @param dns Elige el origen de las bases de datos: Porcupine "Pnew", Cantábrico "Cant, Golfo de Cádiz "Arsa", Medits: "Medi"
#' @param lmax Debe incluir los datos de Linf de cada una de las especies en el campo esps. Si se usa NA en un caso utiliza la talla máxima de la campaña camp, se puede dar para cada esps dejando un NA en las especies que se quiera utilizar el maximo de la campaña, o en general
#' @param incl2 Si F no tiene en cuenta los lances especiales, si T si los tiene en cuenta, pero da problemas por que no puede calcular las abundancias estratificadas
#' @param excl.sect Excluye los sectores y estratos en cuestion, si NA usa toda el area.
#' @param plot Si T saca un gráfico con la evolución de la talla media máxima a lo largo de la serie historica
#' @param es Si T saca las etiquetas y titulos en español, Si F en inglés
#' @param ti Si F no saca titulo en el gráfico, si T lo saca por defecto Talla media maxima, se puede utilizar para sacar el titulo que se quiera: p.ej: "Talla media maxima elasmobranquios"
#' @return Devuelve una lista con un data.frame datos por especie: gr, esps, unid, n, lmax y el valor del indice la talla maxima media ponderada a la abundancia
#' @family MSFD
#' @examples MeanMaxL.camps(gr=1,c(50,42,43,44,45),Nsh[7:27],"Cant")
#' @export
MeanMaxL.camps<- function(gr=1,esps,camps,dns="Cant",lmax=NA,incl2=FALSE,excl.sect=NA,plot=TRUE,es=TRUE,ti=FALSE) {
  esp<-format(esps,width=3,justify="r")
  if (length(esps)==1) {stop("Sólo se ha seleccionado una especie, indicador para varias especies")}
  if (length(gr)>1 & c(length(gr)!=length(esps))) {stop("Si se seleccionan grupos distintos el número de grupos y especies debe ser igual")}
  dumb1<-MeanMaxL.camp(gr=gr,esps=esps,camps[1],dns=dns,lmax=lmax,incl2=incl2,excl.sect=excl.sect)
  dumbdatos<-cbind(dumb1$datos,year=camptoyear(camps[1]))
  dumbind=data.frame(MeanLmax=dumb1$ind,year=camptoyear(camps[1]))
  for (i in 2:length(camps)) {
    dumb1<-MeanMaxL.camp(gr=gr,esps=esps,camps[i],dns=dns,lmax=lmax,incl2=incl2,excl.sect=excl.sect)
    dumbdatos<-rbind(dumbdatos,cbind(dumb1$datos,year=camptoyear(camps[i])))
    dumbind<-rbind(dumbind,data.frame(MeanLmax=dumb1$ind,year=camptoyear(camps[i])))
  }
  #  dumb$year<-camptoyear(dumb$camp)
  if (plot) {
    plot(MeanLmax~year,dumbind,type="o",pch=21,bg="grey",ylim=c(0,ceiling(hablar::max_(dumbind$MeanLmax)*1.1)),yaxs="i",
         ylab=paste(ifelse(es,"Talla máxima media","Mean maximum length")," (",dumbdatos$unid[1],")",sep=""),xlab=ifelse(es,"Año","Year"))
    grid()
    if (is.logical(ti)) {
      if (ti) {title(main=ifelse(es,"Talla máxima media","Mean maximum length"),font=2)}
    }
    else title(main=ti)
  }
  print(dumbind)
  list(dumbdatos,dumbind)
}
