#' Talla máxima media del grupo de especies elegidas
#' 
#' Debería de usar especies de un ecotipo similar (mismo gr) y sobre todo medidas en la misma unidad, en caso contrario da un error y avisa del problema.
#' Para cada especie calcula la abundancia estratificada y la talla máxima, con ello hace una media ponderada, la salida
#' @param gr Grupo de la especie: 1 peces, 2 crustáceos 3 moluscos (4 equinodermos 5 invertebrados habitualmente no medidos)
#' @param esps Código de las especies seleccionadas. Si se mezclan grupos hay que introducir el mismo numero de elementos en gr que en esps
#' @param camp Campaña de la que se extraen los datos: Demersales NYY, Porcupine PYY, Arsa primavera 1YY y Arsa otoño 2YY, Medits MYY
#' @param dns Elige el origen de las bases de datos: Porcupine "Pnew", Cantábrico "Cant, Golfo de Cádiz "Arsa", Medits: "Medi"
#' @param lmax Debe incluir los datos de Linf de cada una de las especies en el campo esps. Si se usa NA en un caso utiliza la talla máxima de la campaña camp, se puede dar para cada esps dejando un NA en las especies que se quiera utilizar el maximo de la campaña, o en general
#' @param cor.time Si T corrige las abundancias en función de la duración del lance
#' @param especiales Si F no tiene en cuenta los lances especiales, si T si los tiene en cuenta, pero da problemas por que no puede calcular las abundancias estratificadas
#' @param excl.sect Excluye los sectores y estratos en cuestion, si NA usa toda el area.
#' @return Devuelve una lista con un data.frame datos por especie: gr, esps, unid, n, lmax y el valor del indice la talla maxima media ponderada a la abundancia
#' @seealso MeanMaxL.camps {\link{MeanMaxL.camps}}
#' @examples MeanMaxL.camp(gr=1,c(50,42,43,44,45),"N05","Cant")
#' @export
MeanMaxL.camp<- function(gr=1,esps,camp,dns="Cant",lmax=NA,cor.time=T,especiales=F,excl.sect=NA) {
  #   n se calcula con CV.camp, luego no tiene en cuenta lances especiales.
  #   especiales=F hace que Lmax sólo salga de los lances estándares, si se hace igual a T tiene en cuenta todos los lances.
  #   excl.sect=NA por defecto, si se selecciona sectores p.ej. c(1:3) dejar?a el resto de los sectores, 4 y 5 en Cantabrico y Galicia
  #     tb sirve para excluir estratos geográficos.
  esps<-format(esps,width=3,justify="r")
  if (length(camp)>1) {stop("seleccionadas más de una campaña, no se pueden sacar resultados de más de una")}
  if (length(esps)==1) {stop("Sólo se ha seleccionado una especie, indicador para varias especies")}
  if (length(gr)>1 & c(length(gr)!=length(esps))) {stop("Si se seleccionan grupos distintos gr y esps deben tener igual longitud")}
  require(RODBC)
  dumbdat<-data.frame(gr=gr,esps=esps)
  dumbdat<-cbind(dumbdat,unid=NA,n=NA,lmax=lmax)
  datlan<-datlan.camp(camp,dns,incl2=ifelse(especiales,T,F),excl.sect=excl.sect)
  #  if (any(!is.na(excl.sect))) {
  #	  datlan$sector<-gsub("NA","N",datlan$sector) # print(datos)
  #		for (i in 1:length(excl.sect)) {datlan<-datlan[-grep(excl.sect[i],as.character(datlan$sector)),]}
  #		datlan$sector<-factor(as.character(datlan$sector))
  #		}
  # 	ch1<-odbcConnect(dns)
  #	odbcSetAutoCommit(ch1, FALSE)
  for (i in 1:nrow(dumbdat)) {
    dumbdat$unid[i]<-ifelse(unid.camp(dumbdat$gr[i],dumbdat$esps[i])[1]==1,"cm","mm")
    dumbdat$n[i]<-CV.camps(dumbdat$gr[i],dumbdat$esps[i],camp,dns,cor.time=cor.time,excl.sect=excl.sect)$number
    #browser()
    ch1<-odbcConnect(dns)
    odbcSetAutoCommit(ch1, FALSE)
    ntalls<-sqlQuery(ch1,paste("select lance,peso_gr,peso_m,talla,sexo,numer from NTALL",camp,
                               " where grupo='",dumbdat$gr[i],"' and esp='",as.character(dumbdat$esps[i]),"'",sep=""))
    odbcClose(ch1)
    names(ntalls)<-gsub("_", ".",names(ntalls))
    if (any(c(!especiales,!is.na(excl.sect)))) ntalls<-ntalls[ntalls$lance %in% datlan$lance,]  # quita los lances especiales si se quiere, pero sólo para Lmax
    #    if (!is.na(excl.sect)) ntalls<-ntalls[ntalls$lance %in% datlan$lance,]  # quita los lances especiales si se quiere, pero sólo para Lmax
    dumbdat$lmax[i]<-ifelse(is.na(dumbdat$lmax[i]),max(ntalls$talla),dumbdat$lmax[i])
  }
  #  browser()
  if (length(levels(as.factor(dumbdat$unid)))>1) { stop("Distintas unidades entre las especies elegidas, rev?salo") }
  ind<-weighted.mean(dumbdat$lmax,dumbdat$n,na.rm=T)
  dumbdat=list(datos=dumbdat,ind=ind)
  dumbdat
}