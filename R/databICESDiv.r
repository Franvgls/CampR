#' Datos de abundancia por división ICES para una especie resúmen para grupos de trabajo
#' 
#' Salida de datos a csv para rellenar los informes de grupo de trabajo, filas con datos ab estratificada (Biomasa y N) y error estándar por subdivisión ICES función para Demersales Norte (saca IXa, VIIIc y total) 
#' @param gr Grupo de la especie: 1 peces, 2 crustáceos 3 moluscos 4 equinodermos 5 invertebrados
#' @param esp Código de la especie numérico o carácter con tres espacios. 999 para todas las especies del grupo
#' @param camp Campaña de la que se extraen los datos: un año comcreto (XX): Demersales "NXX", Porcupine "PXX", Arsa primavera "1XX" y Arsa otoño "2XX"
#' @param dns Elige el origen de las bases de datos: Porcupine "Pnew", Cantábrico "Cant, Golfo de Cádiz "Arsa" (únicamente para sacar datos al IBTS, no gráficos)
#' @param cor.time Si T corrige las abundancias en función de la duración del lance
#' @param Nas Permite calcular los errores estándar aunque sólo haya un lance en algún estrato (haciendo varianza =0 en ese estrato, incorrecto pero da una idea válido cuando sólo un estrato entre varios tiene sólo un lance)
#' @return Devuelve un número con nombres organizado en dos líneas (biomasa y número) en columnas por subdivisiones ICES por columnas abundancia estratificada media por XIa, VIIIcE, VIIIcW
#' @seealso databICES {\link{databICES.r}}
#' @export
databICESDiv<-function(gr,esp,camp,dns="Cant",cor.time=T,Nas=F) {
  if (length(camp)>1) {stop("seleccionadas más de una campaña, no se pueden sacar resultados de más de una")}
  if (all(substr(dns,1,4)!="Cant",substr(dns,1,4)!="Cnew")) {stop("Función sólo disponible para Demersales Costa Norte divisiones IXa, VIIIc Este y VIIIc Oeste")}
  esp<-format(esp,width=3,justify="r")
  dumb1<-sapply(CV.camp(gr,esp,camp,dns,cor.time=cor.time,excl.sect=c(2:5),Nas=Nas)$sectores[1:2,],t)
  dumb2<-t(CV.camp(gr,esp,camp,dns,cor.time=cor.time,excl.sect=c(1),Nas=Nas)$total[1:2,])
  dumb3<-t(CV.camp(gr,esp,camp,dns,cor.time=cor.time,Nas=Nas)$total[1:2,])
  dumb4<-sapply(CV.camp(gr,esp,camp,dns,cor.time=cor.time,ind="n",excl.sect=c(2:5),Nas=Nas)$sectores[1:2,],t)
  dumb5<-t(CV.camp(gr,esp,camp,dns,cor.time=cor.time,ind="n",excl.sect=c(1),Nas=Nas)$total[1:2,])
  dumb6<-t(CV.camp(gr,esp,camp,dns,cor.time=cor.time,ind="n",Nas=Nas)$total[1:2,])
  dumb7<-rbind(c(dumb1,dumb2,dumb3),
               c(dumb4,dumb5,dumb6))
  rownames(dumb7)<-c(paste(buscaesp(gr,esp),camp,"p",sep="_"),paste(buscaesp(gr,esp),camp,"n",sep="_"))
  colnames(dumb7)<-c("IXaN_Avg","IXaN_SE","VIIIcAvg","VIIIcSE","Tot_Avg","Tot_SE")
  dumb7
}
