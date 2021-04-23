#' Abundancia estratificada para un rango de talla
#'
#' Extrae los datos de abundancia de una especie o conjunto de especies con un rango de tallas determinado a partir de las distribuciones de talla.También puede mostrar los datos de biomasa a partir de la relación talla-peso
#' @param gr Grupo de la especie: 1 peces, 2 crustáceos 3 moluscos 4 equinodermos 5 invertebrados
#' @param esp Código de la especie numérico o carácter con tres espacios. 999 para todas las especies del grupo
#' @param camps campañas (años) a representar en el mapa: Demersales "NXX", Porcupine "PXX", Arsa primavera "1XX" y Arsa otoño "2XX"
#' @param dns Elige el origen de las bases de datos: Porcupine "Porc" o "Pnew", Cantábrico "Cant, Golfo de Cádiz "Arsa" (únicamente para sacar datos al IBTS, no gráficos)gr Grupo de la especie: 1 peces, 2 crustáceos 3 moluscos 4 equinodermos 5 invertebrados
#' @param cor.time Si T corrige las abundancias en función de la duración del lance
#' @param excl.sect Sectores a excluir como carácter, se pueden elegir tanto los sectores como estratos
#' @param years Si T muestra los datos por años, si F por campañas (siguiendo el formato del parámetro camps)
#' @param mult por defecto 100 para evitar que la distribución en números decimales de número haga que no salgan datos
#' @param ti Si T en el gráfico muestra el nombre de la especie y el rango de tallas comprendido
#' @param las Controla el sentido de las etiquetas del gráfico, 2 perpendicular al eje, mejor para etiquetas de años
#' @param plot Saca el gráfico (T) o lo guarda como objeto para componer con otros gráficos (F)
#' @param es Si T gráfico en castellano, si F gráfico en inglés
#' @return Devuelve un vector con nombre con el número estratificado del rango de tallas deseados por campaña/año. Si se solicita plot=TRUE saca un gráfico de barras que muestra la abundancia por año. En peso sólo saca los resultados para una especie.
#' @examples dattal.camps(2,19,Psh,"Porc",1,15,ind="n",plot=TRUE)
#' @seealso {\link{dattal.camp}}
#' @export
talbox.camps<- function(gr,esp,camps,dns,cor.time=TRUE,boxplot=T,excl.sect=NA,years=TRUE,mult=100,ti=TRUE,las=NA,es=FALSE,bw=TRUE) {
  options(scipen=2)
  esp<-format(esp,width=3,justify="r")
  if (length(esp)>1) warning("Seguro que tiene sentido mezclar más de una especie para sacar el rango de talla")
  dumb<-data.frame(dattal.camp(gr,esp,camps[1],dns,cor.time=cor.time,excl.sect=excl.sect,sex=FALSE),camp=camps[1])
  if (length(camps)>1) {
    for (i in camps[2:length(camps)]) {
      dumb<-rbind(dumb,data.frame(dattal.camp(gr,esp,i,dns,cor.time=cor.time,excl.sect=excl.sect,sex=FALSE),camp=i))
    }
  }
  if (boxplot) boxplot(rep(dumb$talla+.5,dumb$numero*mult)~rep(dumb$camp,dumb$numero*mult),na.rm=T,
                       xlab=ifelse(years,ifelse(es,"Año","Year"),ifelse(es,"Campaña","Survey")),
                       ylab=ifelse(es,"Talla (cm)","Length (cm)"),names=ifelse(years,camptoyear(camps),camps),las=ifelse(is.na(las),ifelse(length(camps)>7,2,1),las))
  else vioplot::vioplot(rep(dumb$talla+.5,dumb$numero)~rep(dumb$camp,dumb$numero))
}
