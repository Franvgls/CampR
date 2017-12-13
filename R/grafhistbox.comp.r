#' Gráficas grafhistbox combinadas biomasa y peso
#'
#' Ver documentación grafhistbox
#' @param gr Grupo de la especie: 1 peces, 2 crustáceos 3 moluscos 4 equinodermos 5 invertebrados 6 para deshechos y otros. 9 incluye todos los grupos a excepción del 6
#' @param esp Codigo de la especie numérico o carácter con tres espacios. 999 para todas las especies del grupo 
#' @param camps campañas de la serie de datos a representar en el gráfico de abundancias Demersales Nsh, Porcupine Psh, Arsa primavera As1 y Arsa otoño As2
#' @param dns Elige el origen de las bases de datos: Porcupine "Porc" o "Pnew", Cantábrico "Cant", Golfo de Cádiz "Arsa" (únicamente para sacar datos al IBTS, no gráficos)
#' @param cor.time Si T corrige las abundancias en función de la duración del lance
#' @param kg Si T el gráfico está en kgs, si F en gramos
#' @param ci.lev El intervalo de confianza a representar
#' @param DLS si T dibuja líneas en los valores medios de los últimos dos años y de los cinco previos
#' @param idi Nombre científico de la especie ("l") o nombre común ("e")
#' @param SE Si T dibuja las cajas representando el error estándar, si F no aparecen la cajas
#' @param es Si T ejes y unidades en español, si F en inglés
#' @param excl.sect Sectores a excluir como carácter, se pueden elegir tanto los sectores como estratos
#' @param sector Alternativa a excl.sect para un sólo sector. Si especificado como carácter solo toma el sector elegido
#' @param Nas Si F no representa las cajas en los sectores/estratos en los que algún sector o estrato tiene un sólo lance. Si T utiliza el valor del estrato y evita los NAs
#' @param ymax Valor máximo del eje de las y, tiene doble valor para regular biomasa y número
#' @param ti Permíte sólo el nombre de la especie que aparece en el gráfico superior y sale siempre en cursiva.
#' @param sub Añade un subtítulo con el valor que se le ponga si no F
#' @param mar Si se quiere dejar un margen ya establecido hacerlo igual a F
#' @param tline Si T dibuja una línea de tendencia a traves de un glm con los datos de abundancia. Gráficos evaluación MSFD.
#' @param years Si T saca los años como nombre de campaña en el eje de las equis en vez del nombre de campaña
#' @return Crea una gráfica doble de evolución de las abundancias en biomasa y número. 
#' @seealso {\link{grafhistbox}}, {\link{grafhistbox.comp}}
#' @examples grafhistbox(1,45,Nsh[7:27],"Cant",DLS=T,es=FALSE,years=TRUE,tline=TRUE,ti=TRUE,sub=TRUE)
#' @export
grafhistbox.comp<-function(gr,esp,camps,dns="Porc",cor.time=TRUE,kg=TRUE,ci.lev=.8,DLS=F,idi="l",SE=TRUE,es=TRUE,sector=NA,
	Nas=FALSE,excl.sect=NA,ymax=c(NA,NA),tline=FALSE,years=TRUE,ti=TRUE,mar=NA) {
  op<- par(no.readonly = TRUE) # the whole list of settable par's.
  par(mfrow=c(2,1))
  grafhistbox(gr=gr,esp=esp,camps=camps,dns=dns,ind="p",cor.time=cor.time,kg=kg,DLS=DLS,es=es,sector=sector,ti=ti,Nas=Nas,excl.sect=excl.sect,
    ymax=ymax[1],mar=c(4, 4, 2.5, 2.5) + 0.1,tline=tline,years=years,sub=TRUE)
  grafhistbox(gr=gr,esp=esp,camps=camps,dns=dns,ind="n",cor.time=cor.time,kg=kg,DLS=DLS,es=es,sector=sector,ti=FALSE,Nas=Nas,excl.sect=excl.sect,
    ymax=ymax[2],mar=c(4, 4, 1, 2.5) + 0.1,tline=tline,years=years,sub=TRUE)
  par(op)
  }
