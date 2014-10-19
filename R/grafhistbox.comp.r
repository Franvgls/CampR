#' Gráficas grafhistbox combinadas biomasa y peso
#'
#' Ver documentación grafhistbox
#' @param gr Grupo de la especie: 1 peces, 2 crustáceos 3 moluscos 4 equinodermos 5 invertebrados
#' @param esp Codigo de la especie numérico o carácter con tres espacios. 999 para todas las especies del grupo 
#' @param camps campañas de la serie de datos a representar en el gráfico de abundancias Demersales Nsh, Porcupine Psh, Arsa primavera Ash y Arsa otoño 2sh
#' @param dns Elige el origen de las bases de datos: Porcupine "Pnew", Cantábrico "Cant, Golfo de Cádiz "Arsa" (únicamente para sacar datos al IBTS, no gráficos)
#' @param cor.time Si T corrige las abundancias en función de la duración del lance
#' @param kg Si T el gráfico está en kgs, si F en gramos
#' @param ci.lev El intervalo de confianza a representar
#' @param idi Nombre científico de la especie ("l") o nombre común ("e")
#' @param SE Si T dibuja las cajas representando el error estándar, si F no aparecen la cajas
#' @param es Si T ejes y unidades en español, si F en inglés
#' @param excl.sect Sectores a excluir como carácter, se pueden elegir tanto los sectores como estratos
#' @param sector Alternativa a excl.sect para un sólo sector. Si especificado como carácter solo toma el sector elegido
#' @param Nas Si F no representa las cajas en los sectores/estratos en los que algún sector o estrato tiene un sólo lance. Si T utiliza el valor del estrato y evita los NAs
#' @param ymax Valor máximo del eje de las y, tiene doble valor para regular biomasa y número
#' @param ti Permíte sólo el nombre de la especie que aparece en el gráfico superior y sale siempre en cursiva.
#' @param mar Si se quiere dejar un margen ya establecido hacerlo igual a F
#' @param tline Si T dibuja una línea de tendencia a traves de un glm con los datos de abundancia. Gráficos evaluación MSFD.
#' @param years Si T saca los años como nombre de campaña en el eje de las equis en vez del nombre de campaña
#' @seealso grafhistbox {\link{grafhistbox}}
#' @return Crea una gráfica doble de evolución de las abundancias en biomasa y número. 
#' @seealso grafhistbox.comp {\link{grafhistbox.comp}}
#' @examples grafhistbox(1,45,Nsh[7:27],"Cant",es=F,years=T,tline=T,ti=T,subtit=T)
#' @export
grafhistbox.comp<-function(gr,esp,camps,dns="Pnew",cor.time=T,kg=T,ci.lev=.8,idi="l",SE=T,es=T,sector=NA,
	Nas=F,excl.sect=NA,ymax=c(NA,NA),tline=F,years=F,ti=T,mar=NA) {
  op<- par(no.readonly = TRUE) # the whole list of settable par's.
  par(mfrow=c(2,1))
  grafhistbox(gr=gr,esp=esp,camps=camps,dns=dns,ind="p",cor.time=cor.time,kg=kg,es=es,sector=sector,ti=ti,Nas=Nas,excl.sect=excl.sect,
    ymax=ymax[1],mar=c(4, 4, 2.5, 2.5) + 0.1,tline=tline,years=years,subtit=T)
  grafhistbox(gr=gr,esp=esp,camps=camps,dns=dns,ind="n",cor.time=cor.time,kg=kg,es=es,sector=sector,ti=F,Nas=Nas,excl.sect=excl.sect,
    ymax=ymax[2],mar=c(4, 4, 1, 2.5) + 0.1,tline=tline,years=years,subtit=T)
  par(op)
  }
