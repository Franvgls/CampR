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
histboxplot.comp<-function(gr,esp,camps,dns="Porc",cor.time=TRUE,incl2=TRUE,es=T,bw=TRUE,ti=TRUE,sub=NULL,idi="l",
                           ceros=TRUE,cex.leg=1,years=TRUE,profrange=NA) {
  op<- par(no.readonly = TRUE) # the whole list of settable par's.
  options(scipen=2)
  par(mfrow=c(2,1))
  histboxplot(gr=gr,esp=esp,camps=camps,dns=dns,cor.time = cor.time,incl2=incl2,bw=bw,ti=ti,sub=T,idi=idi,ind="p",ceros=ceros,cex.leg=cex.leg,years=years,profrange=profrange)
  histboxplot(gr=gr,esp=esp,camps=camps,dns=dns,cor.time = cor.time,incl2=incl2,bw=bw,ti=F,sub=T,idi=idi,ind="n",ceros=ceros,cex.leg=cex.leg,years=years,profrange=profrange)
  par(op)
    }
