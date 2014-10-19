#' Comparación histograma tallas de una especie entre una campaña y serie histórica
#' 
#'  Histograma de la distribución de tallas media estratificada por sexos a partir de ficheros del camp
#' @param gr Grupo de la especie: 1 peces, 2 crustáceos 3 moluscos 4 equinodermos 5 invertebrados
#' @param esp Código de la especie numérico o carácter con tres espacios. 999 para todas las especies del grupo 
#' @param camps serie histórica de campañas para comparar entre la primera y el total, o entre camps y camp: Demersales "NXX", Porcupine "PXX", Arsa primavera "1XX" y Arsa otoño "2XX"
#' @param dns Elige el origen de las bases de datos: Porcupine "Pnew", Cantábrico "Cant", Golfo de Cádiz "Arsa" (proporciona los datos para Medits pero no saca mapas)
#' @param camp campaña para comparar con las serie historica camps, debe ser una de las campañas dentro de la serie. Si NA (por defecto) compara la primera con el conjunto de la serie histórica incluida la primera
#' @param cor.time Si T corrige las abundancias en función de la duración del lance
#' @param excl.sect Sectores a excluir como carácter, se pueden elegir tanto los sectores como estratos
#' @param years Si T saca los años como nombre de campaña en los paneles lattice de campañas
#' @param ymax Valor maximo del eje de la y para ambos gráficos de barras
#' @param horiz F por defecto saca el histograma en vertical, se puede poner horizontal haciendolo T
#' @seealso {\link{dtall.camp}}
#' @examples dtallbarplot(1,34,Nsh[19:28],"Cnew",excl.sect=c(1),ymax=1.5)
#' @export
dtallbarplot<-function (gr, esp, camps, dns, camp = NA, excl.sect=NA, cor.time=T, ymax = NA, horiz = T, years=F) 
{
  dumb <- dtall.camp(gr, esp, camps, dns, excl.sect=excl.sect,cor.time=cor.time,out.dat = T, plot = F,years=years)
  if (years & !is.na(camp)) {camptoyear(camp)}
  if (is.na(ymax)) 
    mx <- ceiling(max(rowMeans(dumb), dumb[, ifelse(is.na(camp),ncol(dumb), camp)], na.rm = T))
  else mx <- ymax
  op <- par(no.readonly = T)
  tallas <- rep(NA, length(row.names(dumb)))
  tallas[seq(0, length(row.names(dumb)), by = 5)] <- row.names(dumb)[seq(0,length(row.names(dumb)), by = 5)]
  if (horiz) 
    par(mfrow = c(1, 2))
  else par(mfrow = c(2, 1))
  if (is.na(camp)) {
    barplot(dumb[, ncol(dumb)], ylim = c(0, mx * 1.1), main = ifelse(years,camptoyear(camps[length(camps)]),camps[length(camps)]), 
            names.arg = tallas, xlab = "Length (cm)", ylab = expression("Ind.  haul   "^-1), 
            space = 0, las = 2, cex.names = 0.8)
  }
  else barplot(dumb[, camp], ylim = c(0, mx * 1.1), main = ifelse(years,camptoyear(camp),camp), 
               xlab = "Length (cm)", names.arg = tallas, ylab = expression("Ind.  haul   "^-1), 
               space = 0, las = 2, cex.names = 0.8)
  box()
  barplot(rowMeans(dumb), ylim = c(0, mx * 1.1), main = paste("Mean ", 
                                                              ifelse(years,camptoyear(camps[1]),camps[1]), "-", ifelse(years,camptoyear(camps[length(camps)]),camps[length(camps)]), sep = ""), names.arg = tallas, 
          xlab = "Length (cm)", ylab = expression("Ind.  haul   "^-1), 
          space = 0, las = 2, cex.names = 0.8)
  box()
}
