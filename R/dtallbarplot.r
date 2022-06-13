#' Comparación histograma tallas de una especie entre una campaña y serie histórica
#'
#'  Histograma de la distribución de tallas media estratificada por sexos a partir de ficheros del camp
#' @param gr Grupo de la especie: 1 peces, 2 crustáceos 3 moluscos 4 equinodermos 5 invertebrados
#' @param esp Código de la especie numérico o carácter con tres espacios. 999 para todas las especies del grupo
#' @param camps serie histórica de campañas para comparar con los de la campaña especificada en camp, o entre camps y camp: Demersales "NXX", Porcupine "PXX", Arsa primavera "1XX" y Arsa otoño "2XX"
#' @param dns Elige el origen de las bases de datos: Porcupine "Pnew", Cantábrico "Cant", Golfo de Cádiz "Arsa" (proporciona los datos para Medits pero no saca mapas)
#' @param camp campaña para comparar con las de la serie historica camps. Si NA (por defecto) compara la primera con el conjunto de la serie histórica incluida la primera
#' @param cor.time Si T corrige las abundancias en función de la duración del lance
#' @param excl.sect Sectores a excluir como carácter, se pueden elegir tanto los sectores como estratos
#' @param ymax Valor maximo del eje de la y para ambos gráficos de barras
#' @param horiz F por defecto saca el histograma en vertical, se puede poner horizontal haciendolo T
#' @param years Si T saca los años como nombre de campaña en los paneles lattice de campañas
#' @param es si T saca los títulos en castellano, si no en inglés
#' @param ti por defecto NA si es = un texto p.ej. "8c Division" sale como título general por encima de los dos gráficos
#' @family Distribuciones de tallas
#' @examples dtallbarplot(1,34,Nsh[19:28],"Cant",excl.sect=c(1),ymax=1.5)
#' @examples dtallbarplot(1,c(34),camps=Nsh[27:37],"Cant",camp="N21",excl.sect=1,ymax=1.5,es=F,ti="8c Division")
#' @export
dtallbarplot<-function(gr, esp, camps, dns, camp = NA, excl.sect=NA, cor.time=TRUE, ymax = NA, horiz = T, years=TRUE,es=T,ti=NA)
{
  if (length(camp)>1) stop("Camp contiene más de una campaña, se comparan una serie de campañas contra una, o una serie de campañas contra la última de camps")
  dumb <- dtall.camp(gr, esp, camps, dns, excl.sect=excl.sect,cor.time=cor.time,out.dat = T, plot = F,years=years)
  if(!is.na(camp)) dumbc <- dtall.camp(gr, esp, camp, dns, excl.sect=excl.sect,cor.time=cor.time,out.dat = T, plot = F,years=years)
  else dumbc<-dtall.camp(gr,esp,camps[length(camps)],dns,excl.sect=excl.sect,cor.time=cor.time,out.dat=T,plot=F,years=years)
  #if (years) {camps<-camptoyear(camps);if (!is.na(camp)) camp=camptoyear(camp)}
  mx <- ceiling(hablar::max_(c(rowMeans(dumb), dumbc)))
  if (is.na(ymax)) mx <- ceiling(hablar::max_(c(rowMeans(dumb), dumbc)))
  else mx<-ymax # ifelse(c(mx>ymax*1.8),mx,ymax)}
  increm<-unid.camp(gr,esp)["INCREM"]
  medida<-ifelse(unid.camp(gr,esp)["MED"]==1,"cm",ifelse(increm==5,"x5 mm","mm"))
  op <- par(no.readonly = T)
  tallas <- rep(NA, length(row.names(dumbc)))
  tallas[seq(0, length(row.names(dumbc)), by = 5)] <- row.names(dumbc)[seq(0,length(row.names(dumbc)), by = 5)]
  tallas1 <- rep(NA, length(row.names(dumb)))
  tallas1[seq(0, length(row.names(dumb)), by = 5)] <- row.names(dumb)[seq(0,length(row.names(dumb)), by = 5)]
  if (horiz & !is.na(ti)) {
    par(mfrow = c(1, 2),oma=c(0,0,2,0))
  }
  if (horiz & is.na(ti)) par(mfrow = c(1, 2))
  if (!horiz) par(mfrow = c(2, 1))
  if (es) {ax<-c(paste0("Talla (",medida,")"),expression("Ind"%*%"lan"^-1))}
  else {ax<-c(paste0("Length (",medida,")"),expression("Ind"%*%"haul"^-1))}
  if (is.na(camp)) {
    barplot(dumb[, ncol(dumb)], ylim = c(0, mx * 1.1), main = ifelse(years,camptoyear(camps[length(camps)]),camps[length(camps)]),
            names.arg = tallas, xlab = ax[1], ylab = ax[2],
            space = 0, las = 2, cex.names = 0.8)
    grid(nx=NA,ny=NULL,col=gray(.4))

  }
  else barplot(dumbc[, camp], ylim = c(0, mx * 1.1), main = ifelse(years,camptoyear(camp),camp),
               xlab = ax[1], names.arg = tallas, ylab = ifelse(es,expression("Ind.  lance "^-1),expression("Ind. haul"^-1)),
               space = 0, las = 2, cex.names = 0.8)
  grid(nx=NA,ny=NULL,col=gray(.4))
  box()
  if (!is.na(camp)) {
    barplot(rowMeans(dumb[,-ncol(dumb)]), ylim = c(0, mx * 1.1), main = paste(ifelse(es,"Media ","Mean "),
                                                              ifelse(years,camptoyear(camps[1]),camps[1]), "-", ifelse(years,camptoyear(camps[length(camps)]),camps[length(camps)]), sep = ""), names.arg = tallas1,
          xlab = ax[1], ylab = ifelse(es,expression("Ind.  lance "^-1),expression("Ind. haul"^-1)),
          space = 0, las = 2, cex.names = 0.8)
  }
  else barplot(rowMeans(dumb[,1:c(ncol(dumb)-1)]), ylim = c(0, mx * 1.1), main = paste(ifelse(es,"Media ","Mean "),
                                                                                     ifelse(years,camptoyear(camps[1]),camps[1]), "-", ifelse(years,camptoyear(camps[length(camps)-1]),camps[length(camps)-1]), sep = ""), names.arg = tallas1,
                   xlab = ax[1], ylab = ifelse(es,expression("Ind.  lance "^-1),expression("Ind. haul"^-1)),
                   space = 0, las = 2, cex.names = 0.8)
  grid(nx=NA,ny=NULL,col=gray(.4))
  box()
  if (!is.na(ti)) mtext(ti, line=0, side=3, outer=TRUE, cex=1.5,font=2)
  par(op)
}
