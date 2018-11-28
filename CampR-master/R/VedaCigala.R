#' VedaCigala: Veda Cigala Porcupine
#' 
#' Polígono que delimita la zona de veda de la cigala en el Porcupine Bank, la zona de veda fue propuesta por los armadores españoles, y puesta en vigor en 2010 desde el 1 de mayo al 31 de julio. Desde entonces ha estado vigente en diversos periodos, pero al menos todo el mes de mayo desde 2013 a 2016.
#' @name VedaCigala
#' @docType data
#' @title Polígono área de veda
#' @usage VedaCigala
#' @format A data.frame with long and lat points defining the closed area
#' @references Medidas de gestión del stock de Cigala del banco de Porcupine FU 16 \url{http://www.nwwac.org/_fileupload/Opinions and Advice/Year 12/Dictamen_CCANOC_Gestion_Cigala_Porcupine_UF16_2017_ES.pdf}
#' @return data.frame con lat y long
#' @examples 
#' mapporco();lines(lat~long,VedaCigala,col=2,lwd=2)
#' @family mapas
#' @family Porcupine
#' @export
VedaCigala<-data.frame(long=c(-12.31666667,-12.5,-12.66,-12.93333333,-13.89716667,-14.4,-14.05,-13.41666667,-13.125,-12.91666667,-12.71666667,-12.61666667,-12.38333333,-12.31666667),
lat=c(52.45,52.66666667,52.78333333,52.78333333,52.225,51.36666667,51.36666667,52.16666667,52.53333333,52.71666667,52.71666667,52.64666667,52.45,52.45))
