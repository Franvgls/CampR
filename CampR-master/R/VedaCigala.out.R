#' VedaCigala.out: Zona ampliada de la zona de veda de Cigala en Porcupine
#' 
#' Polígono que delimita una zona ampliada más grande que la zona de veda de la cigala en el Porcupine Bank, la zona de veda fue propuesta por los armadores españoles, y puesta en vigor en 2010 desde el 1 de mayo al 31 de julio. Desde entonces ha estado vigente en diversos periodos, pero al menos todo el mes de mayo desde 2013 a 2016.
#' @name VedaCigala.out
#' @docType data
#' @title Polígono área de veda
#' @usage VedaCigala.out
#' @format A data.frame with long and lat points defining the closed area
#' @references Medidas de gestión del stock de Cigala del banco de Porcupine FU 16 \url{http://www.nwwac.org/_fileupload/Opinions and Advice/Year 12/Dictamen_CCANOC_Gestion_Cigala_Porcupine_UF16_2017_ES.pdf}
#' @return data.frame con long y lat
#' @examples 
#' mapporco();lines(lat~long,VedaCigala,col=2,lwd=2); lines(lat~long,VedaCigala.out,col="blue",lwd=2)
#' @family mapas
#' @family Porcupine
#' @export
VedaCigala.out<-data.frame(long=c(-14.62475,-14.14485,-12.96959,-12.6366,-12.26443,-12.1567,-12.50928,
                                  -12.80309,-13.36134,-13.59639,-13.78248,-14.09588,-14.56598,-14.62475),
                                  lat=c(51.3254,52.55079,53.03333,53.04603,52.90635,2.43651,52.33492,
                                  2.33492,51.96032,51.7,51.33809,51.14762,51.28095,1.3254))


