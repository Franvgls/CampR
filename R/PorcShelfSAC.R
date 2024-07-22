#' PorcShelfSAC: Area de especial protección irlandesa
#'
#' Polígono que delimita una zona ampliada más grande que la zona de veda de la cigala en el Porcupine Bank, la zona de veda fue propuesta por los armadores españoles, y puesta en vigor en 2010 desde el 1 de mayo al 31 de julio. Desde entonces ha estado vigente en diversos periodos, pero al menos todo el mes de mayo desde 2013 a 2016.
#' @name PorShelfSAC
#' @docType data
#' @title Polígono SAC de
#' @usage PorcShelfSAC
#' @format A data.frame with long and lat points defining the closed area
#' @references Medidas de protección zonas vulnerables Porcupine \url{http://www.nwwac.org/_fileupload/Opinions and Advice/Year 12/Dictamen_CCANOC_Gestion_Cigala_Porcupine_UF16_2017_ES.pdf}
#' @return data.frame con long y lat
#' @examples
#' mapporco(SACs=T);
#' @family mapas
#' @family Porcupine
#' @export
PorcShelfSAC<-data.frame(long=c(-14.05,-12.25,-10.1,-9.7,-10.40166667,-10.80166667,-10.80166667,-11.40166667,-11.40166667,-13.83166667,-14.05),
                                  lat=c(54.12277778,54.48333333,55.61666667,55.46666667,54.8325,54.8325,54.66583333,54.66583333,54.11666667,53.75916667,54.12277778))


