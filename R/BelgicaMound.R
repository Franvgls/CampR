#' BelgicaMound: Area de especial protección irlandesa
#'
#' Polígono que delimita una zona zona de protección especial de VME al este del banco de Porcupine denominada Belgica Mound
#' @name BelgicaMound
#' @docType data
#' @title Polígono SAC de
#' @usage BelgicaMound
#' @format A data.frame with long and lat points defining the closed area
#' @references Medidas de protección zonas vulnerables Porcupine \url{http://www.nwwac.org/_fileupload/Opinions and Advice/Year 12/Dictamen_CCANOC_Gestion_Cigala_Porcupine_UF16_2017_ES.pdf}
#' @return data.frame con long y lat
#' @examples
#' mapporco(SACs=T);
#' @family mapas
#' @family Porcupine
#' @export
BelgicaMound<-data.frame(long=c(-11.86166667,-11.69166667,-11.55166667,-11.74166667,-11.86166667),
                                  lat=c(51.48888889,51.53888889,51.28888889,51.22888889,51.48888889))


