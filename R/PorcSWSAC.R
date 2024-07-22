#' PorcSWSAC: Area de especial protección irlandesa
#'
#' Polígono que delimita una zona zona de protección especial de VME del banco de Porcupine denominada Porcupine SouthWestern shelf SAC
#' @name PorcSWSAC
#' @docType data
#' @title Polígono SAC de
#' @usage PorcSWSAC
#' @format A data.frame with long and lat points defining the closed area
#' @references Medidas de protección zonas vulnerables Porcupine \url{http://www.nwwac.org/_fileupload/Opinions and Advice/Year 12/Dictamen_CCANOC_Gestion_Cigala_Porcupine_UF16_2017_ES.pdf}
#' @return data.frame con long y lat
#' @examples
#' mapporco(SACs=T);
#' @family mapas
#' @family Porcupine
#' @export
PorcSWSAC<-data.frame(long=c(-15.12,-14.92,-14.92,-15.17,-15.1,-15.12),
                                  lat=c(51.91,51.91,51.7,51.7,51.82,51.91))


