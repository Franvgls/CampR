#' PorcNWSAC: Area de especial protección irlandesa
#'
#' Polígono que delimita una zona zona de protección especial de VME del banco de Porcupine denominada Porcupine NorthWestern shelf SAC
#' @name PorcNWSAC
#' @docType data
#' @title Polígono SAC de
#' @usage PorcNWSAC
#' @format A data.frame with long and lat points defining the closed area
#' @references Medidas de protección zonas vulnerables Porcupine \url{http://www.nwwac.org/_fileupload/Opinions and Advice/Year 12/Dictamen_CCANOC_Gestion_Cigala_Porcupine_UF16_2017_ES.pdf}
#' @return data.frame con long y lat
#' @examples
#' mapporco(SACs=T);
#' @family mapas
#' @family Porcupine
#' @export
PorcNWSAC<-data.frame(long=c(-14.456,-13.88733333,-13.82466667,-14.17466667,-14.23733333,-14.47466667,-14.53733333,-14.456),
                                  lat=c(53.58733333,53.856,53.756,53.58733333,53.52466667,53.4,53.506,53.58733333))


