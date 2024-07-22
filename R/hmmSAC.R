#' hmmSAC: Area de especial protección irlandesa
#'
#' Polígono que delimita una zonade protección especial de las autoridades Irlandesas, con sus VMEs
#' @name hmmSAC
#' @docType data
#' @title Polígono SAC de
#' @usage hmmSAC
#' @format A data.frame with long and lat points defining the closed area
#' @references Medidas de protección zonas vulnerables Porcupine \url{http://www.nwwac.org/_fileupload/Opinions and Advice/Year 12/Dictamen_CCANOC_Gestion_Cigala_Porcupine_UF16_2017_ES.pdf}
#' @return data.frame con long y lat
#' @examples
#' mapporco(SACs=T);
#' @family mapas
#' @family Porcupine
#' @export
hmmSAC<-data.frame(long=c(-13.21,-12.97,-12.9,-12.49,-12.49,-12.88,-12.93333333,-13.18,-13.21),
                                  lat=c(52.27,52.4,52.28,52.28,52.07,52.07,52.15,52.15,52.27))


