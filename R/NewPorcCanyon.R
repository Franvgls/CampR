#' NewPorcCanyon: Area de especial protección irlandesa
#'
#' Polígono que delimita una zona zona de protección especial de VME al oeste del banco de Porcupine denominada NewPorcCancyon
#' @name NewPorcCanyon
#' @docType data
#' @title Polígono SAC de
#' @usage NewPorcCanyon
#' @format A data.frame with long and lat points defining the closed area
#' @references Medidas de protección zonas vulnerables Porcupine \url{http://www.nwwac.org/_fileupload/Opinions and Advice/Year 12/Dictamen_CCANOC_Gestion_Cigala_Porcupine_UF16_2017_ES.pdf}
#' @return data.frame con long y lat
#' @examples
#' mapporco(SACs=T);
#' @family mapas
#' @family Porcupine
#' @export
NewPorcCanyon<-data.frame(long=c(-15.35777778,-14.84,-14.76472222,-15.12166667,-15.10166667,-15.17166667,-15.35777778),
                                  lat=c(51.93861111,52.23972222,52.12277778,51.90888889,51.81888889,51.69888889,51.93861111))


