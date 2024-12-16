#' Función de búsqueda del datos de una especie en una campaña de DATRAS
#'
#' La búsqueda se hace a través de los códigos CAMP, el nombre de la campaña en DATRAS, el año y el trimestre de la campaña.
#' @param gr Grupo de la especie: 1 peces, 2 crustáceos 3 moluscos 4 equinodermos 5 invertebrados
#' @param esp Código de la especie numérico o carácter con tres espacios. 999 para todas las especies del grupo
#' @param camp este es el código de campaña DATRAS, no los códigos del IEO
#' @param year el año de la campaña
#' @param quarter el trimestre de la campaña
#' @param id Salida como: "l" Nombre científico en latín, "e" nombre en español, o "i" nombre en inglés
#' @family datos_especies
#' @examples buscaesp(1,50)
#' @export
buscaesp.datras<- function(gr,esp,camp,year,quarter,dns="Camp") {
  esp<-format(esp,width=3,justify="r")
  bb<-buscacod(buscaesp(gr,esp))
  aphia<-as.character(bb$APHIA)[1]
  dplyr::filter(icesDatras::getHLdata(camp,year,quarter),Valid_Aphia==trimws(aphia))
  }
