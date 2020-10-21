#' Comprueba que están medidas todas las especies en capturas y viceversa en la campaña. Comprueba concordancia entre números y pesos en los ficheros de tallas NTALLXXX.dbf y fauna FAUNAXXX.dbf
#'
#' Sirve para control de calidad y asegurándose que no faltan datos o están incompletos (comprueba coherencia entre faunaXXX.dbf y NtallXXX.dbf y avisa cuando no son coherentes) Existe una funciòn escondida para procesar de una vez toda la campaña `qcTalPez.camp(camp=XXX,dns,gr=1)`, pero una vez encontrado que el lance en que está el error es mejor utilizar la función lance por lance
#' @param camp campaña a revisar los datos en formato Camp Xyy
#' @param dns Origen de bases de datos: "Cant" cantábrico, "Porc" o "Pnew" Porcupine
#' @param gr El grupo que se quiere comprobar 1 peces, 2 crustaceos, 3 cefalópodos. 4, 5 y 6 nunca se miden.
#' @return Devuelve la lista de especies capturadas pero no medidas o viceversa
#' @examples qcTalPez.camp("N20",dns="Cant",gr=1)
#' @family Control de calidad
#' @export
qcTalPez.camp<-function(camp,dns,gr=1) {
  lancamp<-datlan.camp(camp,dns,incl0=FALSE)$lance
  for (i in lancamp) {
    print(paste0("Errores lance ",i," de ",length(lancamp),": "))
    print(qcTalPez.lan(camp,dns,i,gr))
#    print(" ")
    Sys.sleep(.0001)
  }
  invisible(i)
}
