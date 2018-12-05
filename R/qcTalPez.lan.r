#' Comprueba que están medidas todas las especies en capturas y viceversa. Comprueba concordancia entre números y pesos en los ficheros de tallas NTALLXXX.dbf y fauna FAUNAXXX.dbf
#'
#' Sirve para control de calidad y asegurándose que no faltan datos o están incompletos (comprueba coherencia entre faunaXXX.dbf y NtallXXX.dbf y avisa cuando no son coherentes) Existe una funciòn escondida para procesar de una vez toda la campaña `qcTalPez.camp(camp=XXX,dns,gr=1)`, pero una vez encontrado que el lance en que está el error es mejor utilizar la función lance por lance
#' @param camp campaña a revisar los datos en formato Camp Xyy
#' @param dns Origen de bases de datos: "Cant" cantábrico, "Porc" o "Pnew" Porcupine
#' @param lan lance para el que se quiere comprobar que estan medidas todas las especies (sólo comprueba peces, por defecto gr=1, si se quiere otros cambiar gr) Si lan=NA comprueba todos los lances de la campaña
#' @param gr El grupo que se quiere comprobar 1 peces, 2 crustaceos, 3 cefalópodos. 4, 5 y 6 nunca se miden.
#' @return Devuelve la lista de especies capturadas pero no medidas o viceversa
#' @examples qcTalPez.lan("C14",dns="Cant",lan=NA,gr=1)
#' @family Control de calidad
#' @export
qcTalPez.lan<-function (camp, dns = "Cant", lan, gr = 1) {
  if (length(camp) > 1) {
    stop("seleccionadas más de una campaña, no se pueden sacar resultados de más de una")
  }
  if (length(lan) > 1) {
    stop("seleccionados más de un lance, no se pueden comprobar errores para más de uno")
  }
  if (!gr %in% 1:3) {
    stop("Solo se miden los grupos peces y algunos crustáceos y moluscos, cambie parámetro gr")
  }
  ch1 <- RODBC::odbcConnect(dsn = dns)
  RODBC::odbcSetAutoCommit(ch1, FALSE)
  lan <- format(lan, width = 3, justify = "r")
  listsps <- RODBC::sqlQuery(ch1, paste("select lance,grupo,esp,peso_gr,numero from FAUNA",
                                        camp, " where lance='", lan, "'", sep = ""))
  if (nrow(listsps) == 0) {
    dumblan <- datlan.camp(camp, dns)
    if (any(dumblan$lance == gsub(" ", "", lan)))
      stop(paste("el lance", gsub(" ", "", lan), "no tiene tallas"))
    else stop(paste("No existe el lance: ", gsub(" ", "",
                                                 lan)))
  }
  listsps <- listsps[listsps$grupo == gr, ]
  tals <- RODBC::sqlQuery(ch1, paste("select lance,grupo,esp,peso_m,peso_gr,cate,talla,numer from NTALL",
                                     camp, " where lance='", lan, "'", sep = ""))
  tals <- tals[tals$grupo == gr, ]
  tals$numpond <- tals$numer * tals$peso_gr/tals$peso_m
  RODBC::odbcClose(ch1)
  fau <- levels(as.factor(format(listsps$esp, width = 3, justify = "r")))
  talspes <- rowSums(tapply(tals$peso_gr, tals[, c("esp", "cate")],
                            mean), na.rm = T)
  talsnum <- rowSums(tapply(tals$numpond, tals[, c("esp", "cate")],
                            sum), na.rm = T)
  talsdat <- levels(as.factor(format(tals$esp, width = 3, justify = "r")))
  nomed <- fau[is.na(match(fau, talsdat))]
  sincapt <- talsdat[is.na(match(talsdat, fau))]
  #print(paste("Lance ", gsub(" ", "", lan), ":"), sep = )
  if (length(nomed) > 0) {
    print(paste("Lance ", gsub(" ", "", lan), ": ","Especies con captura y sin medir:", sep = ""))
    for (i in 1:length(nomed)) print(paste(nomed[i], "-",
                                           buscaesp(gr, nomed[i])))
  }
  if (length(sincapt) > 0) {
    print(paste("Lance ", gsub(" ", "", lan),":"," Especies medidas sin capturas en fauna:"))
    for (i in 1:length(sincapt)) print(paste(sincapt[i],
                                             "-", buscaesp(gr, sincapt[i])))
  }
  if (length(sincapt) == 0 & length(nomed) == 0) {}
#    print("No hay especies sin medir")
  listspsmed <- listsps[!listsps$esp %in% as.numeric(nomed),]
  errpesos <- listspsmed[, c("peso_gr")] - talspes[as.character(listspsmed$esp)]
  errnumer <- listspsmed[, c("numero")] - talsnum[as.character(listspsmed$esp)]
  if (any(abs(errpesos) > 1)) {
    print(paste("Lance ", gsub(" ", "", lan), ":","Discordancias entre pesos en tallas y fauna para especie",
                buscaesp(gr, names(which(errpesos != 0)))))
    print(errpesos[which(abs(errpesos) > 1)])
    print(paste("Tallas:", round(talspes[names(which(abs(errpesos) >
                                                       1))], 1), "- Fauna", listsps[listsps$esp == names(which(errpesos !=
                                                                                                                 0)), c("peso_gr")]))
  }
  if (any(abs(errnumer) > 1)) {
    print(paste("Lance ", gsub(" ", "", lan), ":","Discordancias entre números en tallas y fauna para especie",
                buscaesp(gr, names(which(abs(errnumer) > 1)))))
    print(errnumer[which(abs(errnumer) > 1)])
    print(paste("Tallas:", round(talsnum[names(which(abs(errnumer) >
                                                       1))], 1), "- Fauna", listsps[listsps$esp == names(which(abs(errnumer) >
                                                                                                                 1)), c("numero")]))
  }
}

qcTalPez.camp<-function(camp,dns,gr) {
  lancamp<-datlan.camp(camp,dns,incl0=FALSE)$lance
  for (i in lancamp) {
    print(paste0("Errores lance ",i," de ",length(lancamp),": "))
    print(qcTalPez.lan(camp,dns,i,gr))
#    print(" ")
    Sys.sleep(.0001)
  }
  invisible(i)
}
