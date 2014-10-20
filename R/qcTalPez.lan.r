#' Comprueba que están medidas todas las especies en capturas y viceversa
#' 
#' Sirve para control de calidad y asegurarse que no faltan datos o estan incompletos
#' @param camp campaña a revisar los datos en formato Camp Xyy
#' @param dns Origen de bases de datos: Cant cantábrico, Pnew Porcupine
#' @param lan lance para el que se quiere comprobar que estan todas las especies medidas y talladas (sólo comprueba peces, por defecto gr=1, si se quiere otros cambiar gr)
#' @param gr El grupo que se quiere comprobar 1 peces, 2 crustaceos, 3 cefalópodos. 4, 5 y 6 nunca se miden.
#' @return Devuelve la lista de especies capturadas pero no medidas o viceversa
#' @examples qcTalPez.lan("C14",dns="Cant",24,gr=1)
#' @family Control de calidad
#' @export
qcTalPez.lan<- function(camp,dns="Cant",lan,gr=1) {
  if (length(camp)>1) {stop("seleccionadas más de una campaña, no se pueden sacar resultados de más de una")}
  require(RODBC)
  ch1<-odbcConnect(dsn=dns)
  odbcSetAutoCommit(ch1, FALSE)
  lan<-format(lan,width=3,justify="r")
  listsps<-sqlQuery(ch1,paste("select lance,grupo,esp,peso_gr,numero from FAUNA",camp," where lance='",lan,"'",sep=""))
  if (nrow(listsps)==0) {
    dumblan<-datlan.camp(camp,"Cant")
    if (any(dumblan$lance==gsub(" ","",lan))) stop(paste("el lance",gsub(" ","",lan),"no tiene tallas"))
    else stop(paste("No existe el lance: ",gsub(" ","",lan)))
    }
  listsps<-listsps[listsps$grupo=="1",]
  tals<-sqlQuery(ch1,paste("select lance,grupo,esp,peso_m,peso_gr,talla,numer from NTALL",camp," where lance='",lan,"'",sep=""))
  tals<-tals[tals$grupo=="1",]
  odbcClose(ch1)
  fau<-levels(as.factor(format(listsps$esp,width=3,justify="r")))
  tals<-levels(as.factor(format(tals$esp,width=3,justify="r")))
  nomed<-fau[is.na(match(fau,tals))]
  sincapt<-tals[is.na(match(tals,fau))]
  if (length(nomed)>0) {
    print(paste("Lance ",gsub(" ","",lan),": especies con captura y sin medir:",sep=""))
    for (i in 1:length(nomed)) print(paste(nomed[i],"-",buscaesp(gr,nomed[i])))
  }
  if (length(sincapt)>0) {
    print("Especies medidas sin capturas en fauna:")
    for (i in 1:length(sincapt)) print(paste(sincapt[i],"-",buscaesp(gr,sincapt[i])))
  }
  if (length(sincapt)==0 & length(nomed)==0) print("No hay especies sin medir")
}