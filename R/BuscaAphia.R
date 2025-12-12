#' Recupera los AphiaId de las especies sin Aphiaid en el fichero especies.dbf
#'
#' Función de comprobación y recuperación de AphiaIDs, funciona sólo con el fichero especies.dbf del raiz del camp, no utiliza lances ni fauna de una campaña
#' Sirve para crear un fichero del que se pueden importar los códigos AphiaID al especies.dbf a través de los programas de dBase recaphia.prg o similares.
#' @param gr Grupo del que se desea buscar el AphiaID: 1 peces, 2 crustáceos, 3 moluscos, 4 equinodermos, 5 otros invertebrados
#' @param dns Directorio en el que buscar el especies.dbf según se haya definido con los ODBC
#' @param export Si T crea un fichero especies.csv con todos los datos corregidos (APHIAs) en el directorio CAMP donde está el especies.dbf este es importable al especies.dbf con un append from deli with, quitando todos los peces grupo="1"
#' @return Devuelve un data.table con datos de cada especie en el formato HL de DATRAS. DATRAS requiere que los datos no tengan cabecera y el trimestre sea el que corresponde a la campaña, además de no tener "". Por ello se debe pasar a fichero con la orden: write.table(CAMPtoHH(Xyy,dns),"nombrearchivo.csv",sep=",",quote=F,col.names=F,row.names=F))
#' @family datos_especies
#' @examples # BuscaAphia(1,"P14","Porc")
#' @export
BuscaAphia <-function(gr = 1,dns="Camp",export = TRUE) {
    ch2 <- DBI::dbConnect(odbc::odbc(), dsn = dns)
    on.exit(DBI::dbDisconnect(ch2), add = TRUE)
    especies <-as.data.table(DBI::dbReadTable(ch2, "ESPECIES"))
    #DBI::dbDisconnect(ch2)
    names(especies) <- tolower(names(especies))
    especies <- subset(especies, especies$grupo == gr)
    especies$aphia<-as.numeric(especies$aphia)
    if (nrow(especies[especies$aphia>0 | is.na(especies$aphia),])==0) stop("Todas las especies tienen dato de AphiaID relleno")
    #    especies<-subset(especies,especies$esp %in% unique(ntalls[ntalls$grupo==2,"esp"]))
    especies <- arrange(especies, esp)
    especies %>% mutate_if(is.factor, as.character) -> especies
    especies<-especies[is.na(especies$aphia) | especies$aphia==0,c("grupo","esp","especie","aphia")]
    #especies$especie[1] <- buscaesp(especies$grupo[1], especies$esp[1])
    # if (substr(x = especies$especie[1],start = nchar(especies$especie[1]) - 3,
    #            stop = nchar(especies$especie[1])) == " sp.") {
    #   especies$especie[1] <-sub(" sp.","",buscaesp(especies$grupo[1], especies$esp[1]),
    #       perl = T,useBytes = T)
    # }
    # # especies<-especies[is.na(especies$aphia) | especies$especie==0,]
    especies$especie<-sub(" sp.","",especies$especie,fixed = TRUE)
    print("Especies a buscar:")
    print(especies)
    if (any(is.na(especies$aphia[1])|especies$aphia==0)) especies$aphia[1] <-worms::wormsbynames(as.character(especies$especie[1]))$valid_AphiaID
      for (i1 in 2:nrow(especies)) {
        if (any(is.na(especies$aphia[i1]) | especies$aphia[i1]==0)) {
          especies$especie[i1] <- buscaesp(especies$grupo[i1], especies$esp[i1])
          if (substr(x = especies$especie[i1],start = nchar(especies$especie[i1]) - 3,
            stop = nchar(especies$especie[i1])) == " sp.") {
            especies$especie[i1] <-sub(" sp.","",buscaesp(especies$grupo[i1], especies$esp[i1]),
                perl = TRUE,useBytes = TRUE,fixed = TRUE)
          }
          especies$aphia[i1] <- worms::wormsbynames(especies$especie[i1],verbose = F)$valid_AphiaID
          print(especies[i1,c("especie","aphia")])
#          write.csv(especies[,c("especie","aphia")], "c:/camp/peces.csv", row.names = F)
        }
      print(especies[any(is.na(especies$aphia)|especies$aphia==0),])
      if (export) {
        write.csv(especies,"c:/camp/especies.csv",row.names = FALSE)
        }
      }
    print(especies)
    print("Fichero salida: c:/camp/especies.csv")
  }
