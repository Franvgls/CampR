#' Exporta datos de formato CAMP a formato DATRAs HL. Depende de que los códigos Aphia estén correctos en especies.dbf da error si son incompletos
#'
#' Función de Salida de datos a DATRAS:
#' Extrae las características de las capturas por lance para una campaña desde el fichero NTALLxxx.DBF y los transforma en formato DATRAS HL. De momento sólo funciona con peces y en el SPNGFS y SPPORC (Para completar crustáceos y moluscos hay que añadir los AphiaID, y para ARSA añadirlos al especies de ARSA)
#' @param gr Grupo del que se desea buscar el AphiaID: 1 peces, 2 crustáceos, 3 moluscos, 4 equinodermos, 5 otros invertebrados
#' @param camp Campaña de la que se extraen los datos: año concreto (XX): Demersales "NXX", Porcupine "PXX", Arsa primavera "1XX" y Arsa otoño "2XX"
#' @param dns Elige el origen de las bases de datos: Porcupine "Porc", Cantábrico "Cant, Golfo de Cádiz "Arsa"
#' @param inclSpecie si T incluye el nombre de la especie y el Código, si no sólo el Aphia
#' @param quart si F deja en cada lance el valor del trimestre en que se realizó el lance, si T se deja el que tiene la campaña por defecto, 1 para Arsa 1Q, 3 para Porcupine y 4 para Arsa 4Q y Demersales Northern Shelf
#' @param incl2 Si F deja fuera los lances especiales que actualmente no se transmiten a DATRAS, si T los incluye
#' @param export Si T crea un fichero csv con todos los datos corregidos (APHIAs) en el directorio CAMP donde está el especies.dbf este es importable al especies.dbf con un append from deli with, quitando todos los peces grupo="1"
#' @return Devuelve un data.table con datos de cada especie en el formato HL de DATRAS. DATRAS requiere que los datos no tengan cabecera y el trimestre sea el que corresponde a la campaña, además de no tener "". Por ello se debe pasar a fichero con la orden: write.table(CAMPtoHH(Xyy,dns),"nombrearchivo.csv",sep=",",quote=F,col.names=F,row.names=F))
#' @examples # CAMPtoHL("P14","Porc")
#' @export
BuscaAphia <-function(gr = 1,camp,dns,inclSpecie = FALSE,quart = TRUE,incl2 = FALSE,export = FALSE) {
    require(dplyr)
    if (length(camp) > 1) {
      stop("seleccionadas más de una campaña, no se pueden sacar resultados de más de una")
    }
    DB <-data.table::as.data.table(datlan.camp(camp,dns,redux = F,incl0 = F,incl2 = incl2))
    ch1<-DBI::dbConnect(odbc::odbc(), dns)
    ntalls <-data.table::as.data.table(DBI::dbGetQuery(ch1,paste0("select * from NTALL",camp," where GRUPO='gr'")))
    DBI::dbDisconnect(ch1)
    names(ntalls) <- tolower(names(ntalls))
    ch2 <- DBI::dbConnect(odbc::odbc(), dsn = "Camp")
    especies <-data.table::as.data.table(DBI::dbReadTable(ch2, "ESPECIES"))
    DBI::dbDisconnect(ch2)
    names(especies) <- tolower(names(especies))
    especies <- subset(especies, especies$grupo == gr)
    #    especies<-subset(especies,especies$esp %in% unique(ntalls[ntalls$grupo==2,"esp"]))
    especies <- dplyr::arrange(especies, esp)
    especies %>% dplyr::mutate_if(is.factor, as.character) -> especies
    especies<-especies[is.na(especies$aphia),c("grupo","esp","especie","aphia")]
    especies$especie[1] <- buscaesp(especies$grupo[1], especies$esp[1])
    if (substr(x = especies$especie[1],start = nchar(especies$especie[1]) - 3,
               stop = nchar(especies$especie[1])) == " sp.") {
      especies$especie[1] <-sub(" sp.","",buscaesp(especies$grupo[1], especies$esp[1]),
          perl = T,useBytes = T)
    }
    especies<-especies[is.na(especies$aphia),]
    if (is.na(especies$aphia[1])) especies$aphia[1] <-worrms::wm_name2id(as.character(especies$especie[1]))
      for (i1 in 2:nrow(especies)) {
        if (is.na(especies$aphia[i1])) {
          especies$especie[i1] <- buscaesp(especies$grupo[i1], especies$esp[i1])
          if (substr(x = especies$especie[i1],start = nchar(especies$especie[i1]) - 3,
            stop = nchar(especies$especie[i1])) == " sp.") {
            especies$especie[i1] <-sub(" sp.","",buscaesp(especies$grupo[i1], especies$esp[i1]),
                perl = T,useBytes = T)
          }
          especies$aphia[i1] <- worrms::wm_name2id(especies$especie[i1])
          print(especies[i1,c("especie","aphia")])
          write.csv(especies[,c("especie","aphia")], "c:/camp/peces.csv", row.names = F)
        }
      print(especies[is.na(especies$aphia),])
      write.csv(especies, "c:/camp/peces.csv", row.names = F)
      }
  }
