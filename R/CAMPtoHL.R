#' Exporta datos de formato CAMP a formato DATRAs HL. Depende de que los códigos Aphia estén correctos en especies.dbf da error si son incompletos
#'
#' Función de Salida de datos a DATRAS:
#' Extrae las características de las capturas por lance para una campaña desde el fichero NTALLxxx.DBF y los transforma en formato DATRAS HL. De momento sólo funciona con peces y en el SPNGFS y SPPORC (Para completar crustáceos y moluscos hay que añadir los AphiaID, y para ARSA añadirlos al especies de ARSA)
#' @param camp Campaña de la que se extraen los datos: año concreto (XX): Demersales "NXX", Porcupine "PXX", Arsa primavera "1XX" y Arsa otoño "2XX"
#' @param dns Elige el origen de las bases de datos: Porcupine "Porc", Cantábrico "Cant, Golfo de Cádiz "Arsa"
#' @param inclSpecie si T incluye el nombre de la especie y el Código, si no sólo el Aphia
#' @param quart si F deja en cada lance el valor del trimestre en que se realizó el lance, si T se deja el que tiene la campaña por defecto, 1 para Arsa 1Q, 3 para Porcupine y 4 para Arsa 4Q y Demersales Northern Shelf
#' @param incl2 Si F deja fuera los lances especiales que actualmente no se transmiten a DATRAS, si T los incluye
#' @param export Si T crea un fichero csv con todos los datos corregidos (APHIAs) en el directorio CAMP donde está el especies.dbf este es importable al especies.dbf con un append from deli with, quitando todos los peces grupo="1"
#' @return Devuelve un data.table con datos de cada especie en el formato HL de DATRAS. DATRAS requiere que los datos no tengan cabecera y el trimestre sea el que corresponde a la campaña, además de no tener "". Por ello se debe pasar a fichero con la orden: write.table(CAMPtoHH(Xyy,dns),"nombrearchivo.csv",sep=",",quote=F,col.names=F,row.names=F))
#' @examples # CAMPtoHL("P14","Porc")
#' @export
CAMPtoHL <-
  function(camp,dns,inclSpecie = FALSE,quart = TRUE,incl2 = FALSE,export = FALSE) {
    require(dplyr)
    if (length(camp) > 1) {
      stop("seleccionadas más de una campaña, no se pueden sacar resultados de más de una")
    }
    DB <-data.table::as.data.table(datlan.camp(camp,dns,redux = F,incl0 = F,incl2 = incl2))
    ch1<-DBI::dbConnect(odbc::odbc(), dns)
    on.exit(DBI::dbDisconnect(ch1), add = TRUE)
    ntalls <-data.table::as.data.table(DBI::dbGetQuery(ch1,paste0("select * from NTALL",camp," where GRUPO='1'")))
    #DBI::dbDisconnect(ch1)
    names(ntalls) <- tolower(names(ntalls))
    if (dns=="Arsa") {
      ch2<-DBI::dbConnect(odbc::odbc(),dsn="Arsa")
      on.exit(DBI::dbDisconnect(ch2), add = TRUE)
    }
    if (dns=="Cant" |dns=="Porc"){
      ch2 <- DBI::dbConnect(odbc::odbc(), dsn= "Camp")
      on.exit(DBI::dbDisconnect(ch2), add = TRUE)
    }
    especies <-data.table::as.data.table(DBI::dbReadTable(ch2, "ESPECIES"))
    #DBI::dbDisconnect(ch2)
    names(especies) <- tolower(names(especies))
    especies <- subset(especies, especies$grupo == 1)
    #    especies<-subset(especies,especies$esp %in% unique(ntalls[ntalls$grupo==2,"esp"]))
    especies <- dplyr::arrange(especies, esp)
    especies %>% dplyr::mutate_if(is.factor, as.character) -> especies
    especies$especie[1] <- buscaesp(especies$grupo[1], especies$esp[1])
    if (substr(x = especies$especie[1],start = nchar(especies$especie[1]) - 3,
               stop = nchar(especies$especie[1])) == " sp.") {
      especies$especie[1] <-sub(" sp.","",buscaesp(especies$grupo[1], especies$esp[1]),fixed = TRUE)
      }
    if (is.na(especies$aphia[1])) especies$aphia[1] <-worrms::wm_name2id(as.character(especies$especie[1]))
    if (export) {
      for (i1 in 2:nrow(especies)) {
        if (is.na(especies$aphia[i1])) {
          especies$especie[i1] <- buscaesp(especies$grupo[i1], especies$esp[i1])
          if (substr(x = especies$especie[i1],start = nchar(especies$especie[i1]) - 3,
            stop = nchar(especies$especie[i1])) == " sp.") {
            especies$especie[i1] <-sub(" sp.","",buscaesp(especies$grupo[i1], especies$esp[i1]),
                perl = T)
          }
          especies$aphia[i1] <- worrms::wm_name2id(especies$especie[i1])
          write.csv(especies[,c("especie","aphia")], "c:/camp/peces.csv", row.names = F)
        }
      }
      write.csv(especies, "c:/camp/peces.csv", row.names = F)
    }
    if (substr(dns, 1, 4) == "Cant" | substr(dns, 1, 4) == "Cnew") {
      DB$Survey = "SP-NORTH"
      DB$Gear = "BAK"
      if (any(DB$barco !="29MO")) {DB$barco = ifelse(DB$barco == "MOL", "29MO", ifelse(DB$barco == "CDS", "29CS"))}
      DB$GearEx = -9
      DB$DoorType = ifelse(DB$barco == "29CS", "W", "P")
      if (quart)
        DB$quarter <- "4"
      DB$lance <- format(DB$lance, width = 3,justify="r")
      ntalls$lance <- format(as.integer(ntalls$lance), width = 3,justify="r")
      DB$StNo = DB$lance
    }
    if (substr(dns, 1, 4) == "Pnew" | substr(dns, 1, 4) == "Porc") {
      DB$Survey = "SP-PORC"
      DB$barco = "29EZ"
      DB$Gear = "PORB"
      DB$GearEx = -9
      DB$DoorType = "P"
      if (quart)
        DB$quarter <- "3"
      DB$lance <- format(as.integer(DB$lance), width = 2,justify="r")
      ntalls$lance <- format(as.integer(ntalls$lance), width = 2,justify="r")
      DB$StNo <- format(as.integer(DB$cuadricula),width = 3,justify="r")
      }
    if (substr(dns, 1, 4) == "Arsa") {
      DB$Survey = "SP-ARSA"
      if (any(DB$barco !="29MO")) {DB$barco = ifelse(substr(DB$barco, 1, 3) == "COR",
                        "CDS",
                        ifelse(DB$barco == "MOL", "29MO"))}
      DB$Gear = "BAK"
      DB$GearEx = -9
      DB$DoorType = ifelse(substr(DB$barco, 1, 3) == "COR", "W", "P")
      if (quart)
        DB$quarter <- ifelse(substr(camp, 1, 1) == "1", "1", "4")
      DB$lance <- format(DB$lance, width = 2,justify="r")
      ntalls$lance <- format(ntalls$lance, width = 2,justify="r")
      DB$StNo = DB$lance
    }
    DB <-DB[, c("Survey","year","barco","quarter","Gear","malletas","GearEx","DoorType","lance","StNo","validez","prof_l","prof_v")]
    ntalls <- ntalls[lance %in% DB$lance, ]
    ntalls <- subset(ntalls, grupo == 1)
    ntalls$SubFactor <- round(ntalls$peso_gr / ntalls$peso_m, 4)
    dumb <- ntalls[, .(NoMeas = sum(numer)), by = .(lance, esp, sexo, cate)]
    dumb <- dumb[, c("lance", "esp", "sexo", "cate", "NoMeas")]
    ntallsdumb <- merge(ntalls, dumb, all.x = TRUE)
    ntallsdumb$esp<-as.integer(ntallsdumb$esp)
    ntallsdumb$SpecCode <-
      as.character(especies$aphia[match(as.integer(ntallsdumb$esp), as.integer(especies$esp))])
    ntallsdumb$Specie <-
      as.character(especies$especie[match(as.integer(ntallsdumb$esp), as.integer(especies$esp))])
    ntallsdumb$med <-
      as.character(especies$med[match(ntallsdumb$esp, especies$esp)])
    if (nrow(dplyr::filter(ntallsdumb,is.na(SpecCode)))>1) {
      print(dplyr::filter(ntallsdumb,is.na(SpecCode)))
    }
    ntallsdumb$incr <-
      as.character(especies$increm[match(as.integer(ntallsdumb$esp), as.integer(especies$esp))])
    ntallsdumb$LngtCode <- NA
    ntallsdumb$LngtCode[ntallsdumb$med == 1] <- "1"
    ntallsdumb$LngtCode[ntallsdumb$med == 2] <- "."
    ntallsdumb$LngtCode[ntallsdumb$incr == 5] <- "0"
    DB1 <-
      merge(ntallsdumb,
            data.table::as.data.table(DB),
            all.x = T,
            by = "lance")
    DB1$Sex <-
      as.character(factor(
        DB1$sexo,
        levels = as.character(1:3),
        labels = c("M", "F", "U")
      ))
    if (inclSpecie == T) {
      HL_north <-
        data.table::data.table(
          RecordType = "HL",
          Survey=DB1$Survey,
          Quarter = DB1$quarter,
          Country = "ES",
          Ship = DB1$barco,
          Gear = DB1$Gear,
          SweepLngt = DB1$malletas,
          GearEx = DB1$GearEx,
          DoorType = DB1$DoorType,
          StNo = DB1$StNo,
          HaulNo = DB1$lance,
          Year = DB1$year,
          SpecCodeType = "W",
          SpecCode = DB1$SpecCode,
          specie = DB1$Specie,
          SpecVal = ifelse(DB1$validez==1,1,0),
          Sex = DB1$Sex,
          TotalNo = round(DB1$NoMeas * DB1$SubFactor, 2),
          CatIdentifier = DB1$cate,
          NoMeas = DB1$NoMeas,
          SubFactor = DB1$SubFactor,
          SubWgt = DB1$peso_m,
          CatCatchWgt = DB1$peso_gr,
          LngtCode = DB1$LngtCode,
          LngtClass = DB1$talla,
          HLNoAtLngt = DB1$numer,
          DevStage=-9,
          LenMeasType=-9,
          DateofCalculation=-9,
          Valid_Aphia=-9
          )
    }
    else
      HL_north <-
      data.table::data.table(
        RecordType = "HL",
        Survey = DB1$Survey,
        Quarter = DB1$quarter,
        Country = "ES",
        Ship = DB1$barco,
        Gear = DB1$Gear,
        SweepLngt = DB1$malletas,
        GearEx = DB1$GearEx,
        DoorType = DB1$DoorType,
        StNo = DB1$StNo,
        HaulNo = DB1$lance,
        Year = DB1$year,
        SpecCodeType = "W",
        SpecCode = DB1$SpecCode,
        SpecVal = ifelse(DB1$validez==1,1,0),
        Sex = DB1$Sex,
        TotalNo = round(DB1$NoMeas * DB1$SubFactor, 2),
        CatIdentifier = DB1$cate,
        NoMeas = DB1$NoMeas,
        SubFactor = DB1$SubFactor,
        SubWgt = DB1$peso_m,
        CatCatchWgt = DB1$peso_gr,
        LngtCode = DB1$LngtCode,
        LngtClass = DB1$talla,
        HLNoAtLngt = DB1$numer,
        DevStage=-9,
        LenMeasType=-9,
        DateofCalculation=-9,
        Valid_Aphia=-9
      )
    if (any(is.na(HL_north$SpecCode))) {
      HL_north[is.na(HL_north$SpecCode), ]
      message(
        "Algunas especies no tienen código AphiaID, conversión incompleta, revise especies.dbf"
      )
    }
    HL_north[order(as.numeric(HL_north$HaulNo), HL_north$SpecCode, HL_north$LngtClass), ]
  }
