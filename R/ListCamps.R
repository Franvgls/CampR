#' Listado de campañas presentes en dns con ficheros que contienen cada una mejor utilizar {\link\{CampsDNS.camp}}
#'
#' Función de acceso a datos:
#' A partir de ficheros de camp.dbf,fauna.dbf,ntall.dbf da un listado de campañas presentes en el directorio con sus ficheros
#' @param dns Elige el origen de las bases de datos: Porcupine "Porc", Cantábrico "Cant, Golfo de Cádiz "Arsa" (únicamente para sacar datos al IBTS, no gráficos)
#' @param tablas Si T muestra al final de los resultados una lista con todos los ficheros de definición (CAMP con su identificador de campaña), fauna y tallas presentes en el directorio.
#' @return Devuelve el listado de ficheros dbf campañas que hay en el directorio; campXXX.dbf,faunaXXX.dbf, ntallXXX.dbf, edadXXX.dbf, hidroxxx.dbf
#' @examples ListCamps_b(dns="Porc",tablas=T)
#' @family Series datos
#' @export
ListCamps<- function(dns,tablas=TRUE) {
   ch1<-DBI::dbConnect(odbc::odbc(), dns)
   dumbdir<-DBI::dbConnect(odbc::odbc(), dns)@info$dbname
   dumb<-DBI::dbListTables(ch1)
   dumb<-unlist(dumb)
   dumb<-dumb[nchar(dumb)<9]
   typear<-c("EDAD","CAMP","LANCE","FAUNA","NTALL","HIDRO")
   dumb_b<-dumb[c(grepl(typear[1],substr(dumb,1,4)) & nchar(dumb)==7)]
   dumb_b<-c(dumb_b,dumb[grepl(typear[2],substr(dumb,1,4))])
   for (i in typear[3:5]) {
      dumb_b<-c(dumb_b,dumb[grepl(i,substr(dumb,1,5))])
      }
   dumb_b<-data.frame(fic=dumb_b[nchar(dumb_b)<9],camp=ifelse(nchar(dumb_b)==7,substr(dumb_b,5,7),substr(dumb_b,6,8)))  #fichasnxx.dbf
   for (i in as.character(unique(dumb_b$camp))) {
      print(c(i,as.character(dumb_b[dumb_b$camp==i,]$fic)))
   }
   }

# camps.c<-substr(dumb[grepl("CAMP",dumb)],5,7)
   # camps.l<-substr(dumb[grepl("LANCE",dumb)],6,8)
   # camps.f<-substr(dumb[grepl("FAUNA",dumb)],6,8)
   # camps.t<-substr(dumb[grepl("NTALL",dumb)],6,8)
   # camps.h<-substr(dumb[grepl("HIDRO",dumb)],6,8)
   # camps.e<-substr(dumb[grepl("EDAD",dumb)],5,7)
   # if (nrow(DBI::dbGetQuery(ch1,paste0("select IDENT from CAMP",camps.c[1])))>0){
   #    idCamp<-as.character(DBI::dbGetQuery(ch1,paste0("select IDENT from CAMP",camps.c[1])))
   # }
   # for (i in 2:length(camps.c)) {
   #    idCamp<-c(idCamp,as.character(DBI::dbGetQuery(ch1,paste0("select IDENT from CAMP",camps.c[i]))))
   # }
   # DBI::dbDisconnect(ch1)
   # if (tablas) data.table(campana=dumb[grepl("CAMP",dumb)],idCamps=idCamp,lances=dumb[grepl("LANCE",dumb)],
   #                        fauna=dumb[grepl("FAUNA",dumb)],tallas=dumb[grepl("NTALL",dumb)],hidro=dumb[grepl("HIDRO",dumb)],
   #                        edades=dumb[grepl("EDAD",dumb)])


# fics=c("CAMP","LANCE","FAUNA","NTALL","EDAD")
# dumbfics<-data.frame(fic=dumb[dumb %in% grep(fics[1],dumb,value = T)],camp=substring(dumb[dumb %in% grep(fics[1],dumb,value = T)],nchar(fics[1])+1,8))
# for (i in fics[2:length(fics)]) {
#    dumbfics<-rbind(dumbfics,data.frame(fic=dumb[dumb %in% grep(i,dumb,value=T)],camp=substring(dumb[dumb %in% grep(i,dumb,value = T)],nchar(i)+1,8)))
#    }
# dumbfics

# for (i in as.character(unique(dumb_b$camp))) {
#    print(as.character(dumb_b[dumb_b$camp==i,]$fic))
#    }
