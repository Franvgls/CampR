#' Datos de presencia de especie en campañas en dns
#'
#' Función de acceso a datos:
#' A partir de ficheros de fauna.dbf presentes en directorio comprueba presencia de especie
#' @param gr Grupo de la especie: 1 peces, 2 crustáceos 3 moluscos 4 equinodermos 5 invertebrados 6 desechos y otros
#' @param esp Código de la especie numérico o carácter con tres espacios. Función para una sola especie
#' @param dns Elige el origen de las bases de datos: Porcupine "Porc", Cantábrico "Cant, Golfo de Cádiz "Arsa" (únicamente para sacar datos al IBTS, no gráficos)
#' @param tablas Si T muestra al final de los resultados una lista con todos los ficheros de definición (CAMP), fauna y tallas presentes en el directorio.
#' @return Devuelve dos vectores con las campañas con presencia de la especie, y con las campañas con datos de talla de la especie. Tb incluye información sobre campañas que no tienen datos en el fichero FAUNAXXX.dbf y/o NTALLXXX.dbf
#' @examples PresenciaEsp.camp(1,220,"Porc")
#' @export
PresenciaEsp.camp<- function(gr,esp,dns,tablas=FALSE) {
   if (any(length(esp)>1 | esp=="999" | gr=="9")) stop("Seleccionadas mas de una especie, función para sólo una especie")
   esp<-format(esp,width=3,justify="r")
   ch1<-DBI::dbConnect(odbc::odbc(), dns)
   dumbdir<-DBI::dbConnect(odbc::odbc(), dns)@info$dbname
   dumb<-DBI::dbListTables(ch1)
   dumb<-unlist(dumb)
   dumb<-dumb[nchar(dumb)<9]
   camps.f<-substr(dumb[grepl("FAUNA",dumb)],6,8)
   camps.t<-substr(dumb[grepl("NTALL",dumb)],6,8)
   camps.c<-substr(dumb[grepl("CAMP",dumb)],5,7)
   if (nrow(DBI::dbGetQuery(ch1,paste0("select * from FAUNA",camps.f[1]," where grupo='",gr,"' and esp='",esp,"'")))>0){
     fauna<-cbind(camp=camps.f[1],DBI::dbGetQuery(ch1,paste0("select * from FAUNA",camps.f[1]," where grupo='",gr,"' and esp='",esp,"'")))
   }
   else fauna<-data.frame(camp=NULL,LANCE=NULL,GRUPO=NULL,ESP=NULL,PESO_GR=NULL,NUMERO=NULL)
   f_names<-c("LANCE","GRUPO","ESP","PESO_GR","NUMERO")
   if (nrow(DBI::dbGetQuery(ch1,paste0("select * from NTALL",camps.t[1]," where grupo='",gr,"' and esp='",esp,"'")))>0){
   tallas<-cbind(camp=camps.t[1],DBI::dbGetQuery(ch1,paste0("select * from NTALL",camps.t[1]," where grupo='",gr,"' and esp='",esp,"'")))
   }
   else tallas<-data.frame(camp=NULL,LANCE=NULL,GRUPO=NULL,ESP=NULL,CATE=NULL,SEXO=NULL,PESO_M=NULL,PESO_GR=NULL,TALLA=NULL,NUMER=NULL)
   t_names<-c("LANCE","GRUPO","ESP","CATE","SEXO","PESO_M","PESO_GR","TALLA","NUMER")
   for (i in 2:length(camps.f)) {
      if(nrow(DBI::dbGetQuery(ch1,paste0("select * from FAUNA",camps.f[i]," where grupo='",gr,"' and esp='",esp,"'")))>0) {
        fauna<-rbind(fauna,cbind(camp=camps.f[i],DBI::dbGetQuery(ch1,paste0("select * from FAUNA",camps.f[i]," where grupo='",gr,"' and esp='",esp,"'"))[,c(f_names)]))
        }
     #else message(paste0("La campaña ",paste0("FAUNA",camps.f[i])," no tiene capturas de ",buscaesp(gr,esp)))
   }
   for (i in 2:length(camps.t)) {
      if (nrow(DBI::dbGetQuery(ch1,paste0("select * from NTALL",camps.t[i]," where grupo='",gr,"' and esp='",esp,"'")))>0) {
          tallas<-rbind(tallas,cbind(camp=camps.t[i],DBI::dbGetQuery(ch1,paste0("select * from NTALL",camps.t[i]," where grupo='",gr,"' and esp='",esp,"'"))[,c(t_names)]))
          }
     #else message(paste0("La campaña ",paste0("NTALL",camps.f[i])," no tiene datos de tallas de ",buscaesp(gr,esp)))
   }
   DBI::dbDisconnect(ch1)
   fauna$ESP<-as.numeric(as.character(fauna$ESP))
   tallas$ESP<-as.numeric(as.character(tallas$ESP))
   if (nrow(fauna)==0) {
     message(paste("No hay capturas de",buscaesp(gr,esp),"en ninguna campaña del directorio",dumbdir))
   }
   else {
   absp<-fauna[fauna$GRUPO==as.integer(gr) & fauna$ESP==as.integer(esp),c(1:2,4:5)]
   ntalls<-tallas[tallas$GRUPO==as.integer(gr) & tallas$ESP==as.integer(esp),c(1,4,7,6,8,5,9)]
   absp<-absp[absp$PESO_GR>0,]
   absp$camp<-as.factor(as.character(absp$camp))
   ntalls$camp<-as.factor(as.character(ntalls$camp))
   message(paste0("En el directorio ",dumbdir,": "))
   if (length(levels(absp$camp))>0) {
      message(paste0(buscaesp(gr,esp)," aparece en las campañas: ",paste(levels(absp$camp),sep=", ",collapse=", ")))
      if(sum(!camps.t %in% levels(absp$camp))>0) message(paste0("no aparece en las campañas ",paste(camps.f[!camps.t %in% levels(absp$camp)],collapse=", ")))
      if (length(levels(ntalls$camp))>0) {
        message(paste0("Hay datos de tallas de ",buscaesp(gr,esp)," en las campañas: ",paste(levels(ntalls$camp),collapse=", ")))
        if(sum(!camps.t %in% levels(ntalls$camp))>0) message(paste0("no hay datos de tallas en las campañas ",paste(camps.t[!camps.t %in% levels(ntalls$camp)],collapse=", ")))
      }
      else message(paste("No hay información de tallas para",buscaesp(gr,esp)))
   }
   if (tablas) data.table(campañas=dumb[grepl("CAMP",dumb)],faunas=dumb[grepl("FAUNA",dumb)],tallas=dumb[grepl("NTALL",dumb)])
   }
   }


