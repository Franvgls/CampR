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
   esp<-formatC(esp,width=3,flag=" ")
   ch1<-DBI::dbConnect(odbc::odbc(), dns)
   dumb<-DBI::dbListTables(ch1)
   dumb<-unlist(dumb)
   dumb<-dumb[nchar(dumb)<9]
   camps.f<-substr(dumb[grepl("FAUNA",dumb)],6,8)
   camps.t<-substr(dumb[grepl("NTALL",dumb)],6,8)
   camps.c<-substr(dumb[grepl("CAMP",dumb)],5,7)
   fauna<-cbind(camp=camps.f[1],DBI::dbGetQuery(ch1,paste0("select * from FAUNA",camps.f[1]," where grupo='",gr,"'")))   #sqlFetch(ch1,paste("FAUNA",camps.f[1],sep="")))
   fauna<-filter(fauna,ESP %in% esp)
   f_names<-c("LANCE","GRUPO","ESP","PESO_GR","NUMERO")
   tallas<-cbind(camp=camps.t[1],DBI::dbGetQuery(ch1,paste0("select * from NTALL",camps.t[1]," where grupo='",gr,"'")))
   tallas<-filter(tallas,ESP %in% esp)
   t_names<-c("LANCE","GRUPO","ESP","CATE","SEXO","PESO_M","PESO_GR","TALLA","NUMER")
   for (i in 2:length(camps.f)) {
      if(nrow(DBI::dbGetQuery(ch1,paste0("select * from FAUNA",camps.f[i])))>0) {
        fauna<-rbind(fauna,cbind(camp=camps.f[i],DBI::dbGetQuery(ch1,paste0("select * from FAUNA",camps.f[i]," where grupo='",gr,"'"))[,c(f_names)]))
        fauna<-filter(fauna,ESP %in% esp)
        }
     else warning(paste("Revisa campaña:",paste0("FAUNA",camps.f[i]),"no contiene datos"))
   }
   for (i in 2:length(camps.t)) {
      if (nrow(DBI::dbGetQuery(ch1,paste0("select * from NTALL",camps.t[i])))>0) {
          tallas<-rbind(tallas,cbind(camp=camps.t[i],DBI::dbGetQuery(ch1,paste0("select * from NTALL",camps.t[i]," where grupo='",gr,"'"))[,c(t_names)]))
          tallas<-filter(tallas,ESP %in% esp)
          }
     else warning(paste("Revisa campaña:",paste0("NTALL",camps.f[i]),"no contiene datos de tallas"))
   }
   DBI::dbDisconnect(ch1)
   fauna$ESP<-as.numeric(as.character(fauna$ESP))
   tallas$ESP<-as.numeric(as.character(tallas$ESP))
   absp<-fauna[fauna$GRUPO==as.integer(gr) & fauna$ESP==as.integer(esp),c(1:2,4:5)]
   ntalls<-tallas[tallas$GRUPO==as.integer(gr) & tallas$ESP==as.integer(esp),c(1,4,7,6,8,5,9)]
   absp<-absp[absp$PESO_GR>0,]
   absp$camp<-as.factor(as.character(absp$camp))
   ntalls$camp<-as.factor(as.character(ntalls$camp))
   if (length(levels(absp$camp))>0) {
      print(paste("Especie",buscaesp(gr,esp),"en campañas:"))
      print(levels(absp$camp))
      if (length(levels(ntalls$camp))>0) {
        print(paste("Especie",buscaesp(gr,esp),"con datos de tallas en campañas:"))
        print(levels(ntalls$camp))
      }
      else print(paste("No hay información de tallas para",buscaesp(gr,esp)))
   }
   else print(paste("No hay capturas de",buscaesp(gr,esp),"ninguna campaña"))
   if (tablas) data.table(campañas=dumb[grepl("CAMP",dumb)],faunas=dumb[grepl("FAUNA",dumb)],tallas=dumb[grepl("NTALL",dumb)])
}


