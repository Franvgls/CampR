#' Datos de presencia de especie en campañas en dns
#'
#' Función de acceso a datos:
#' A partir de ficheros de fauna.dbf presentes en directorio comprueba presencia de especie
#' @param gr Grupo de la especie: 1 peces, 2 crustáceos 3 moluscos 4 equinodermos 5 invertebrados 6 desechos y otros
#' @param esp Código de la especie numérico o carácter con tres espacios. Función para una sola especie
#' @param dns Elige el origen de las bases de datos: Porcupine "Porc", Cantábrico "Cant, Golfo de Cádiz "Arsa" (únicamente para sacar datos al IBTS, no gráficos)
#' @return Devuelve un vector con las campañas con presencia de la especie
#' @examples PresenciaEsp.camp(1,220,"Porc")
#' @export
PresenciaEsp.camp<- function(gr,esp,dns) {
   if (any(length(esp)>1 | esp=="999" | gr=="9")) stop("Seleccionadas mas de una especie, función para sólo una especie")
   ch1<-RODBC::odbcConnect(dsn=dns)
   RODBC::odbcSetAutoCommit(ch1, FALSE)
   RODBC::odbcTables(ch1)
   dumb<-RODBC::sqlFetchMore(ch1)
   dumb$TABLE_NAME<-as.character(dumb$TABLE_NAME)
   camps.f<-substr(dumb$TABLE_NAME[grepl("FAUNA",dumb$TABLE_NAME)],6,8)
   camps.t<-substr(dumb$TABLE_NAME[grepl("NTALL",dumb$TABLE_NAME)],6,8)
   fauna<-cbind(camp=camps.f[1],RODBC::sqlFetch(ch1,paste("FAUNA",camps.f[1],sep="")))
   f_names<-c("LANCE","GRUPO","ESP","PESO_GR","NUMERO")  
   tallas<-cbind(camp=camps.t[1],RODBC::sqlFetch(ch1,paste("NTALL",camps.t[1],sep="")))
   t_names<-c("LANCE","GRUPO","ESP","CATE","SEXO","PESO_M","PESO_GR","TALLA","NUMER")
   for (i in 2:length(camps.f)) {
       fauna<-rbind(fauna,cbind(camp=camps.f[i],RODBC::sqlFetch(ch1,paste("FAUNA",camps.f[i],sep=""))[,f_names]))
   }
   for (i in 2:length(camps.t)) {
       if (nrow(RODBC::sqlFetch(ch1,paste("NTALL",camps.t[i],sep="")))>0) {
          tallas<-rbind(tallas,cbind(camp=camps.t[i],RODBC::sqlFetch(ch1,paste("NTALL",camps.t[i],sep=""))[,t_names])) 
          }
       }
   RODBC::odbcClose(ch1)
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
}


