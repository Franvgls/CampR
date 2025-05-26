#' Listado de campañas presentes en el directorio e información sobre ellas
#'
#' Función de acceso a datos:
#' A partir de ficheros de fauna.dbf presentes en directorio comprueba presencia de especie
#' @param dns Elige el origen de las bases de datos: Porcupine "Porc", Cantábrico "Cant, Golfo de Cádiz "Arsa" (únicamente para sacar datos al IBTS, no gráficos)
#' @return Un data.frame con columnas con la lista de archivos campXXX,lanceXXX,faunaXXX,ntallXXX e hidroXXX en el directorio, rellena los que falten con "."
#' @examples CampsDNS.camp("Porc")
#' @family Series datos
#' @export
CampsDNS.camp<- function(dns) {
  ch1<-DBI::dbConnect(odbc::odbc(), dns)
  on.exit(DBI::dbDisconnect(ch1), add = TRUE)
  dumbdir<-DBI::dbConnect(odbc::odbc(), dns)@info$dbname
  on.exit(DBI::dbDisconnect(dumbdir), add = TRUE)
  dumb<-DBI::dbListTables(ch1)
  dumb<-unlist(dumb)
  dumb<-dumb[nchar(dumb)<9 & grepl(c("CAMP|LANCE|FAUNA|NTALL|EDAD|HIDRO"),dumb)]
  camps<-dplyr::tibble(tipo="CAMP",ident=substr(dumb[grepl("CAMP",dumb)],5,7))
  camps<-rbind(camps,data.frame(tipo=substr(dumb[!grepl("CAMP",dumb)],1,5),ident=substr(dumb[!grepl("CAMP",dumb)],6,8)))
  camps.c<-dplyr::tibble(tipo=dumb[grepl("CAMP",dumb)],ident=substr(dumb[grepl("CAMP",dumb)],5,7))
  camps.l<-dplyr::tibble(tipo=dumb[grepl("LANCE",dumb)],ident=substr(dumb[grepl("LANCE",dumb)],6,8))
  camps.f<-dplyr::tibble(tipo=dumb[grepl("FAUNA",dumb)],ident=substr(dumb[grepl("FAUNA",dumb)],6,8))
  camps.t<-dplyr::tibble(tipo=dumb[grepl("NTALL",dumb)],ident=substr(dumb[grepl("NTALL",dumb)],6,8))
  camps.h<-dplyr::tibble(tipo=dumb[grepl("HIDRO",dumb)],ident=substr(dumb[grepl("HIDRO",dumb)],6,8))
  camps.e<-dplyr::tibble(tipo=dumb[grepl("EDAD",dumb)],ident=substr(dumb[grepl("EDAD",dumb)],5,7))
  camps.e<-camps.e[!nchar(camps.e$tipo)>7,]
  if (length(DBI::dbGetQuery(ch1,paste0("select IDENT from ",paste0(camps$tipo[camps$tipo=="CAMP"],camps$ident[camps$tipo=="CAMP"])[1])))>0){
    NomCamp<-cbind(camp=paste0("CAMP",camps$ident[camps$tipo=="CAMP"][1]),DBI::dbGetQuery(ch1,paste0("select IDENT from ",paste0("CAMP",camps$ident[camps$tipo=="CAMP"][1]))[1]))
  }
  else NomCamp<-data.frame(camp=NA,ident=NA)
  for (i in 2:length(paste0(camps$tipo[camps$tipo=="CAMP"],camps$ident[camps$tipo=="CAMP"]))) {
    if (length(DBI::dbGetQuery(ch1,paste0("select IDENT from ",paste0("CAMP",camps$ident[camps$tipo=="CAMP"])[i])))>0) {
      NomCamp<-rbind(NomCamp,cbind(camp=paste0("CAMP",camps$ident[camps$tipo=="CAMP"][i]),ident=DBI::dbGetQuery(ch1,paste0("select IDENT from ",paste0("CAMP",camps$ident[camps$tipo=="CAMP"][i])))))
    }
  }
  if (nrow(DBI::dbGetQuery(ch1,paste0("select FECHA from ","LANCE",camps$ident[camps$tipo=="LANCE"][1])))>0){
    m<-year(as.Date((DBI::dbGetQuery(ch1,paste0("select FECHA from ","LANCE",camps$ident[camps$tipo=="LANCE"][1])))[1,],format="%Y-%m-%d")) %% 100
    m<-ifelse(m>70,1900+m,2000+m)
    YeCamp<-dplyr::tibble(camp=paste0("LANCE",camps$ident[camps$tipo=="LANCE"][1][1]),year=m)
  }
  else YeCamp<-dplyr::tibble(camp=NA,year=NA)
  for (i in 2:length(paste0("LANCE",camps$ident[camps$tipo=="LANCE"]))) {
    if (nrow(DBI::dbGetQuery(ch1,paste0("select FECHA from ","LANCE",camps$ident[camps$tipo=="LANCE"][i])))>0) {
      m<-year(as.Date((DBI::dbGetQuery(ch1,paste0("select FECHA from ","LANCE",camps$ident[camps$tipo=="LANCE"][i])))[1,],format="%Y-%m-%d")) %% 100
      m<-ifelse(m<70,2000+m,1900+m)
      YeCamp<-rbind(YeCamp,cbind(camp=paste0("LANCE",camps$ident[camps$tipo=="LANCE"])[i],year=m))
      }
    }
  #DBI::dbDisconnect(ch1)
  nombres<-NomCamp$IDENT
  #anyos<-NomCamp$anyos
  anyos_lan<-as.data.frame(YeCamp)$year
  Narchs<-max(nrow(camps.c),nrow(camps.l),nrow(camps.f),nrow(camps.t),nrow(camps.h),length(nombres))
  #cbind(camps.c[order(substr(camps.c,5,7))],camps.l[order(substr(camps.l,6,8))],camps.f[order(substr(camps.f,6,8))]
  #      ,camps.t[order(substr(camps.t,6,8))],camps.h[order(substr(camps.h,6,8))])
  if (length(nombres)<Narchs) nombres<-c(nombres,rep(".",Narchs-length(nombres)))
  if (length(camps.c$tipo)<Narchs) camps.c<-c(as.character(camps.c$tipo),rep(".",Narchs-nrow(camps.c)))
  #if (length(anyos)<Narchs) anyos-c(anyos,rep(".",Narchs-length(anyos)))
  if (length(camps.l$tipo)<Narchs) camps.l<-c(as.character(camps.l$tipo),rep(".",Narchs-nrow(camps.l)))
  if (length(anyos_lan)<Narchs) anyos_lan<-c(anyos_lan,rep(".",Narchs-length(anyos_lan)))
  if (length(camps.f$tipo)<Narchs) camps.f<-c(as.character(camps.f$tipo),rep(".",Narchs-nrow(camps.f)))
  if (length(camps.t$tipo)<Narchs) camps.t<-c(as.character(camps.t$tipo),rep(".",Narchs-nrow(camps.t)))
  if (length(camps.h$tipo)<Narchs) camps.h<-c(as.character(camps.h$tipo),rep(".",Narchs-nrow(camps.h)))
  if (length(camps.e$tipo)<Narchs) camps.e<-c(as.character(camps.e$tipo),rep(".",Narchs-nrow(camps.e)))
  message(paste("Directorio:",dumbdir))
  DD<-dplyr::tibble(NomCamp=nombres,Year=anyos_lan,Camp=camps.c,Lance=camps.l,years=anyos_lan,Fauna=camps.f,
                 Tallas=camps.t,Edad=camps.e,Hidro=camps.h)
  print(DD[order(as.character(DD$Year),DD$NomCamp),],n=Inf) #c(1:4,7,9:11)
}

# cbind(camps.c[substr(camps.f,6,8) %in% substr(camps.c,5,7)],camps.l[substr(camps.c,5,7) %in% substr(camps.l,6,8)],camps.f[substr(camps.c,5,7) %in% substr(camps.f,6,8)])
