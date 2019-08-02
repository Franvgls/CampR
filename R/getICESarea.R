#' Includes ICESrect in an HH file with lat and long data
#'
#' Function to complete HH with ICESrect and area
#' @param camp Campaña de la que se extraen los datos: año concreto (XX): Demersales "NXX", Porcupine "PXX", Arsa primavera "1XX" y Arsa otoño "2XX"
#' @param dns Elige el origen de las bases de datos: Porcupine "Pnew", Cantábrico "Cant, Golfo de Cádiz "Arsa" (únicamente para sacar datos al IBTS, no gráficos)gr Grupo de la especie: 1 peces, 2 crustáceos 3 moluscos 4 equinodermos 5 invertebrados
#' @param incl2 Si T se incluyen los lances extra no incluidos para las abundancias o biomasas estratificadas
#' @param incl0 Si T se incluyen los lances nulos
#' @return A data.table with haul variables, ICESrect and ICESarea
#' @examples # getICESrect(icesDatras::getHHdata("SP-NORTH",2015,4),latname="haullat",longname="haullong")
#' @export
getICESarea<-function(camp,dns,incl2=T,incl0=F) {
  lebels=c(paste0("C",0:9),paste0("D",0:9),paste0("E",0:9),paste0("F",0:9),paste0("F",0:9),paste0("G",0:9),paste0("H",0:9))
  DB<-data.table::as.data.table(CampR::datlan.camp(camp,dns,incl2=incl2,incl0 = incl0,redux=T))
  #names(DB)<-tolower(names(DB))
  DB$rectlong<-cut(DB$long,breaks=seq(from=-30,to=40,by=1),labels=lebels) # ,"D9","D8"
  DB$rectlat<-cut(DB$lat,breaks=seq(from=36.0,to=71,by=.5),labels=formatC(c(1:70),flag=0,width=2))
  DB$StatRec<-paste0(DB$rectlat,DB$rectlong)
  DB$rectlat <- NULL
  DB$rectlong <- NULL
  DB$icesArea<-Area[match(DB$StatRec,Area$ICESNAME),"Area"]
  DB
  }


