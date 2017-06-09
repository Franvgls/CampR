#' Exporta datos de formato CAMP a formato DATRAs HL. Depende de que los códigos Aphia estén correctos en especies.dbf da error si son incompletos
#'
#' Función de Salida de datos a DATRAS:
#' Extrae las características de las capturas por lance para una campaña desde el fichero NTALLxxx.DBF y los transforma en formato DATRAS HL. De momento sólo funciona con peces y en el SPNGFS y SPPORC (Para completar crustáceos y moluscos hay que añadir los AphiaID, y para ARSA añadirlos al especies de ARSA)
#' @param camp Campaña de la que se extraen los datos: año concreto (XX): Demersales "NXX", Porcupine "PXX", Arsa primavera "1XX" y Arsa otoño "2XX"
#' @param dns Elige el origen de las bases de datos: Porcupine "Pnew", Cantábrico "Cant, Golfo de Cádiz "Arsa" 
#' @param inclSpecie si T incluye el nombre de la especie y el Código, si no sólo el Aphia
#' @param quart si F deja en cada lance el valor del trimestre en que se realizó el lance, si T se deja el que tiene la campaña por defecto, 1 para Arsa 1Q, 3 para Porcupine y 4 para Arsa 4Q y Demersales Northern Shelf
#' @param incl2 Si F deja fuera los lances especiales que actualmente no se transmiten a DATRAS, si T los incluye
#' @return Devuelve un data.table con datos de cada especie en el formato HL de DATRAS. DATRAS requiere que los datos no tengan cabecera y el trimestre sea el que corresponde a la campaña, además de no tener "". Por ello se debe pasar a fichero con la orden: write.table(CAMPtoHH(Xyy,dns),"nombrearchivo.csv",sep=",",quote=F,col.names=F,row.names=F))
#' @examples # CAMPtoHL("P14","Porc")
#' @export
CAMPtoHL<-function(camp,dns,inclSpecie=F,quart=T,incl2=F) {
    if (length(camp)>1) {stop("seleccionadas más de una campaña, no se pueden sacar resultados de más de una")}
    DB<-datlan.camp(camp,dns,redux=F,incl0 = F,incl2=incl2)
   	ch1<-RODBC::odbcConnect(dsn=dns)
   	RODBC::odbcSetAutoCommit(ch1, FALSE)
   	ntalls<-RODBC::sqlFetch(ch1,paste0("NTALL",camp))
    names(ntalls)<-tolower(names(ntalls))
    RODBC::odbcClose(ch1)
    ch2<-RODBC::odbcConnect(dsn="Camp")
 	  RODBC::odbcSetAutoCommit(ch2, FALSE)
 	  especies<-RODBC::sqlFetch(ch2,"ESPECIES")
 	  RODBC::odbcClose(ch2)
 	  names(especies)<-tolower(names(especies))
    especies<-subset(especies,especies$grupo==1)
    if (substr(dns,1,4)=="Cant" | substr(dns,1,4)=="Cnew") {
       DB$Gear="BAK"
       DB$barco=ifelse(DB$barco=="MOL","29MO",ifelse(DB$barco=="CDS","CDS"))
       DB$GearExp=-9
       DB$DoorType=ifelse(DB$barco=="CDS","W","P")
       if(quart) DB$quarter<-"4"
       DB$lance<-formatC(DB$lance,flag=0,width=3)
       ntalls$lance<-formatC(ntalls$lance,flag=0,width=3)
       DB$StNo=DB$lance
    }
    if (substr(dns,1,4)=="Pnew" | substr(dns,1,4)=="Porc") {
       DB$barco="EZA"
       DB$Gear="PORB"
       DB$GearExp=-9
       DB$DoorType="P"
       if(quart) DB$quarter<-"3"
       DB$lance<-formatC(DB$lance,flag=0,width=2)
       ntalls$lance<-formatC(ntalls$lance,flag=0,width=2)
       DB$StNo<-DB$cuadricula
    }
    if (substr(dns,1,4)=="Arsa") {
      DB$barco=ifelse(substr(DB$barco,1,3)=="COR","CDS",ifelse(DB$barco=="MOL","29MO"))
      DB$Gear="BAK"
      DB$GearExp=-9
      DB$DoorType=ifelse(substr(DB$barco,1,3)=="COR","W","P")
      if(quart) DB$quarter<-ifelse(substr(camp,1,1)=="1","1","4")
      DB$lance<-formatC(DB$lance,flag=0,width=2)
      ntalls$lance<-formatC(ntalls$lance,flag=0,width=2)
      DB$StNo=DB$lance
    }
    DB<-DB[,c("year","barco","quarter","Gear","malletas","GearExp","DoorType","lance","StNo")]
    ntalls<-ntalls[ntalls$lance %in% DB$lance,]
    ntalls<-subset(ntalls,grupo==1)
    ntalls$SubFact<-round(ntalls$peso_gr/ntalls$peso_m,4)
    dumb<-aggregate(numer~lance+esp+sexo+cate,data=ntalls,sum)
    dumb$NoMeas<-dumb$numer
    dumb<-dumb[,c("lance","esp","sexo","cate","NoMeas")]
    ntallsdumb<-merge(ntalls,dumb,all.x=TRUE)
    ntallsdumb$SpecCode<-as.character(especies$aphia[match(ntallsdumb$esp,especies$esp)])
    ntallsdumb$Specie<-as.character(especies$especie[match(ntallsdumb$esp,especies$esp)])
    ntallsdumb$med<-as.character(especies$med[match(ntallsdumb$esp,especies$esp)])
    ntallsdumb$incr<-as.character(especies$increm[match(ntallsdumb$esp,especies$esp)])
    ntallsdumb$LngtCode<-NA
    ntallsdumb$LngtCode[ntallsdumb$med==1]<-"1"
    ntallsdumb$LngtCode[ntallsdumb$med==2]<-"."
    ntallsdumb$LngtCode[ntallsdumb$incr==5]<-"0"
    DB1<-merge(ntallsdumb,DB,all.x=T)
    DB1$Sex<-as.character(factor(DB1$sexo,levels=as.character(1:3),labels=c("M","F","U")))
    if (inclSpecie==T) {
      HL_north<-data.table::data.table(RecordType="HL",Quarter=DB1$quarter,Country="SPA",Ship=DB1$barco,Gear=DB1$Gear,SweepLngt=DB1$malletas,GearExp=DB1$GearExp,DoorType=DB1$DoorType,StNo=DB1$StNo,HaulNo=DB1$lance,Year=DB1$year,SpecCodeType="W",SpecCode=DB1$SpecCode,specie=DB1$Specie,SpecVal=1,Sex=DB1$Sex,TotalNo=round(DB1$NoMeas*DB1$SubFact,2),CatIdentifier=DB1$cate,NoMeas=DB1$NoMeas,SubFact=DB1$SubFact,SubWgt=DB1$peso_m,CatCatchWgt=DB1$peso_gr,LngtCode=DB1$LngtCode,LngtClass=DB1$talla,HLNoAtLngt=DB1$numer)
      }
    else HL_north<-data.table::data.table(RecordType="HL",Quarter=DB1$quarter,Country="SPA",Ship=DB1$barco,Gear=DB1$Gear,SweepLngt=DB1$malletas,GearExp=DB1$GearExp,DoorType=DB1$DoorType,StNo=DB1$StNo,HaulNo=DB1$lance,Year=DB1$year,SpecCodeType="W",SpecCode=DB1$SpecCode,SpecVal=1,Sex=DB1$Sex,TotalNo=round(DB1$NoMeas*DB1$SubFact,2),CatIdentifier=DB1$cate,NoMeas=DB1$NoMeas,SubFact=DB1$SubFact,SubWgt=DB1$peso_m,CatCatchWgt=DB1$peso_gr,LngtCode=DB1$LngtCode,LngtClass=DB1$talla,HLNoAtLngt=DB1$numer)
        if (any(is.na(HL_north$SpecCode))) {warning("Algunas especies no tienen código AphiaID, conversión incompleta, revise especies.dbf")}
    HL_north[order(HL_north$HaulNo,HL_north$SpecCode,HL_north$LngtClass),]               
    }

