#' Da el percentil .95 de la distribucion de tallas estratificada
#' 
#' Funcion auxiliar indicador de la MSFD: Da el percentil perc (por defecto 95, en decimal) de la distribución de tallas de la especie en las campañas seleccionadas.  
#' @param gr Grupo de la especie: 1 peces, 2 crustáceos 3 moluscos (4 equinodermos 5 invertebrados habitualmente no medidos)
#' @param esp ha de ser código de la especie seleccionada
#' @param camps Campañas de las que se extraen los datos: un año comcreto (XX): Demersales "NXX", Porcupine "PXX", Arsa primavera "1XX" y Arsa otoño "2XX"
#' @param dns Elige el origen de las bases de datos: Porcupine "Pnew", Cantábrico "Cant, Golfo de Cádiz "Arsa" (únicamente para sacar datos al IBTS, no gráficos)
#' @param excl.sect Excluye el los sectores y estratos en cuestion, si NA usa toda el area.
#' @param perc El percentil de la distribucion de tallas a calcular, .95 es el valor utilizado habitualmente como indicador del maximo de la distribucione de tallas
#' @return Devuelve un data.frame con columnas camp y p95 tal con el Percentil 95 de la correspondiente campaña, una con el peso o número de cada especie del grupo solicitado, lat, long, prof
#' @examples p95tal.camp(1,50,Nsh[7:27],"Cant")
#' @export
p95tal.camp<-function(gr=1,esp,camps,dns="Cant",excl.sect=NA,perc=.95,cex.leg=1.1) {
  # Da el percentil perc (por defecto 95, en decimal) de la distribución de tallas de la especie en las campañas seleccionadas. 
  # Se ha eliminado el mult puesto que al revisar la forma de calcular el p95 ya no es necesario
  # Calcula la distribución de tallas estratificada de la especie sin sexos y de ah? saca el percentil 
  if (length(esp)>1) {stop("seleccionadas más de una especie, este indicador es monoespecífico")}
  esp<-format(esp,width=3,justify="r")
  dumb<-dattal.camp(gr,esp,camps[1],dns,excl.sect=excl.sect,sex=FALSE)
  output<-data.frame(camp=camps[1],p95tal=dumb[cumsum(dumb$numero)>sum(dumb$numero)*perc,"talla"][1])  
  if (length(camps)>1) {
    for (i in 2:length(camps)) {
      dumb<-dattal.camp(gr,esp,camps[i],dns,sex=FALSE)
      output<-rbind(output,data.frame(camp=camps[i],p95tal=dumb[cumsum(dumb$numero)>sum(dumb$numero)*perc,"talla"][1]))  
    }
  }
  output
}