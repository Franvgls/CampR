#' Comprueba la distancia recorrida en los lances y la consistencia con recorrido por camp
#' 
#' Sirve para control de calidad y asegurarse que los datos de distancias y posiciones son correctos
#' @param camp campaña a revisar los datos en formato Camp Xyy
#' @param dns Origen de bases de datos: Cant cantábrico, Pnew Porcupine
#' @param todos Por defecto F. Si T lista todos los lances con valores, si no sólo los que pc.error>error
#' @param pc.error porcentaje de error aceptable para no mostrar los lances erróneos
#' @return Devuelve un data.frame con lance, recorrido según camp, recorrido haversine y % de error
#' @examples qcdistlan.camp("C14","Cant",pc.error=.01)
#' @references gcd.hf function gives the haversine calculation of distance between two geographic points (see: Pineda-Krch, M. 2010. http://www.r-bloggers.com/great-circle-distance-calculations-in-r/
#' @family Control de calidad
#' @export
qcdistlan.camp<-function(camp,dns="Cant",todos=F,pc.error=2) {
  gcd.hf <- function(long1, lat1, long2, lat2) {
    long1<-abs(long1)*pi/180
    long2<-abs(long2)*pi/180
    lat1<-lat1*pi/180
    lat2<-lat2*pi/180
    R <- 6371 # Earth mean radius [km]
    delta.long <- (long2 - long1)
    delta.lat <- (lat2 - lat1)
    a <- sin(delta.lat/2)^2 + cos(lat1) * cos(lat2) * sin(delta.long/2)^2
    c <- 2 * asin(min(1,sqrt(a)))
    d = R * c
    return(d) # Distance in kmeans
    }
  dumblan<-datlan.camp(camp,dns,redux=F)
  dumblan$rec.vel<-(dumblan$weight.time*30/60)*dumblan$velocidad*1852
  dumblan$dist.hf<-NA
  for (i in 1:nrow(dumblan)) {
    dumblan$dist.hf[i]<-round(gcd.hf(dumblan$longitud_l[i],dumblan$latitud_l[i],dumblan$longitud_v[i],dumblan$latitud_v[i]),4)*1000
    }
  dumblan$error.vel<-round((dumblan$rec.vel-dumblan$recorrido)*100/dumblan$rec.vel,2)
  dumblan$error.dist<-round((dumblan$dist.hf-dumblan$recorrido)*100/dumblan$dist.hf,2)
  dumblan$error.disc<-round((dumblan$rec.vel-dumblan$dist.hf)*100/dumblan$rec.vel,2)
  if (todos) print(dumblan[,c("lance","recorrido","dist.hf","error.dist","rec.vel","error.vel","error.disc")])
  dumblan[abs(dumblan$error.dist)>pc.error | abs(dumblan$error.vel)>pc.error*2 | abs(dumblan$error.disc)>pc.error*2,
          c("lance","recorrido","dist.hf","error.dist","rec.vel","error.vel","error.disc")]
  }

