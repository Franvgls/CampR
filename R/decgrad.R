#'Transforma grados y minutos decimales a grados decimales
#'
#'Transforma grados y minutos decimales a grados decimales, avisa si el dato es incorrecto, es decir minutos más de 60
#'@param x vector con grados y minutos en formato decimal
#'@exampl.es decgrad(43.7923)
#'@family Conversion unidades
#'@export
decgrad<- function(x) {
  if (all(x<0)) x<-c(x*c(-1))
  if (all(c(x-trunc(x))<.6)) warning("Parece que todos los datos están en formato hexadecimal, comprueba")
  round(trunc(x)+(x-trunc(x))*60/100,4)
}
