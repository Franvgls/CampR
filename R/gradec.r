#'Transforma grados y minutos decimales a grados decimales
#'
#'Transforma grados y minutos decimales a grados decimales, avisa si el dato es incorrecto, es decir minutos más de 60
#'@param x grados en formato decimal
#'@examples camptoyear(Nsh)
#'@export
gradec<- function(x) {
  if (any(c(x-trunc(x))>.6)) stop("Los datos en grados y minutos decimales no pueden ser más de .60, revise datos")
  trunc(x)+(x-trunc(x))*100/60
}
