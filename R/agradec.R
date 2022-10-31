#'Transforma grados decimales a grados y minutos decimales
#'
#'Transforma grados decimales a grados y minutos decimales en formato XXºxxxx' donde el valor tras el decimal son los minutos decimales pero sin decimal
#'@param x vector con grados y minutos en formato decimal
#'@examples agradec(43.968)
#'@family Conversión unidades
#'@export
agradec<- function(x) {
  #if (any(c(x-trunc(x))>.6)) stop("Los datos en grados y minutos decimales no pueden ser más de .60, revise datos")
  trunc(x)+(x-trunc(x))*.60
}
