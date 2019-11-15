#'Transforma series de nombres de campaña en años
#'
#'Transforma series de nombres de campañas en formato Camp XYY a años
#'@param x Vector con la serie de nombres de campaña a transformar a añoa
#'@examples camptoyear(Nsh)
#'@export
camptoyear<- function(x) {
  if (any(nchar(as.character(x))!=3)) stop("Los valores a transformar no son nombres de campaña formato Camp, revise la entrada")
  if (is.numeric(as.numeric(substr(x,2,3)))) {
  as.numeric(paste0(ifelse(as.numeric(substr(x,2,3)>50),19,20),substr(x,2,3)))}
  else 0
}
