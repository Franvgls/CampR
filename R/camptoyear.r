#'Transforma series de nombres de campaña en años
#'
#'Transforma series de nombres de campañas en formato Camp XYY a años
#'@param x Vector con la serie de nombres de campaña a transformar a añoa
#'@examples camptoyear(Nsh)
#'@export
camptoyear<- function(x) {
  x<-sub("X","",x)
  if (any(nchar(as.character(x))!=3)) stop("Los valores a transformar no responden a nombres de campaña formato Camp, revise la entrada")
  if (!suppressWarnings({is.na(as.numeric(substr(x,2,3)))})) {
    m<-as.numeric(substr(x,2,3))
    ifelse(m<70,2000+m,1900+m)
    }
  else 0
  }
