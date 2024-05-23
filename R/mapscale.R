#' Auxiliar para incluir escala en un mapa, tomada de paquete maps pero con opción para elejir español (es=T) o inglés (es=F)
#'
#' Función auxiliar para los mapas
#' @param x Define coordinada horizontal de inserción de la escala
#' @param y Define coordinada horizontal de inserción de la escala
#' @param relwidth proporción horizontal de visualización que se utilizará para la escala. El valor predeterminado es 0,15 (15%).
#' @param metric Si T la unidad será en km, si falso en millas
#' @param es Si es TRUE texto de la escala en español, si FALSE (por defecto) en inglés
#' @param ... otros parámetros graficos
#' @return Incluye la escala en el mapa
#' @references maps::map.scale function pone una escala en un mapa, en esta función se ha añadido la opción de idioma \code{\link[maps]{map.scale}}#' @seealso {\link{MapNort}}, {\link{MapArsa}}
#' @examples MapArsa(escala=TRUE,cex.scala=.9,es=TRUE)
#' @examples MapNort(escala=T,cex.scala=.6,es=T)
#' @family mapas base
mapscale<-function (x, y, relwidth = 0.15, metric = TRUE, ratio = TRUE,es=F,...)
{
  format.pretty <- function(x, digits = 2) {
    x = signif(x, 2)
    prettyNum(formatC(x, format = "fg", digits = digits),
              big.mark = ",")
  }
  usr <- par("usr")
  if (missing(y))
    y <- (9 * usr[3] + usr[4])/10
  if (abs(y) >= 90)
    warning("location of scale out of this world!")
  if (missing(x))
    x <- (9 * usr[1] + usr[2])/10
  cosy <- cos((2 * pi * y)/360)
  perdeg <- (2 * pi * (6356.78 + 21.38 * cosy) * cosy)/360
  scale <- (perdeg * 100000)/(2.54 * (par("pin")/diff(par("usr"))[-2])[1])
  if (metric)
    unit <- "km"
  else {
    perdeg <- perdeg * 0.6213712
    unit <- "mi"
  }
  len <- perdeg * relwidth * (usr[2] - usr[1])
  ats <- pretty(c(0, len), n = 2)
  nats <- length(ats)
  labs <- as.character(ats)
  labs[nats] <- paste(labs[nats], unit)
  linexy <- matrix(NA, ncol = 2, nrow = 3 * nats)
  colnames(linexy) <- c("x", "y")
  cxy <- par("cxy")
  dy <- cxy[2] * par("tcl")
  dx <- ats[nats]/perdeg/(nats - 1)
  linexy[1, ] <- c(x, y)
  linexy[2, ] <- c(x, y + dy)
  for (i in 1:(nats - 1)) {
    linexy[3 * i, ] <- c(x + (i - 1) * dx, y)
    linexy[3 * i + 1, ] <- c(x + i * dx, y)
    linexy[3 * i + 2, ] <- c(x + i * dx, y + dy)
  }
  lines(linexy)
  text(x + ats/perdeg, y + dy - 0.5 * cxy[2], labs, adj = c(0.4,
                                                            0.5), ...)
  if (ratio)
    text(x, y + 0.5 * cxy[2], paste(ifelse(es,"escala aprox 1:","scale approx 1:"),
                                    format.pretty(scale), sep = ""), adj = 0, ...)
  invisible(scale)
}
