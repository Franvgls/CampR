% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/qcTalPez.camp.r
\name{qcTalPez.camp}
\alias{qcTalPez.camp}
\title{Comprueba que están medidas todas las especies en capturas y viceversa en la campaña. Comprueba concordancia entre números y pesos en los ficheros de tallas NTALLXXX.dbf y fauna FAUNAXXX.dbf}
\usage{
qcTalPez.camp(camp, dns, gr = 1)
}
\arguments{
\item{camp}{campaña a revisar los datos en formato Camp Xyy}

\item{dns}{Origen de bases de datos: "Cant" cantábrico, "Porc" o "Pnew" Porcupine}

\item{gr}{El grupo que se quiere comprobar 1 peces, 2 crustaceos, 3 cefalópodos. 4, 5 y 6 nunca se miden.}
}
\value{
Devuelve la lista de especies capturadas pero no medidas o viceversa
}
\description{
Sirve para control de calidad y asegurándose que no faltan datos o están incompletos (comprueba coherencia entre faunaXXX.dbf y NtallXXX.dbf y avisa cuando no son coherentes) Existe una funciòn escondida para procesar de una vez toda la campaña \code{qcTalPez.camp(camp=XXX,dns,gr=1)}, pero una vez encontrado que el lance en que está el error es mejor utilizar la función lance por lance
}
\examples{
qcTalPez.camp("N20",dns="Cant",gr=1)
}
\seealso{
Other Control de calidad: 
\code{\link{qcLW.camp}()},
\code{\link{qcLWbucl.camp}()},
\code{\link{qcTalPez.lan}()},
\code{\link{qcdistlan.camp}()},
\code{\link{qcdtall.camp}()},
\code{\link{qcdtallrev.camp}()},
\code{\link{qclandatr.camp}()}
}
\concept{Control de calidad}
