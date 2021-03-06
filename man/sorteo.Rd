% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sorteo.R
\name{sorteo}
\alias{sorteo}
\title{Sorteo de distribución de lances en Porcupine}
\usage{
sorteo(buffer = 0.1177, srv.dsg = Porc.dsg)
}
\arguments{
\item{buffer}{Distancia entre cuadrículas contiguas que es posible elegir. El valor por defecto asegura que no se seleccionan dos cuadrículas contíguas}

\item{srv.dsg}{Diseño del muestreo. data.frame de dos columnas, strat, stations (estrato y número de lances)}
}
\value{
Devuelve un diseño de muestreo con los lances en cada sector con columnas x,y,pt (número del punto del grid de cuadriculas), strat (el estrato al que pertenece el lance)
}
\description{
Realiza el sorteo de las cuadriculas de muestreo en la campaña de Porcupine con el número correspondiente de estaciones por sector y estrato y aplicando una distancia mínima entre lances contiguos
}
\seealso{
{\link{mapsorteo}}, {\link{sacagrid}}
}
