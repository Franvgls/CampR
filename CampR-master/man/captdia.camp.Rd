% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/captdia.camp.R
\name{captdia.camp}
\alias{captdia.camp}
\title{Capturas del día para las especies peces}
\usage{
captdia.camp(camp = "C14", dns = "Cant", dias, peces = c(50, 42, 43,
  44, 45, 50, 51, 80, 90, 60, 99), crust = c(19, 29))
}
\arguments{
\item{camp}{Es la campaña de la que queremos tomar los datos}

\item{dns}{Origen de Base de Datos seleccionado. Se usa sobretodo para "Porc" Porcupine pero ver "Cant", "Arsa", "Medi"}

\item{dias}{Los días del mes en que queremos tomar los datos, pueden pedirse varios juntos. No acepta mezclar meses distintos}

\item{peces}{Las especies de peces que se quieren incluir en los datos}

\item{crust}{Las especies de crustáceos que se incluir en los datos}
}
\value{
Devuelve la fecha de la que se toman los datos, un listado de lances contemplados y un data.frame con variables Especie,captura (kgs) ordenados por abundancia en las capturas
}
\description{
Da un resumen de las especies capturadas en el día solicitado (o los días solicitados). Sirve para dar los resúmenes de capturas que piden en el "Vizconde de Eza" y otros barcos peces y crust permiten seleccionar las especies que a extraer
}
