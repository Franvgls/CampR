\name{datos.camp1}
\alias{datos.camp1}
\title{Datos de abundancia y biomasa de una especie}
\usage{
datos.camp1(gr, esp, camp, dns, cor.time = T, kg = T)
}
\arguments{
  \item{gr}{Grupo de la especie: 1 peces, 2 crustáceos 3
  moluscos 4 equinodermos 5 invertebrados}

  \item{esp}{Código de la especie numérico o carácter
  con tres espacios. 999 para todas las especies del grupo}

  \item{camp}{Campaña de la que se extraen los datos: un
  año comcreto (XX): Demersales "NXX", Porcupine "PXX",
  Arsa primavera "1XX" y Arsa otoño "2XX"}

  \item{dns}{Elige el origen de las bases de datos:
  Porcupine "Pnew", Cantábrico "Cant, Golfo de Cádiz
  "Arsa" (únicamente para sacar datos al IBTS, no
  gráficos)}

  \item{cor.time}{Si T corrige las abundancias en función
  de la duración del lance}

  \item{kg}{Si T datos en kgs, si F en gramos}
}
\value{
Devuelve un data.frame con los datos de la especie por
lance: sector,lance,peso,numero,arsect (área del sector al
que pertence el lance)
}
\description{
Función de acceso a datos: Extrae los datos de abundancia
y biomasa de una especie o conjunto de especies a partir de
las faunísticas de una campaña. Crea una tabla con
información del sector(número) y estrato (letra), lance,
peso, número y arsect Funciones para obtener data.frame de
especies concretas a partir de los ficheros del Camp
}

