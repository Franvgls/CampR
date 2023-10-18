---
title: "README.md"
author: "Fran Velasco"
date: "10 de enero de 2020"
output: html_document
---

# CampR

Libreria de funciones en R para el programa Camp. Última versión de Camp es Camp 12 para Windows 10 compilada en Harbour.
CAMP es una base de datos en clipper y DBase III para recoger y en parte procesar y elaborar datos recogidos en campañas científicas de evaluación de recursos demersales por arrastre de fondo. El programa CAMP se compila en Harbour para poder utilizarlo en Windows 10.
CampR surge como librería de R para procesar los resultados de las campañas almacenadas en las bases de datos de CAMP y explotar gráficamente esos datos.
Dado que se utiliza en R de 32bits requiere versiones particulares de algunas librerías
cli 2.5.0
data.table 1.14.8
dplyr 1.0.6
glue 1.4.2
lattice 0.20-38
magrittr 2.0.1
pillar 1.6.4
rlang 0.4.11
tibble 3.1.1
vctrs 0.3.8
Estas se pueden cargar con remotes::install_version(....) 
