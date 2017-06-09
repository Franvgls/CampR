#' Proporcion en peso de peces mayores de L50
#' 
#' Calcula la proporcion en peso de peces mayores de la talla de madurez (L50) en una campaña. Parece lógico calcularlo en peso puesto que entiendo que con este indicador se persigue seguir la evolución de la SSB, lógicamente en peso. Y si se hace en número el valor dependera mucho del reclutamientro anual, sobre todo en especies en las que este es muy importante en la epoca de la campaña, cuyo objetivo inicial no debemos de olvidar que es evaluar la fuerza del reclutamiento.
#' Dado que no hay datos de pesos individuales utiliza la regresion talla peso de la especie y convierte en peso la distribucion de tallas estratificada.
#' Si hay que sacarla por stock o subarea funciona el excl.sect =1 (8.c) =c(2:5) (9.a) (Para Demersales)
#' @param gr Grupo de la especie: 1 peces, 2 crustáceos 3 moluscos (4 equinodermos 5 invertebrados habitualmente no medidos)
#' @param esp Código de la especie seleccionada
#' @param camp Campaña de la que se extraen los datos: Demersales NYY, Porcupine PYY, Arsa primavera 1YY y Arsa otoño 2YY, Medits MYY
#' @param dns Elige el origen de las bases de datos: Porcupine "Pnew", Cantábrico "Cant, Golfo de Cádiz "Arsa", Medits: "Medi"
#' @param l50 Talla de primera madurez L50 para la especie en cuestion
#' @param b Indica si existe regresión talla peso y por tanto calcula el peso a partir de la regresión talla-peso de la especie en lugar del peso calculado.
#' @param excl.sect Excluye los sectores y estratos en cuestion, si NA usa toda el area.
#' @return Da resultados con el peso de mayores PesoG, el de menores PesoS y la proporción del de mayores Prop
#' @seealso {\link{talpes.camp}}
#' @examples propmat.camp(1,50,"N05",l50=37)
#' @export
propmat.camp<-function(gr=1,esp,camp,dns="Cant",l50=37,b=TRUE,excl.sect=NA) {
  if (length(camp)>1) {stop("seleccionadas más de una campaña, no se pueden sacar resultados de más de una")}
  if (length(esp)>1) {stop("seleccionadas más de una especie, este indicador es monoespec?fico")}
  esp<-format(esp,width=3,justify="r")
  dumb<-dattal.camp(gr,esp,camp,dns,sex=FALSE,excl.sect=excl.sect)
  if (b) {
    ab<-talpes.camp(gr,esp)
    dumb$peso<-c(dumb$numero*ab[1]*c(dumb$talla+.5)^ab[2])
   }
  else dumb$peso<-dumb$numero 
  outputL<-sum(dumb$peso[dumb$talla>=l50])
  outputS<-sum(dumb$peso[dumb$talla<l50])
  output<-outputL/c(outputL+outputS)
  c(PesoG=outputL,PesoS=outputS,Prop=output)
}