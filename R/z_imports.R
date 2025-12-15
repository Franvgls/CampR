#-- contenedor de entrada librería CampR general facilitar y asegurar la fluidez en el uso de paquetes y librerías

#' @keywords internal
"_PACKAGE"

# --- DATOS ---
#' @import data.table
#' @importFrom dplyr %>% mutate_if arrange filter mutate select rename group_by summarize relocate left_join
# --- CONEXIONES ---
#' @importFrom DBI dbConnect dbDisconnect dbGetQuery dbReadTable
#' @importFrom odbc odbc
#' @importFrom foreign read.dbf
# --- utilidades
#' @importFrom worrms wm_name2id
NULL
