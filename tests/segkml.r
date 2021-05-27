# -*- encoding: utf-8 -*-

## --- funciones para generar KML puntos seguimiento ---
library(magest)

#' Mes-encuesta
#' @description Produce objeto Date corresp. primer día del mes que se
#'     hace la encuesta
#' @param año
#' @param mes
#' @examples
#' mes_seg(2021, 4)
mes_seg <- function(año, mes) as.Date(sprintf("%i-%02i-01", año, mes))

#' Sustituir
#' @description Aplica función remplazar y los datos NA que quedan
#'     los sustituye por arg. param. si_na si este no es cadena vacía
#' @param busca datos a buscar
#' @param buscaen donde buscar
#' @param remplazo remplazo de los encontrados
#' @examples
match_sustituir <- function(busca, buscaen, remplazo,
                            si_na = "") {
    w <- remplazar(NULL, busca, buscaen, remplazo, msg = FALSE)
    if (nzchar(si_na)) {
        w[is.na(w)] <- si_na
    }
    w
}

#' Sustituir-NA
#' @description Los elementos que son NA o cadenas vacías los
#'     sustituye por arg. al param. si_na
#' @param x vector character
#' @param si_na sustituto
sustituir_na <- function(x, si_na = "desconocido") {
    w[is.na(w) || !nzchar(w)] <- si_na
    w
}

#' Podar espacios
#' @description quita los espacios antes y después de una frase
#' @param x frase
podar_str <- function(x = character()){
    regmatches(x, regexpr("\\b.*\\b", x, perl = TRUE))
}

#' Sustituir espacios
#' @description Sustituye ristra de espacios por un espacio
#' @param x frase
dejar_un_espacio <- function(x = character()){
    podar_str(gsub("[[:space:]]+", " ", x))
}

nombre_propio <- function(x) {
    dejar_un_espacio(x) %>% a_propio()
}

#' la tabla de municipios-departamentos
muni <- magmun::municipios()

u <- iconv(muni$municipio, "UTF-8", "")
muni["municipio"] <- u
u <- iconv(muni$departamento, "UTF-8", "")
muni["departamento"] <- u

## -- constantes generales --
WD <- "c:/encuestas/ciclo2021/datos" # directorio datos R

FCH <- mes_seg(2021, 4)   # !!! mes encuesta
AME <- format(FCH, "%Y%m")# año-mes
MES <- format(FCH, "%m")  # mes

ADA <- format(FCH, "%b%Y") %>% # archivo de datos .rda
    sub("\\.", "", .) %>% paste0(".rda") %>%
    file.path(WD, .)

## archivo con las coordenadas de puntos y delegaciones
## y otros datos complementarios para construir los KML
## list_off(KML) para ver lo que hay
KML <- file.path(WD, "kml-data.rda")

## data.frame con los datos del directorio
## letra "d" seguida de mes expresado como dos dígitos
## p.ej. si datos de abril DFM igual a "d04"
DFM <- glue::glue("d{MES}")

## claves de control de cuestionario
CCC <- setNames(1:7, c("completo", "no-agrícola", "incompleto",
                       "no-informante", "rechazo", "no-acceso",
                       "pendiente"))

## <<- directorio del mes ->>
#' campos de la tabla de datos
#' !!! modificar si cambiaran
cam <-  c("quest", "c5000", "copiade", "nombreproductor", "c002",
          "c003", "telefono", "ciudadlocal", "ptoreferencia",
          "dir_productor", "c004", "c005", "name_expagrp",
          "ciudadfinca", "ptoreffinca", "dir_explagrop", "nombretec")

DBF <- "encuestas2020"       # base datos SQL
TAB <- "caracterizacion2021" # !!! tabla datos mes

z <- db_sql(server = "10.22.168.199",
            database = DBF) %>%
    get_data(xsql_s(TAB, cam)) %>%
    mutate(c5000    = cero_na(c5000),
           ccontrol = names(CCC)[c5000],
           copiade  = sub("([^[:digit:]]|\\b)+", "0", copiade),
           copiade  = type.convert(copiade, na.string = "0"),
           munprod  = magmun::concatenar_int(c002, c003),
           dptprod  = match_sustituir(munprod, muni$mun,
                                      muni$departamento),
           munprod  = match_sustituir(munprod, muni$mun,
                                      muni$municipio),
           munfinca = magmun::concatenar_int(c004, c005),
           dptfinca = match_sustituir(munfinca, muni$mun,
                                      muni$departamento),
           munfinca = match_sustituir(munfinca, muni$mun,
                                      muni$municipio)) %>%
    rename(localidad = "ciudadlocal",
           finca     = "name_expagrp",
           dirfinca  = "dir_explagrop") %>%
    select(-c5000)

#' campos adicionales al directorio
#' !!! modificar a conveniencia
cam <- c("quest", "c050") # giro encuesta caracterización
giro <- c("agrícola", "pecuario", "forestal", "inactiva")

y <- db_sql(server = "10.22.168.199",
            database = DBF) %>%
    get_data(xsql_s(TAB, cam)) %>%
    mutate(giro = sustituir(c050, 1:4, giro))

z %<>% left_join(y)

## guarda los datos en data.frame DFM y archivo ADA
meta(z) <- glue::glue("directorio-{AME}") # metadatos data.frame
save_df(z, DFM, file = ADA)

## <<- prepara los datos de los puntos ->>
z <- get_dff(DFM, ADA)
y <- get_dff(pdel, KML)

read_off(pun, file = KML)
x <- sf::st_drop_geometry(pun)

x["delegacion"] <- match_sustituir(x$punto, y$punto, y$delegacion)

y <- get_dff(tec, KML)
x["tecnico"] <- match_sustituir(z$nombretec, y$codtec, y$tecnico) %>%
    {match_sustituir(x$punto, z$quest, ., si_na = "desconocido")}

## si no se ha ubicado el punto
## departamento y municipio es al cual está asignado el punto
## en la distribución de la muestra
## si punto ubicado, departamento y municipio donde está la sede
x["departamento"] <- match_sustituir(x$mun, muni$muni,
                                     muni$departamento) %>%
    {remplazar(., x$punto, z$quest, z$dptfinca)}

x["municipio"] <- match_sustituir(x$mun, muni$muni,
                                  muni$municipio) %>%
    {remplazar(., x$punto, z$quest, z$munfinca)}

x["localidad"] <- nombre_propio(z$localidad) %>%
    {match_sustituir(x$punto, z$quest, ., si_na = "n.d")}

x["finca"] <- nombre_propio(z$finca) %>%
    {match_sustituir(x$punto, z$quest, ., si_na = "n.d")}

x["dirfinca"] <- nombre_propio(z$dirfinca) %>%
    {match_sustituir(x$punto, z$quest, ., si_na = "n.d")}

x["control"] <- match_sustituir(x$punto, z$quest, z$ccontrol,
                                si_na = "pendiente")

#' adicional: giro encuesta caracterización
x["giro"] <- match_sustituir(x$punto, z$quest, z$giro,
                             si_na = "n.d")

