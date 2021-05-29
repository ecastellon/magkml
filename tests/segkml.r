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

#' Podar espacios
#' @description quita los espacios antes y después de una frase
#' @param x frase
podar_str <- function(x = character()) {
    r <- regexpr("\\b.*\\b", x, perl = TRUE)
    w <- vector("character", length = length(x))
    w[r > 0] <- regmatches(x, r)
    w
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

#' href delegación
#' @description Construye la hyper referencia a las delegaciones
#' @details Utiliza la abreviatura del departamento registrada en la
#'     base de datos de departamentos
#' @param x character: nombre delegación (departamento)
#' @param sufijo character: sufijo específico de KML
#' @examples heref_deleg("Boaco") #-> "#BO;balloon"
href_delegacion <- function(x, sufijo = "") {
    mm <- match(x, dpto$departamento)
    paste0("#", dpto$abr[mm], sufijo, x)
}

#' Descripción del avance
#' @description Devuelve el nodo description que muestra el avance
#' @details De los datos selecciona el nivel (grupo) al cual se han
#'     resumido los datos, calcula el porcentaje de avance general y
#'     sustituye en la plantilla html
#' @param w data.frame con los datos resumen
#' @param html la plantilla html de la descripción
#' @param grupo el nivel del resumen (delegación, municipio)
#' @examples
#' descripcion_avance(w, "html_nac", "delegación")
descripcion_avance <- function(w, html, grupo) {
    
    pmt_gauge <- magest::pct(sum(w$asignados - w$pendiente),
                             sum(w$asignados)) %>%
        paste0(., ",'", ., "%'")

    pmt_tabla <- select(w, grupo, asignados, levantados, pendiente) %>%
        jsonlite::toJSON() %>%
        as.character()

    fecha_repo <- DIA
    grupo <- paste0("'", grupo, "'")

    ht <- get_off_c(html, file = KML) %>%
        glue::glue(.open = "\\{", .close = "\\}")

    node_description(ht, cdata = TRUE)
}

## <<- generales ->>

#' la tabla de municipios-departamentos
muni <- magmun::municipios()

u <- iconv(muni$municipio, "UTF-8", "")
muni["municipio"] <- u
u <- iconv(muni$departamento, "UTF-8", "")
muni["departamento"] <- u

dpto <- magmun::qry_dm("select * from departamento",
                       Sys.getenv("DBDEPMUN"))
u <- iconv(dpto$departamento, "UTF-8", "")
dpto["departamento"] <- u

## -- constantes --
WD <- "c:/encuestas/ciclo2021/datos" # directorio datos R

HOY <- Sys.time()
DIA <- format(HOY, "%d de %B del %Y") # !!! fecha reporte

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
    mutate(giro = match_sustituir(c050, 1:4, giro))

z %<>% left_join(y)

## guarda los datos en data.frame DFM y archivo ADA
meta(z) <- glue::glue("directorio-{AME}") # metadatos data.frame
save_df(z, DFM, file = ADA)

## <<- prepara los datos de los puntos ->>
z <- get_dff(DFM, ADA)

## !! sustituto
giro <- c("agrícola", "pecuario", "forestal", "inactiva")
z <- get_dff(d04, "c:/encuestas/ciclo2021/datos/abr2021.rda") %>%
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
                                      muni$municipio),
           giro     = match_sustituir(c050, 1:4, giro)) %>%
    rename(localidad = "ciudadlocal",
           finca     = "name_expagrp",
           dirfinca  = "dir_explagrop") %>%
    select(-c5000)
##

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
## si error digitación, dep. y mun. sustituido por el cual asignado
x["departamento"] <- match_sustituir(x$mun, muni$mun,
                                     muni$departamento) %>%
    {remplazar(., x$punto, z$quest, z$dptfinca)}

x["municipio"] <- match_sustituir(x$mun, muni$mun,
                                  muni$municipio) %>%
    {remplazar(., x$punto, z$quest, z$munfinca)}

#' !!! hay errores digitación
if (anyNA(x$municipio)) {
    ii <- is.na(x$municipio)
    mm <- match(x$mun, muni$mun)
    x[ii, "municipio"] <- (muni$municipio[mm])[ii]
    x[ii, "departamento"] <- (muni$departamento[mm])[ii]
}

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

## <<- construir nodos KML ->>

## -- resumen de avance --
## folder cuyos placemark son las delegaciones
## más el placemark "nacional"

## la tabla de avance en los nodos delegación puede ser por municipio
## o por técnico. (Falta ver cómo se puede filtrar en DataTables para
## tener una sola tabla con filtro municipio/técnico)
w <- group_by(x, delegacion, municipio) %>%
    summarise(asignados     = n(),
              pendiente     = sum(control == "pendiente"),
              levantados    = asignados - pendiente,
              avance        = magest::pct(levantados, asignados),
              completo      = sum(control == "completo"),
              incompleto    = sum(control == "incompleto"),
              noagricola    = sum(control == "no-agrícola"),
              rechazo       = sum(control == "rechazo"),
              noinformante  = sum(control == "no-informante"),
              inaccesible   = sum(control == "inaccesible")) %>%
    ungroup() %>%
    rename(grupo = municipio)

nd <- split(w, w$delegacion) %>%
    lapply(descripcion_avance, html = "html_del", grupo = "municipio")

y <- get_off("coor_del", file = KML)
mm <- match(names(d), y$departamento)
y <- y[mm,]

z <- match_sustituir(names(d), dpto$departamento, dpto$abr)

ww <- data.frame(id          = z,
                 name        = names(d),
                 Snippet     = y$ciudad,
                 styleUrl    = "#sty-del",
                 coordinates = I(coord_lista(y)),
                 description = I(nd))

pd <- purrr::pmap(ww, node_placemark)

#' nacional
w <- group_by(x, delegacion) %>%
    summarise(asignados     = n(),
              pendiente     = sum(control == "pendiente"),
              levantados    = asignados - pendiente,
              avance        = magest::pct(levantados, asignados),
              completo      = sum(control == "completo"),
              incompleto    = sum(control == "incompleto"),
              noagricola    = sum(control == "no-agrícola"),
              rechazo       = sum(control == "rechazo"),
              noinformante  = sum(control == "no-informante"),
              inaccesible   = sum(control == "inaccesible")) %>%
    ungroup() %>%
    rename(grupo = delegacion)

#' para crear h-referencias a los nodos delegaciones
#' y desplegar el correspondiente cuadro de información
#' modificar construcción DataTables del html para ajustar
#' la substring a la longitud de href según el sufijo
w["grupo"] <- href_delegacion(w$grupo, sufijo = ";balloon")

nd <- descripcion_avance(w, "html_nac", "delegación")
pn <- node_placemark(name = "Nacional", styleUrl = "#sty-nac",
                     Snippet = "Resumen nacional",
                     description = nd,
                     coordinates = list(x = -85.00, y = 13.00),
                     id = "NI")

fn <- node_element("Folder")
xml_add_child(fn, node_element("name", "Nacional"))
xml_add_child(fn, node_element("open", 1L))
xml_add_child(fn, node_element("visibility", 1L))

for (np in pd) {
    xml_add_child(fn, np)
}

xml_add_child(fn, pn)

## este estilo pasarlo al folder Document
sn <- sty_sty(id = "sty-nac",
              icon = sty_ico(url = url_google_ico("shapes",
                                                  "ranger_station"),
                             pos = list(x = 0.5, y = 0.5,
                                        xunits = "fraction",
                                        yunits = "fraction"),
                             escala = 1.0,
                             color = "ffff901e"),
              label = sty_lab(escala = 0.8,
                              color = "FF61AEFD"))

ed <- sty_sty(id = "sty-del",
              icon = sty_ico(url = url_google_ico("pal3",
                                                  "icon31"),
                             pos = list(x = 0.5, y = 0.5,
                                        xunits = "fraction",
                                        yunits = "fraction"),
                             escala = 1.0,
                             color = "ff0000ff"),
              label = sty_lab(escala = 0.8,
                              color = "FF61AEFD"))

km <- kml_doc(visibility = 0,
              estilos = list(sn, ed),
              folders = list(fn))

write_xml(km, "c:/eddy/code/web/sisea/ava-del.kml", encoding = "UTF-8")
