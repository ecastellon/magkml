# -*- encoding: utf-8 -*-

## especificaciones
## - Document
##   name: nombre de encuesta; e.g. "Seguimiento, marzo"
##   Snippet: fecha reporte; e.g. "15 de marzo"
##   visibility: 0
##   open: 0
## - Folder delegaciones
##   name: "delegaciones"
##   Snippet: nada
##   open: 0
##   visibility: 0
##   - Placemark
##     - ExtendedData
##       - Data
##         puntos asignados
##         puntos visitados
##         porcentaje visitados
##         técnico: puntos visitados
##     - description
##       - CDATA
##         los datos en nodo Data
##         gráfica de código de control
##         gráfica maptree(?)
##         foto tropa
## - Folder departamento
##   name: nombre departamento
##   Snippet: nada
##   open: 0
##   visibility: 0
## - Folder municipio
##   name: nombre municipio
##   Snippet: nada
##   open: 0
##   visibility: 0
##   - Placemark
##     name: punto
##     Snippet: nombre del técnico
##     visibility: 1
##     open: 1
##     - ExtendedData
##       - Data
##         técnico
##         departamento
##         municipio
##         localidad
##         nombre finca
##         dirección finca
##     - description
##       - CDATA
##         los datos en nodos Data
##         foto
##     - Point
##       - coordinates

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

#' Municipios-utf-8
#' @description Produce data.frame de municipios en utf-8
#'
df_municipios <- function() {
    muni <- magmun::municipios()

    u <- iconv(muni$municipio, "UTF-8", "")
    muni["municipio"] <- u
    
    u <- iconv(muni$departamento, "UTF-8", "")
    muni["departamento"] <- u
    
    muni
}

#' Departamentos
#' @description data.frame de departamentos y su abreviatura
#' @return data.frame
df_departamentos <- function() {
    dpto <- magmun::qry_dm("select * from departamento",
                           Sys.getenv("DBDEPMUN"))
    u <- iconv(dpto$departamento, "UTF-8", "")
    dpto["departamento"] <- u

    dpto
}

#' Directorio
#' @description Datos actualizados del directorio y control de
#'     cuestionario
#' @param cam character: los campos que conforman directorio
#' @return data.frame
datos_directorio <- function(cam) {
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
               dirfinca  = "dir_explagrop")

    invisible(z)
}

#' Complemento
#' @description Datos complementarios a los de directorio
#' @return data.frame
datos_complementarios <- function() {
    giro <- c("agrícola", "pecuario", "forestal", "inactiva")
    cam <- c("quest", "c050") # giro encuesta caracterización
    
    z <- db_sql(server = "10.22.168.199",
                database = DBF) %>%
        get_data(xsql_s(TAB, cam)) %>%
        mutate(giro = match_sustituir(c050, 1:4, giro)) %>%
        select(-c050)

    invisible(z)
}

#' Datos georeferenciados
#' @description Devuelve data.frame con todos los puntos. Los que ya
#'     fueron ubicados y levantado datos traen la información que les
#'     corresponde
#' @param z data.frame: datos leídos de la base de datos
#' @param x data.fram : los datos de los puntos
datos_puntos <- function(z, x) {

    y <- get_dff(pdel, KML) # asignados por delegación para levantar
    x["delegacion"] <- match_sustituir(x$punto, y$punto, y$delegacion)

    y <- get_dff(tec, KML)  # !!! asignación por técnico
    x["tecnico"] <- match_sustituir(z$nombretec, y$codtec,
                                    y$tecnico) %>%
        {match_sustituir(x$punto, z$quest, ., si_na = "desconocido")}

    #' departamento y municipio según ubicación de la sede
    #' puede no coincidir con los donde fue asignado el punto
    #' durante selección de la muestra

    ## si no se ha ubicado el punto
    ## departamento y municipio asignado en la muestra
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

    invisible(x)
}

#' dpto es data.frame departamentos
#' @description Nodos placemar delegaciones
#' @details
#' @param x data.frame: Datos de los puntos para calcular avance
#' @param ht character: plantilla html ("html_del" o "html_del2")
#' @param por_mun logical: resumen por municipio? (TRUE por omisión)
#' @return list
nodos_delegaciones <- function(x, ht = "html_del", por_mun = TRUE) {
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
        lapply(descripcion_avance, html = ht, grupo = "municipio")

    y <- get_off("coor_del", file = KML)
    mm <- match(names(nd), y$departamento)
    y <- y[mm,]

    xy <- coord_lista(y)
    nt <- lapply(xy, node_look, tlt = 13L)

    ab <- match_sustituir(names(nd), dpto$departamento, dpto$abr)

    ww <- data.frame(id          = ab,
                     name        = names(nd),
                     Snippet     = y$ciudad,
                     styleUrl    = "#sty-del",
                     coordinates = I(xy),
                     LookAt      = I(nt),
                     description = I(nd))

    #' nodos placemark delegaciones
    pd <- purrr::pmap(ww, node_placemark)
    names(pd) <- ab
    
    invisible(pd)
}

#' Placemark nacional
#' @description Placemark del nodo resumen nacional
#' @details
#' @param x data.frame: datos del avance
nodo_nacional <- function(x) {
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

    pn <- node_placemark(name = "Nacional", styleUrl = "#sty-nac",
                         Snippet = "Resumen nacional",
                         description = descripcion_avance(w, "html_nac",
                                                          "delegación"),
                         LookAt = node_look(list(x = -85.00, y = 13.00),
                                            tlt = 13L),
                         coordinates = list(x = -85.00, y = 13.00),
                         id = "NI")
    invisible(pn)
}

#' Color puntos
#' @description Los colores asignados al control de cuestionario
#' @param id_color_p character: id asignados a los colores
#' @return character
color_control_cuestionario <- function(id_color_p) {
    RColorBrewer::brewer.pal(10, "RdYlGn") %>%
        extract(c(9, 1, 8, 4, 3, 2)) %>%
        vapply(BGR, "") %>%
        c("FFFFFF") %>%     #"pendiente": color blanco
        paste0("FF", .) %>%
        set_names(id_color_p)
}

#' Estilos-puntos
#' @description Estilos de los puntos según control de cuestionario
#' @param id_color_p character: id de los colores
#' @return list
estilos_puntos <- function(ico = "raqueta", colico = "blanco",
                           id_color_p = character()) {

    if (!filled_char(id_color_p)) {
        id_color_p <- c("comp", "noag", "inco", "noen", "rech",
                        "inac", "pend")
    }

    col <- color_control_cuestionario(id_color_p)

    ## estilo labels
    sb <- sty_lab(escala = 0.8,
                  color = "FF61AEFD")

    es <- map2(col, names(col),
               function(x, y, z) {
                   sty_sty(id = y,
                           icon = sty_ico(ico = ico,
                                          col = colico,
                                          escala = 1.0,
                                          color = x),
                           label = z)
               }, z = sb)
    
    invisible(es)
}

#' Estilos-resumen
#' @description Estilos del KML resumen de avance delegaciones y
#'     nacional
#' @return list
estilos_resumen <- function() {

    es <- list(nac = sty_sty(id = "sty-nac",
                             icon = sty_ico(url = url_google_ico("shapes",
                                                                 "ranger_station"),
                                            pos = list(x = 0.5, y = 0.5,
                                                       xunits = "fraction",
                                                       yunits = "fraction"),
                                            escala = 1.0,
                                            color = "ff0000ff"),
                             label = sty_lab(escala = 0.8,
                                             color = "FF61AEFD"),
                             ball  = sty_bal(texto = "$[description]")),

               del = sty_sty(id = "sty-del",
                             icon = sty_ico(url = url_google_ico("pal3",
                                                                 "icon31"),
                                            pos = list(x = 0.5, y = 0.5,
                                                       xunits = "fraction",
                                                       yunits = "fraction"),
                                            escala = 1.0,
                                            color = "ff0000ff"),
                             label = sty_lab(escala = 0.8,
                                             color = "FF61AEFD"),
                             ball  = sty_bal(texto = "$[description]"))
               )
    
    invisible(es)
}

#' Estilos puntos y delegación
#' @description Los estilos de la delegación y los de los puntos
#' @return list
estilos_puntos_dele <- function() {
    er <- estilos_resumen()
    ep <- estilos_puntos()
    c(er["del"], ep)
}

#' KML-nacional
#' @description
#' @param pd lista: nodos Placemark delegaciones
#' @param pn xml_node: nodo Placemark nacional
#' @param es lista: estilos de delegación, nacional
#' @param file character: archivo de salida
kml_resumen_nac <- function(pd, pn, es, file = "") {
    
    fn <- node_element("Folder")
    xml_add_child(fn, node_element("name", "Nacional"))
    xml_add_child(fn, node_element("open", 1L))
    xml_add_child(fn, node_element("visibility", 1L))

    ## agrega placemark delegaciones
    for (np in pd) {
        xml_add_child(fn, np)
    }

    ## y nacional
    xml_add_child(fn, pn)

    km <- kml_doc(visibility = 0,
                  estilos = es,
                  folders = list(fn))

    if (nzchar(file)) {
        write_xml(km, file, encoding = "UTF-8")
    }
    
    invisible(km)
}

## -- descartar --
#' Folder-data
#' @description Nodo folder con ExtendedData
#' @details El data.frame de datos debe traer la columna 'punto' (id
#'     del punto) y su 'estilo', más los que se van a incluir en nodos
#'     ExtendedData para desplegar en el "balloon". La lista 'desc' es
#'     lista de los nodos 'description' (opcional) y la lista 'look'
#'     de los nodos 'LookAt' (opcional). La lista 'display' tiene como
#'     nombres los nombres de las variables (atributo 'name' de los
#'     nodos Data) y el valor es el título con el que se va a
#'     desplegar en el "balloon". El data.frame de coordenadas debe
#'     estar "alineado" al de los datos.
#' @param w character: nombre del folder
#' @param x data.frame con los datos
#' @param y data.frame con las coordenadas de los puntos
#' @param display lista: título para desplegar las variables
#'     (displayName)
#' @param open integer: al inicio mostrar abierto (1), cerrado (0) el
#'     folder
#' @param vis integer: folder visible (1) o cerrado (0)
#' @param look lista: nodos LookAt (opcional)
#' @param desc lista: nodos description (opcional)
#' @return xml_node
folder_ext_data <- function(w = character(), x, y, display,
                            open = 0L, vis = 0L, look = NULL,
                            desc = NULL) {

    ## arregla datos para procesar el grupo
    z <- data.frame(name         = x$punto,
                    visibility   = 1L,
                    styleUrl     = x$estilo,
                    coordinates  = I(coord_lista(y)),
                    ExtendedData = I(datos_lista(x)))
    
    if (!is.null(look)) {
        z["LookAt"] <- I(look)
    }

    if(!is.null(desc)) {
        z["description"] <- I(desc)
    }
    
    nf <- node_folder(name = w, data_pm = z, open = open,
                      visibility = vis, displayName = display)

    invisible(nf)
}

#' Folder-puntos
#' @description
#' @details Los puntos con los datos asociados presentados en un
#'     "balloon" simple que despliega los datos tomados de nodos
#'     ExtendedData.
#' @param x data.frame: datos de los puntos
#' @param name character: nombre del folder (municipio)
#' @param disp lista: los títulos para el "balloon" (displayName)
folder_puntos <- function(x, name, disp) {
    z <- purrr::pmap(x, node_placemark, displayName = disp)
    nf <- node_folder(children = z, name = name, visibility = 0L)
    invisible(nf)
}

#' Folder-delegación
#' @description El folder de la delegación con los folderes hijos de
#'     municipio.
#' @details El folder tiene un placemark (la delegación) con un
#'     resumen del avance por municipio (o técnico), más folderes con
#'     los puntos de cada municipio. (explicar "balloon" y displayName
#'     ...)
#' @param x data.frame con los datos preparados para llamar la función
#'     node_placemark
#' @param w character: nombre de la delegación
#' @param pm_del xml_node: nodo Placemark correspondiente a delegación
#' @param file character: file.path de salida
#' @param display lista: los títulos que aparecerán en el "balloon"
#' @param estilos lista: los estilos generales
#' @param ... adicionales para folder
folder_delegacion <- function(x, w, pm_del, file = "", display,
                              estilos, ...) {

    fd <- node_folder(list(pm_del), w, ...)
    
    ## folder por municipio
    z <- split(x, x$municipio)

    ## folders puntos de municipios
    y <- purrr::imap(z, folder_puntos, disp = display)

    ## y los agrega al folder
    for (m in y) xml_add_child(fd, m)

    ## hace documento si hace falta
    if (filled_char(file) && nzchar(file)) {
        km <- kml_doc(visibility = 0,
                      estilos = estilos,
                      folders = list(fd))

        write_xml(km, file, encoding = "UTF-8")
    }
    
    invisible(fd)
}

## <<- generales ->>

## -- constantes --
WD <- "c:/encuestas/ciclo2021/datos" # directorio datos R
WK <- "c:/eddy/code/web/sisea"       # directorio archivos KML

## archivo con las coordenadas de puntos y delegaciones
## y otros datos complementarios para construir los KML
## list_off(KML) para ver lo que hay
KML <- file.path(WD, "kml-data.rda")

HOY <- Sys.time()
DIA <- format(HOY, "%d de %B del %Y") # !!! fecha reporte

FCH <- mes_seg(2021, 4)   # !!! mes encuesta
AME <- format(FCH, "%Y%m")# año-mes
MES <- format(FCH, "%m")  # mes

#' data.frame y archivo donde se guardará el resultado de consultas

## data.frame con los datos del directorio
## letra "d" seguida de mes expresado como dos dígitos
## p.ej. si datos de abril DFM igual a "d04"
DFM <- glue::glue("d{MES}")

## archivo con resultados: iniciales del mes más año
## p.ej. para abril 2021, el archivo será abr2021.rda
ADA <- format(FCH, "%b%Y") %>% # archivo de datos .rda
    sub("\\.", "", .) %>% paste0(".rda") %>%
    file.path(WD, .)

#' constantes punto de vista visualización del punto
#' opciones por default función node_look
ALT <- 500             # altitude
RNG <- 600000          # range
TLT <- 27              # tilt
ALM <- "clampToGround" # altitudeMode
HDN <- 0               # heading

#' la tabla de municipios-departamentos
muni <- df_municipios()
dpto <- df_departamentos()

## <<- directorio del mes ->>

#' directorio según avance: nombres productor, finca, etc.
#' más los controles de cuestionario: rechazos, pendientes, etc.

DBF <- "encuestas2020"       # base datos SQL
TAB <- "caracterizacion2021" # !!! tabla datos mes

CCC <- setNames(1:7, c("completo", "no-agrícola", "incompleto",
                       "no-informante", "rechazo", "no-acceso",
                       "pendiente"))
