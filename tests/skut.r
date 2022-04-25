# -*-encoding: utf-8 -*-
library(glue)
library(magest)
library(magmun)
library(magkml)
library(sf)

## funciones de ayuda KML seguimiento
#' Mes-encuesta
#' @description Produce objeto Date corresp. primer día del mes
#' @param año integer
#' @param mes integer
#' @examples
#' primer_dia_mes(2021, 4)
primer_dia_mes <- function(año, mes) {
    as.Date(sprintf("%i-%02i-01", año, mes))
}

## -- helper functions KML --

#' href delegación
#' @description Construye la hiper referencia a las delegaciones
#' @details Utiliza la abreviatura del departamento registrada en la
#'     base de datos de departamentos
#' @param x character: nombre delegación (departamento)
#' @param sufijo character: sufijo específico de KML
#' @examples
#' href_deleg("Boaco", ";balloon") #-> "#BO;balloon"
href_delegacion <- function(x, sufijo = "") {
    dp <- departamentos()
    mm <- match(x, dp$departamento)
    paste0("#", dp$abr[mm], sufijo, x)
}

#' href puntos
#' @description Retorna las hyperreferencia de los estilos de los
#'     puntos
#' @param p data.frame: los datos de los puntos
#' @param d data.frame: los datos de la encuesta
#' @return character
#' @examples
#' href_puntos() #->#com si cuest. completo
href_puntos <- function(p, d) {
    hr <- iniciar_vec(p$punto, d$punto, ACC[d$c5000],
                      si_na = ACC["pendiente"])

    if ( is.element("especial", names(p)) ) {
        hr[p$especial] <- paste0("e", hr[p$especial])
    }
    hr <- paste0("#", hr)
    invisible(hr)
}

#' Estilo-puntos-color
#' @description Los colores asignados al control de cuestionario
#' @param id_color_p character: id asignados a los colores
#' @return character
color_control_cuestionario <- function(id_color_p = character()) {
    if (!length(id_color_p)) {
        id_color_p <- ACC
    }

    RColorBrewer::brewer.pal(10, "RdYlGn") %>%
        extract(c(9, 1, 8, 4, 3, 2)) %>%
        vapply(BGR, "") %>%
        c("FFFFFF") %>% # "pendiente": color blanco
        paste0("FF", .) %>%
        set_names(id_color_p)
}

#' Estilos íconos colores
#' @description Lista de IconStyle con diferentes nodo "color"
#' @details (colores dependientes de control de cuestionario)
#' @param url character: url del ícono
#' @param colores character: colores asociados con los estilos
#' @param ... parámetros adicionales sty_ico (pos, escala)
#' @seealso color_control_cuestionario, sty_ico
#' @return lista xml_node
sty_ico_colores <- function(url, colores, ...) {
    ## chk. parece url
    stopifnot("url inadmisible" = is.character(url) &&
        length(url) > 0 && length(colores) > 0) ##filled_char(colores)

    pm <- list(...)
    np <- names(pm)

    if (is.element("pos", np)) {
        pos <- pm$pos
    } else {
        pos <- list(
            x = 0.5, y = 0.5,
            xunits = "fraction",
            yunits = "fraction"
        )
    }

    if (is.element("escala", np)) {
        escala <- pm$escala
    } else {
        escala <- 1L
    }

    ic <- purrr::map(colores, sty_ico, url = url, pos = pos,
                     escala = escala)
    invisible(ic)
}

#' Estilos-puntos
#' @description Estilos de los puntos según control de cuestionario
#' @details
#' @param url character: url del ícono; por omisión: raqueta color
#'     blanco
#' @param ids character: id de los estilos
#' @param ... otros parámetros pasados a sty_ico_colors
#' @return lista xml_node
sty_puntos <- function(url = character(), ids = character(),
                           ...) {

    ## estilo etiqueta, balloon constantes
    ee <- sty_lab(
        escala = 0.8,
        color = "FF61AEFD"
    )
    eb <- sty_bal(texto = "$[description]")

    ## estilo ícono
    if (!length(url)) { # default raqueta blanca
        url <- icon_def(ico = "raq", col = "blanco")$url
    }

    ## lista de IconStyle
    ## colores producidos por color_control_cuestionario
    ci <- color_control_cuestionario(ids)
    ei <- sty_ico_colores(url = url, colores = ci, ...)

    es <- purrr::map2(names(ei), ei, sty_sty, label = ee, ball = eb)
    ## es <- purrr::map2(ei, names(ci),
    ##     function(w, x, y, z) {
    ##         sty_sty(
    ##             id = x, icon = w, label = y,
    ##             ball = z
    ##         )
    ##     },
    ##     y = ee, z = eb
    ## )

    invisible(es)
}

#' Estilos-avance-nacional
#' @description Estilos del KML resumen de avance delegaciones y
#'     nacional
#' @return list
sty_avance_nac <- function() {
    es <- list(
        nac = sty_sty(
            id = "sty-nac",
            icon = sty_ico(
                url = url_google_ico(
                    "shapes",
                    "ranger_station"
                ),
                pos = list(
                    x = 0.5, y = 0.5,
                    xunits = "fraction",
                    yunits = "fraction"
                ),
                escala = 1.0,
                color = "ff0000ff"
            ),
            label = sty_lab(
                escala = 0.8,
                color = "FF61AEFD"
            ),
            ball = sty_bal(texto = "$[description]")
        ),

        del = sty_sty(
            id = "sty-del",
            icon = sty_ico(
                url = url_google_ico("pal3", "icon31"),
                pos = list(
                    x = 0.5, y = 0.5,
                    xunits = "fraction",
                    yunits = "fraction"
                ),
                escala = 1.0,
                color = "ff0000ff"
            ),
            label = sty_lab(
                escala = 0.8,
                color = "FF61AEFD"
            ),
            ball = sty_bal(texto = "$[description]")
        )
    )

    invisible(es)
}

#' Estilos puntos y delegación
#' @description Los estilos de la delegación y los de los puntos
#' @return list
sty_puntos_dele <- function() {
    er <- sty_avance_nac()
    ep <- sty_puntos()
    c(er["del"], ep)
}

#' Descripción del avance
#' @description Devuelve el nodo description que muestra el avance
#' @details De los datos selecciona el nivel (grupo) al cual se han
#'     resumido los datos, calcula el porcentaje de avance general y
#'     sustituye en la plantilla html.
#'
#'     En el archivo que se pasa de argumento del parámetro «file»,
#'     deben estar las plantillas html, con nombre «html_nac»
#'     (nacional), «html_del» (delegación con link al nacional),
#'     «html_del2» (sin link al nacional).
#' @param w data.frame con los datos resumen
#' @param dia character: fecha del reporte
#' @param html la plantilla html de la descripción
#' @param grupo el nivel del resumen (delegación, municipio)
#' @param file character: ruta de acceso del archivo que trae las
#'     plantillas html
#' @examples
#' descripcion_avance(w, DIA, "html_nac", "delegacion")
descripcion_avance <- function(w, fecha = character( ),
                               html = character( ), grupo, file) {
    stopifnot("arg.fecha" = char_lleno(fecha))
    pmt_gauge <- magest::pct(
        sum(w$asignados - w$pendiente),
        sum(w$asignados)
    ) %>%
        paste0(., ",'", ., "%'")

    ## ÔjÔ sin resolver: nombre municipios que llevan acento
    ## desplegados con símbolos "extraños". He intentado cambiando el
    ## "encoding" (de latin1 a UTF-8) con función iconv, pero sin
    ## resultado deseado. Extraño es que cuando se construye el
    ## resumen nacional, los nombres aparecen bien; sólo es cuando se
    ## construye el resumen por delegación y se despliegan todos los
    ## puntos. Una primera solución fue cambiar el atributo encoding
    ## del archivo KML, de UTF-8 a ISO8859-1, y después dejar que
    ## emacs recodificara a latin1 las palabras pertinentes cuando se
    ## graba el archivo modificado; pero es tedioso estar haciendo eso
    ## para cada una de las 17 delegaciones.
    if (html == "html_del2") {
        w["grupo"] <- chartr(
            c("áéíóú"),
            c("aeiou"),
            w$grupo
        )
    }

    pmt_tabla <- select(w, grupo, asignados, levantados, pendiente) %>%
        jsonlite::toJSON() %>%
        as.character()

    fecha_repo <- fecha
    grupo <- paste0("'", grupo, "'")

    ht <- get_off_c(html, file = file) %>%
        glue::glue(.open = "\\{", .close = "\\}") %>%
        as.character()

    node_description(ht, cdata = TRUE)
}

#' Descripción punto
#' @description Hace el nodo "description" con los datos asociados a
#'     un punto (finca)
#' @details Consta de una tabla sencilla y la referencia a una imagen
#'     si existe. La tabla es un sustituto de la descripción mostrada
#'     por google-earth cuando se incluyen ExtendedData: una columna
#'     para el nombre de la variable y otra con el dato
#'     correspondiente. La sustitución es para poder agregar imágenes
#'     al cartel, lo que no se puede hacer con la opción por defecto
#'     de google-earth
#' @param datos list: los datos asociados al punto (fila de un d.f)
#' @param titulo character: títulos con los que se muestran los datos
#' @param image character: url de imagen asociada al punto y mostrada
#'     en el cartel
#' @return xml_node
descripcion_punto <- function(datos = list(),
                              image = "",
                              titulo = character()) {
    x <- data.frame(
        tit = titulo,
        dat = simplify2array(datos)
    )
    tt <- make_table(x)
    if (nzchar(image)) {
        tt <- paste0("<img width='400' src='", image, "'>") %>%
            paste0(tt, .)
    }

    nd <- node_description(tt, cdata = TRUE)

    invisible(nd)
}

#' Descripción
#' @description Genera lista de nodos "description" de los puntos
#' @param p data.frame: datos de los puntos
#' @param variables character: variables que serán mostradas en el
#'     cartel
#' @param titulos character: nombres de las variables tal como se
#'     verán en la primera columna de la tabla mostrada en el cartel
#' @param pim data.frame: nombre del archivo con una imagen asociada
#'     al punto (punto, img)
#' @seealso descripcion_punto
#' @return list xml_node
descripcion_puntos <- function(p, variables = character(),
                               titulos = character(),
                               pim = NULL) {
    ## validar variables columnas de p
    ## misma longitud de titulos
    x <- select(p, one_of(variables)) %>%
        purrr::pmap(list)

    titulos <- names(variables)
    if (is.null(pim)) {
        w <- purrr::map(x, descripcion_punto, titulo = titulos)
    } else {
        y <- iniciar_vec(p$punto, pim$punto, pim$img, si_na = "")
        w <- purrr::map2(x, y, descripcion_punto, titulo = titulos)
    }

    invisible(w)
}

#' Folder-puntos
#' @description Construye nodos PlaceMark y los devuelve en un Folder
#' @details Los ingredientes para hacer los nodos de los PlaceMark
#'     vienen en listas que forman las columnas del data.frame
#'     (arg. "x")
#' @seealso node_folder, node_placemark
#' @param x data.frame: datos de los puntos
#' @param name character: nombre del folder
#' @param ... argumentos adicionales (open, visibility, ...) pasados a
#'     node_folder
folder_puntos <- function(x, name, ...) {
    z <- purrr::pmap(x, node_placemark)

    nf <- node_folder(children = z, name = name, ...)

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
#' @param file character: file.path de salida (extensión kml)
#' @param kmz logical: archivo kmz?; TRUE por omisión
#' @param estilos lista: los estilos generales
#' @param ... adicionales para folder
folder_delegacion <- function(x, w, pm_del, file = "", kmz = TRUE,
                              estilos, ...) {
    fd <- node_folder(list(pm_del), w, ...)

    ## folder por municipio
    z <- split(x, x$municipio)

    ## folders puntos de municipios
    y <- purrr::imap(z, folder_puntos, visibility = 0L)

    ## y los agrega al folder
    for (m in y) xml_add_child(fd, m)

    ## hace documento si hace falta
    if (is.character(file) && length(file) && nzchar(file)) {
        km <- kml_doc(
            visibility = 0,
            estilos = estilos,
            folders = list(fd)
        )

        write_xml(km, file, encoding = "UTF-8")
    }

    if (kmz) {
        kz <- file.path(
            dirname(file),
            gsub("l$", "z", basename(file))
        )
        zip(kz, file, extras = "-q -m")
        ## unlink(file)
    }

    invisible(fd)
}

#' Placemark nacional
#' @description Placemark del nodo resumen nacional
#' @param x data.frame: datos del avance
#' @param ht character: plantilla html
#' @param coordenadas lista: longitud y latitud del «placemark»
#'     nacional
#' @param dia character: día del reporte
#' @param file character: ruta de acceso archivo donde están las
#'     plantillas html
nodo_nacional <- function(x, ht, coordenadas, dia, file) {
    w <- group_by(x, delegacion) %>%
        summarise(
            asignados = n(),
            pendiente = sum(control == "pendiente"),
            levantados = asignados - pendiente,
            avance = magest::pct(levantados, asignados),
            completo = sum(control == "completo"),
            incompleto = sum(control == "incompleto"),
            noagricola = sum(control == "no-agrícola"),
            rechazo = sum(control == "rechazo"),
            noinformante = sum(control == "no-informante"),
            inaccesible = sum(control == "inaccesible")
        ) %>%
        ungroup() %>%
        rename(grupo = delegacion)

    #' para crear h-referencias a los nodos delegaciones
    #' y desplegar el correspondiente cuadro de información
    #' modificar construcción DataTables del html para ajustar
    #' la substring a la longitud de href según el sufijo
    ##w["grupo"] <- utf8::as_utf8(w$grupo)

    w["grupo"] <- href_delegacion(w$grupo, sufijo = ";balloon")

    pn <- node_placemark(
        name = "Nacional", styleUrl = "#sty-nac",
        Snippet = "Resumen nacional",
        description = descripcion_avance(w, dia, ht,
            "delegación", file),
        LookAt = node_look(coordenadas, tlt = 13L),
        coordinates = coordenadas,
        id = "NI"
    )
    invisible(pn)
}

## la tabla de avance en los nodos delegación puede ser por municipio
## o por técnico. (Falta ver cómo se puede filtrar en DataTables para
## tener una sola tabla con filtro municipio/técnico)

#' delegaciones-nodos
#' @description Nodos placemar delegaciones
#' @details Las coordenadas de las delegaciones se recuperan del
#'     data.frame «coor_del» guardado en el archivo con el nombre
#'     pasado de argumento en «file»
#' @param x data.frame: Datos de los puntos para calcular avance
#' @param ht character: plantilla html ("html_del" o "html_del2")
#' @param por_mun logical: resumen por municipio? (TRUE por omisión)
#' @param dia character: fecha del reporte
#' @param file character: ruta de acceso del archivo con las
#'     plantillas html y las coordenadas de las delegaciones
#' @return list
nodos_delegaciones <- function(x, ht = "html_del", por_mun = TRUE,
                               dia, file) {
    w <- group_by(x, delegacion, municipio) %>%
        summarise(
            asignados = n(),
            pendiente = sum(control == "pendiente"),
            levantados = asignados - pendiente,
            avance = magest::pct(levantados, asignados),
            completo = sum(control == "completo"),
            incompleto = sum(control == "incompleto"),
            noagricola = sum(control == "no-agrícola"),
            rechazo = sum(control == "rechazo"),
            noinformante = sum(control == "no-informante"),
            inaccesible = sum(control == "inaccesible")
        ) %>%
        ungroup() %>%
        rename(grupo = municipio)

    nd <- split(w, w$delegacion) %>%
        lapply(descripcion_avance, fecha = dia, html = ht,
               grupo = "municipio", file = file)

    y <- get_off("coor_del", file = file)
    mm <- match(names(nd), y$departamento)
    y <- y[mm, ]

    xy <- coord_lista(y)
    nt <- lapply(xy, node_look, tlt = 13L)

    dpto <- departamentos()

    ab <- iniciar_vec(names(nd), dpto$departamento, dpto$abr)

    nm <- names(nd) %>% iconv("latin1", "UTF-8")
    if (ht == "html_del2") {
        nm <- iconv(nm, "UTF-8", "")
    }

    ww <- data.frame(
        id = ab,
        name = nm,
        Snippet = y$ciudad,
        styleUrl = "#sty-del",
        coordinates = I(xy),
        LookAt = I(nt),
        description = I(nd)
    )

    #' nodos placemark delegaciones
    pd <- purrr::pmap(ww, node_placemark)
    names(pd) <- ab

    invisible(pd)
}

#' KML-nacional
#' @description KML del avance nacional
#' @details Produce el KML con los «placemark» de las delegaciones y
#'     el nacional (aprox. centroide del país). Cuando "click" en uno
#'     de ellos, despliega la gráfica general del avance y una tabla
#'     con las cifras de boletas levantadas por delegación
#'     («placemark» nacional) o por municipio (delegaciones) y la
#'     distribución según el control del cuestionario. Devuelve el
#'     xml_doc (invisible) y lo manda a guardar en un archivo.
#' @param z data.frame: los datos de los puntos de la encuesta
#'     actualizados según el avance
#' @param fout character: archivo de salida (extensión kml)
#' @param kmz logical: archivo kmz?; TRUE por omisión
#' @param dia character: fecha del reporte
#' @param fapo character: ruta de acceso archivo con coordenadas
#'     delegaciones y plantillas html
#' @return KML doc
kml_avance_nac <- function(z, fout = "", kmz = TRUE, dia, fapo) { 
    lon <- PNLON
    lat <- PNLAT

    fn <- node_element("Folder")
    xml_add_child(fn, node_element("name", "Nacional"))
    xml_add_child(fn, node_element("open", 1L))
    xml_add_child(fn, node_element("visibility", 1L))

    pn <- nodo_nacional(z, "html_nac",
                        coordenadas = list(x = lon, y = lat),
                        dia, file = fapo)
    pd <- nodos_delegaciones(z, "html_del", dia = dia,
                             file = fapo)

    ## agrega placemark delegaciones
    for (np in pd) {
        xml_add_child(fn, np)
    }

    ## y nacional
    xml_add_child(fn, pn)

    es <- sty_avance_nac()
    km <- kml_doc(
        visibility = 0,
        estilos = es,
        folders = list(fn), enc = "UTF-8"
    )

    if (nzchar(fout)) {
        write_xml(km, fout, encoding = "UTF-8")
    }

    if (nzchar(fout) && kmz) {
        km <- file.path(
            dirname(fout),
            sub("l$", "z", basename(fout))
        )
        zip(km, fout, extras = "-q -m")
    }

    invisible(km)
}

#' @description Recibe los datos y coordenadas de los puntos de un
#'     municipio, y produce el folder KML de los «placemark»
#' @param x data.frame: las coordenadas y los datos con los que se
#'     construye el cartel descriptivo de cada punto
#' @param cartel character: variables en el cartel; nombre con los
#'     nombres de las columnas
#' @return xml_node
folder_municipio <- function(x, cartel) {
    nk <- lapply(x$coordinates, node_look,
                 alt = 100L, rng = 1000L, tlt = 13L)
    ## nd <- descripcion_puntos(x, variables = VBC,
    ##                          titulos = TIT)
    nd <- descripcion_puntos(x, variables = cartel)

    nr <- nrow(x)
    ## p <- list(name = x$punto,
    ##           coordinates = x$coordinates,
    ##           styleUrl = paste0("#", x$id_ctrl),
    ##           description = nd,
    ##           LookAt = nk,
    ##           visibility = integer(nr) + 1L)
    p <- data.frame(name = x$punto,
                    coordinates = x$coordinates, 
                    styleUrl = paste0("#", x$id_ctrl), 
                    description = nd %>% I,
                    LookAt = nk %>% I,
                    visibility = integer(nr) + 1L)
    ##nm <- x$municipio[1] %>% iconv("latin1", "UTF-8")
    nm <- x$municipio[1] #%>% enc2native()
    z <- folder_puntos(p, nm, visibility = 0L)
    invisible(z )
}

#' @description Recibe lista de folder de los municipios de un
#'     departamento y produce el xml_doc y lo guarda en un archivo
#' @param dep character: nombre de la lista x que identifica al
#'     departamento
#' @param fmu lista de xml_node: folderes con puntos de los municipios
#' @param dir character: directorio donde guardar archivo KML
#' @param estilos lista de estilos de los puntos
#' @return xml_doc
kml_departamento <- function(dep, fmu, dir, estilos, ...) {
    
    fd <- node_folder(children = fmu, name = dep, ...)

    ## hace documento si hace falta
    if (is.character(dir) && nzchar(dir)) {
        km <- kml_doc(
            visibility = 0,
            estilos = estilos,
            folders = list(fd)
        )

        file <- file.path(dir, paste0(dep, ".kml"))
        ## chk. file o dir para evitar error
        write_xml(km, file, encoding = "UTF-8")
        kz <- file.path(dir, paste0(dep, ".kmz"))
        zip(kz, file, extras = "-q -m")
    }

    invisible(fd)
}

## -- helper func. datos --

#' Coordenadas puntos
#' @description Devuelve una lista con las coordenadas de los puntos
#' @param df character: data.frame que contiene las coordenadas
#' @param file character
#' @return data.frame
xy_puntos <- function(df, file) {
    w <- get_off_c(df, file = file)
    x <- coord_lista(w)

    ## supone primera columna con numeración de puntos
    data.frame(punto = w[[1]], coordinates = I(x)) %>%
        invisible()
}

#' punto-localización
#' @description Devuelve los datos de localización de los puntos de la
#'     encuesta.
#' @details Supone que el data.frame con los puntos se llama «pun», y
#'     que la columna «replica» trae el número de la réplica a la cual
#'     está asignado cada punto.
#'
#'     Los datos de localización son las coordenadas (devueltas en una
#'     lista), el departamento y el municipio.
#' @param rep_mes integer: las réplicas incluidas en el mes
#' @param file character: ruta de acceso donde guardado el data.frame
#'     con los datos de los puntos
#' @return data.frame
#' @examples
#' puntos_geo(c(1, 2, 4), "puntos.rda")
puntos_geo <- function(rep_mes = 1:4, file) {
    d <- municipios()
        
    z <- get_off(pun, file = file) %>%
        filter(replica %in% rep_mes)

    x <- coord_lista(z)
    y <- z %>%
        st_drop_geometry() %>%
        select(punto, dpt, mun) %>%
        bind_cols(coordinates = I(x))

    invisible(y)
}
