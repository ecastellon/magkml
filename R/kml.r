## -*- encoding: utf-8 -*-

## -- crear archivos kml --

## node_element("table", atr = list(class = "aja") no produce
## nodo con atributo class. Ha de ser porque as_xml_document
## redefine atributo a xml_document xml_node y descarta la
## asignación inicial
## u <- read_xml("<table class = 'aja' />") produce lo esperado

#' Elemento
#' @description Construye un elemento XML
#' @details No valida la gramática del nombre del elemento
#' @param tag character: nombre del elemento
#' @param val character o numeric: valor del elemento
#' @param atr lista: lista con los atributos
#' @param as_xml logical: devuelve lista u objeto xml_node (TRUE
#'     por omisión)
#' @return lista u objeto xml_node
#' @examples
#' node_element("point")
#' node_element("name", val = "elemento", atr = list(id = "5001"))
#' @export
node_element <- function(tag = character(), val = "", atr = list(),
                           as_xml = TRUE) {

    stopifnot("tag inválido" = filled_char(tag),
              "val inválido" = filled_char(val) || filled_num(val),
              "atr inválido" = is.list(atr))

    x <- list(val)
    attributes(x) <- atr

    x <- structure(list(x), names = tag)

    if (as_xml) x <- as_xml_document(x)
    invisible(x)
}

#' Ícono GE
#' @description url de los íconos más comunes de google-earth
#' @details Hay muchos íconos que ofrece GE para señalar la posición
#'     de los puntos
#'     (\code{http://kml4earth.appspot.com/icons.html}). Los tipos que
#'     más se utilizan son las tachuelas (pushspin), raquetas (paddle)
#'     y cuadrados a diferentes colores. La función devuelve la
#'     dirección url de los íconos de esos 3 tipos, a 5 colores, junto
#'     con las coordenadas que determinan la ubicación del ícono con
#'     respecto a las del punto. Estos dos elementos se utilizan para
#'     construir los elementos "href" y "hotSpot" de los nodos
#'     IconStyle de los archivos KML.
#'
#'     El parámetro "ico" es para indicar el tipo (tachuela, raqueta,
#'     cuadro) y "col" para el color del ícono (rojo, verde, azul,
#'     amarillo, blanco). Es suficiente pasar como argumento la
#'     primera letra de las opciones, excepto para los colores
#'     amarillo y azul que son necesarias las dos primeras para evitar
#'     la ambigüedad. Si cualquiera de los dos no es una opción
#'     valida, se devuelve la url de un círculo a color blanco.
#' @param ico character: el tipo de ícono
#' @param col character: el color del ícono
#' @return list con "url" y "pos" para construir los elementos href y
#'     hotSpot de los nodos StyleIcon de los archivos KML
#' @keywords internal
#' @examples
#' icon_def(col = "az") #tachuela color azul
#' icon_def("r", "am")  #raqueta color amarillo
#' icon_def("")         #círculo blanco
icon_def <- function(ico = "tachuela", col = "blanco") {
    stopifnot(exprs = {
        "arg. ico" = filled_char(ico)
        "arg. col" = filled_char(col)
    })

    tico <- c(t = "tachuela", g = "raqueta", c = "cuadro")
    cico <- c(r = "rojo", g = "verde", b = "azul", a = "amarillo",
              w = "blanco")

    ## default: círculo color blanco con centro negro
    urlb <- "http://maps.google.com/mapfiles/kml/"
    icon <- "shapes/placemark_circle.png"
    hspt <- c(x = "0.5", y = "0.5", xunits = "fraction",
              yunits = "fraction")

    ico <- pmatch(ico, tico)
    col <- pmatch(col, cico)
    ## el default si no especifica tipo ícono y su color
    mx <- paste("pushpin", c("red", "grn", "blue", "ylw", "wht"),
                sep = "/") %>%
        paste("pushpin.png", sep = "-") %>%
        c(paste("paddle", c("red", "grn", "blu", "ylw", "wht"),
                sep = "/") %>%
          paste("blank.png", sep = "-")) %>%
        c(paste("paddle", c("red", "grn", "blu", "ylw", "wht"),
                sep = "/") %>%
          paste("blank-lv.png", sep = "-")) %>%
        matrix(nrow = 3, byrow = TRUE)

    hs <- list(c(x = "20",  xunits = "pixels",
                 y = "1",   yunits = "pixels"),
               c(x = "0.5", xunits = "fraction",
                 y = "1",   yunits = "pixels"),
               c(x = "0.5", xunits = "fraction",
                 y = "0.5", yunits = "fraction"))

    if (!(is.na(ico) || is.na(col))) {
        icon <- mx[ico, col]
        hspt <- hs[[ico]]
    }

    list(url = paste0(urlb, icon),
         pos = hspt)
}

#' IconStyle
#' @description Elemento IconStyle
#' @details (\code{https://developers.google.com/kml/documentation/})
#' @param as_xml logical: devuelve documento xml o lista; TRUE por
#'     omisión
#' @param ... admisibles: id, ico, col, color, escala, rumbo, url, pos
#' @return nodo IconStyle
#' @seealso \code{icon_def}
#' @export
#' @examples
#' sty_ico()
#' sty_ico(ico = "tach", col = "rojo")
#' sty_ico(ico = "")
sty_ico <- function(..., as_xml = TRUE) {
    ## TODO: chk. args

    ## -- especificaciones --
    ## default
    w <- icon_def() # default tachuela blanca
    z <- list(color     = "ff0000ff", # rojo
              colorMode = "normal",
              escala    = 0.7,
              rumbo     = 0,
              url       = w$url,
              pos       = w$pos)

    ## cambios por argumentos en ...
    y <- list(...)
    if (filled(y)) {
        x <- c("ico", "col")
        if (all(is.element(x, names(y)))) { # calculada
            w <- do.call("icon_def", y[x])
            y[["url"]] <- w$url
            y[["pos"]] <- w$pos
        }
        x <- intersect(names(y), names(z))
        if (filled_char(x)) {
            z[x] <- y[x]
        }
    }

    ## nodo a partir de lista de spec
    id <- ""
    if (is.element(id, names(y))) {
        id <- y[["id"]]
    }
    w <- list(IconStyle = structure(
                  list(
                      list(color   = list(z$color),
                           colorMode = list(z$colorMode),
                           scale   = list(z$escala),
                           heading = list(z$rumbo),
                           Icon    = list(href = list(z$url)),
                           hotSpot = structure(list(),
                                               x = z$pos[["x"]],
                                               y = z$pos[["y"]],
                                               xunits = z$pos[["xunits"]],
                                               yunits = z$pos[["yunits"]])
                           )), id = id))

    if (as_xml) w <- as_xml_document(w)
    invisible(w)
}

#' LabelStyle
#' @description Elemento LabelStyle
#' @details Estilo para el nombre del punto en el mapa
#' @param as_xml logical: devuelve documento xml o lista; TRUE por
#'     omisión
#' @param ... admisibles: id, color, colorMode, escala
#' @return nodo LabelStyle
#' @export
#' @examples
#' sty_lab()
#' sty_lab(id = "labRojo", color = "ffffff00", escala = 1.2)
sty_lab <- function(..., as_xml = TRUE) {
    ## default
    z <- list(color     = "ff000000", # negro
              colorMode = "normal",
              escala    = 0.7)

    y <- list(...)
    if (filled(y)) {
        x <- intersect(names(y), names(z))
        if (filled_char(x)) {
            z[x] <- y[x]
        }
    }

    ## nodo a partir de lista de spec
    id <- ""
    if (is.element(id, names(y))) {
        id <- y[["id"]]
    }

    w <- list(LabelStyle = structure(
                  list(list(color = list(z$color),
                            colorMode = list(z$colorMode),
                            scale   = list(z$escala))
                       ), id = id))

    if (as_xml) w <- as_xml_document(w)
    invisible(w)
}

## Kindle sepia color code reading
## background:#FBF0D9;color:#5F4B32;

#' BalloonStyle
#' @description Elemento BalloonStyle
#' @details Estilo del cuadro que emerge cuando "click" el ícono del
#'     punto
#' @param as_xml logical: devuelve documento xml o lista; TRUE por
#'     omisión
#' @param ... admisibles: bgcol, txtcol, texto, modo
#' @return nodo BalloonStyle
#' @export
#' @examples
#' sty_bal()
sty_bal <- function(..., as_xml = TRUE) {
    ## default
    z <- list(bgcol  = "ffffffff", # blanco
              txtcol = "ff000000", # negro
              texto  = "$[name]",
              modo   = "default")  # hide

    y <- list(...)
    if (filled(y)) {
        x <- intersect(names(y), names(z))
        if (filled_char(x)) {
            z[x] <- y[x]
        }
    }

    ## nodo a partir de lista de spec
    id <- ""
    if (is.element(id, names(y))) {
        id <- y[["id"]]
    }

    w <- list(BalloonStyle = structure(
                  list(list(bgColor     = list(z[[1]]),
                            textColor   = list(z[[2]]),
                            text        = list(z[[3]]),
                            displayMode = list(z[[4]]))
                       ), id = id))

    if (as_xml) w <- as_xml_document(w)
    invisible(w)
}

## produce StyleMap
## normal, destacado: id del estilo para cada caso

#' StyleMap
#' @description Elemento StyleMap
#' @details Vea documentación oficial
#' @param as_xml logical: devuelve documento xml o lista; TRUE por
#'     omisión
#' @param id character: atributo id del estilo
#' @param normal character: atributo id del estilo normal
#' @param destacado character: atributo id del estilo alterno
#' @return nodo StyleMap
#' @export
#' @examples
#' sty_map(id = "estilo", "estilo1", "estilo2")
sty_map <- function(id = character(),
                    normal = character(),
                    destacado = character(), as_xml = TRUE) {
    stopifnot(exprs = {
        "arg. id" = filled_char(id) && is_scalar(id)
        "arg. norm." = filled_char(normal) && is_scalar(normal)
        "arg. dest." = filled_char(destacado) && is_scalar(destacado)
    })

    ## valida inicial
    normal <- paste0("#", sub("[^[:alnum:]]", "", normal))
    destacado <- paste0("#", sub("[^[:alnum:]]", "", destacado))

    w <- list(StyleMap = structure(
                  list(Pair = list(
                           list(key = list("normal"),
                                styleUrl = list(normal))),
                       Pair = list(
                           list(key = list("highlight"),
                                styleUrl = list(destacado))
                       )), id = id))

    if (as_xml) w <- as_xml_document(w)
    invisible(w)
}

#' Style
#' @description Elemento Style
#' @details Vea documentación oficial
#' @param id character: atributo id del nodo Style
#' @param icon nodo IconStyle
#' @param label nodo LabelStyle
#' @param ball nodo BalloonStyle
#' @return nodo Style
#' @export
#' @examples
#' sty_sty("estilo")
#' sty_sty("est", sty_ico())
sty_sty <- function(id = character(), icon = NULL,
                    label = NULL, ball = NULL) {
    stopifnot(exprs = {
        "arg. id" = filled_char(id) && is_scalar(id)
        "arg. icon" = is.null(icon) ||
            (inherits(icon, "xml_node") &&
             tolower(xml_name(icon)) == "iconstyle")
        "arg. label" = is.null(label) ||
            (inherits(label, "xml_node") &&
             tolower(xml_name(label)) == "labelstyle")

        "arg. ball" = is.null(ball) ||
            (inherits(ball, "xml_node") &&
             tolower(xml_name(ball)) == "balloonstyle") })

    ## nodo base
    w <- read_xml(paste0("<Style id = '", id, "'/>"))

    if (!is.null(icon)) {#chk válido
        xml_add_child(w, icon)
    }

    if (!is.null(label)) {
        xml_add_child(w, label)
    }

    if (!is.null(ball)) {
        xml_add_child(w, ball)
    }

    invisible(w)
}

#' Coordenadas
#' @description Elemento \code{coordinates}
#' @param x numeric: longitud
#' @param y numeric: latitud
#' @return xml_node
#' @export
#' @examples
#' node_coordinates(-80.23, 10.12)
node_coordinates <- function(x, y) {
    node_element(tag = "coordinates", val = paste(x, y, sep = ","))
}

#' Point
#' @description Elemento \code{Point}
#' @param x numeric: longitud
#' @param y numeric: latitud
#' @return xml_node
#' @export
#' @examples
#' node_point(-80.23, 10.12)
node_point <- function(x, y) {
    p <- node_element("Point")
    xml_add_child(p, node_coordinates(x, y))
    invisible(p)
}

#' LookAt
#' @description Elemento que determina el punto de vista desde el que
#'     se visualiza al punto
#' @details Latitud y longitud son las coordenadas del punto; los
#'     demás nodos hijos comunes a todos los puntos
#' @param xy lista: latitud (y) y longitud (x)
#' @param alt integer: altitud
#' @param rng integer: distancia desde el punto de vista
#' @param tlt integer: grado de inclinación
#' @param alm character: referencia altitud; "clampToGround"
#' @param hdn integer: orientación al norte
#' @export
#' @examples
#' node_look(list(x= -83, y = 13))
node_look <- function(xy, alt = 500L, rng = 600000L, tlt = 28L,
                      alm = "clampToGround", hdn = 0L) {
    nd <- node_element("LookAt")
    xml_add_child(nd, node_element("latitude", xy$y))
    xml_add_child(nd, node_element("longitude", xy$x))
    xml_add_child(nd, node_element("altitudeMode", alm))
    xml_add_child(nd, node_element("altitude", alt))
    xml_add_child(nd, node_element("range", rng))
    xml_add_child(nd, node_element("tilt", tlt))
    xml_add_child(nd, node_element("heading", hdn))
    
    invisible(nd)
}

#' Description
#' @description Elemento \code{description}
#' @param x character: la descripción
#' @param cdata logical: incluir la descripción dentro de un nodo
#'     CDATA; FALSE por omisión
#' @export
#' @examples
#' node_description("Bla bla Bla")
#' node_description("<html></html>", cdata = TRUE)
node_description <- function(x = character(), cdata = FALSE) {
    stopifnot(filled_char(x) && nzchar(x))
    if (cdata) {
        w <- node_element("description")
        xml_add_child(w, xml_cdata(x))
    } else {
        w <- node_element("description", x)
    }

    invisible(w)
}

## disp_names es lista nombrada con el nombre del Data
## es pasado como argumento suplementario para evitar
## tener que pasarlo en columnas del data.frame con el que
## se construyen los Placemark. Puede ser xml_cdata o character

#' Data-node
#' @description Construye elemento Data
#' @details Ver capítulo "Datos personalizados" en la documentación
#'     oficial. Un elemento Data necesariamente tiene un nodo hijo
#'     ("<value></value>"") con el valor del dato, y de manera opcional
#'     un nodo displayName. El parámetro disp_names es una lista con
#'     nombres igual a los atributos names de los diferentes elementos
#'     Data que pueden estar contenidos en un nodo ExtendedData. El
#'     valor del elemento displayName es tomado de esa lista según el
#'     parámetro data_name. Si el valor de displayName es "", no se
#'     agrega el elemento displayName al elemento Data.
#' @param value numeric o character: valor del elemento value
#' @param data_name character: atributo name del elemento Data
#' @param disp_names lista de character o de elementos CDATA
#' @export
#' @return xml_node
#' @examples
#' node_data(3, "area", list(area = "", ciudad = ""))
#' cd <- xml_cdata("<i>Manzanas finca</i>:")
#' node_data(3, "area", list(area = cd, ciudad = ""))
node_data <- function(value, data_name, disp_names) {
    disp_name <- disp_names[[data_name]]
    stopifnot("sin valor" = filled_char(value) || filled_num(value),
              "sin atr. name" = filled_char(data_name),
              "display-name" = filled_char(disp_name) ||
                  inherits(disp_name, "xml_cdata"))

    x <- node_element("Data", atr = list(name = data_name))

    y <- NULL
    if (inherits(disp_name, "xml_cdata")) {
        y <- node_element("displayName")
        xml_add_child(y, disp_name)
    } else {
        if (nzchar(disp_name)) {
            y <- node_element("displayName", disp_name)
        }
    }

    if (!is.null(y)) {
        xml_add_child(x, y)
    }

    y <- node_element("value", value)
    xml_add_child(x, y)

    invisible(x)
}

## list(value = list(), display_name = list())
## data es lista con nombre que contiene los datos
## dname es lista con el formato displayName

#' Extended-data node
#' @description Construye elemento ExtendedData
#' @details ExtendedData es el elemento que anida a uno o más
#'     elementos Data. Los valores de los Data son pasados como
#'     argumento del parámetro data, en una estructura de lista cuyos
#'     nombres se utilizan de valor del atributo name de los elementos
#'     Data. El argumento al parámetro dname contiene los valores de
#'     los elementos displayName de los elementos Data. Esta debe ser
#'     una lista con el mismo número de elementos que la lista pasada
#'     al parámetro data.
#' @seealso node_data
#' @param data lista con nombres
#' @param dname lista
#' @return xml_node
#' @export
#' @examples
#' dd <- list(area = 3, ciudad = "Managua")
#' dn <- list(area = "Manzanas",
#'            ciudad = xml_cdata("<b>Ciudad</b>"))
#' node_extended_data(dd, dn)
node_extended_data <- function(data = list(), dname = list()) {

    x <- node_element("ExtendedData")

    d <- purrr::map2(data, names(data), node_data, disp_name = dname)

    for (nd in d) xml_add_child(x, nd)

    invisible(x)
}

## recibe lista con los datos de un lugar
## - el nombre del dato en la lista sirve para mapear
##   el dato con el nodo hijo
## - para construir ExtendedData, debe venir la lista
##   de los valores y del atributo name del nodo Data
## - no pueden faltar las coordenadas
## - algunos como visibility se dan por default

#' Placemark node
#' @description Construye elemento Placemark
#' @details Vea la documentación oficial de KML. Los parámetros tienen
#'     nombre igual o similar a los correspondientes elementos hijos
#'     de Placemark: name, open, visibility, snippet, description,
#'     style_url, extended_data, display_name, coordinates. Todos son
#'     opcionales excepto coordinates que es una lista con los
#'     elementos x (longitud) e y (latitud). El parámetro
#'     extended_data es una lista con los valores de los elementos
#'     Data, con nombres igual al atributo name del elemento Data;
#'     asociado a este, el parámetro display_name es una lista con los
#'     valores del elemento displayName.
#' @seealso node_data, node_extended_data
#' @param ... listas
#' @param id character o numeric: valor del atributo id de Placemark
#' @return xml_node
#' @export
#' @examples
#' node_placemark(coordinates = list(x = -84.4, y = 10.12))
#' node_placemark(coordinates = list(x = -84.4, y = 10.12),
#'                ExtendedData = list(area = 3, ciudad = "Ocotal"),
#'                displayName = list(area = "", ciudad = "Ciudad"))
node_placemark <- function(...) {
    x <- list(...)
    nm <- names(x)
    nx <- c("name", "open", "visibility", "Snippet", "styleUrl")

    stopifnot("falta nodo" = filled_char(nm),
              "sin coordenadas" = en("coordinates", nm))

    if (is.element("id", nm)) {
        pm <- node_element("Placemark", atr = list(id = x$id))
    } else {
        pm <- node_element("Placemark")
    }

    if (filled_char(nm)) {
        z <- purrr::walk2(x, names(x),
                   function(x, y) {
                       if (y %in% nx) {
                           xml_add_child(pm, node_element(y, x))
                       }
                   })
    }

    if (is.element("description", nm)) {
        ## if (inherits(x$description, "xml_cdata")) {
        ##     w <- node_element("description")
        ##     xml_add_child(w, x$description)
        ## } else {
        ##     if (filled_char(x$description) && nzchar(x$description)) {
        ##         w <- node_element("description", x$description)
        ##     } else {
        ##         w <- character()
        ##     }
        ## }
        if (inherits(x$description, "xml_node")) {
            xml_add_child(pm, x$description)
        }
    }

    if (is.element("LookAt", nm)) {
        if (inherits(x$LookAt, "xml_node")) {
            xml_add_child(pm, x$LookAt)
        }
    }

    if (is.element("ExtendedData", nm)) {
        ## tiene que estar displayName; si no, list() e igual num. elem.
        ## xd <- lapply(names(x$extended_data), function(x){
        ##     list(name = x)})
        ## names(x$extended_data) <- NULL
        ## x$extended_data es una lista de lista
        ## display_name es lista con tantos elementos
        ## como datos
        xm <- node_extended_data(x$ExtendedData, x$displayName)
        xml_add_child(pm, xm)
    }

    p <- node_point(x$coordinates$x, x$coordinates$y)
    xml_add_child(pm, p)

    invisible(pm)
}

## produce nodo Folder
## recibe data.frame con los datos de los puntos
## para generar los nodos placemark
## nombres de columnas deben corresponder
## con nombres de lista que recibe node_placemark
## c("name", "open", "visibility", "snippet",
##   "description", "style_url", "extended_data",
##   "coordinates")
## coordinates: columnas del df con las coordenadas
## extended_data: ídem data-values
## names_data: los atr. name de elem. Data ??
## styleUrl en Folder no afecta los Placemark

#' Folder node
#' @description Construye elemento Folder
#' @details Ver documentación oficial de KML
#' @seealso node_placemark, node_data
#' @param id character o numeric: valor del atributo id de Folder
#' @param name character: nombre del folder
#' @param open numeric: 1 ó 0 según se quiera que el folder se vea
#'     "abierto" o "cerrado" (0 por omisión) al inicio.
#' @param visibility numeric: 1 ó 0 según se quiera que los Placemark
#'     en el folder al inicio se muestren visibles o no (0 por
#'     omisión)
#' @param snippet character: leyenda del folder; "" por omisión
#' @param data_pm data.frame cuyas columnas llevan las listas con los
#'     valores de los elementos coordinates y ExtendedData (es
#'     opcional; si se incluye, la columna del data.frame se llama
#'     extended_data) de los elementos Placemark que conforman el
#'     folder.
#' @param display_name lista con los elementos del elemento
#'     displayName del elemento Data (opcional)
#' @return xml_node
#' @export
node_folder_x <- function(id = "", data_pm = NULL, displayName = list(),
                        name = "", open = 0L,
                        visibility = 0L, Snippet = "") {

    x <- node_element("Folder", atr = list(id = id))

    xml_add_child(x, node_element("open", open))
    xml_add_child(x, node_element("visibility", visibility))
    if (nzchar(name)) {
        xml_add_child(x, node_element("name", name))
    }
    if (nzchar(Snippet)) {
        xml_add_child(x, node_element("Snippet", Snippet))
    }

    ## si dpm no null
    ## - coordinates existen
    ## si nzchar(extended_data) -> dpm no es null,
    ##   y mismos elementos en names_data
    ## construye los place_mark
    if (!is.null(data_pm)) {
        pm <- purrr::pmap(data_pm, node_placemark,
                   displayName = displayName)

        if (filled_list(pm)) {
            for (z in pm) xml_add_child(x, z)
        }
    }
    invisible(x)
}

#' Folder node
#' @description Construye elemento Folder
#' @details Ver documentación oficial de KML. Orden de los parámetros
#'     facilita usar purrr::imap con una lista.
#' @seealso node_placemark, node_data
#' @param children list: lista de nodos hijos; e.g. placemark
#' @param name character: nombre del folder
#' @param ... puede incluir atributo id, open, visibility, Snippet,
#'     styleUrl, description
#' @return xml_node
#' @export
node_folder <- function(children = list(), name = character(), ...) {
    x <- list(...)
    nm <- names(x)

    ## hijos admisibles; especificaciones incluye ExtendedData y otros
    nx <- c("open", "visibility", "Snippet", "styleUrl",
            "description")

    nf <- node_element("Folder")
    xml_add_child(nf, node_element("name", name))
    if (filled_char(nm)) {
        if (is.element("id", nm)) {
            xml_set_attr(nf, "id", x$id)
        }

        w <- purrr::iwalk(x, function(z, y) {
            if (is.element(y, nx)) {
                xml_add_child(nf, node_element(y, z))
            }
        })
    }
    
    if (filled_list(children)) {
        for (w in children) { #chk es xml_node?
            xml_add_child(nf, w)
        }
    }

    invisible(nf)
}

#' KML document
#' @description Construye elemento raíz de KML
#' @param estilos lista con elementos de estilos de uso común en el
#'     documento
#' @param ... parámetros adicionales con los valores de los elementos
#'     hijos del elemento Document (name, open, visibility, Snippet)
#' @return xml_node
#' @export
kml_doc <- function(..., estilos = list(), folders = list(),
                    enc = "UTF-8") {
    x <- list(...)

    w <- node_element("Document", atr = list(id = "root"))

    if (filled_list(x)) {
        ## chk character c/u
        nx <- c("name", "open", "visibility", "Snippet")

        z <- purrr::walk2(x, names(x), function(x, y) {
            if (y %in% nx) {
                if (filled_num(x) ||
                    (filled_char(x) && nzchar(x))) {
                    xml_add_child(w, node_element(y, x))
                }
            }
        })
    }

    if (filled_list(estilos)) {
        purrr::walk(estilos, function(x) {
            if (inherits(x, "xml_node")) {
                xml_add_child(w, x)
            }
        })
    }

    if (filled_list(folders)) {
        purrr::walk(folders, function(x) {
            if (inherits(x, "xml_node")) {
                xml_add_child(w, x)
            }
        })
    }

    k <- xml_new_root("kml",
                      xmlns = "http://www.opengis.net/kml/2.2",
                      .encoding = enc)

    xml_add_child(k, w)

    invisible(k)
}

#' Html-table
#' @description Construye tabla html simple
#' @param df data.frame: el cuerpo de la tabla
#' @param atrs list: lista de atributos
#' @param head character: encabezado de la tabla
#' @param foot character: pie de la tabla
#' @return character
#' @export
make_table <- function(df, atrs = list(), cab = character(),
                       foot = character()) {
    ## -- valida
    
    nt <- node_element("table", atr = atrs) %>%
        xml_find_first("//table")

    ## ver nota antes de node_element
    if (is.element("class", names(atrs))) {
        xml_attr(nt, "class") <- atrs$class
    }

    ## -- alineación: numeric, derecha; character, izquierda
    an <- sapply(df, is.character)
    x <- rep("text-align:right", length(df))
    x[an] <- "text-align:left"

    ## ss <- sprintf("%s%s%s", "<col style='", x, "'>")
    ## nc <- node_element("colgroup", paste(ss, collapse = ""))

    nc <- node_element("colgroup")
    for ( n in seq_along(x) ) {
        xml_add_child(nc, node_element("col",
                                       atr = list(style = x[n])))
    }
    
    xml_add_child(nt, nc)

    ## -- header
    if ( length(cab) > 0 ) {
        nr <- node_element("tr")
        purrr::walk(cab, function(x) {
            xml_add_child(nr, node_element("th", x))
        })
        
        nc <- node_element("thead")
        xml_add_child(nc, nr)
        xml_add_child(nt, nc)
    }

    ## -- body
    nr <- purrr::pmap(df, function(...) {
        z <- list(...)
        nr <- node_element("tr")
        purrr::walk(z, function(x) {
            xml_add_child(nr, node_element("td", x))
        })
        nr
    })

    nc <- node_element("tbody")
    purrr::walk(nr, function(x) xml_add_child(nc, x))
    xml_add_child(nt, nc)

    ## !!! -- footer
    if ( length(foot) > 0 ) {
    }
    
    invisible(as.character(nt))
}
