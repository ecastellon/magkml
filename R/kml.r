## -*- encoding: utf-8 -*-

## -- crear archivos kml --

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
#' @description Produce un nodo IconStyle de documento KML
#' @details (\code{https://developers.google.com/kml/documentation/})
#' @param as_xml logical: devuelve documento xml o lista; FALSE por
#'     omisión
#' @param ... admisibles: id, ico, col, color, escala, rumbo, url, pos
#' @return nodo IconStyle
#' @seealso \code{icon_def}
#' @export
#' @examples
#' sty_ico()
#' sty_ico(ico = "tach", col = "rojo")
#' sty_ico(ico = "")
sty_ico <- function(..., as_xml = FALSE) {
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
    id = ""
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
#' @description Nodo LabelStyle documento KML
#' @details Estilo para el nombre del punto en el mapa
#' @param as_xml logical: devuelve documento xml o lista; FALSE por
#'     omisión
#' @param ... admisibles: id, color, colorMode, escala
#' @return nodo LabelStyle
#' @export
#' @examples
#' sty_lab()
#' sty_lab(id = "labRojo", color = "ffffff00", escala = 1.2)
sty_lab <- function(..., as_xml = FALSE) {
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
    id = ""
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
#' @description Nodo BalloonStyle documento KML
#' @details Estilo del cuadro que emerge cuando "click" el ícono del
#'     punto
#' @param as_xml logical: devuelve documento xml o lista; FALSE por
#'     omisión
#' @param ... admisibles: bgcol, txtcol, texto, modo
#' @return nodo BalloonStyle
#' @export
#' @examples
#' sty_bal()
sty_bal <- function(..., as_xml = FALSE) {
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
    id = ""
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
#' @description Nodo StyleMap documento KML
#' @details ver especificaciones GE
#' @param as_xml logical: devuelve documento xml o lista; FALSE por
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
                    destacado = character(), as_xml = FALSE) {
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
#' @description Nodo Style documento KML
#' @details ver especificaciones
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

## ...: name, Snippet, visibility, open
kml_root <- function(..., as_xml = TRUE) {

    nodes <- c("name", "Snippet", "visibility", "open")
    z <- list(list(name = list("root")),
              list(Snippet = list("root")),
              list(visibility = list(0)),
              list(open = list(0)))
    
    y <- dots_values_as_list(...)
    if (filled(y)) {
        ny <- names(y)
        iy <- which(ny %in% nodes)
        if (filled(iy)) {
            y <- lapply(iy, function(x) y[x])
            iz <- which(nodes %in% ny)
            z[iz] <- y
        }
    }

    w <- list(Document = structure(z, id = "id_root"))
    if (as_xml) w <- as_xml_document(w)
    invisible(w)
}

node_element <- function(tag = character(), val = "", atr = list(),
                           as_xml = TRUE) {
    ## tag es character escalar, no vacío
    ## atr es lista
    ## val es escalar, vector atómico
    x <- list(val)
    attributes(x) <- atr
    
    x <- structure(list(x), names = tag)

    if (as_xml) x <- as_xml_document(x)
    invisible(x)
}



node_set_element <- function(tag = character(), val = "", atr = list(),
                                as_xml = TRUE) {

    purrr::map2(val, atr, node_element, tag = tag, as_xml = as_xml)
}

aa <- node_set_element("name", 1:3,
                      list(list(id = "a"), list(id = "b"),
                           list(id = "c")))

length(aa)
as.character(aa[[2]])

## alt. si partial tag, as_xml
node_set_element <- function(fun_element, val, atr) {
    purrr::map2(val, atr, fun_element)
}

## -- construir nodos con hijos
##    elaborando listas y luego as_xml_document

## sólo argumentos val y atr, pasados en lista
node_nodes <- function(fun_node, arg) {
    map2(fun_node, arg, function(f, x) {
        if (is.function(f)) {
            f(x$val, x$atr)
        } else {
            node_nodes(f, x)
        }})
}

## abstracción
## lista de funciones de los nodos hijos pasada como argumento
node_padre_hijo <- function(fun_hijo, padre, hijo) {
    ## validar argumentos
    nf <- node_nodes(fun_hijo, list(hijo))
    if (filled_list(padre["atr"])) {
        atr <- c(padre$atr, list(names = names(nf[[1]])))
        attributes(nf[[1]]) <- atr
    }
    nf
}



node_name <- purrr::partial(node_element, tag = "name",
                            as_xml = FALSE)
node_open <- purrr::partial(node_element, tag = "open",
                            as_xml = FALSE)
node_visibility <- purrr::partial(node_element, tag = "visibility",
                                  as_xml = FALSE)
node_snippet <- purrr::partial(node_element, tag = "Snippet",
                               as_xml = FALSE)
node_style_url <- purrr::partial(node_element, tag = "styleUrl",
                            as_xml = FALSE)
node_value <- purrr::partial(node_element, tag = "value",
                             as_xml = FALSE)
node_display_name <- purrr::partial(node_element,
                                    tag = "displayName",
                                    as_xml = FALSE)



node_coordinates <- function(x, y) {
    node_element(tag = "coordinates", val = paste(x, y, sep = ","),
                 as_xml = FALSE)
}

node_point <- function(x, y) {
    list(Point = node_coordinates(x, y))
}

node_display_name_cdata <- function(val, atr) {
    node_element(tag = "displayName",
                 val = paste0("<![CDATA[", val, "]]>"),
                 as_xml = FALSE)
}

## displayName?
node_data <- function(value, data_name = list(), disp_name = "") {
    ## atr_data debe ser lista nombrada
    if (nzchar(disp_name)) {
        node_padre_hijo(list(Data = list(node_value,
                                         node_display_name)),
                        list(val = "", atr = data_name),
                        list(list(val = value, atr = list()),
                             list(val = disp_name, atr = list())))

    } else {
        node_padre_hijo(list(Data = node_value),
                        list(val = "", atr = data_name),
                        list(val = value, atr = list()))
    }
}

aa <- node_data(10, list(name = "si"))
fuu(aa)

aa <- node_data(10, list(name = "si"), "$dato")

node_data_set <- function(values, data_names) {
    ## atrs_data: lista de listas
    ## values: lista
    node_set_element(node_data, values, data_names)
}


aa <- node_data_set(list(1, 2, 3), list(list(name = "a"),
                                        list(name = "b"),
                                        list(name = "c")))

## displayNames?
node_extended_data <- function(values, data_names) {
    list(ExtendedData = map2(values, data_names, node_data))
}

aa <- node_extended_data(list(1, 2, 3),
                         list(list(name = "a"),
                              list(name = "b"),
                              list(name = "c")))

## recibe lista con los datos de un lugar
## - el nombre del dato en la lista sirve para mapear
##   el dato con el nodo hijo
## - para construir ExtendedData, debe venir la lista
##   de los valores y del atributo name del nodo Data
## - no pueden faltar las coordenadas
## - algunos como visibility se dan por default
node_placemark <- function(..., id = "") {
    x <- list(...)
    nx <- intersect(c("name", "open", "visibility", "snippet",
                      "description", "style_url", "extended_data",
                      "coordinates"), names(x))
    ## si nd es vacío, terminar
    ## si coordenadas x, y no en la lista, terminar
    
    pm <- node_element("Placemark", atr = list(id = id))

    xm <- node_point(x$coordinates$x, x$coordinates$y)
    xml_add_child(pm, as_xml_document(xm))

    if (is.element("extended_data", nx)) {
        xd <- lapply(names(x$extended_data), function(x){
            list(name = x)})
        names(x$extended_data) <- NULL
        xm <- node_extended_data(x$extended_data, xd)
        xml_add_child(pm, as_xml_document(xm))
    }
    
    ## id <- which(c("name", "open", "visibility", "snippet",
    ##               "description", "style_url",
    ##               "extended_data", "coordinates") %in% names(x))
    if (is.element("name", nx)) {
        xml_add_child(pm, as_xml_document(node_name(x[["name"]])))
    }

    if (is.element("open", nx)) {
        xml_add_child(pm, node_element("open", x[["open"]]))
    }

    if (is.element("visibility", nx)) {
        xml_add_child(pm, node_element("visibility",
                                       x[["visibility"]]))
    }

    if (is.element("snippet", nx)) {
        xml_add_child(pm, node_element("snippet",
                                       x[["snippet"]]))
    }

    if (is.element("description", nx)) {
        xml_add_child(pm, node_element("description",
                                       x[["description"]]))
    }

    if (is.element("style_url", nx)) {
        xml_add_child(pm, node_element("styleUrl",
                                       x[["style_url"]]))
    }

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
node_folder <- function(id = "", name = "", open = 0L,
                        visibility = 0L, snippet = "",
                        style_url = "", dpm = NULL) {
                        ## coordinates = c("x", "y"),
                        ## extended_data = "",
                        ## names_data = "") {

    x <- list(Folder = structure(list(node_name(name),
                                      node_open(open),
                                      node_visibility(visibility),
                                      node_snippet(snippet),
                                      node_style_url(style_url)),
                                 id = id)) %>%
        as_xml_document()

    ## si dpm no null
    ## - coordinates existen
    ## si nzchar(extended_data) -> dpm no es null,
    ##   y mismos elementos en names_data
    ## construye los place_mark
    if (!is.null(dpm)) {
        ## prepara df para placemark
        ## ii <- names(dpm) %in% coordinates
        ## names(dpm)[which(ii)] <- c("x", "y")
        ## ii <- names(dpm) %in% extended_data
        ## names(dpm)[which(ii)] <- names_data
        
        ## dpm["coordinates"] <- pmap(dpm[, coordinates], list)
        ## dpm["extended_data"] <- pmap(dpm[, names_data], list)
        
        pm <- pmap(dpm, node_placemark)
        for(z in pm) xml_add_child(x, z)
    }
    invisible(x)
}


