## -*- encoding: utf-8 -*-

## -- crear archivos kml --

## comprobar el crs del archivo con las coordenadas
## transformar de UTM a lon-lat

#' Elemento
#' @description Construye un elemento XML
#' @details No valida la gramática del nombre del elemento
#' @param tag character: nombre del elemento
#' @param val character o numeric: valor del elemento
#' @param atr lista: lista de con los atributos
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

#' Elementos
#' @description Lista de elementos con mismo nombre
#' @details Utiliza purrr::map2 para generar un grupo de elementos
#' @param tag character: nombre del elemento
#' @param val lista de character o numeric: valores de los elementos
#' @param atr lista: lista de listas con atributos
#' @param as_xml logical: devuelve lista u objeto xml_node (TRUE
#'     por omisión)
#' @return lista de listas u objetos xml_node
#' @examples
#' aa <- node_set_element("name", 1:3,
#'                         list(list(id = "a"), list(id = "b"),
#'                              list(id = "c")))
#' @export
node_set_element <- function(tag = character(), val = "", atr = list(),
                                as_xml = TRUE) {

    stopifnot("tag no-vale" = filled_char(tag) && is_scalar(tag),
              "val no-vale" = filled_char(val) || filled_num(val),
              "atr no-vale" = is.list(atr) && length(val) == length(atr))

    invisible(purrr::map2(val, atr, node_element, tag = tag,
                          as_xml = as_xml))
}

#' Elementos
#' @description Lista de elementos con mismo nombre
#' @details Alternativa a \code{node_set_element} si se utiliza
#'     \code{purrr::partial} para inicializar el parámetro \code{tag}
#'     o \code{as_xml}
#' @param fun_element function: función que determina al elemento
#' @param val lista de character o numeric: valores de los elementos
#' @param atr lista: lista de listas con atributos
#' @param as_xml logical: devuelve lista u objeto xml_node (TRUE por
#'     omisión)
#' @return lista u objeto xml_node
#' @examples
#' fufu <- purrr::partial(node_element, tag = "name")
#' aa <- node_set_element_p(fufu, 1:3,
#'                          list(list(id = "a"), list(id = "b"),
#'                               list(id = "c")))
#' @export
node_set_element_p <- function(fun_element, val, atr) {
    
    stopifnot("fun no-vale" = is.function(fun_element),
              "val no-vale" = filled_char(val) || filled_num(val),
              "atr no-vale" = is.list(atr) && length(val) == length(atr))

    invisible(purrr::map2(val, atr, fun_element))
}

## -- construir nodos con hijos
##    elaborando listas y luego as_xml_document

## sólo argumentos val y atr, pasados en lista

#' Elementos anidados
#' @description Nodos anidados en otros
#' @details 
#' @param fun_node lista de funciones
#' @param arg lista del par \code{val} (valor nodo) y \code{atr} (lista
#'     de atributos del nodo)
#' @return lista de listas o de objetos xml_node
#' @examples
#' @keywords internal
nest_node <- function(fun_node, arg) {
    map2(fun_node, arg, function(f, x) {
        if (is.function(f)) {
            f(x$val, x$atr)
        } else {
            nest_node(f, x)
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
    
    invisible(nf)
}

## node_name <- purrr::partial(node_element, tag = "name",
##                             as_xml = FALSE)
## node_open <- purrr::partial(node_element, tag = "open",
##                             as_xml = FALSE)
## node_visibility <- purrr::partial(node_element, tag = "visibility",
##                                   as_xml = FALSE)
## node_snippet <- purrr::partial(node_element, tag = "Snippet",
##                                as_xml = FALSE)
## node_style_url <- purrr::partial(node_element, tag = "styleUrl",
##                             as_xml = FALSE)
## node_value <- purrr::partial(node_element, tag = "value",
##                              as_xml = FALSE)
## node_display_name <- purrr::partial(node_element,
##                                     tag = "displayName",
##                                     as_xml = FALSE)

## displayName?
node_data2 <- function(value, data_name = list(), disp_name = "") {
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


## aa <- node_data(10, list(name = "si"))
## fuu(aa)

## aa <- node_data(10, list(name = "si"), "$dato")

node_data_set <- function(values, data_names) {
    ## atrs_data: lista de listas
    ## values: lista
    node_set_element(node_data, values, data_names)
}


## aa <- node_data_set(list(1, 2, 3), list(list(name = "a"),
##                                         list(name = "b"),
##                                         list(name = "c")))


## displayNames?
node_extended_data2 <- function(values, data_names) {
    list(ExtendedData = map2(values, data_names, node_data))
}

## aa <- node_extended_data(list(1, 2, 3),
##                          list(list(name = "a"),
##                               list(name = "b"),
##                               list(name = "c")))

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

#' Coordenadas
#' @description Produce el elemento \code{coordinates}
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
#' @description Produce el elemento \code{Point}
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

## disp_names es lista nombrada con el nombre del Data
## es pasado como argumento suplementario para evitar
## tener que pasarlo en columnas del data.frame con el que
## se construyen los Placemark. Puede ser xml_cdata o character

#' Data-node
#' @description Construye elemento Data
#' @details Ver capítulo "Datos personalizados" en la documentación
#'     oficial. Un elemento Data necesariamente tiene un nodo hijo
#'     (<value></value>) con el valor del dato, y de manera opcional
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
#' @description Construye un elemento ExtendedData
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

    d <- map2(data, names(data), node_data, disp_name = dname)

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
#' @description Construye un elemento Placemark
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
#' @param ...
#' @param id character o numeric: valor del atributo id de Placemark
#' @return xml_node
#' @export
#' @examples
#' node_placemark(coordinates = list(x = -84.4, y = 10.12))
#' node_placemark(coordinates = list(x = -84.4, y = 10.12),
#'                extended_data = list(area = 3, ciudad = "Ocotal"),
#'                display_name = list(area = "", ciudad = "Ciudad"))
node_placemark <- function(..., id = "") {
    x <- list(...)
    nx <- intersect(c("name", "open", "visibility", "snippet",
                      "description", "style_url", "extended_data",
                      "display_name", "coordinates"), names(x))

    stopifnot("falta nodo" = filled_char(nx),
              "sin coordenadas" = en("coordinates", nx))
        
    pm <- node_element("Placemark", atr = list(id = id))

    p <- node_point(x$coordinates$x, x$coordinates$y)
    xml_add_child(pm, p)

    if (is.element("extended_data", nx)) {
        ## xd <- lapply(names(x$extended_data), function(x){
        ##     list(name = x)})
        ## names(x$extended_data) <- NULL
        ## x$extended_data es una lista de lista
        ## display_name es lista con tantos elementos
        ## como datos
        xm <- node_extended_data(x$extended_data, x$display_name)
        xml_add_child(pm, xm)
    }
    
    ## id <- which(c("name", "open", "visibility", "snippet",
    ##               "description", "style_url",
    ##               "extended_data", "coordinates") %in% names(x))
    if (is.element("name", nx)) {
        xml_add_child(pm, node_element("name", x$name))
    }

    if (is.element("open", nx)) {
        xml_add_child(pm, node_element("open", x$open))
    }

    if (is.element("visibility", nx)) {
        xml_add_child(pm, node_element("visibility",
                                       x$visibility))
    }

    if (is.element("snippet", nx)) {
        xml_add_child(pm, node_element("Snippet",
                                       x$snippet))
    }

    w <- NULL
    if (is.element("description", nx)) {
        w <- NULL
        if (inherits(x$description, "xml_cdata")) {
            w <- node_element("description")
            xml_add_child(w, x$description)
        } else {
            if (filled_char(x$description) && nzchar(x$description)) {
                w <- node_element("description", x$description)
            }
        }

        if (inherits(w, "xml_node")) {
            xml_add_child(pm, w)
        }
    }

    if (is.element("style_url", nx)) {
        xml_add_child(pm, node_element("styleUrl",
                                       x$style_url))
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
#' @examples
node_folder <- function(id = "", name = "", open = 0L,
                        visibility = 0L, snippet = "",
                        data_pm = NULL,
                        display_name = list()) {
                        ## coordinates = c("x", "y"),
                        ## extended_data = "",
                        ## names_data = "") {

    ## x <- list(Folder = structure(list(node_name(name),
    ##                                   node_open(open),
    ##                                   node_visibility(visibility),
    ##                                   node_snippet(snippet)),
    ##                              id = id)) %>%
    ##     as_xml_document()

    x <- node_element("Folder", atr = list(id = id))
    if (nzchar(name)) {
        xml_add_child(x, node_element("name", name))
    }
    xml_add_child(x, node_element("open", open))
    xml_add_child(x, node_element("visibility", visibility))
    if (nzchar(snippet)) {
        xml_add_child(x, node_element("Snippet", snippet))
    }
    ## si dpm no null
    ## - coordinates existen
    ## si nzchar(extended_data) -> dpm no es null,
    ##   y mismos elementos en names_data
    ## construye los place_mark
    if (!is.null(data_pm)) {
        ## prepara df para placemark
        ## ii <- names(dpm) %in% coordinates
        ## names(dpm)[which(ii)] <- c("x", "y")
        ## ii <- names(dpm) %in% extended_data
        ## names(dpm)[which(ii)] <- names_data
        
        ## dpm["coordinates"] <- pmap(dpm[, coordinates], list)
        ## dpm["extended_data"] <- pmap(dpm[, names_data], list)
        
        pm <- pmap(data_pm, node_placemark, display_name = display_name)
        for(z in pm) xml_add_child(x, z)
    }
    invisible(x)
}


## ...: name, Snippet, visibility, open

#' KML root
#' @description
#' @param ...
#' @return xml_node
#' @export
#' @examples
kml_root <- function(...) {

    ##nodes <- c("name", "Snippet", "visibility", "open")
    z <- list(list(name = list("root")),
              list(Snippet = list("root")),
              list(visibility = list(0)),
              list(open = list(0)))
    
    y <- list(...)
    if (filled_list(y)) {
        z <- remplazar(z, names(z), names(y), y)
        ## ny <- names(y)
        ## iy <- which(ny %in% nodes)
        ## if (filled(iy)) {
        ##     y <- lapply(iy, function(x) y[x])
        ##     iz <- which(nodes %in% ny)
        ##     z[iz] <- y
        ## }
    }

    w <- list(Document = structure(z, id = "id_root"))
    ##if (as_xml)
    w <- as_xml_document(w)
    invisible(w)
}

#' KML document
#' @description Construye el documento KML
#' @param estilos lista con elementos de estilos de uso común en el
#'     documento
#' @param ... parámetros adicionales con los valores de los elementos
#'     hijos del elemento Document
#' @return xml_node
#' @export
kml_doc <- function(estilos = list(), ...) {
    x <- list(...)

    w <- node_element("Document", list(id = "root"))
    if (filled_list(x) || filled_list(estilos)) {
        
        if (filled_list(x)) {
        }
        
        if (filled_list(estilos)) {
            ## deben ser xml_node
            for (n in estilos) {
                xml_add_child(w, n)
            }
        }
    }
    
    k <- xml_new_root("kml",
                       xmlns = "http://www.opengis.net/kml/2.2")
    xml_add_child(k, w)
    
    invisible(k)
}


## - KML delegaciones
WD <- "c:/encuestas/ciclo2021"

list_off(file.path(WD, "deleg.rda"))
read_off(y, file = file.path(WD, "deleg.rda"))

## names(y)
## [1] "dpt"          "departamento" "ciudad"   "xutm"  "yutm"        
## [6] "geometry"     "puntos"       "asignado"

xy <- sf::st_coordinates(y) %>% as.data.frame %>%
    set_names(tolower(names(.)))

x <- sf::st_drop_geometry(y) %>%
    select(departamento, ciudad, puntos, asignado) %>%
    bind_cols(xy)

z <- rename(x, name = departamento,
            snippet = ciudad)

z[["coordinates"]] <- pmap(z[,c("x", "y")], list)
z[["extended_data"]] <- pmap(z[,c("puntos", "asignado")], list)
##z[["display_name"]] <- rep(list(puntos = "pun", asignado = "asi"),
                           each = 17)
##z[["display_name"]] <- NULL
z[["style_url"]] <- "#stym"

ig <- "http://maps.google.com/mapfiles/kml/pal3"
s1 <- sty_ico(url = file.path(ig, "icon31.png"),
              pos = list(x = 0.5, y = 0.5,
                         xunits = "fraction",
                         yunits = "fraction"),
              escala = 0.8, as_xml = TRUE)

s2 <- sty_ico(url = file.path(ig, "icon23.png"),
              pos = list(x = 0.5, y = 0.5,
                         xunits = "fraction",
                         yunits = "fraction"),
              escala = 1.0, as_xml = TRUE)

sb <- sty_lab(as_xml = TRUE)

sn <- sty_sty(id = "norm", icon = s1, label = sb)
sh <- sty_sty(id = "dest", icon = s2, label = sb)
sm <- sty_map(id = "stym", "norm", "dest", as_xml = TRUE)

## kd <- node_element("Document", atr = list(id = "root"))
kd <- kml_root()
xml_add_child(kd, sn)
xml_add_child(kd, sh)
xml_add_child(kd, sm)


nf <- node_folder(name = "Delegaciones", visibility = 1L,
                  dpm = z,
                  display_name = list(puntos = xml_cdata("<i>pun</i>"),
                                      asignado = xml_cdata("<b>asi</>")))

xml_add_child(kd, nf)


km <- xml_new_root("kml",
                   xmlns = "http://www.opengis.net/kml/2.2")
xml_add_child(km, kd)
write_xml(km, "c:/eddy/code/web/sisea/dele.kml")
