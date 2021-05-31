# -*- coding: utf-8 -*-

## -- tests --

#' length
#' @description vector has length equal zero
#' @param x vector
#' @return logical
#' @keywords internal
is_vacuo <- function(x) {
    length(x) == 0L
}

#' length
#' @description vector has length equal zero
#' @param x vector
#' @return logical
#' @keywords internal
is_empty <- function(x) {
    length(x) == 0L
}

#' Escalar
#' @description Es vector con un elemento?
#' @param x vector
#' @return logical
#' @keywords internal
is_scalar <- function(x) {
    length(x) == 1L
}

#' Escalar
#' @description Es vector vacío o con un elemento?
#' @param x vector
#' @return logical
#' @keywords internal
is_scalar0 <- function(x) {
    length(x) <= 1L
}

#' length
#' @description vector has length greater than zero?
#' @param x vector
#' @return logical
#' @keywords internal
#' @author eddy castellón
filled <- function(x) {
    length(x) > 0
}

#' character type
#' @description vector is of character type and has elements?
#' @param x vector
#' @return logical
#' @keywords internal
filled_char <- function(x) {
    is.character(x) && length(x)
}

#' numeric mode
#' @description vector is of numeric mode and has elements?
#' @param x vector
#' @return logical
#' @keywords internal
filled_num <- function(x) {
    is.numeric(x) && length(x)
}

#' integer type
#' @description vector is of integer type and has elements?
#' @param x vector
#' @return logical
#' @keywords internal
filled_int <- function(x) {
    is.integer(x) && length(x)
}

#' logical type
#' @description vector is of logical type and has elements?
#' @param x vector
#' @return logical
#' @keywords internal
filled_log <- function(x) {
    is.logical(x) && length(x)
}

#' list-no-vac
#' @description vector is of logical type and has elements?
#' @param x vector
#' @return logical
#' @keywords internal
filled_list <- function(x) {
    is.list(x) && length(x)
}

#' valida-name
#' @description Valida nombres de variables
#' @param x vector
#' @return logical
#' @keywords internal
is_name <- function(x) {
    length(x) && identical(x, make.names(x))
}

#' nombre-scalar
#' @description Valida nombre escalar
#' @param x vector
#' @return logical
#' @keywords internal
is_scalar_name <- function(x) {
    length(x) == 1L && identical(x, make.names(x))
}

## --- helper func.

#' Lon-Lat
#' @description Proyección es lon-lat?
#' @param x Objeto clase sf
#' @return logical
#' @export
es_lonlat <- function(x) {
    stopifnot("arg. x inválido" = inherits(x, "sf"))

    ## st_is_longlat
    sf::st_crs(x)$epsg == 4326
}

#' Proyectar lon-lat
#' @description Reproyecta a lon-lat
#' @param x Objeto de clase sf
#' @return objeto de clase sf
#' @export
proyectar_lonlat <- function(x) {
    stopifnot("arg. x inválido" = inherits(x, "sf"))
    ## posible error si x no tiene asignada proyección
    if (!es_lonlat(x)) {
        x <- sf::st_transform(x, crs = 4326)
    }
    invisible(x)
}

#' Coordenadas
#' @description Construye lista con la listas de coordenadas
#' @details Extrae las coordenadas de un objeto sf y las convierte en
#'     una lista cuyos elementos son listas tipo list(x = , y =)
#' @param x Objeto de clase sf
#' @return lista con listas de coordenadas x, y
#' @export
coord_lista <- function(x) {
    stopifnot("arg. x inválido" = inherits(x, "sf"),
              "arg. x no son puntos" = all(sf::st_is(x, "POINT")))

    xy <- sf::st_coordinates(x) %>% as.data.frame %>%
        set_names(c("x", "y")) %>%
        purrr::pmap(list)

    invisible(xy)
}

#' Lista datos
#' @description Construye lista cuyos elementos son listas de datos
#' @details con los datos de algunas variables
#' @param df data.frame
#' @param vb character o integer: columnas seleccionadas del
#'     data.frame. Cuando sin elementos, seleccionadas todas.
#' @return lista
#' @export
datos_lista <- function(df, vb = character()) {
    stopifnot("arg. df inválido" = is.data.frame(df) && nrow(df) > 0,
              "arg. vb inválido" = is.character(vb) || is.integer(vb))

    if (inherits(df, "sf")) {
        df <- sf::st_drop_geometry(df)
    }

    if (filled(vb)) {
        df <- df[, vb]
    }
    
    x <- purrr::pmap(df, list)
    invisible(x)
}

#' Ícono-google
#' @description Url de íconos google-earth
#' @details Los íconos están agrupados en "paletas". El argumento al
#'     parámetro pal puede ser uno de pal2, pal3, pal4, pal5 o
#'     shapes. Vea \code{http://kml4earth.appspot.com/icons.html} para
#'     escoger.
#' @param pal character: paleta de íconos; "pal3" por defecto
#' @param ico character: nombre del archivo (sin extensión) que
#'     corresponde al ícono
#' @return character
#' @export
#' @examples
#' url_google_ico("pal3", "incon31")
url_google_ico <- function(pal = "pal3", ico = character()) {
    file.path("http://maps.google.com/mapfiles/kml", pal,
              paste0(ico, ".png"))
}

#'BGR-color
#' @description Convierte RGB a BGR
#' @details En KML los canales de los colores se especifican en la
#'     secuencia ABGR, donde A es el canal "alpha", B el azul, G el
#'     verde y R el rojo
#' @param x character: La secuencia RGB de caracteres que definen el
#'     número hexadecimal del color
#' @return character
#' @export
BGR <- function(x = character()) {
    stopifnot("arg. x inválido" = filled_char(x) && is_scalar(x))
    ## asegurar que al menos 6 caracteres
    sub("(\\W?)(\\w{2})(\\w{2})(\\w{2})", "\\4\\3\\2", x)
}

#' %in%
#' @description Cuáles elementos de un vector hacen pareja con los
#'     elementos de otro vector
#' @details Alias del operador infijo \code{%in%}
#' @param x vector
#' @param y vector
#' @return logical
#' @keywords internal
en <- function(x, y) match(x, y, nomatch = 0) > 0

#' buscar-remplazar
#' @description Busca elementos de un vector en otro, y remplaza con
#'     otro donde haya un match.
#' @details Hace un match del arg. 'busca' en el arg. 'buscaen'. Los
#'     elementos del arg. 'remplazo' donde la función match no
#'     devuelva NA, remplazan los correspondientes del arg. 'x'. El
#'     número de elementos del arg. 'x' debe ser igual al del
#'     arg. 'busca', y los del arg. 'buscaen' a los del
#'     arg. 'remplazo'. El modo del arg. 'x' debe ser igual al de
#'     'remplazo' (excepto cuando arg. 'x' es objeto NULL), y el modo
#'     del arg. 'busca' al de 'buscaen'.
#'
#'     El arg. 'x' es NULL por omisión. En este caso arg. 'x' se
#'     inicializa a vector con igual número de elementos de
#'     arg. 'busca' y mismo modo que arg. 'remplazo'. Los elementos de
#'     arg. 'x' son ceros o NA, según lo diga el arg. 'toNA'. Son NA
#'     si arg. 'toNA' es TRUE (por omisión).
#' @param x vector o NULL (por omisión)
#' @param busca vector con los elementos a buscar
#' @param buscaen vector donde se buscan los elementos
#' @param remplazo vector con los elementos que remplazarán los
#'     correspondientes en 'x'
#' @param msg TRUE por omisión; FALSE suprime mensajes de advertencia
#' @param toNA logical: TRUE por omisión.
#' @return vector
#' @examples
#' x <- letters[1:4]
#' y <- 8:1
#' z <- letters[1:8]
#' (remplazar(busca = x, buscaen = z, remplazo = y))
#' w <- 1:4
#' (remplazar(w, x, z, y))
#' @keywords internal
#' @author eddy castellón
remplazar <- function(x = NULL, busca, buscaen, remplazo,
                      msg = TRUE, toNA = TRUE) {
    stopifnot(exprs = {
        "arg. incompat." = filled(buscaen) && filled(remplazo) &&
            length(buscaen) == length(remplazo)
        "arg. incompat." = filled(busca) &&
            mode(busca) == mode(buscaen)
        "arg. x inadmisible" = is.null(x) ||
            (length(x) == length(busca) &&
                mode(x) == mode(remplazo))
    })

    if (is.null(x)) {
        x <- vector(mode(remplazo), length(busca))
        if (toNA) {
            is.na(x) <- seq_along(x)
        }
    }

    mm <- match(busca, buscaen)

    ii <- !is.na(mm)
    if (any(ii)) {
        x[ii] <- remplazo[mm[ii]]
        if (msg) {
            message("... ", sum(ii), " remplazos !!!")
        }
    } else {
        if (msg) {
            message("... ningún remplazo !!!")
        }
    }

    invisible(x)
}
