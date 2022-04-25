## -*-encoding: utf-8 -*-
## --- constantes ---

## - fechas -
FCH <- primer_dia_mes(2021, 6) # !!! mes encuesta

AME <- format(FCH, "%Y%m") # año-mes
MES <- format(FCH, "%m") # mes

HOY <- Sys.time()
DIA <- format(HOY, "%d de %B del %Y") # fecha reporte

## - directorios -
WD <- "c:/encuestas/ganado/datos" # directorio datos R
WK <- "c:/eddy/code/web/sisea" # directorio archivos KML

## archivo con las coordenadas de puntos y delegaciones
## y otros datos complementarios para construir los KML
## list_off(KML) para ver lo que hay
KML <- file.path(WK, "kml-data.rda")

## archivo con KML resumen nacional
KRN <- file.path(WK, "ava-nac.kml") # !!!

PNLON <- -85.0
PNLAT <- 13.0
#' constantes punto de vista visualización del punto
#' opciones por default función node_look
ALT <- 500 # altitude
RNG <- 600000 # range
TLT <- 27 # tilt
ALM <- "clampToGround" # altitudeMode
HDN <- 0 # heading
## puntos
PTALT <- 100  # altitude
PTRNG <- 1000 # range
PTTLT <- 13   # tilt

## archivo R con datos del avance
## iniciales del mes + año
## p.ej. para abril 2021, el archivo será abr2021.rda
ADA <- format(FCH, "%b%Y") %>%
    sub("\\.", "", .) %>%
    paste0(".rda") %>%
    file.path(WD, .)

## data.frame con los datos del directorio
## letra "d" seguida de mes expresado como dos dígitos
## p.ej. si datos de abril DFM igual a "d04"
DFM <- glue::glue("d{MES}") %>% as.character()

## - archivo excel con el diccionario abarcado por un rango con nombre
## una columna con el nombre de la variable (variable)
## otra con el número de caracteres asignados (length)
## otra con la posición del primer caracter (start)
ADI <- "c:/encuestas/ganado/ganado2021/hatodic.xlsx"
## nombre del rango que abarca los datos
TDI <- "dic"

## -- tabla datos encuesta y variables
TAB <- "hato_dict"

## variables que se van a leer base datos
VBB <- c(
    tok_str("quest nombretec nombreproductor name_expagrp"),
    tok_str("c004 c005 ciudadfinca ptoreffinca dir_explagrop c5000"))

VBB <- setNames(VBB, c("punto", "codtec", "productor", "finca",
                "dpte", "mune", "localidad", "ptoref", "dirf",
                "c5000"))
TVB <- c("integer", "integer", "character", "character",
         "integer", "integer", "character", "character", "character",
         "integer")

VBA <- ""
#VBA <- c(giro = "c020")
TVA <- ""
#TVA <- "integer"

VBL <- c(VBB, VBA)
TIP <- c(TVB, TVA )
## variables mostradas en cartel de puntos
VBC <- c(
        "delegacion", "tecnico", "control", "departamento", "municipio",
        "localidad", "finca", "dirfinca", "giro"
)
## título de la variable en el cartel
TIT <- c(
        "Delegación", "Técnico", "Control", "Departamento", "Municipio",
        "Localidad", "Finca", "Dirección", "Giro principal"
)

## - otros
## control de cuestionario y abreviatura
CCC <- setNames(1:7, c(
    "completo", "no-agrícola", "incompleto",
    "no-informante", "rechazo", "no-acceso",
    "pendiente"
    ))

ACC <- c(
    completo = "com", noagricola = "noa", incompleto = "inc",
    noinformante = "noi", rechazo = "rch", inaccesible = "nac",
    pendiente = "pen"
)

## giro principal
## las NA -> 1 y se corren las claves en el procesamiento
GPP <- setNames(1:8, c(
    "n.d", "agrícola", "forestal", "leche",
    "carne", "doble-propósito", "ganado-menor",
    "inactiva"
))

## -- leer los datos de la base de datos CSpro

## --- función de apoyo ---

#' Avance
#' @description Procesa los datos para calcular el porcentaje de
#'     avance por delegación
#' @details El data.frame que se pasa como argumento tiene que
#'     contener: el número de la boleta, los códigos de departamento y
#'     municipio (sin el prefijo del departamento; p.ej 10 en lugar de
#'     5510) de la sede de la finca, el control del cuestionario y el
#'     código del técnico que hizo la entrevista. Esos datos deben
#'     estar en las columnas «punto», «dpte», «mune», «c5000»,
#'     «codtec». El archivo que se pasa como argumento debe contener
#'     los data.frame «pdel» (asignación de puntos a las delegaciones)
#'     y «tec» (código y nombre de los técnicos).
#' @param d data.frame: datos de avance (ver detalles)
#' @param file character: ruta de acceso del archivo con los datos de
#'     las delegaciones y técnicos
#' @return data.frame
datos_avance <- function(d, file) {
    variables <- c("punto", "dpte", "mune", "c5000", "codtec")
    stopifnot(exprs = {
        "arg.z inválido" = df_lleno(d)
        "arg.z columnas inválidas" = all(variables %in% names(d))
    })

    tryCatch({
        ad <- get_dff(pdel, file)
        at <- get_dff(tec, file)
    },
    error = function(e) {
        stop("... ", file, " no tiene datos delegación, técnico")
        }
    )

    ## imputa donde inconsistencia
    mu <- magmun::municipios()
    if (any(ii <- d$mune < 100)) {
        d[ii, "mune"] <- magmun::concatenar_int(d$dpt[ii], d$mune[ii])
    }
    d["c5000"] <- na0(d$c5000)
    d[!en(d$c5000, CCC), "c5000"] <- CCC["pendiente"]
    
    d %<>% mutate(
               delegacion = iniciar_vec(punto, ad$punto,
                                        ad$delegacion),
               tecnico = iniciar_vec(codtec, at$codtec, at$tecnico,
                                     si_na = "desc."),
               departamento = iniciar_vec(mune, mu$mun,
                                      mu$departamento),
               municipio = iniciar_vec(mune, mu$mun,
                                      mu$municipio),
               control = names(CCC)[c5000],
               id_ctrl = ACC[c5000])

    mm <- match(d$mune, mu$mun)
    
    if (any(ii <- is.na(d$delegacion))) {
        d[ii, "delegacion"] <- mu$departamento[mm[ii]]
    }
    if (any(ii <- is.na(d$municipio))) {
        d[ii, "municipio"] <- mu$municipio[mm[ii]]
    }

    invisible(d)
}

#' Datos-básicos
#'
#'
datos_basicos <- function(z, ...) {
    invisible(z)
}

#' Datos-ubicación
#'
#'
#' @param z data.frame: los datos
datos_ubicacion <- function(z, ...) {
    du <- tok_str("productor finca localidad ptoref dirf")
    nm <- names(z)
    stopifnot(exprs = {
        "arg.z inválido" = is.data.frame(z) && nrow(z) > 0
        "arg.z columnas ubicación inválidas" = all(du %in% nm)
    })

    z %<>% mutate(productor = na_char(productor) %>% nombre_propio(),
                  finca = na_char(finca) %>% nombre_propio(),
                  localidad = na_char(localidad) %>% nombre_propio(),
                  ptoref = na_char(ptoref),
                  dirf = na_char(dirf),
                  dirf = paste(ptoref, dirf, sep = " ") %>%
                      tolower())
    invisible(z)
}

#' Datos encuesta
#' @description Procesa los datos del avance de la encuesta
#' @details Modifica los datos del avance que corresponden a cada
#'     "punto", para adaptarlos al formato de presentación en el
#'     cartel que se despliega cuando "click" en la posición
#'     geográfica del "punto".
#'
#'     Los datos básicos son el nombre del productor y los datos de la
#'     localización de la finca: nombre de la finca, departamento,
#'     municipio y localidad de la sede; el control del cuestionario y
#'     el código del técnico que hizo la entrevista. En el d.f, esos
#'     datos deben venir en las columnas «productor», «finca», «dpte»
#'     (código del departamento), «mune» (código del municipio sin el
#'     prefijo del departamento; p.ej 10 y no 5510), «localidad»,
#'     «ptoref» (punto de referencia de la dirección de la finca),
#'     «dirf» (dirección de la finca a partir del punto de
#'     referencia), «c5000» (control del cuestionario), «codtec»
#'     (código del técnico).
#'
#'     A estos datos se agregan los de la delegación y nombre del
#'     técnico. Para eso, el archivo que se pasa como argumento al
#'     parámetro "file" debe contener la correspondencia
#'     punto-delegación (en el d.f «pdel») y tec-nombre del técnico
#'     (en el d.f «tec»)
#' @param z data.frame: los datos "crudos"
#' @param file character: ruta de acceso al archivo con los datos de
#'     las delegaciones y los técnicos
#' @return data.frame
datos_encuesta <- function(z, file) {

    nm <- c(tok_str("productor finca dpte mune localidad"),
            tok_str("ptoref dirf c5000 codtec"))
    stopifnot(exprs = {
        "arg.z inválido" = is.data.frame(z) && nrow(z) > 0
        "arg.z columnas inválidas" = all(names(z) %in% nm )
    })

    tryCatch({
    ad <- get_dff(pdel, file) # asignados delegación
    at <- get_dff(tec, file)  # asignación por técnico
    },
    error = function(e) stop("... ", file,
                             " no tiene datos delegación, técnico")
    )
    
    mu <- municipios()
    z %<>% mutate(
               delegacion = iniciar_vec(
                   punto, ad$punto,
                   ad$delegacion
               ),
               tecnico = iniciar_vec(codtec, at$codtec,
                                     at$tecnico,
                                     si_na = "desc."
                                     ),
               productor = na_char(nombreproductor) %>%
                   nombre_propio(),
               finca = na_char(name_expagrp) %>%
                   nombre_propio(),
               mune = magmun::concatenar_int(dpte, mune),
               departamento = iniciar_vec(mune, mu$mun,
                                      mu$departamento,
                                      si_na = "desc."
                                      ),
               municipio = iniciar_vpec(mune, mu$mun,
                                      mu$municipio,
                                      si_na = "desc."
                                      ),
               localidad = na_char(ciudadfinca) %>%
                   nombre_propio(),
               ptoreffinca = na_char(ptoreffinca),
               dir_explagrop = na_char(dir_explagrop),
               dirfinca = paste(ptoreffinca, dir_explagrop,
                                sep = " "
                                ) %>% tolower(),
               c5000 = cero_na(c5000),
               control = names(CCC)[c5000],
               id_ctrl = ACC[c5000],
               giro = na0(giro) + 1L,
               giro = names(GPP)[giro]
           ) %>%
        select(
            punto, delegacion, tecnico,
            productor, finca, departamento, municipio,
            localidad, dirfinca, giro, control, id_ctrl
        )

    ## si no asignado delegación -> asignado muestra
    ii <- is.na(z$delegacion)
    if (any(ii)) {
        z[ii, "delegacion"] <- z$dpt[ii]
    }

    invisible(z)
}

#' @description Devuelve d.f con los datos de los puntos de la
#'     encuesta.
#' @details Supone que data.frame «pdel» tiene los datos de asignación
#'     administrativa de los puntos a las delegaciones, en las
#'     columnas «punto», «delegacion»; y que el d.f «tec» trae los
#'     datos de los técnicos en las columnas «codtec» (código con el
#'     que digita) y «tecnico» (nombre).
#'
#'     El d.f con los datos de avance contiene las columnas «punto» y
#'     las demás variables que se van a desplegar en el mapa cuando
#'     "click" en el punto. Entre las variables se esperan los datos
#'     de localización de la finca: departamento, municipio,
#'     localidad, dirección, y otros adicionales. Si no hay datos de
#'     avance, no hace falta pasar el d.f a la función.
#' @param rep integer: réplicas incluidas en la muestra del mes
#' @param d data.frame: datos del avance de la encuesta (NULL por
#'     omisión)
#' @param file character: archivo donde guardados data.frame con
#'     asignación de puntos por delegación y técnico.
#' @export
datos_puntos <- function(rep, d = NULL, file = KML) {
    stopifnot("arg.rep inválido" = filled_int(rep))
    tryCatch({
        del <- get_dff(pdel, file) # asignados delegación
        tec <- get_dff(tec, file)  # asignación por técnico
    },
    error = function(e) {
        stop("... no encuentra d.f con los puntos asignados ",
             "por delegación o técnico")
    }
    )

    x <- puntos_geo(rep, file)
    if (is.null(d)) {
        return(x)
    } else {
        x %<>% left_join(d)
    }

    ## limpieza, si boleta no levantada
    if (any(ii <- is.na(x$delegacion))) {
        x[ii, "delegacion"] <- x$dpt[ii]        
    }
    if (any(ii <- is.na(x$tecnico))) x[ii, "tecnico"] <- "desc."

    ## limpieza datos que faltan
    nm <- names(x)
    if (is.element("control", nm) &&
        any(ii <- is.na(x$control))) {
        x[ii, "id_ctrl"] <- ACC["pendiente"]
        x[ii, "control"] <- "pendiente"
    }
    if (is.element("departamento", nm) &&
        any(ii <- is.na(x$departamento))) {
        x[ii, "departamento"] <- x$dpt[ii]
    }
    if (is.element("municipio", nm) &&
        any(ii <- is.na(x$municipio))) {
        x[ii, "municipio"] <- x$mun[ii]
    }

    if (is.element("productor", nm)) {
        x[["productor"]] <- na_char(x$productor, "n.d")
    }
    if (is.element("finca", nm)) {
        x[["finca"]] <- na_char(x$finca, "n.d")
    }
    if (is.element("localidad", nm)) {
        x[["localidad"]] <- na_char(x$localidad, "n.d")
    }
    if (is.element("dirfinca", nm)) {
        x[["dirfinca"]] <- na_char(x$dirfinca, "n.d")
    }
    if (is.element("giro", nm)) {
        x[["giro"]] <- na_char(x$giro, "n.d")
    }
    invisible(x)
}

modificar_nombres <- function(x, y) {
    variables <- c("punto", "dpte", "mune", "c5000", "codtec",
                   "productor", "finca", "localidad", "ptoref",
                   "dirf")

    remplazar(x, x, y, names(y))
}

preparar_datos <- function(d, mapa = "avance", dic_vb = character( ),
                           file) {

    z <- datos_avance(d, file)
    if (mapa == "puntos") {
        
    }
    invisible(z)
}

## prepara el archivo donde van los datos de apoyo
fufu <- function( ) {
    ## excel: opciones->avanzadas->web->codificación:europeo
    ## occ.(windows) (windows-1252) ver iconvlist()


    w <- openxlsx::read.xlsx("c:/encuestas/ciclo2021/codtec202107.xlsx",
                   rows = 1:134, cols = 3:5) %>%
        mutate(tecnico = enc2native(tecnico) %>%
                   magest::nombre_propio() %>%
                   enc2utf8()) %>%
        rename(delegacion = dpt)
    meta(w) <- "códigos-técnicos-202107"
    save_df(w, "tec", file.path(WK, "kml-data.rda"))
}
