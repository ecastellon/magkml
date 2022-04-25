# -*- encoding: utf-8 -*-

## --- Construir los KML de avance del seguimiento ---
source(file.path(Sys.getenv("HOME"), "onedrive/depo/code/skut.r"),
        encoding = "UTF-8")
source(file.path(Sys.getenv("HOME"), "onedrive/depo/code/skay.r"),
        encoding = "UTF-8")

## --- código ---

ACTUALIZAR <- FALSE
KMLAVANAC <- FALSE
KMLPUNTOS <- FALSE

REPLICAS_MES <- c(1, 2, 3, 4)

if ( ACTUALIZAR ) {
    z <- get_data_cspro( ) %>%
        rename(punto = quest,
                dpte = c004,
                mune = c005)
    
    save_df(z, name = DFM, file = ADA)
} else {
    ##z <- get_off_c(DFM, file =  ADA)
}

z <- get_dff(d04,
             file = "c:/encuestas/ciclo2021/datos/abr2021.rda") %>%
    rename(punto = quest,
           dpte = c004,
           mune = c005,
           giro = c050)

z %<>% datos_encuesta() %>% datos_puntos(REPLICAS_MES)

## resumen avance nacional
if (KMLAVANAC) {
    z %<>% preparar_datos("avance", file = KML)
    km <- kml_avance_nac(z, fout = KRN, kmz = TRUE, lon = PNLON,
                         lat = PNLAT, dia = DIA, file = KML)
}

km <- mapa_resumen(d, fecha, fout, kmz, fapo)


#' @param d data.frame: datos boletas levantadas (base de datos)
#' @param fecha character: fecha del reporte
#' @param replica integer: réplicas en la encuesta
#' @param ardap character: ruta archivo con datos de apoyo
#' @param arkml character: ruta archivo KML para guardar
#' @param kmz logical: comprimir KML
resumen_avance <- function(d = NULL, fecha = character( ),
                           replica = 1:4, ardap = KML,
                           arkml = character( ), kmz = TRUE) {
    z <- puntos_geo(replica, ardap)
    if (is.null(d)) {
        
    }
}

#' @param mapa character: tipo de mapa: resumen o puntos
#' @param cobertura character: cobertura del mapa: nacional,
#'     delegación
#' @param fecha character: fecha del reporte
#' @param d data.frame: datos boletas levantadas (base de datos)
#' @param replica integer: réplicas en la encuesta
#' @param ardap character: ruta archivo con datos de apoyo
#' @param arkml character: ruta archivo KML para guardar
#' @param kmz logical: comprimir KML
#' @param cartel character: los datos que se desplegarán en el cartel
#'     emergente cuando "click" en punto
#' @param format_inf function: función que deja a punto los datos que
#'     se mostrarán en el cartel
#' @return kml_doc
kml_avance <- function(mapa = "res", cobertura = "nac",
                       fecha = character(),
                       d = NULL, replica = 1:4,
                       ardap = KML,
                       arkml = "", kmz = TRUE,
                       cartel = character( ),
                       format_dat = datos_ubicacion) {
    cobertura <- tolower(cobertura)
    mapa <- tolower(mapa)
    
    z <- puntos_geo(replica, ardap)
    if (is.null(d)) {
        d <- data.frame(punto = z$punto,
                        c5000 = integer(nrow(z)),
                        dpte = z$dpt,
                        mune = z$mun,
                        codtec = integer(nrow(z)))
    }

    stopifnot("datos más filas que puntos" = nrow(z) >= nrow(d))

    z %<>% left_join(d) %>%
        datos_avance(ardap)
    
    if (mapa == "resumen") {
        km <- kml_avance_nac(z, fout = arkml, kmz = kmz,
                             dia = fecha, fapo = ardap)
    } else {
        dpt <- magmun::departamentos( )
        dele <- dpt$abr
        if (cobertura == "nacional" | cobertura == "delegacion") {
            cobertura <- dele
        } else {
            cobertura <- toupper(cobertura)
        }

        stopifnot(exprs = {
            "mapas > archivos kml" = length(cobertura) <= length(arkml)
            "cobert. inválida" = all(en(cobertura, dele))
            "datos no existen" = all(en(cartel, names(z)))
        })

        z["abr"] <- iniciar_vec(z$dpte, dpt$dpt, dpt$abr)
        y <- filter(z, en(abr, cobertura)) %>%
            format_dat(cartel)
            ##split(.$cobertura)
        km <- y
    }
    invisible(km)
}

df_lleno <- function(x ) TRUE
char_lleno <- function(x ) TRUE
DIA
VBC
TIT

## variables en el cartel todas del mismo tipo: character o num.
## se convertirán en columna de un data.frame
vbas <- setNames(c("tecnico", "delegacion", "municipio", "control"),
                 c("Técnico", "Delegación", "Municipio", "Control"))
names(vbas) <- enc2native(names(vbas))

z <- kml_avance("pun", DIA, cobertura = "nacional",
                arkml = letters[1:18],
                cartel = vbas,
                format_dat = datos_basicos)

y <- filter(z, dpte == 20)

w <- split(y, y$mune) %>% ##extract2(1)
    purrr::map(folder_municipio, cartel = vbas)

nk <- lapply(w$coordinates, node_look, alt = 100, rng = 1000, tlt = 13
             )
nd <- descripcion_puntos(w, variables = vbas )

x <- select(w, one_of(vbas ) ) %>% purrr::pmap(list )
a <- descripcion_punto(x[[1]], titulo = names(vbas ) )
as.character(a )

write_xml(w[[1]], file.path("ciclo2021", "madriz.kml"),
          encoding = "UTF-8")

ep <- sty_puntos(ids = ACC)
kk <- kml_departamento("Madriz", w, "ciclo2021", estilos = ep,
                       visibility = 1, open = 1)

km <- kml_doc(
    visibility = 0,
    estilos = ep,
    folders = list(kk)
)

write_xml(km, file.path("ciclo2021", "madriz.kml" ),
          encoding = "UTF-8")


dim(y)
names(z )
head(z$tecnico )
head(z$abr )
ACC



## !!! los id_mun en los datos
## por municipio (previsión comp. paralelo)
if (KMLPUNTOS) {
    w <- split(z, z$id_mun) %>% #mun donde localizado
        purr::map(folder_municipio)

    ep <- estilos_puntos(ids = ACC)
    d <- type.convert(names(w)) %/% 100L #cod. departamento
    e <- departamentos(locale = FALSE)
    f <- iniciar_vec(d, e$dpt, e$departamento)
    
    x <- split(w, f) %>%
        purrr::imap(kml_departamento, dir = WK, estilos = ep,
                    visibility = 1L, open = 1L)
}
