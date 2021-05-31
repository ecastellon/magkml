# -*- encoding: utf-8 -*-

## --- funciones para generar KML puntos seguimiento ---
source("c:/eddy/code/r/magkml/tests/segut.r", encoding = "UTF-8")

## <<- directorio del mes ->>

#' directorio según avance: nombres productor, finca, etc.
#' más los controles de cuestionario: rechazos, pendientes, etc.
#' más datos complementarios
#' se juntan en data.frame DFM y se guardan en archivo ADA

#' !!! modificar si cambiaran
cam <-  c("quest", "c5000", "copiade", "nombreproductor", "c002",
          "c003", "telefono", "ciudadlocal", "ptoreferencia",
          "dir_productor", "c004", "c005", "name_expagrp",
          "ciudadfinca", "ptoreffinca", "dir_explagrop", "nombretec")

z <- datos_directorio(cam)

y <- datos_complementarios() #!!! a conveniencia

z %<>% left_join(y)

meta(z) <- glue::glue("directorio-{AME}") # metadatos data.frame
save_df(z, DFM, file = ADA)

## <<- prepara los datos de los puntos ->>

## !! sustituto para pruebas
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
           dirfinca  = "dir_explagrop")
##

#' datos leídos de base de datos se asocian a los puntos (coordenadas)
#' para visualización espacial

## z <- get_dff(DFM, ADA)  # leídos de base de datos

read_off(pun, file = KML) # coordenadas y asig. municipios

x <- datos_puntos(z, sf::st_drop_geometry(pun))

#' !!! ajuste a datos complementarios si punto no se ha ubicado
x["giro"] <- match_sustituir(x$punto, z$quest, z$giro,
                             si_na = "n.d")

## <<- construir nodos KML ->>

## -- resumen de avance delegación y nacional --
## folder cuyos placemark son las delegaciones
## más el placemark "nacional"

pd <- nodos_delegaciones(x)

pn <- nodo_nacional(x)
er <- estilos_resumen()

km <- kml_resumen_nac(pd, pn,
                      es = er,
                      file.path("c:/eddy/code/web/sisea",
                                "ava-del.kml"))

## --- resumen delegación y puntos delegación ---

## placemark para cada punto más el de la delegación
## los puntos agrupados en folder por municipio


## para agregar imágenes? ponerlas en description y luego referenciar
## la description en nodo text

#' prepara los datos de los puntos

cc <- c("delegacion", "tecnico", "control", "departamento", "municipio",
        "localidad", "finca", "dirfinca", "giro")

#' nombres de colores puntos para href estilo
ss <- c("comp", "noag", "inco", "noen", "rech", "inac", "pend")

#' falta description si foto

xy <- coord_lista(pun)
nt <- lapply(xy, node_look, alt = 100L, rng = 1000L, tlt = 13L)

w <- data.frame(name = x$punto,
                municipio = x$municipio,
                visibility = 1L,
                styleUrl = "#pend",
                ExtendedData = datos_lista(x, cc) %>% I,
                LookAt = I(nt),
                coordinates = I(xy))

hr <- paste0("#", ss)
w["styleUrl"] <- remplazar(w$styleUrl, w$name, z$quest, hr[z$c5000])

dn <- list(delegacion   = "Delegación",
           tecnico      = "Técnico",
           control      = "Control",
           departamento = "Departamento",
           municipio    = "Municipio",
           localidad    = "Localidad",
           finca        = "Finca",
           dirfinca     = "Dirección",
           giro         = "Giro")

## folder puntos

## folder delegación
aa <- filter(w, x$dpt == 70)

## names(aa)
## bb <- split(aa, aa$municipio)
## names(bb)

## y <- purrr::imap(bb, folder_puntos, disp=dn)


es <- c(er["del"], estilos_puntos())
fc <- folder_delegacion(aa, "Granada", pd[["GR"]],
                        file = file.path("c:/eddy/code/web/sisea",
                                         "granada.kml"),
                        display = dn,
                        estilos = es, visibility = 1L)

## kk <- kml_doc(estilos = estilos_puntos(),
##               folders = list(fc))

## write_xml(kk, file.path("c:/eddy/code/web/sisea",
##                         "granada.kml"), encoding = "UTF-8")

v <- split(w, x$municipio)

u <- purrr::map2(v, names(v), function(x, y, vis, dis) {
    node_folder(name = y, data_pm = x, visibility = vis,
                displayName = dis)
}, vis = 0L, dis = dn)

