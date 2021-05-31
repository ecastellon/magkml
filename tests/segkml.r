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
                      file.path(WK, "ava-del.kml"))

## --- resumen delegación y puntos delegación ---

## placemark para cada punto más el de la delegación
## los puntos agrupados en folder por municipio


## para agregar imágenes? ponerlas en description y luego referenciar
## la description en nodo text

#' prepara los datos de los puntos

#' href de estilos de los puntos
ep <- estilos_puntos()
hr <- paste0("#", names(ep))
hr <- match_sustituir(x$punto, z$quest, hr[z$c5000], si_na = "#pend")

#' estilos de punto y delegación
es <- c(er["del"], ep)

#' LookAt
xy <- coord_lista(pun)
nk <- lapply(xy, node_look, alt = 100L, rng = 1000L, tlt = 13L)

#' description ??

#' variables que se visualizarán en "balloon"
#' delegación es departamento al que está asignado adm. el punto
#' departamento y municipio es donde ubicada la UP

cc <- c("delegacion", "tecnico", "control", "departamento", "municipio",
        "localidad", "finca", "dirfinca", "giro")

#' displayName

dn <- list(delegacion   = "Delegación",
           tecnico      = "Técnico",
           control      = "Control",
           departamento = "Departamento",
           municipio    = "Municipio",
           localidad    = "Localidad",
           finca        = "Finca",
           dirfinca     = "Dirección",
           giro         = "Giro")

w <- data.frame(name         = x$punto,
                delegacion   = x$delegacion, #split
                municipio    = x$municipio,  #split
                visibility   = 1L,
                styleUrl     = hr,
                ExtendedData = datos_lista(x, cc) %>% I,
                LookAt       = I(nk),
                coordinates  = I(xy))

## alinear pd en orden con split(w, delegaciones)
## lista de nombres de archivos en mismo orden
## usar pmap para generar todos los archivos

#' !! prueba
aa <- filter(w, x$dpt == 70)

fc <- folder_delegacion(aa, "Granada", pd[["GR"]],
                        file = file.path(WK, "granada.kml"),
                        display = dn,
                        estilos = es, visibility = 1L)

