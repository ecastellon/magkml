# -*- coding: utf-8 -*-

## - KML avance puntos
## estilos de los puntos
##   - color del ícono según código de control
##   - ícono, raqueta simple color blanco
##   - estilo de etiquetas el mismo

sb <- sty_lab(escala = 0.85,
              color = "FF61AEFD")

## estilos íconos
col <- RColorBrewer::brewer.pal(10, "RdYlGn") %>%
    extract(c(9, 1, 8, 4, 3, 2)) %>%
    vapply(BGR, "") %>%
    c("FFFFFF") %>%
    paste0("FF", .) %>%
    set_names(c("comp", "noag", "inco", "noen", "rech",
                "inac", "pend"))

es <- map2(col, names(col),
           function(x, y, z) {
               sty_sty(id = y,
                       icon = sty_ico(ico = "raq",
                                      col = "blanco",
                                      escala = 0.9,
                                      color = x),
                       label = z)
           }, z = sb)

## folder
WD <- "c:/encuestas/ciclo2021"

## avance abril
fd <- file.path(WD, "datos", "abr2021.rda")
z <- get_dff(d04, fd)

## coordenadas de los puntos
fp <- file.path(WD, "datos", "puntos2021.rda")
y <- get_off(pun, file = fp) %>%
    proyectar_lonlat()

x <- sf::st_drop_geometry(y)

hr <- paste0("#", names(col))

ccq <- setNames(1:7, hr)

w <- data.frame(name = x$punto,
                coordinates = coord_lista(y) %>% I,
                styleUrl = "#pend")

w["styleUrl"] <- remplazar(w$styleUrl, w$name, z$quest, hr[z$c5000])

nf <- node_folder(name = "Nueva Segovia", visibility = 1L,
                  data_pm = filter(w, x$dpt == 5))


km <- kml_doc(estilos = es,
              folders = list(nf))

write_xml(km, "c:/eddy/code/web/sisea/pun.kml")


## - KML delegaciones

list_off(file.path(WD, "deleg.rda"))
read_off(y, file = file.path(WD, "deleg.rda"))

## names(y)
## [1] "dpt"          "departamento" "ciudad"   "xutm"  "yutm"        
## [6] "geometry"     "puntos"       "asignado"

s1 <- sty_ico(url = url_google_ico(ico = "icon31"),
              pos = list(x = 0.5, y = 0.5,
                         xunits = "fraction",
                         yunits = "fraction"),
              escala = 0.8)

s2 <- sty_ico(url = url_google_ico(ico = "icon23"),
              pos = list(x = 0.5, y = 0.5,
                         xunits = "fraction",
                         yunits = "fraction"),
              escala = 1.0)

sb <- sty_lab()

sn <- sty_sty(id = "norm", icon = s1, label = sb)
sh <- sty_sty(id = "dest", icon = s2, label = sb)
sm <- sty_map(id = "stym", "norm", "dest")

x <- coord_lista(y)
## si no I() cada elemento de la lista x -> variable
z <- data.frame(coordinates = I(x))
z[["ExtendedData"]] <- datos_lista(y, c("puntos", "asignado"))
z[["name"]] <- y$departamento
z[["Snippet"]] <- y$ciudad
z[["styleUrl"]] <- "#stym"

nf <- node_folder(name = "Delegaciones", visibility = 1L,
                  data_pm = z,
                  displayName = list(puntos = xml_cdata("<i>pun</i>"),
                                     asignado = xml_cdata("<b>asi</>")))

km <- kml_doc(name = "aja", estilos = list(sn, sh, sm),
              folders = list(nf))


write_xml(km, "c:/eddy/code/web/sisea/dele.kml")

## --- viejo ---
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
##                           each = 17)
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


fufi <- function(...) {
    x <- list(...)
    return(x[[3]])
    names(x)
}

fufi(a=1,b=2,3)
