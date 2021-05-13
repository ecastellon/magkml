# -*- coding: utf-8 -*-

## - KML avance puntos
## estilos de los puntos
##   - color del ícono según código de control
##   - ícono, raqueta simple color blanco
##   - estilo de etiquetas el mismo

## folder
WD <- "c:/encuestas/ciclo2021"
nom_propio <- magest::a_propio
muni <- magmun::municipios()

u <- iconv(muni$municipio, "UTF-8", "")
muni["municipio"] <- u
u <- iconv(muni$departamento, "UTF-8", "")
muni["departamento"] <- u

## avance abril
ccq <- setNames(1:7, c("completo", "no-agrícola", "incompleto",
                       "no-informante", "rechazo", "no-acceso",
                       "pendiente"))

fd <- file.path(WD, "datos", "abr2021.rda")
z <- get_dff(d04, fd) %>%
    rename(finca = "name_expagrp",
           dirfinca = "dir_explagrop") %>%
    mutate(finca = nom_propio(finca),
           local = nom_propio(ciudadfinca),
           dirfinca = nom_propio(paste(ptoreffinca,
                                       dirfinca,
                                       sep = "; ")),
           mun = magmun::concatenar_int(c004, c005))

y <- get_dff(tecnico, file.path(WD, "deleg.rda"))

z["tecnico"] <- remplazar(NULL, z$nombretec, y$codtec, y$tecnico)
z[is.na(z$tecnico), "tecnico"] <- "desconocido"

z["giro"] <- c("agrícola","pecuario", "forestal",
               "inactiva")[z$c050]
z[is.na(z$giro), "giro"] <- "desconocido"

z["municipio"] <- remplazar(NULL, z$mun, muni$mun, muni$municipio)
z["departamento"] <- remplazar(NULL, z$mun, muni$mun,
                               muni$departamento)
## z[is.na(z$municipio), "municipio"] <- "desconocido"
## z[is.na(z$departamento), "departamento"] <- "desconocido"

ii <- is.na(z$local) || !nzchar(z$local)
z[ii, "local"] <- "desconocido"

z["ccontrol"] <- names(ccq)[z$c5000]

## -- los puntos --
fp <- file.path(WD, "datos", "puntos2021.rda")
y <- get_off(pun, file = fp) %>%
    proyectar_lonlat()

## -- prepara los datos --
x <- sf::st_drop_geometry(y)

## puntos asignados por delegación
w <- get_dff(ptodpt, file.path(WD, "deleg.rda"))
u <- c(ns = 5, ji = 10, mz = 20, es = 25, ch = 30, le = 35,
       mt = 40, bo = 50, mg = 55, my = 60, ct = 65, gr = 70,
       cz = 75, ri = 80, sj = 85, rn = 91, rs = 93)
w["dpt"] <- u[w$dpt]
w["mun"] <- magmun::concatenar_int(w$dpt, 5)
w["delega"] <- remplazar(NULL, w$mun, muni$mun, muni$departamento)

x["delega"] <- remplazar(NULL, x$punto, w$punto, w$delega)

## ubicación, nombre y direcc. finca
## esta secuencia por si en z hay NA
mm <- match(x$mun, muni$mun)

x["municipio"] <- remplazar(NULL, x$punto, z$quest,
                            z$municipio)
ii <- is.na(x$municipio)
x[ii, "municipio"] <- (muni$municipio[mm])[ii]

x["departamento"] <- remplazar(NULL, x$punto, z$quest,
                            z$departamento)
ii <- is.na(x$departamento)
x[ii, "departamento"] <- (muni$departamento[mm])[ii]

x["municipio"] <- remplazar(NULL, x$mun, muni$mun, muni$municipio)
x["departamento"] <- remplazar(NULL, x$mun, muni$mun,
                              muni$departamento)

x["local"] <- remplazar(NULL, x$punto, z$quest, z$local)
x[is.na(x$local), "local"] <- "desconocido"

x["finca"] <- remplazar(NULL, x$punto, z$quest, z$finca)
x[is.na(x$finca), "finca"] <- "desconocido"

x["dirfinca"] <- remplazar(NULL, x$punto, z$quest, z$dirfinca)
x[is.na(x$dirfinca), "dirfinca"] <- "desconocido"

## control cuestionario
x["control"] <- remplazar(NULL, x$punto, z$quest, z$ccontrol)
x[is.na(x$control), "control"] <- "pendiente"

## giro principal
x["giro"] <- remplazar(NULL, x$punto, z$quest, z$giro)
x[is.na(x$giro), "giro"] <- "desconocido"

## tecnico
x["tecnico"] <- remplazar(NULL, x$punto, z$quest, z$tecnico)
x[is.na(x$tecnico), "tecnico"] <- "desconocido"

## -- puntos --
## estilos íconos
col <- RColorBrewer::brewer.pal(10, "RdYlGn") %>%
    extract(c(9, 1, 8, 4, 3, 2)) %>%
    vapply(BGR, "") %>%
    c("FFFFFF") %>%
    paste0("FF", .) %>%
    set_names(c("comp", "noag", "inco", "noen", "rech",
                "inac", "pend"))

## estilo labels
sb <- sty_lab(escala = 0.8,
              color = "FF61AEFD")

es <- map2(col, names(col),
           function(x, y, z) {
               sty_sty(id = y,
                       icon = sty_ico(ico = "raq",
                                      col = "blanco",
                                      escala = 1.0,
                                      color = x),
                       label = z)
           }, z = sb)

## folder puntos
cc <- c("delega", "tecnico", "control", "departamento", "municipio",
        "local", "finca", "dirfinca", "giro")
w <- data.frame(name = x$punto,
                visibility = 0L,
                open = 0L,
                coordinates = coord_lista(y) %>% I,
                ExtendedData = datos_lista(x, cc) %>% I,
                styleUrl = "#pend")

hr <- paste0("#", names(col))
w["styleUrl"] <- remplazar(w$styleUrl, w$name, z$quest, hr[z$c5000])

dn <- list(delega       = "Delegación",
           tecnico      = "Técnico",
           control      = "Control",
           departamento = "Departamento",
           municipio    = "Municipio",
           local        = "Localidad",
           finca        = "Finca",
           dirfinca     = "Dirección",
           giro         = "Giro")

## folder por municipio-departamento
v <- split(w, x$municipio)

u <- purrr::map2(v, names(v), function(x, y, vis, dis) {
    node_folder(name = y, data_pm = x, visibility = vis,
                displayName = dis)
}, vis = 0L, dis = dn)

mm <- match(names(u), muni$municipio)
tt <- split(u, muni$departamento[mm])

ss <- map2(tt, names(tt), function(x, y, z) {
    n <- node_folder(name = y, visibility = z)
    for (e in x) xml_add_child(n, e)
    n
}, z = 0L)

## nf <- node_folder(name = "Nueva Segovia", visibility = 1L,
##                   data_pm = filter(w, x$dpt == 5),
##                   displayName = dn)

## -- Delegaciones --
yy <- get_off(y, file = file.path(WD, "deleg.rda"))

xx <- sf::st_drop_geometry(yy)

## resumen de avance de los puntos asignados
dd <- group_by(x, delega, municipio) %>%
    summarise(asignados     = n(),
              pendiente     = sum(control == "pendiente"),
              levantados    = asignados - pendiente,
              avance        = magest::pct(levantados, asignados),
              completo      = sum(control == "completo"),
              incompleto    = sum(control == "incompleto"),
              noagricola    = sum(control == "no-agrícola"),
              rechazo       = sum(control == "rechazo"),
              noinformante  = sum(control == "no-informante"),
              inaccesible   = sum(control == "inaccesible")) %>%
    ungroup()
## json para prueba tabla html
## uu <- filter(dd, delega == "Managua") %>%
## select(municipio, asignados, pendiente, levantados) %>%
## jsonlite::toJSON()
## cat(uu, file = "c:/eddy/code/web/sisea/mg.json")

ww <- group_by(x, delega) %>%
    summarise(asignados     = n(),
              pendiente     = sum(control == "pendiente"),
              levantados    = asignados - pendiente,
              avance        = magest::pct(levantados, asignados),
              completo      = sum(control == "completo"),
              incompleto    = sum(control == "incompleto"),
              noagricola    = sum(control == "no-agrícola"),
              rechazo       = sum(control == "rechazo"),
              noinformante  = sum(control == "no-informante"),
              inaccesible   = sum(control == "inaccesible")) %>%
    ungroup()

mm <- match(xx$departamento, ww$delega)
xx %<>% bind_cols(ww[mm,])

## estilos delegaciones
## color rojo
## mapa de estilos para estados normal y seleccionado
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

ed <- list(normal = sty_sty(id = "norm", icon = s1,
                            label = sty_lab()),
           selec  = sty_sty(id = "dest", icon = s2,
                            label = sty_lab()),
           mapeo  = sty_map(id = "stym", "norm", "dest"))

## estilos según avance
## escala lineal del ícono a partir de 0.7 hasta 1.1
## cada punto en escala corresponde a 20% en avance
## <=20, 40, 60, 80, >=80
pa <- c(0, 20, 40, 60, 80, 100)            #pct. avance
ei <- setNames(c(0.7, 0.8, 0.9, 1.0, 1.1), #escala ícono
               paste0("p", pa[-1]))

iu <- url_google_ico(ico = "icon31")
hp <- list(x = 0.5, y = 0.5,
          xunits = "fraction",
          yunits = "fraction")

ed <- map2(ei, names(ei),
           function(x, y) {
               sty_sty(id = y,
                       icon = sty_ico(escala = x,
                                      url    = iu,
                                      pos    = hp,
                                      color  = "ff0000ff"),
                       label = sty_lab())
           })

pa <- setNames(pa, paste0("#", c(names(ei), "p100")))

ia <- findInterval(xx$avance, pa, left.open = TRUE,
                   rightmost.closed = TRUE)

## folder
cc <- c("puntos", "asignados", "levantados", "pendiente", "avance",
        "noagricola", "completo", "incompleto", "rechazo",
        "noinformante", "inaccesible")

ww <- data.frame(name         = xx$delega,
                 Snippet      = xx$ciudad,
                 styleUrl     = names(pa)[ia],
                 coordinates  = coord_lista(yy) %>% I,
                 ExtendedData = datos_lista(xx, cc) %>% I)

dn <- list(avance       = "<p style='color:#D73027'>Pct.avance</p>",
           puntos       = "Puntos-muestra",
           asignados    = "Puntos-asignados",
           levantados   = "Boletas-levantadas",
           pendiente    = "Boletas-pendientes",
           noagricola   = "UP-no-agrícola",
           completo     = "Boletas-completa",
           incompleto   = "Boletas-incompleta",
           rechazo      = "Boletas-rechazo",
           noinformante = "Boletas-sin-informante",
           inaccesible  = "Punto-inaccesible")

nf <- node_folder(name = "Delegaciones", visibility = 1L,
                  data_pm = ww,
                  displayName = dn)

## NOT RUN - KML deleg
km <- kml_doc(visibility = 0,
              estilos = ed,
              folders = list(nf))
write_xml(km, "c:/eddy/code/web/sisea/del.kml")

## - resumen nacional -
fnac <- node_element("Folder")
xml_add_child(fnac, node_element("open", 1L))
xml_add_child(fnac, node_element("visibility", 1L))

nd <- sty_sty(id = "sty-nac",
              icon = sty_ico(url = url_google_ico("shapes",
                                                  "ranger_station"),
                             pos = list(x = 0.5, y = 0.5,
                                        xunits = "fraction",
                                        yunits = "fraction"),
                             escala = 1.0,
                             col = "ff0000ff"),
              label = sty_lab(escala = 0.8,
                              color = "FF61AEFD"))
xml_add_child(fnac, nd)

np <- node_element("Placemark")
xml_add_child(np, node_element("name", "Nacional"))
xml_add_child(np, node_element("styleUrl", "#sty-nac"))
xml_add_child(np, node_element("Snippet", "Resumen nacional"))

ww <- group_by(x, delega) %>%
    summarise(asignados     = n(),
              pendiente     = sum(control == "pendiente"),
              levantados    = asignados - pendiente,
              avance        = magest::pct(levantados, asignados),
              completo      = sum(control == "completo"),
              incompleto    = sum(control == "incompleto"),
              noagricola    = sum(control == "no-agrícola"),
              rechazo       = sum(control == "rechazo"),
              noinformante  = sum(control == "no-informante"),
              inaccesible   = sum(control == "inaccesible")) %>%
    ungroup() %>%
    rename(grupo = delega)

pmt_gauge <- magest::pct(sum(ww$asignados - ww$pendiente),
                         sum(ww$asignados)) %>%
    paste0(., ",'", ., "%'")

pmt_tabla <- select(ww, grupo, asignados, levantados, pendiente) %>%
    jsonlite::toJSON() %>%
    as.character()

fecha_repo <- "20 de Abril del 2021"
grupo <- "'delegación'"

ht <- readLines("c:/eddy/code/web/sisea/dele.html") %>%
    glue::glue_collapse(sep = "\n") %>%
    glue::glue(.open = "\\{", .close = "\\}")

nd <- node_element("description")
xml_add_child(nd, xml_cdata(ht))
xml_add_child(np, nd)

xml_add_child(np, node_point(-85.00, 13.00))
xml_add_child(fnac, np)

## NOT RUN - KML nacional
km <- kml_doc(visibility = 0,
              estilos = list(),
              folders = list(fnac))
write_xml(km, "c:/eddy/code/web/sisea/nac.kml")

## -- puntos + deleg + nacional
## documento puntos + deleg
## km <- kml_doc(estilos = es,
##               folders = list(nf))
km <- kml_doc(visibility = 0,
              estilos = c(es, ed),
              folders = c(list(Delegaciones = nf), ss))


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

sn <- sty_sty(id = "norm", icon = s1, label = sty_lab())
sh <- sty_sty(id = "dest", icon = s2, label = sty_lab())
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


a <- data.frame(a = TRUE, b = TRUE)
cat(jsonlite::toJSON(a))
