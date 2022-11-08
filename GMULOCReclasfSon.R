
rm(list = ls())
# Paquetes requeridos
if(!require('pacman')) install.packages('pacman')
pacman::p_load(tidyverse, zip, readxl, 'Cairo', htmltools, leaflet, htmlwidgets, rgdal, rgeos, viridis, rcartocolor, leaflet.extras,leafem)

# Carga de datos

marginacion_urbana_sonora <- read_csv("C:/Users/luism/OneDrive/R/Marginación/03Resultados/GMUrbnSon.csv", 
                      col_types = cols(X1 = col_skip(), GMSon_2020 = col_factor(levels = c("Muy bajo", "Bajo", "Medio", "Alto", "Muy alto"))), 
                      locale = locale(encoding = "ISO-8859-1"))

## Niveles
# levels=c("Muy bajo", "Bajo", "Medio", "Alto", "Muy alto")

# Carga de datos de localidades

marginacion_loc_sonora <-  read_csv("C:/Users/luism/OneDrive/R/Marginación/03Resultados/GMLocSon.csv", 
                                   col_types = cols(X1 = col_skip(), GMSon_2020 = col_factor(levels = c("Muy bajo", "Bajo", "Medio", "Alto", "Muy alto"))), 
                                   locale = locale(encoding = "ISO-8859-1")) 
# Carga de datos municipales

marginacion_municipal_sonora <-  read_csv("C:/Users/luism/OneDrive/R/Marginación/03Resultados/GMMunSon.csv", 
                                    col_types = cols(X1 = col_skip(), GMSon_2020 = col_factor(levels = c("Muy bajo", "Bajo", "Medio", "Alto", "Muy alto"))), 
                                    locale = locale(encoding = "ISO-8859-1")) 


# Se carga el shapefile

capa_ageb <- readOGR("02Shapes/conjunto_de_datos", layer="26a",  encoding = "UTF-8", use_iconv=TRUE)
capa_ageb<- spTransform(capa_ageb, 
                        CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))

capa_mun <- readOGR("02Shapes/conjunto_de_datos", layer="26mun",  encoding = "UTF-8", use_iconv=TRUE)
capa_mun<- spTransform(capa_mun, 
                       CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))

capa_loc <- readOGR("02Shapes/conjunto_de_datos", layer="26l",  encoding = "UTF-8", use_iconv=TRUE)
capa_loc<- spTransform(capa_loc, 
                       CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))

str(capa_ageb@data)



## Renombramos variable CVE_AGEB

marginacion_urbana_sonora <- rename(marginacion_urbana_sonora, CVEGEO=CVE_AGEB)

capa_ageb <- capa_ageb %>% merge(marginacion_urbana_sonora)



capa_ageb <- capa_ageb[!is.na(capa_ageb@data$GMSon_2020),]
str(capa_ageb@data)
view(capa_ageb@data)


Colores <- carto_pal(5, "TealRose")

margpal <-  colorFactor(Colores, levels=c("Muy bajo", "Bajo", "Medio", "Alto", "Muy alto"), na.color =alpha("#e8e6e6", 0))

labs <- c("Muy bajo", "Bajo", "Medio", "Alto", "Muy alto")

marginacion_municipal_sonora <- rename(marginacion_municipal_sonora, CVEGEO=CVE_MUN)

capa_mun <- capa_mun %>% merge(marginacion_municipal_sonora)

marginacion_loc_sonora <- rename(marginacion_loc_sonora, CVEGEO=CVE_LOC)

capa_loc <- capa_loc %>% merge(marginacion_loc_sonora)

capa_loc <- capa_loc[!is.na(capa_loc@data$GM_2020),]

popmun <- paste0(
  "<b>", "Municipio: ", "</b>", as.character(capa_mun$NOM_MUN),"<br>",
  "<b>", "Grado de marginación Sonora:   ", "</b>",   as.character(capa_mun$GMSon_2020),      "<br>",
  "<b>", "Grado de marginación CONAPO:   ", "</b>",   as.character(capa_mun$GM_2020),      "<br>",
  "<b>", "Población total:   ", "</b>",   prettyNum(as.numeric(capa_mun$POB_TOT), big.mark=",", preserve.width="none"),      "<br>",
  "<b>", "15 años o más analfabetas:   ", "</b>",   round(capa_mun$ANALF,1),"%",      "<br>",
  "<b>", "15 años o más sin educación básica:   ", "</b>",   round(capa_mun$SBASC,1),"%",      "<br>",
  "<b>", "Sin drenaje ni excusado:   ", "</b>",   round(capa_mun$OVSDE,1),    "%",  "<br>",
  "<b>", "Sin energía eléctrica:   ", "</b>",   round(capa_mun$OVSEE,1),     "%", "<br>",
  "<b>", "Sin agua entubada:   ", "</b>",   round(capa_mun$OVSAE,1),     "%", "<br>",
  "<b>", "Con piso de tierra:   ", "</b>",   round(capa_mun$OVPT,1),   "%",   "<br>",
  "<b>", "Con hacinamiento:   ", "</b>",   round(capa_mun$VHAC,1),    "%",  "<br>",
  "<b>", "En localidades menores a 5 mil habs:   ", "</b>",   round(capa_mun$PL.5000,1),   "%",   "<br>",
  "<b>", "Ocupados con ingresos de hasta 2 S.M.:   ", "</b>",   round(capa_mun$PO2SM,1),     "%", "<br>",
  "<b>", "www.luisarmandomoreno.com", "</b>")  %>% lapply(htmltools::HTML)

poploc <- paste0(
  "<b>", "Municipio: ", "</b>", as.character(capa_loc$NOM_MUN),"<br>",
  "<b>", "Localidad: ", "</b>", as.character(capa_loc$NOM_LOC),"<br>",
  "<b>", "Grado de marginación Sonora:   ", "</b>",   as.character(capa_loc$GMSon_2020),      "<br>",
  "<b>", "Grado de marginación CONAPO:   ", "</b>",   as.character(capa_loc$GM_2020),      "<br>",
  "<b>", "Población total:   ", "</b>",   prettyNum(as.numeric(capa_loc$POB_TOT), big.mark=",", preserve.width="none"),      "<br>",
  "<b>", "15 años o más analfabetas:   ", "</b>",   round(capa_loc$ANALF,1),"%",      "<br>",
  "<b>", "15 años o más sin educación básica:   ", "</b>",   round(capa_loc$SBASC,1),"%",      "<br>",
  "<b>", "Sin drenaje ni excusado:   ", "</b>",   round(capa_loc$OVSDE,1),    "%",  "<br>",
  "<b>", "Sin energía eléctrica:   ", "</b>",   round(capa_loc$OVSEE,1),     "%", "<br>",
  "<b>", "Sin agua entubada:   ", "</b>",   round(capa_loc$OVSAE,1),     "%", "<br>",
  "<b>", "Con piso de tierra:   ", "</b>",   round(capa_loc$OVPT,1),   "%",   "<br>",
  "<b>", "Sin refrigerador:   ", "</b>",   round(capa_loc$OVSREF,1),   "%",   "<br>",
  "<b>", "Con hacinamiento:   ", "</b>",   round(capa_loc$OVHAC,1),    "%",  "<br>",
  "<b>", "www.luisarmandomoreno.com", "</b>")  %>% lapply(htmltools::HTML)

popup <- paste0(
  "<b>", "Municipio: ", "</b>", as.character(capa_ageb$NOM_MUN),"<br>",  
  "<b>", "Localidad: ", "</b>", as.character(capa_ageb$NOM_LOC),"<br>",
  "<b>", "AGEB: ", "</b>", as.character(capa_ageb$CVE_AGEB),"<br>",
  "<b>", "Grado de marginación Sonora:   ", "</b>",   as.character(capa_ageb$GMSon_2020),      "<br>",
  "<b>", "Grado de marginación CONAPO:   ", "</b>",   as.character(capa_ageb$GM_2020),      "<br>",
  "<b>", "Población total:   ", "</b>",   prettyNum(as.numeric(capa_ageb$POB_TOTAL), big.mark=",", preserve.width="none"),      "<br>",
  "<b>", "6 a 14 años que no asiste a la escuela:   ", "</b>",   round(capa_ageb$P6A14NAE,1),"%",      "<br>",
  "<b>", "15 años o más sin educación básica:   ", "</b>",   round(capa_ageb$SBASC,1),"%",      "<br>",
  "<b>", "Sin servicios de salud:   ", "</b>",   round(capa_ageb$PSDSS,1),  "%",    "<br>",
  "<b>", "Sin drenaje ni excusado:   ", "</b>",   round(capa_ageb$OVSDE,1),    "%",  "<br>",
  "<b>", "Sin energía eléctrica:   ", "</b>",   round(capa_ageb$OVSEE,1),     "%", "<br>",
  "<b>", "Sin agua entubada:   ", "</b>",   round(capa_ageb$OVSAE,1),     "%", "<br>",
  "<b>", "Con piso de tierra:   ", "</b>",   round(capa_ageb$OVPT,1),   "%",   "<br>",
  "<b>", "Con hacinamiento:   ", "</b>",   round(capa_ageb$OVHAC,1),    "%",  "<br>",
  "<b>", "Sin refrigerador:   ", "</b>",   round(capa_ageb$OVSREF,1),   "%",   "<br>",
  "<b>", "Sin internet:   ", "</b>",   round(capa_ageb$OVSINT,1),     "%", "<br>",
  "<b>", "Sin celular:   ", "</b>",   round(capa_ageb$OVSCEL,1),    "%",  "<br>")  %>% lapply(htmltools::HTML)

mapaagebmarg <- leaflet(capa_ageb) %>% 
  addProviderTiles(providers$CartoDB.Voyager) %>%
  addPolygons(data= capa_mun,
              stroke= TRUE,
              weight=0.2,                   
              opacity=1,
              fillColor = "transparent",
              color= "black",
              fillOpacity = 0,
              smoothFactor = 0.5) %>%
  addPolygons(data= capa_ageb,
              stroke= TRUE,
              weight=0.2,                   
              opacity=1,
              fillColor = ~margpal(capa_ageb$GMSon_2020),
              color= "white",
              fillOpacity = 0.6,
              smoothFactor = 0.5,
              highlightOptions = highlightOptions(color = "black", 
                                                  weight = 1.2,
                                                  bringToFront = TRUE),
              popup = popup, 
              popupOptions = labelOptions(noHide = F, direction = "auto",  closeOnClick = TRUE, 
                                          style = list(
                                            "color" = "black",
                                            "font-family" = "Arial",
                                            "font-style" = "regular",
                                            "box-shadow" = "2px 2px rgba(0,0,0,0.25)",
                                            "font-size" = "8px",
                                            "border-color" = "rgba(0,0,0,0.5)"
                                          )),
              group= "AGEB Urbana (clasificación Estatal)") %>%
  addPolygons(data= capa_mun,
              stroke= TRUE,
              weight=0.2,                   
              opacity=1,
              fillColor = ~margpal(capa_mun$GMSon_2020),
              color= "black",
              fillOpacity = 1,
              smoothFactor = 0.5,
              highlightOptions = highlightOptions(color = "black", 
                                                  weight = 1.2,
                                                  bringToFront = TRUE),
              popup = popmun, 
              popupOptions = labelOptions(noHide = F, direction = "auto",  closeOnClick = TRUE, 
                                          style = list(
                                            "color" = "black",
                                            "font-family" = "Arial",
                                            "font-style" = "regular",
                                            "box-shadow" = "2px 2px rgba(0,0,0,0.25)",
                                            "font-size" = "8px",
                                            "border-color" = "rgba(0,0,0,0.5)"
                                          )),
              group= "Municipal (clasificación Estatal)") %>%
  addPolygons(data= capa_loc,
              stroke= TRUE,
              weight=0.2,                   
              opacity=1,
              fillColor = ~margpal(capa_loc$GMSon_2020),
              color= "black",
              fillOpacity = 1,
              smoothFactor = 0.5,
              highlightOptions = highlightOptions(color = "black", 
                                                  weight = 1.2,
                                                  bringToFront = TRUE),
              popup = poploc, 
              popupOptions = labelOptions(noHide = F, direction = "auto",  closeOnClick = TRUE, 
                                          style = list(
                                            "color" = "black",
                                            "font-family" = "Arial",
                                            "font-style" = "regular",
                                            "box-shadow" = "2px 2px rgba(0,0,0,0.25)",
                                            "font-size" = "8px",
                                            "border-color" = "rgba(0,0,0,0.5)"
                                          )),
              group= "Localidad (clasificación Estatal)") %>%
    addPolygons(data= capa_mun,
              stroke= TRUE,
              weight=0.2,                   
              opacity=1,
              fillColor = ~margpal(capa_mun$GM_2020),
              color= "black",
              fillOpacity = 1,
              smoothFactor = 0.5,
              highlightOptions = highlightOptions(color = "black", 
                                                  weight = 1.2,
                                                  bringToFront = TRUE),
              popup = popmun, 
              popupOptions = labelOptions(noHide = F, direction = "auto",  closeOnClick = TRUE, 
                                          style = list(
                                            "color" = "black",
                                            "font-family" = "Arial",
                                            "font-style" = "regular",
                                            "box-shadow" = "2px 2px rgba(0,0,0,0.25)",
                                            "font-size" = "8px",
                                            "border-color" = "rgba(0,0,0,0.5)"
                                          )),
              group= "Municipal (clasificación Nacional)") %>%
  addSearchOSM(options = searchOptions(autoCollapse = TRUE, minLength = 2)) %>% 
  addLegend(position = "bottomleft",  pal = margpal, values = ~capa_ageb$GMSon_2020, opacity=1, group= "GRADO DE MARGINACIÓN SONORA", 
            labFormat = function(type, cuts, p) {  
              paste0(labs)} ,
            title = "GRADO DE MARGINACIÓN<br>CONAPO,2020.<br>(click en el área de interés para mayor información)", na.label = "No aplica") %>% 
  addLayersControl( 
    baseGroups = c("AGEB Urbana (clasificación Estatal)","Localidad (clasificación Estatal)", "Municipal (clasificación Estatal)", "Municipal (clasificación Nacional)" ), 
    options = layersControlOptions(collapsed = FALSE, position = "bottomleft")) %>% 
  addLogo("https://www.luisarmandomoreno.com/wp-content/uploads/2022/05/SEDfesp.png",
          src= "remote", position = "topright", url = "https://www.luisarmandomoreno.com/",
          width = 100,
          height = 90)
 

mapaagebmarg

saveWidget(mapaagebmarg,"docs/index.html", title= "Sonora: Marginación - Luis Armando Moreno", selfcontained = F, libdir = "lib")

