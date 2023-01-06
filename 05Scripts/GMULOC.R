
rm(list = ls())
# Paquetes requeridos
if(!require('pacman')) install.packages('pacman')
pacman::p_load(tidyverse, zip, readxl, 'Cairo', htmltools, leaflet, htmlwidgets, rgdal, rgeos, viridis, rcartocolor, leaflet.extras)

# Crear directorios
dir.create("./01Datos")
dir.create("./02Shapes")
# Descarga del Marco Geoestadístico de Sonora

shapes.url <- "https://www.inegi.org.mx/contenidos/productos/prod_serv/contenidos/espanol/bvinegi/productos/geografia/marcogeo/889463807469/26_sonora.zip"
shapes.archivo <- "C:/Users/luism/OneDrive/R/Marginación/02Shapes/26_sonora.zip"

if(!file.exists(shapes.archivo)){
  download.file(shapes.url, destfile = shapes.archivo)  
  unzip(shapes.archivo, exdir = "./02Shapes")
}

# Descarga del Índice de Marginación Urbana 2020 de CONAPO

conapo.url <- "http://www.conapo.gob.mx/work/models/CONAPO/Marginacion/Datos_Abiertos/IMU_2020.zip"
conapo.archivo <- "C:/Users/luism/OneDrive/R/Marginación/01Datos/IMU_2020.zip"

if(!file.exists(conapo.archivo)){
  download.file(conapo.url, destfile = conapo.archivo)  
  unzip(conapo.archivo, exdir = "./01Datos")
  
}


localidad.url <- "http://www.conapo.gob.mx/work/models/CONAPO/Marginacion/Datos_Abiertos/Localidad/IML_2020.zip"
localidad.archivo <- "C:/Users/luism/OneDrive/R/Marginación/01Datos/IML_2020.zip"

if(!file.exists(localidad.archivo)){
  download.file(localidad.url, destfile = localidad.archivo)  
  unzip(localidad.archivo, exdir = "./01Datos")
  
}


# R registra error al descargarlo desde el script, se recomienda bajar del explorador
# mun.url <- "http://www.conapo.gob.mx/work/models/CONAPO/Marginacion/Datos_Abiertos/Municipio/IMM_2020.xls" 
# mun.archivo <- "C:/Users/luism/OneDrive/R/Marginación/01Datos/IMM_2020.xls"



# if(!file.exists(mun.archivo)){
#  download.file(mun.url, destfile = mun.archivo)  
  
# }


# Carga de datos

marginacion_urbana <- read_excel("01Datos/IMU_2020.xls", 
                        sheet = "IMU_2020") 
## Estructura
str(marginacion_urbana)
## Valores únicos
unique(marginacion_urbana$NOM_ENT)
unique(marginacion_urbana$GM_2020)

## Niveles
levels=c("Muy bajo", "Bajo", "Medio", "Alto", "Muy alto")

# Se filtran las AGEBS Urbanas de Sonora, se ordenan los niveles del GM, y se seleccionan las variables
marginacion_urbana_sonora <- marginacion_urbana %>% 
  filter(NOM_ENT=="Sonora") %>% mutate(GM_2020=factor(GM_2020,levels))
marginacion_urbana_sonora %>% group_by(GM_2020) %>% summarise(n())

top_n(marginacion_urbana_sonora, 5, IMN_2020)
top_n(marginacion_urbana_sonora, -5, IMN_2020)

# Carga de datos de localidades

marginacion_localidad <- read_excel("01Datos/IML_2020.xls", 
                                 sheet = "IML_2020_2") 
# Se filtran las localidades de Sonora, se ordenan los niveles del GM, y se seleccionan las variables
marginacion_loc_sonora <- marginacion_localidad %>% 
  filter(NOM_ENT=="Sonora") %>% mutate(GM_2020=factor(GM_2020,levels))
marginacion_loc_sonora %>% group_by(GM_2020) %>% summarise(n())

top_n(marginacion_loc_sonora, 5, IMN_2020)
top_n(marginacion_loc_sonora, -5, IMN_2020)

# Carga de datos municipales

marginacion_municipal <- read_excel("01Datos/IMM_2020.xls", 
                                    sheet = "IMM_2020") 


# Se filtran los municipios de Sonora, se ordenan los niveles del GM, y se seleccionan las variables
marginacion_municipal_sonora <- marginacion_municipal %>% 
  filter(NOM_ENT=="Sonora") %>% mutate(GM_2020=factor(GM_2020,levels))
marginacion_municipal_sonora %>% group_by(GM_2020) %>% summarise(n())

top_n(marginacion_municipal_sonora, 5, IMN_2020)
top_n(marginacion_municipal_sonora, -5, IMN_2020)

str<-strata.cumrootf(marginacion_municipal_sonora$IMN_2020, CV=0.01, Ls=5, nclass=21)


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



capa_ageb <- capa_ageb[!is.na(capa_ageb@data$GM_2020),]
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
  "<b>", "Localidad: ", "</b>", as.character(capa_ageb$NOM_LOC),"<br>",
  "<b>", "Grado de marginación:   ", "</b>",   as.character(capa_mun$GM_2020),      "<br>",
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
  "<b>", "Grado de marginación:   ", "</b>",   as.character(capa_loc$GM_2020),      "<br>",
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
  "<b>", "Grado de marginación:   ", "</b>",   as.character(capa_ageb$GM_2020),      "<br>",
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
  "<b>", "Sin celular:   ", "</b>",   round(capa_ageb$OVSCEL,1),    "%",  "<br>",
  "<b>", "www.luisarmandomoreno.com", "</b>")  %>% lapply(htmltools::HTML)

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
              group= "Municipal") %>%
  addPolygons(data= capa_loc,
              stroke= TRUE,
              weight=0.2,                   
              opacity=1,
              fillColor = ~margpal(capa_loc$GM_2020),
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
              group= "Localidad") %>%
  addPolygons(data= capa_ageb,
              stroke= TRUE,
              weight=0.2,                   
              opacity=1,
              fillColor = ~margpal(capa_ageb$GM_2020),
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
              group= "Urbano") %>%
  addSearchOSM(options = searchOptions(autoCollapse = TRUE, minLength = 2)) %>% 
  addLegend(position = "bottomleft",  pal = margpal, values = ~capa_ageb$GM_2020, opacity=1, group= "GRADO DE MARGINACIÓN", 
            labFormat = function(type, cuts, p) {  
              paste0(labs)} ,
            title = "GRADO DE MARGINACIÓN<br>CONAPO,2020<br>(click en el área de interés para mayor información)", na.label = "No aplica") %>% 
  addLayersControl( 
    baseGroups = c("Municipal", "Localidad", "Urbano (AGEB)"), 
    options = layersControlOptions(collapsed = FALSE, position = "bottomleft"))
 

mapaagebmarg

saveWidget(mapaagebmarg,"docs/index.html", title= "Sonora: Marginación - Luis Armando Moreno", selfcontained = F, libdir = "lib")


