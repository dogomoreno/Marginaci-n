rm(list = ls())

if(!require('pacman')) install.packages('pacman')
pacman::p_load(ggtext, tidyverse, extrafont, scales, 'Cairo', htmltools, leaflet, htmlwidgets, rgdal, rgeos)


casillas <- read_csv("data/SON_GOB_2021.csv", skip = 4)

GOBLIMP <- casillas %>% 
  filter(TOTAL_PERSONAS_VOTARON!=0, CC_MORENA_PT_PVEM_NA!="Ilegible", CC_PAN_PRI_PRD!="Ilegible" ) %>%  
  group_by(DISTRITO_LOCAL, SECCION) %>% 
  summarise(TOTAL_VOTANTES=sum(as.numeric(TOTAL_PERSONAS_VOTARON)), 
            MORENAv=sum(as.numeric(CC_MORENA_PT_PVEM_NA)), ALIANZAv=sum(as.numeric(CC_PAN_PRI_PRD)), 
            LN=sum(as.numeric(LISTA_NOMINAL)), VOTOS=sum(as.numeric(TOTAL_VOTOS_CALCULADO))) %>% rename(seccion=SECCION)
GOBLIMP[is.na(GOBLIMP)] <- 0

GOBLIMP <- GOBLIMP %>% 
  mutate(seccion=as.numeric(seccion), PARTICIPACION=round(VOTOS*100/LN,1), MORENA=round(MORENAv*100/VOTOS,1),ALIANZA=round(ALIANZAv*100/VOTOS,1)) %>% mutate(DIF=MORENA-ALIANZA) %>% mutate(CLASF=if_else(DIF>0,"1","2"))

ayuntamientos <- read_csv("data/SON_AYUN_2021.csv", 
                          skip = 4) %>% select(ID_MUNICIPIO, MUNICIPIO) %>% group_by_all %>% count %>% filter(!is.na(MUNICIPIO)) %>% rename(municipio=ID_MUNICIPIO) %>% mutate(municipio=as.numeric(municipio))

# GOBLIMP <- GOBLIMP %>% left_join(ayuntamientos, by="seccion")

capa_secc <- readOGR("Shapes", layer="seccionsonora",  encoding = "UTF-8", use_iconv=TRUE)

capa_secc <- capa_secc %>% merge(GOBLIMP) %>% merge(ayuntamientos)

incipal <-  colorFactor(c("#73264D","#0097CB"), levels= c("1","2"), na.color ="#e8e6e6")

labs <- c("JUNTOS HAREMOS HISTORIA","VA X SONORA")

popup <- paste0(
  "<b>", as.character(capa_secc$MUNICIPIO), " ",  as.character(capa_secc$seccion), "</b>",     "<br>",
  "<b>", "LISTA NOMINAL:   ", "</b>",   capa_secc$LN,      "<br>",
  "<b>", "TOTAL DE VOTOS:   ",           "</b>",   as.character(capa_secc$VOTOS),      "<br>",
  "<b>", "PARTICIPACION:   ",           "</b>",   as.character(capa_secc$PARTICIPACION),  "%",    "<br>",
  "<b>", "JUNTOS HAREMOS HISTORIA:   ",           "</b>",   as.character(capa_secc$MORENA), "%",     "<br>",
  "<b>", "VA X SONORA:   ",           "</b>",   as.character(capa_secc$ALIANZA),   "%",   "<br>")  %>% lapply(htmltools::HTML)




mapaseccgob <- leaflet(capa_secc) %>% 
  addProviderTiles(providers$OpenStreetMap.Mapnik) %>%
   addLayersControl( 
     baseGroups = c("ELECCIÓN A GOBERNADOR"), 
    options = layersControlOptions(collapsed = FALSE, position = "topright")) %>% 
  addPolygons(data= capa_secc,
              stroke= TRUE,
              weight=0.5,                   
              opacity=1,
              fillColor = ~incipal(capa_secc$CLASF),
              color= "white",
              fillOpacity = 0.7,
              smoothFactor = 0.5,
              highlightOptions = highlightOptions(color = "black", 
                                                  weight = 1.2,
                                                  bringToFront = TRUE),
              label=popup, 
              labelOptions = labelOptions(noHide = F, direction = "top",
                                          style = list(
                                            "color" = "black",
                                            "font-family" = "Lato",
                                            "font-style" = "regular",
                                            "box-shadow" = "2px 2px rgba(0,0,0,0.25)",
                                            "font-size" = "11px",
                                            "border-color" = "rgba(0,0,0,0.5)"
                                          )),
              group= "VENTAJA EN SECCIÓN") %>%
  addLegend(position = "topright", pal = incipal, values = ~capa_secc$CLASF, opacity=1, group= "ELECCIÓN A GOBERNADOR", 
            labFormat = function(type, cuts, p) {  
              paste0(labs)} ,
            title = "VENTAJA EN SECCIÓN", na.label = "INCOMPLETO EN PREP") 

mapaseccgob

saveWidget(mapaseccgob,"index.html", selfcontained = F, libdir = "lib")
