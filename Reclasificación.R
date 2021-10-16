rm(list = ls())
# Paquetes requeridos
if(!require('pacman')) install.packages('pacman')
pacman::p_load(tidyverse, zip, readxl, 'Cairo', htmltools, leaflet, htmlwidgets, rgdal, rgeos, viridis, rcartocolor, leaflet.extras, stratification)

# Reclasificación municipal

## Carga de datos
marginacion_municipal <- read_excel("01Datos/IMM_2020.xls", 
                                    sheet = "IMM_2020") 

marginacion_municipal_sonora <- marginacion_municipal %>% 
  filter(NOM_ENT=="Sonora") 


#Normalizamos IM2020 de la información filtrada

marginacion_municipal_sonora <- marginacion_municipal_sonora %>% 
  mutate(IMNSon_2020=(IM_2020-min(IM_2020))/(max(IM_2020)-min(IM_2020)))

# Método iterativo para la obtención de número de clases óptimo a nivel municipal
start.time <- Sys.time()
i <- 1
sd <- matrix(NA,nrow=nrow(marginacion_municipal_sonora)-4,ncol=3)
meanh <- matrix(NA,nrow=nrow(marginacion_municipal_sonora)-4,ncol=6)
varh <- matrix(NA,nrow=nrow(marginacion_municipal_sonora)-4,ncol=6)
for (n in seq(5,nrow(marginacion_municipal_sonora),1)){
  cum <- strata.cumrootf(x = marginacion_municipal_sonora$IMNSon_2020, CV = 0.05 , Ls = 5, alloc = c(0.5,0,0.5), nclass= n)
  sd[i,] <-  c(n,cum$stderr,cum$CV)
  meanh[i,] <- c(n,cum$meanh)
  varh[i,] <-  c(n,cum$varh)
  i <- i + 1
}


colnames(sd) <- c("n","sderr","CV")
colnames(meanh) <- c("nclass", paste0(rep("Strata",5),1:5))
colnames(varh) <- c("nclass",paste0(rep("Strata",5),1:5))
end.time <- Sys.time()
time.taken <- round(end.time - start.time,2)
time.taken

min.strata <- sd %>% 
  as.data.frame() %>% 
  slice(which.min(.$CV))

strata.DH_2020 <- strata.cumrootf(marginacion_municipal_sonora$IMNSon_2020,
                                  CV = 0.01,
                                  Ls = 5,
                                  alloc = c(0.5,0,0.5), 
                                  nclass = min.strata$n)
strata.DH_2020

## Se agrega a la base de datos los resultados finales 

assign(paste0("marginacion_municipal_sonora"), data.frame(marginacion_municipal_sonora, strata.DH_2020[["stratumID"]])) 

names(marginacion_municipal_sonora) <- c("CVE_ENT","NOM_ENT","CVE_MUN","NOM_MUN","POB_TOT",
                                "ANALF", "SBASC","OVSDE", "OVSEE", "OVSAE","VHAC", "OVPT", "PL.5000","PO2SM", 
                                "IM_2020", "GM_2020", "IMN_2020", "IMNSon_2020",paste0("GMSon_2020"))

## Se cambian los levels de D&H

levels(marginacion_municipal_sonora$GMSon_2020) = c("Muy alto","Alto","Medio","Bajo","Muy bajo")

marginacion_municipal_sonora %>% group_by(GMSon_2020) %>% summarise(n())

write.csv(marginacion_municipal_sonora, "03Resultados/GMMunSon.csv")

# Reclasificación de AGEBS urbanas

marginacion_urbana <- read_excel("01Datos/IMU_2020.xls", 
                                 sheet = "IMU_2020") 

marginacion_urbana_sonora <- marginacion_urbana %>% 
  filter(NOM_ENT=="Sonora") 


#Normalizamos IM2020 de la información filtrada

marginacion_urbana_sonora <- marginacion_urbana_sonora %>% 
  mutate(IMNSon_2020=(IM_2020-min(IM_2020))/(max(IM_2020)-min(IM_2020)))

# Método iterativo para la obtención de número de clases óptimo a nivel municipal
start.time <- Sys.time()
i <- 1
sd <- matrix(NA,nrow=nrow(marginacion_urbana_sonora)-4,ncol=3)
meanh <- matrix(NA,nrow=nrow(marginacion_urbana_sonora)-4,ncol=6)
varh <- matrix(NA,nrow=nrow(marginacion_urbana_sonora)-4,ncol=6)
for (n in seq(5,nrow(marginacion_urbana_sonora),1)){
  cum <- strata.cumrootf(x = marginacion_urbana_sonora$IMNSon_2020, CV = 0.05 , Ls = 5, alloc = c(0.5,0,0.5), nclass= n)
  sd[i,] <-  c(n,cum$stderr,cum$CV)
  meanh[i,] <- c(n,cum$meanh)
  varh[i,] <-  c(n,cum$varh)
  i <- i + 1
}


colnames(sd) <- c("n","sderr","CV")
colnames(meanh) <- c("nclass", paste0(rep("Strata",5),1:5))
colnames(varh) <- c("nclass",paste0(rep("Strata",5),1:5))
end.time <- Sys.time()
time.taken <- round(end.time - start.time,2)
time.taken

min.strata <- sd %>% 
  as.data.frame() %>% 
  slice(which.min(.$CV))

strata.DH_2020 <- strata.cumrootf(marginacion_urbana_sonora$IMNSon_2020,
                                  CV = 0.01,
                                  Ls = 5,
                                  alloc = c(0.5,0,0.5), 
                                  nclass = min.strata$n)
strata.DH_2020

## Se agrega a la base de datos los resultados finales 

assign(paste0("marginacion_urbana_sonora"), data.frame(marginacion_urbana_sonora, strata.DH_2020[["stratumID"]])) 
str(marginacion_urbana_sonora)
names(marginacion_urbana_sonora)
names(marginacion_urbana_sonora) <- c("CVE_AGEB", "ENT", "NOM_ENT",  "MUN",  "NOM_MUN",                      
                                      "LOC" , "NOM_LOC",  "AGEB","POB_TOTAL", "P6A14NAE",                     
                                      "SBASC",  "PSDSS", "OVSDE", "OVSEE", "OVSAE",                        
                                       "OVPT", "OVSREF",  "OVSINT", "OVSCEL", "OVHAC",                        
                                      "IM_2020", "GM_2020",  "IMN_2020", "IMNSon_2020", paste0("GMSon_2020"))

## Se cambian los levels de D&H

levels(marginacion_urbana_sonora$GMSon_2020) = c("Muy alto","Alto","Medio","Bajo","Muy bajo")

marginacion_urbana_sonora %>% group_by(GMSon_2020) %>% summarise(n())

write.csv(marginacion_urbana_sonora, "03Resultados/GMUrbnSon.csv")


# Reclasificación localidades


marginacion_localidad <- read_excel("01Datos/IML_2020.xls", 
                                    sheet = "IML_2020_2") 
marginacion_localidad_sonora <- marginacion_localidad %>% 
  filter(NOM_ENT=="Sonora") 


#Normalizamos IM2020 de la información filtrada

marginacion_localidad_sonora <- marginacion_localidad_sonora %>% 
  mutate(IMNSon_2020=(IM_2020-min(IM_2020))/(max(IM_2020)-min(IM_2020)))

# Método iterativo para la obtención de número de clases óptimo a nivel municipal
start.time <- Sys.time()
i <- 1
sd <- matrix(NA,nrow=nrow(marginacion_localidad_sonora)-4,ncol=3)
meanh <- matrix(NA,nrow=nrow(marginacion_localidad_sonora)-4,ncol=6)
varh <- matrix(NA,nrow=nrow(marginacion_localidad_sonora)-4,ncol=6)
for (n in seq(5,nrow(marginacion_localidad_sonora),1)){
  cum <- strata.cumrootf(x = marginacion_localidad_sonora$IMNSon_2020, CV = 0.05 , Ls = 5, alloc = c(0.5,0,0.5), nclass= n)
  sd[i,] <-  c(n,cum$stderr,cum$CV)
  meanh[i,] <- c(n,cum$meanh)
  varh[i,] <-  c(n,cum$varh)
  i <- i + 1
}


colnames(sd) <- c("n","sderr","CV")
colnames(meanh) <- c("nclass", paste0(rep("Strata",5),1:5))
colnames(varh) <- c("nclass",paste0(rep("Strata",5),1:5))
end.time <- Sys.time()
time.taken <- round(end.time - start.time,2)
time.taken

min.strata <- sd %>% 
  as.data.frame() %>% 
  slice(which.min(.$CV))

strata.DH_2020 <- strata.cumrootf(marginacion_localidad_sonora$IMNSon_2020,
                                  CV = 0.01,
                                  Ls = 5,
                                  alloc = c(0.5,0,0.5), 
                                  nclass = min.strata$n)
strata.DH_2020

## Se agrega a la base de datos los resultados finales 

assign(paste0("marginacion_localidad_sonora"), data.frame(marginacion_localidad_sonora, strata.DH_2020[["stratumID"]])) 
str(marginacion_localidad_sonora)
names(marginacion_localidad_sonora)
names(marginacion_localidad_sonora) <- c("CVE_LOC", "ENT",  "NOM_ENT", "MUN",  "NOM_MUN",                      
                                         "LOC", "NOM_LOC", "POB_TOT", "ANALF", "SBASC",                        
                                         "OVSDE", "OVSEE", "OVSAE", "OVPT", "OVSREF",                       
                                         "OVHAC", "IM_2020",  "GM_2020", "IMN_2020", "IMNSon_2020", paste0("GMSon_2020"))

## Se cambian los levels de D&H

levels(marginacion_localidad_sonora$GMSon_2020) = c("Muy alto","Alto","Medio","Bajo","Muy bajo")

marginacion_localidad_sonora %>% group_by(GMSon_2020) %>% summarise(n())

write.csv(marginacion_localidad_sonora, "03Resultados/GMLocSon.csv")

