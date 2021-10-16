###################################################################################################

#                                 Índice de marginación 2020                                      #

###################################################################################################

# La base de datos del índice de marginación a nivel estatal se encuentra disponible
# en la página de internet del CONAPO https://www.gob.mx/conapo o bien se puede descargar
# directamente del siguiente vinculo https://na01.safelinks.protection.outlook.com/?url=http%3A%2F%2Fwww.conapo.gob.mx%2Fwork%2Fmodels%2FCONAPO%2FMarginacion%2FDatos_Abiertos%2FEntidad_Federativa%2FIME_2020.CSV&amp;data=04%7C01%7C%7Ca03d7e1e4e8b4d1b0f9208d90e8b3719%7C84df9e7fe9f640afb435aaaaaaaaaaaa%7C1%7C0%7C637556813348625152%7CUnknown%7CTWFpbGZsb3d8eyJWIjoiMC4wLjAwMDAiLCJQIjoiV2luMzIiLCJBTiI6Ik1haWwiLCJXVCI6Mn0%3D%7C1000&amp;sdata=aQFuOKidbZGcnLTjn8Lva1r43mtU16qEBN7qSXrjRZg%3D&amp;reserved=0

# Se instalan los paquetes y librerías a utilizar para el cálculo del índice de marginación 

instalar <- function(paquete){
  if(!require(paquete, 
              character.only = TRUE,
              quietly = TRUE,
              warn.conflicts=FALSE)){
    library(paquete,
            character.only = TRUE,
            quietly = TRUE,
            warn.conflicts = FALSE)
  }
}

# En caso de que R o RStudio mande un error con el programa "dplyr", será necesario instalarlo individualmente.

paquetes <- c("dplyr","p2distance","stratification")
lapply(paquetes, instalar)
rm(paquetes, instalar)

###################################################################################################
####################### Indice de marginación a nivel estatal #####################################
###################################################################################################

# Se carga la base de datos a nivel estatal
# Se elimina la fila de información a nivel Nacional   
# Es necesario cambiar la ruta de acceso al documento

IME_2020 <- read.csv("~/IME_2020.CSV") %>% 
  filter(NOM_ENT != "Nacional") 

############################ Método de Distancias DP2 #############################################

#José Bernardo Pena Trapero: `Problemas de la medición del bienestar y conceptos afines (1977)` 

# Para asegurar las propiedades del indicador sintético, un aumento en los indicadores 
# simples implica un aumento en la carencia de los servicios, lo que implicaría una disminución 
# de la calidad de vida, por lo que cada indicador simple se multiplica -1, 
# de esta forma, un aumento en la variable supone una mejora en la calidad de vida. 


# Base de referencia 2010   

# Por otro lado, para permitir la comparación en el tiempo de los indicadores simples,
# se toma como referencia la fecha censal 2010 y se determina el vector base de referencia 
# al valor mínimo, es decir, el peor escenario teórico


minRV_2010 <- c(-17.9063062676492, # ANALF
                -60.1276776392954, # SBASC
                -19.8442777580699, # OVSDE
                -4.93236362397464, # OVSEE
                -29.7931507393921, # OVSAE
                -53.8984835976862, # VHAC
                -19.6138807443748, # OVPT
                -61.5084790431887, #PL.5000
                -69.849037164)     # PO2SM

ind_2020 <- p2distance(matriz = as.matrix(-1*IME_2020[4:12]),
                       reference_vector = minRV_2010,
                       iterations = 50)

## Se anexa el índice a la base de datos

assign(paste0("IME_2020_resultados"),cbind(IME_2020[1:12], ind_2020[["p2distance"]])) 


# Método de Dalenius & Hodges  

#`strata.cumrootf`: cumulative root frequency method by Dalenius and Hodges (1959) 

# Método iterativo para la obtención de número de clases óptimo a nivel estatal
start.time <- Sys.time()
i <- 1
sd <- matrix(NA,nrow=28,ncol=3)
meanh <- matrix(NA,nrow=28,ncol=6)
varh <- matrix(NA,nrow=28,ncol=6)
for (n in seq(5,nrow(IME_2020_resultados),1)){
  cum <- strata.cumrootf(x = IME_2020_resultados[,13], CV = 0.05 , Ls = 5, alloc = c(0.5,0,0.5), nclass= n)
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

min.strata <-  sd %>% 
  as.data.frame() %>% 
  slice(which.min(.$CV))

strata.DH_2020 <- strata.cumrootf(IME_2020_resultados[,13],
                                  CV = 0.05,
                                  Ls = 5,
                                  alloc = c(0.5,0,0.5), 
                                  nclass= 12)
strata.DH_2020

## Se agrega a la base de datos los resultados finales 

assign(paste0("IME_2020_resultados"), data.frame(IME_2020_resultados, strata.DH_2020[["stratumID"]])) 

# Se cambian los nombres de las columnas 

names(IME_2020_resultados) <- c("CVE_ENT","NOM_ENT","POB_TOT",
                                "ANALF", "SBASC","OVSDE", "OVSEE", "OVSAE","VHAC","OVPT","PL.5000" ,"PO2SM",
                                paste0("IM_2020"), paste0("GM_2020"))

# Se cambian los levels del grado de marginación  
for(i in 2020){
  niveles = get(paste0("IME_",i,"_resultados")) 
  levels(niveles[,14]) = c("Muy alto","Alto","Medio","Bajo","Muy bajo")
  assign(paste0("IME_",i,"_resultados"), niveles)
  rm(niveles)
}

# Se guarda la base de datos  
write.csv(IME_2020_resultados, file = paste0("~/IME_2020_1.csv"), sep = ",")

###################################################################################################
##################### Indice de marginación a nivel municipal #####################################
###################################################################################################

# Se carga la base de datos a nivel municipal
# Se elimina la fila de información a nivel Nacional   
# Es necesario cambiar la ruta de acceso al documento

IMM_2020 <- read.csv("~/IMM_2020.CSV") %>% 
  filter(NOM_ENT != "Nacional") 

########################## Método de Distancias DP2 ######################################

#José Bernardo Pena Trapero: `Problemas de la medición del bienestar y conceptos afines (1977)` 

# Para asegurar las propiedades del indicador sintético, un aumento en los indicadores 
# simples implica un aumento en la carencia de los servicios, lo que implicaría una disminución 
# de la calidad de vida, por lo que se multiplica cada indicador por -1, 
# de esta forma, un aumento en la variable supone una mejora en la calidad de vida. 

# Base de referencia 2010   

# Por otro lado, para permitir la comparación en el tiempo de los indicadores simples,
# se toma como referencia la fecha censal 2010 y se determina el vector base de referencia 
# al valor mínimo, es decir, el peor escenario teórico

minRV_2010 <- c(-66.7370644139387, # ANALF
                -94.7904637270239, # SBASC
                -89.9037947621593, # OVSDE
                -69.451507446422,  # OVSEE
                -99.7371879106438, # OVSAE
                -83.2441238073074, # VHAC
                -79.7067901234568, # OVPT
                -100,              #PL.5000
                -98.88268156425)   # PO2SM

ind_2020 <- p2distance(matriz = as.matrix(-1*IMM_2020[6:14]),
                       reference_vector = minRV_2010,
                       iterations = 50)

## Se anexa el índice a la base de datos

assign(paste0("IMM_2020_resultados"), cbind(IMM_2020[1:14], ind_2020[["p2distance"]])) 

# Método de Dalenius & Hodges  
#`strata.cumrootf`: cumulative root frequency method by Dalenius and Hodges (1959) 

# Método iterativo para la obtención de número de clases óptimo a nivel municipal
start.time <- Sys.time()
i <- 1
sd <- matrix(NA,nrow=(nrow(IMM_2020_resultados)-4),ncol=3)
meanh <- matrix(NA,nrow=(nrow(IMM_2020_resultados)-4),ncol=6)
varh <- matrix(NA,nrow=(nrow(IMM_2020_resultados)-4),ncol=6)
for (n in seq(5,nrow(IMM_2020_resultados),1)){
  cum <- strata.cumrootf(x = IMM_2020_resultados[,15], CV= 0.01 , Ls = 5, alloc = c(0.5,0,0.5), nclass= n)
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

strata.DH_2020 <- strata.cumrootf(IMM_2020_resultados[,15],
                                  CV = 0.01,
                                  Ls = 5,
                                  alloc = c(0.5,0,0.5), 
                                  nclass = 21)
strata.DH_2020

## Se agrega a la base de datos los resultados finales 

assign(paste0("IMM_2020_resultados"), data.frame(IMM_2020_resultados, strata.DH_2020[["stratumID"]])) 

# Se cambian los nombres de las columnas 

names(IMM_2020_resultados) <- c("CVE_ENT","NOM_ENT","CVE_MUN","NOM_ENT","POB_TOT",
                                "ANALF", "SBASC","OVSDE", "OVSEE", "OVSAE","VHAC", "OVPT", "PL.5000","PO2SM",
                                paste0("IM_2020"),paste0("GM_2020"))

# Se cambian los levels de D&H
for(i in 2020){
  niveles = get(paste0("IMM_",i,"_resultados")) 
  levels(niveles[,16]) = c("Muy alto","Alto","Medio","Bajo","Muy bajo")
  assign(paste0("IMM_",i,"_resultados"), niveles)
  rm(niveles)
}

# Se guarda la base de datos  
write.csv(IMM_2020_resultados, file = paste0("~/IMM_2020_1.csv"), sep = ",")