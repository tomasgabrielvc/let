require(tidyverse)
require(GGally)


datos <- rio::import("sexo.csv") %>% 
  as.data.frame()

edad <- rio::import("edad.csv") %>% 
  as.data.frame()

datos$Hombres
names(datos)[1] <- "Año"
datos$Hombres <- as.numeric(gsub(".","",datos$Hombres,fixed = T))
datos$Mujeres <- as.numeric(gsub(".","",datos$Mujeres,fixed = T))

datos %>% 
  ggplot() +
  geom_line(aes(Año, Hombres)) +
  geom_point(aes(Año, Hombres, col = "hombres"), size = 3,
             fill = "white",
             shape = 21) +
  geom_line(aes(Año, Mujeres)) +
  geom_point(aes(Año, Mujeres, col = "mujeres"))
  
datos %>% 
  ggplot() +
  
  geom_col(aes(Año, Hombres), fill = Mujeres)



edad[] <- lapply(edad, gsub(".","",datos$Hombres,fixed = T),edad)
