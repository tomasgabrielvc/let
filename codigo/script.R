require(tidyverse)
require(GGally)
require(janitor)

datos <- rio::import("sexo.csv") %>% 
  as.data.frame()

edad <- rio::import("edad.csv", encoding = "UTF-8") %>% 
  as.data.frame()

t(edad)
edad

datos$Hombres
names(datos)[1] <- "Año"
datos$Hombres <- as.numeric(gsub(".","",datos$Hombres,fixed = T))
datos$Mujeres <- as.numeric(gsub(".","",datos$Mujeres,fixed = T))


datos %>% 
  ggplot() +
  
  geom_line(aes(Año, Hombres/1000000), size = 0.6) +
  geom_point(aes(Año, Hombres/1000000, col = "hombres"), size = 3.5,
             fill = "white",
             shape = 21) +
  geom_line(aes(Año, Mujeres/1000000), size = 0.6) +
  geom_point(aes(Año, Mujeres/1000000, col = "mujeres"), size = 3.5,
                 fill = "white",
                 shape = 21) +
  ggtitle("Atenciones en salud mental en Chile (en millones)") +
  labs(y = "Cantidad de atenciones")

hist(edad)


edad[] <- lapply(edad, gsub(".","",datos$Hombres,fixed = T),edad)
