require(tidyverse)
require(GGally)

datos <- rio::import("sexo.csv", encoding = "UTF-8") %>% 
  as.data.frame()

datos
datos$Hombres
names(datos)[1] <- "Año"
datos$Hombres <- as.numeric(gsub(".","",datos$Hombres,fixed = T))
datos$Mujeres <- as.numeric(gsub(".","",datos$Mujeres,fixed = T))


datos %>% 
  ggplot() +
  geom_line(aes(Año, Hombres/1000000), size = 0.6, col = "#F8766D") +
  geom_point(aes(Año, Hombres/1000000, col = "Hombres"), size = 3.5) +
  geom_line(aes(Año, Mujeres/1000000), size = 0.6, col = "#00BFC4") +
  geom_point(aes(Año, Mujeres/1000000, col = "Mujeres"), size = 3.5) +
  ggtitle("Atenciones en salud mental en Chile (en millones)") +
  labs(y = "Cantidad de atenciones", color = "Sexo") +
  geom_vline(xintercept = 2019 + 0.2, linetype = "dotted", size = 1)


edad <- rio::import("edad.csv", encoding = "UTF-8") %>% 
  as_tibble()

edad
t(edad)

anio <- c(replicate(7,c(2014,2015,2016,2017,2018,2019,2020,2021)))

cantidad <- edad[[2]] %>% 
  c(edad[[3]]) %>% 
  c(edad[[4]]) %>% 
  c(edad[[5]]) %>% 
  c(edad[[6]]) %>% 
  c(edad[[7]]) %>% 
  c(edad[[8]])

Edad = replicate(8,names(edad)[2]) %>% 
  c(replicate(8,names(edad)[3])) %>% 
  c(replicate(8,names(edad)[4])) %>% 
  c(replicate(8,names(edad)[5])) %>% 
  c(replicate(8,names(edad)[6])) %>% 
  c(replicate(8,names(edad)[7])) %>%
  c(replicate(8,names(edad)[8]))

df

df <- as.data.frame(anio) %>% 
  cbind(cantidad) %>% 
  cbind(Edad)

datos

datos %>% 
  mutate(total = Hombres + Mujeres) %>% 
  select(-Hombres, -Mujeres) %>% 
  ggplot(aes(Año, total/1000000)) +
  geom_bar(stat = "identity", fill = "2114AC") +
  labs(title = "Atenciones totales en salud mental",
       x = "Año", y = "Cantidad de atenciones (en millones)")
  


datos_test = pivot_longer(datos, cols = Hombres:Mujeres,
                    names_to = "sexo", values_to = "cantidad")




datos_test %>% 
  ggplot(aes(Año, cantidad/1000000, fill = sexo)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Atenciones en salud mental por sexo",
      x = "", y = "Cantidad de atenciones (en millones)")

datos_test
test = pivot_longer(edad, cols = `0 a 9 años`:`60 años  y más`,
                    names_to = "edad", values_to = "cantidad")

test %>% 
  ggplot(aes(Año, cantidad, fill = edad)) +
  geom_bar(stat = "identity", show.legend = F) +
  scale_fill_manual(values = c("#3D5B59","#B5E5CF","#7570B3","#B99095", 
                               "#B2182B", "#2166AC", "#E6A0B3"))+
  labs(title = "Atenciones en salud mental en Chile por edad",
       y="Cantidad de atenciones (en miles)", x = "") +
  facet_wrap(vars(edad))

df
 
df %>%
  ggplot(aes(anio, cantidad, fill= Edad)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = c("#3D5B59","#B5E5CF","#7570B3","#B99095", 
                               "#B2182B", "#2166AC", "#E6A0B3"))+
  labs(title = "Atenciones en salud mental en Chile",
       y="Cantidad de atenciones (en miles)", x = "")


citation("tidyverse")
  