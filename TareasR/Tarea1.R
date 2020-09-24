### Tarea 1

# Librerias
library(pacman) # Abrir bases de datos geograficas
library(leaflet) # Visualización interactiva (mapas)
library(tidyverse) # Manejo de bases de datos
p_load(sf, readxl, janitor, scales, plotly, ggplot2)
library(raster)
library(htmlwidgets) # Para guardar paginas HTML
library(webshot)
library(extrafont)

sismos <- read_excel("01_Datos/sismos.xlsx")

sismos19s <- read_excel("01_Datos/Sismos19S.xlsx")

edo <- st_read("https://raw.githubusercontent.com/JuveCampos/MexicoSinIslas/master/Sin_islas.geojson",  quiet = TRUE)

#### Elabore un documento en HTML (en RMarkdown) que contenga al menos 4 de los siguientes 5 elementos:

###  1. Una gráfica en ggplot.

historial <- sismos %>% 
  group_by(AÑO, `TOTAL SISMOS`) %>% 
  count()

plt <- historial %>% 
  ggplot(aes(x = AÑO, 
             y = `TOTAL SISMOS`,
             fill = AÑO)) +
  geom_col(color = "white") +
  coord_flip() +
  labs(title = "Número de sismos registrados en México 1990-2019", x = "", y = "", caption = "Fuente: SSN") +
  theme(legend.position = "none")
plt

sum(sismos$`Magnitud 0 - 2.9 `, sismos$`Magnitud 3 - 3.9 `, sismos$`Magnitud 4 - 4.9`)
sum(sismos$`Magnitud 5 - 5.9`, sismos$`Magnitud 6 - 6.9`, sismos$`Magnitud 7 - 7.9`)
sum(sismos$`Magnitud 8 - 8.9`)
sum(sismos$`No calculable`)
sum(sismos$`Magnitud 7 - 7.9`)   

### 2. Un mapa estático en ggplot.

Mapa_Est <- merge(x = edo, 
                  y = sismos19s, 
                  by.x = "CVE_EDO", 
                  all.y = TRUE)


## Graficamos

Mapa_Est %>%   
  ggplot(aes(fill = ENTIDAD)) +
  geom_sf() +
  theme_minimal() +
  labs(title = "¿En qué estados volvió a temblar el 19-S?",
       caption = "Fuente: SSN", 
       fill = "CVE_EDO") +
  theme(axis.text = element_blank(), 
        panel.grid = element_blank(), 
        panel.border = element_blank(), 
        panel.background = element_rect(),
        axis.ticks = element_blank())

count(sismos19s)
count(sismos19s, sismos19s$Entidad)
count(sismos19s, sismos19s$Magnitud)

### 3. Una gráfica en plotly (u otra librería de visualización interactiva)

historial <- sismos %>% 
  group_by(AÑO, `TOTAL SISMOS`, `No calculable`, `Magnitud 8 - 8.9`, `Magnitud 5 - 5.9`, `Magnitud 6 - 6.9`, `Magnitud 7 - 7.9`) %>% 
  count()

plt <- historial %>% 
  ggplot(aes(x = AÑO, 
             y = `TOTAL SISMOS`,
             fill = AÑO,
             text = paste0("<b>No. sismos: </b>", `TOTAL SISMOS`, "<br>", "<b>Mayor a 8: </b>", `Magnitud 8 - 8.9`, "<br>", "<b>Entre 7 y 7.9: </b>", `Magnitud 7 - 7.9`, "<br>", "<b>Entre 6 y 6.9: </b>", `Magnitud 6 - 6.9`, "<br>", "<b>Entre 5 y 5.9: </b>", `Magnitud 5 - 5.9`, "<br>", "<b>No fueron calculables: </b>", `No calculable`, "<br>"))) +
  theme(legend.position = "none") +
  geom_col(color = "white") +
  coord_flip() +
  labs(title = "Sismos y magnitudes en México (1990-2019)", x = "", y = "")

### Interactivo

ggplotly(tooltip = "text")  %>% 
  config(displayModeBar = F)


### 4. Un mapa en leaflet (u otra librería de visualización interactiva)
 
leaflet() %>% 
  addProviderTiles(providers$CartoDB.DarkMatter) %>%
  addPolygons(data = Mapa_Est, color = "blue", fill = NA, opacity = .1, popup = paste0("<b>Entidad: </b>", Mapa_Est$Entidad, "<br>", "<b>Magnitud: </b>", Mapa_Est$Magnitud, "<br>", "<b>Fecha y Hora: </b>", Mapa_Est$`Fecha y Hora`, "<br>", "<b>Epicentro: </b>", Mapa_Est$Epicentro, "<br>")) 


