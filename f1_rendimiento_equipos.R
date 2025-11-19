### Proyecto de Análisis de Datos de F1 ###

### Dominancia Histórica en la F1 ###

## Establecer el entorno de trabajo

setwd("C:\\Users\\zorri\\OneDrive\\Escritorio\\Proyecto_F1")

## Etapa 1 Cargar y almacenar los datos
# Instalación de Paquetes

install.packages("tidyverse")
library(tidyverse)

# 1.1. Cargar datos
results_df <- read_csv("results.csv")
races_df <- read_csv("races.csv")
constructors_df <-read_csv("constructors.csv")

# 1.2. Seleccionar y renombrar las columnas clave
# Objetivo evitar confusiones de nombres (ej: nombres de columnas repetidas)
races_clean <- races_df%>% 
  select(raceId, year, name) %>% 
  rename(race_name = name)

constructors_clean <- constructors_df %>% 
  rename(constructor_name = name)

# 1.3. Fusionar (JOIN)  los resultados con la información de carreras y equipos
# left_join para mantener todos los resultados y añadir la información relacionada
data_f1_completa <- results_df %>%
  # Unir con la información de carrea (por raceId)
  left_join(races_clean, by = "raceId") %>% 
  #Unir con los nombres de los constructores (por constructorId)
  left_join(constructors_clean, by = "constructorId")

# 1.4. Exploración rápida del dataset fusionado
print("Estructura de la tabla fusionada (data_f1_completa):")
print(paste("Número total de observaciones:", nrow(data_f1_completa)))
head(data_f1_completa)

## 2. Limpieza y cálculo del Margen de Victoria
margen_victoria_df <- data_f1_completa %>%
  # 1. Agrupar por carrera
  group_by(raceId, year, constructor_name, race_name) %>%
  
  # 2. Obtener el margen:
  mutate(
    # Obtener el valor de 'time' del P2. Esto solo funciona si el P2 terminó en la misma vuelta.
    time_gap_raw = time[position == 2][1],
    winning_constructor = constructor_name[position == 1][1]
  ) %>%
  
  # 3. Limpieza: Extraer solo el número del gap (ej. de "+1.500s" a 1.500)
  # Usamos str_replace para limpiar el '+' y la 's'.
  # Usamos str_remove para eliminar '+', y str_remove para eliminar 's'.
  mutate(
    Margin_s = as.numeric(str_remove(str_remove(time_gap_raw, "\\+"), "s"))
  ) %>%
  
  # 4. Filtrar solo las filas con un margen válido (P2 terminó en la misma vuelta)
  # El margen debe ser positivo, no NA, y menor a 600 segundos (10 minutos)
  filter(!is.na(Margin_s) & Margin_s > 0 & Margin_s < 600) %>% 
  
  # 5. Seleccionar las métricas claves del resultado de la carrera
  ungroup() %>%
  select(year, winning_constructor, race_name, Margin_s) %>%
  
  # 6. Usamos race_name y year para identificar la fila única de la carrera
  distinct(race_name, year, .keep_all = TRUE)

print("--- RESULTADOS DE LA ETAPA 2 SOLUCIÓN FINAL CON 'time' ---")
print(paste("Número de carreras con margen de victoria calculado:", nrow(margen_victoria_df)))
head(margen_victoria_df)
  
## 3. Transformación
  # Un margen MÁS BAJO indica una carrera CERRADA. Un margen MÁS ALTO indica DOMINANCIA
  
  # 1. Creación del dataframe Margen Limpio
margen_limpio_df <- margen_victoria_df %>%
  select(year, winning_constructor, Margin_s)

  # 2. Agregamos el Rendimiento Anual (Dominancia Promedio)
dominancia_anual <- margen_limpio_df %>%
  group_by(year, winning_constructor) %>%
  summarise(
    Avg_Margin_s = mean(Margin_s, na.rm = TRUE),
    Total_Wins = n(),
    .groups = 'drop' 
  ) %>%
  
  # 3. Filtramos por un mínimo de 3 victorias para un análisis significativo
  filter(Total_Wins >= 3) 

  # 4. Identificamos a los 5 constructores más ganadores históricamente para el análisis
top_constructors <- margen_limpio_df %>%
  count(winning_constructor, sort = TRUE) %>%
  slice_head(n = 5) %>%
  pull(winning_constructor)

  # 5. Filtramos los datos anuales para incluir solo esos equipos TOP
dominancia_top_anual <- dominancia_anual %>%
  filter(winning_constructor %in% top_constructors)

  # 6. Identificamos el punto de máxima dominancia para etiquetar (La tabla que da el pico)
pico_dominancia <- dominancia_top_anual %>%
  filter(Avg_Margin_s == max(Avg_Margin_s, na.rm = TRUE))

print("Tablas de agregación y pico creadas. Generando gráfico...")

## 4. Generar Gráfico de Dominancia
# Definimos la ruta y el nombre del archivo de salida
png("dominancia_f1_margen.png", width = 10, height = 6, units = "in", res = 300)

ggplot(dominancia_top_anual, aes(x = year, y = Avg_Margin_s, color = winning_constructor)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  labs(
    title = "Evolución del Margen de Victoria Promedio (Dominancia) de los Top 5 Equipos",
    subtitle = "Margen de victoria promedio (en segundos) en las carreras ganadas (Mínimo 3 victorias/año)",
    x = "Año",
    y = "Margen de Victoria Promedio (Segundos)",
    color = "Equipo Ganador"
  ) +
  
  # 1. Invertimos el eje Y: mayor margen (más dominancia) en la parte superior
  scale_y_reverse() + 
  theme_minimal() +
  
  # 2. Etiquetamos el pico máximo de dominancia histórica (USANDO LA TABLA PRECALCULADA)
  geom_text(data = pico_dominancia,
            aes(label = paste(winning_constructor, "\n", round(Avg_Margin_s, 2), "s"), x = year + 2), 
            vjust = 0, color = "red", fontface = "bold", size = 4) +
  
  # 3. Añadimos una línea de ayuda para el margen cero (carreras muy cerradas)
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50")
  # Cerramos el dispositivo gráfico para guardar el archivo
dev.off()

## 5. Contar Ganadores Únicos por Temporada

# Usamos la tabla completa (data_f1_completa) y nos enfocamos solo en la posición 1.
ganadores_absolutos_df <- data_f1_completa %>%
  
  # 1. Nos quedamos solo con los ganadores de la carrera
  filter(position == 1) %>%
  
  # 2. Excluimos filas donde el nombre del constructor no fue mapeado (NA)
  filter(!is.na(constructor_name)) %>%
  
  # 3. Agrupamos solo por el año
  group_by(year) %>%
  
  # 4. Contamos el número de constructores únicos que ganaron ese año
  summarise(
    Unique_Winners_Absoluto = n_distinct(constructor_name),
    .groups = 'drop' 
  )

print("--- Diversidad de Ganadores por Año (Conteo Absoluto) ---")
head(ganadores_absolutos_df)

## 6. Gráfico de Estabalidad absoluta
# Define la ruta y el nombre del archivo de salida
png("diversidad_f1_ganadores.png", width = 10, height = 6, units = "in", res = 300)

ggplot(ganadores_absolutos_df, aes(x = year, y = Unique_Winners_Absoluto)) +
  geom_line(color = "darkblue", linewidth = 1) +
  geom_point(color = "darkblue", size = 2) +
  # 1. Añadimos etiquetas a los picos (máxima diversidad)
  geom_text(data = ganadores_absolutos_df %>% filter(Unique_Winners_Absoluto == max(Unique_Winners_Absoluto)),
            aes(label = Unique_Winners_Absoluto), vjust = -1, color = "red", fontface = "bold") +
  # 2. Añadimos etiquetas a los valles (mínima diversidad)
  geom_text(data = ganadores_absolutos_df %>% filter(Unique_Winners_Absoluto == min(Unique_Winners_Absoluto)),
            aes(label = Unique_Winners_Absoluto), vjust = 2, color = "red", fontface = "bold") +
  labs(
    title = "Evolución de la Diversidad de Equipos Ganadores por Temporada (Absoluto)",
    subtitle = "Número de constructores únicos que ganaron al menos una carrera por año",
    x = "Año",
    y = "Número de Ganadores Únicos"
  ) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 5)) +
  theme_minimal()

# Cerramos el dispositivo gráfico para guardar el archivo
dev.off()