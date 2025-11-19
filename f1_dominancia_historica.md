🏎️ Estudio de Caso: Dominancia y Competitividad Histórica en la Fórmula 1 (1950-2020)
Resumen
Este proyecto utiliza el análisis de datos para cuantificar la evolución de la competitividad en el deporte de la Fórmula 1 a lo largo de 70 años. Se construyeron dos métricas clave: el Margen de Victoria Promedio y la Diversidad de Equipos Ganadores Anuales. El hallazgo principal es que la F1 moderna es intensa (carreras con márgenes mínimos) pero concentrada (poca diversidad de equipos ganadores), contrastando con la era fundacional (1950s), que era amplia pero desigual.

1. Planteamiento del Problema y Objetivos 🎯
Problema: ¿Cómo ha cambiado la dinámica de dominancia y rendimiento de los equipos de Fórmula 1 a lo largo de las décadas (1950-2020)?

Objetivos del Proyecto:

Adquirir, limpiar y fusionar datos históricos de resultados, carreras y constructores (Kaggle/Ergast).

Calcular métricas que permitan cuantificar la intensidad (Margen de Victoria) y la frecuencia (Diversidad de Ganadores) de la competitividad.

Visualizar ambas tendencias para extraer conclusiones sobre el estado de la F1 moderna.

2. Metodología y Herramientas 💾
Herramientas: Lenguaje R (RStudio) y la librería tidyverse (dplyr, ggplot2).

Fuente de Datos: Dataset histórico de F1 (Kaggle / Ergast API).

Desafíos Resueltos (Ingeniería de Datos)
El principal desafío fue que la columna estándar de tiempo para el P2 (milliseconds) a menudo contenía valores gigantescos (tiempo total de carrera) o \N, no el margen de victoria.

Solución: Se implementó una lógica de limpieza utilizando expresiones regulares sobre la columna de texto time para extraer el margen de victoria en segundos, descartando las carreras en las que el P2 terminó a más de una vuelta.

R

# Lógica para extraer el margen (el gap) de la columna de texto 'time'
Margin_s = as.numeric(str_remove(str_remove(time_gap_raw, "\\+"), "s"))

3. Análisis de Datos y Limpieza (Métricas Clave) 🧪
Margen de Victoria Promedio (Intensidad): Mide el margen de tiempo promedio entre el P1 y el P2 en carreras ganadas (Margen bajo = Alta competitividad).

Diversidad de Equipos Ganadores (Frecuencia): Mide el número de constructores únicos que ganaron al menos una carrera en una temporada.

R

# Código para el cálculo de Diversidad (Frecuencia)
ganadores_absolutos_df <- data_f1_completa %>%
    filter(position == 1, !is.na(constructor_name)) %>%
    group_by(year) %>%
    summarise(Unique_Winners = n_distinct(constructor_name))
    
4. Visualizaciones y Resultados Clave 📊
4.1. Gráfico A: Evolución del Margen de Victoria (Dominancia)
Hallazgo Clave: El período de mayor dominancia ocurrió en 1950, con un margen de victoria promedio de 49.84 segundos, demostrando la disparidad inicial.

Tendencia: Las líneas de tendencia en el eje Y (Margen) se mueven constantemente hacia el cero en la era moderna, lo que indica que la F1 es significativamente más competitiva hoy que en sus inicios.

4.2. Gráfico B: Evolución de la Diversidad de Equipos Ganadores
Hallazgo Clave: La máxima diversidad se registró en 1977, con 7 constructores únicos ganando carreras, reflejando una parrilla muy abierta.

Tendencia: La tendencia moderna muestra una concentración de las victorias. El campo se reduce a un puñado de equipos de élite, evidenciando una menor estabilidad.

5. Conclusiones e Implicaciones (La Tesis Final)
El análisis combinado de ambas métricas revela la naturaleza dual de la F1 moderna:

Competitividad Extrema (Intensidad Alta): La convergencia tecnológica ha hecho que las carreras se ganen por márgenes mínimos (ej. menos de 10 segundos en promedio), lo que demuestra la alta competencia técnica.

Baja Estabilidad (Concentración de Éxito): A pesar de la alta intensidad, la diversidad de ganadores es baja (rara vez supera los 4 o 5 equipos por año). Esto sugiere que solo un puñado de equipos de élite tiene la capacidad de luchar por esas victorias estrechas.

El proyecto concluye que la F1 ha evolucionado desde ser desigual y diversa a ser intensa y concentrada.
