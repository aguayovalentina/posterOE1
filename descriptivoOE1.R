names (base_partoigualigualuno)
View (base_partoigualigualuno)

library(dplyr)
library(ggplot2)

################ MES-AÑO 2000-2024

# Crear vector ordenado de meses-años de enero 2014 a diciembre 2024
meses <- rep(1:12, times = 11)     # 11 años (2014-2024)
anos <- rep(2014:2024, each = 12)
niveles_mes_ano <- paste(meses, anos, sep = "-")

# Crear la variable mes_ano en la base de datos
base_partoigualigualuno$mes_ano <- factor(paste(base_partoigualigualuno$MES_NAC, base_partoigualigualuno$ANO_NAC, sep = "-"),
                                          levels = niveles_mes_ano)

# Calcular proporción de PP == 1 para cada mes_ano
proporcion_PP <- aggregate(PP ~ mes_ano, data = base_partoigualigualuno, 
                           FUN = function(x) mean(x == 1, na.rm = TRUE))

# Graficar
ggplot(proporcion_PP, aes(x = mes_ano, y = PP, group = 1)) +
  geom_line(color = "blue") +
  geom_point() +
  labs(x = "Mes-Año", y = "Proporción de PP = 1",
       title = "Proporción de PP por Mes y Año") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


ggplot(proporcion_PP, aes(x = mes_ano, y = PP, group = 1)) +
  geom_line(color = "blue") +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "purple", size = 1) +  # línea de tendencia lila
  labs(x = "Mes-Año", y = "Proporción de PP = 1",
       title = "Proporción de PP por Mes y Año") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))





########################## quincena 2020-2024
# Crear variable mes-año
base_iiu_20_24$mes_ano <- paste(base_iiu_20_24$MES_NAC, base_iiu_20_24$ANO_NAC, sep = "-")

# Definir niveles cronológicos para mes-año
meses <- rep(1:12, times = 5)        # 5 años (2020-2024)
anos <- rep(2020:2024, each = 12)
niveles_mes_ano <- paste(meses, anos, sep = "-")

# Ordenar mes_ano como factor
base_iiu_20_24$mes_ano <- factor(base_iiu_20_24$mes_ano, levels = niveles_mes_ano)

# Calcular promedio PP por mes-año y quincena
resumen_quincena <- aggregate(
  PP ~ mes_ano + quincena,
  data = base_iiu_20_24,
  FUN = function(x) mean(x, na.rm = TRUE)
)

# Crear una variable numerando las quincenas cronológicamente
resumen_quincena <- resumen_quincena[order(resumen_quincena$mes_ano, resumen_quincena$quincena), ]
resumen_quincena$quincena_index <- seq_len(nrow(resumen_quincena))

# Graficar
library(ggplot2)

ggplot(resumen_quincena, aes(x = quincena_index, y = PP)) +
  geom_line(color = "blue") +
  geom_point(color = "blue") +
  labs(
    x = "Quincena (Enero 2020 - Diciembre 2024)",
    y = "Proporción de PP = 1",
    title = "Proporción de PP por Quincena (2020-2024)"
  ) +
  theme_minimal()
# Crear etiquetas
resumen_quincena$label <- paste0(
  gsub("-", "-", resumen_quincena$mes_ano),
  " Q", resumen_quincena$quincena
)

# Graficar con etiquetas
ggplot(resumen_quincena, aes(x = quincena_index, y = PP)) +
  geom_line(color = "blue") +
  geom_point(color = "blue") +
  scale_x_continuous(breaks = resumen_quincena$quincena_index,
                     labels = resumen_quincena$label) +
  labs(
    x = "Quincena",
    y = "Proporción de PP = 1",
    title = "Proporción de PP por Quincena (2020-2024)"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))



################ SOLO VALPO
# Filtrar solo COMUNA == 5101
base_5101 <- base_iiu_20_24[base_iiu_20_24$COMUNA == 5101, ]

# Crear variable mes-año
base_5101$mes_ano <- paste(base_5101$MES_NAC, base_5101$ANO_NAC, sep = "-")

# Definir niveles cronológicos para mes-año
meses <- rep(1:12, times = 5)        # 5 años (2020-2024)
anos <- rep(2020:2024, each = 12)
niveles_mes_ano <- paste(meses, anos, sep = "-")

# Ordenar mes_ano como factor
base_5101$mes_ano <- factor(base_5101$mes_ano, levels = niveles_mes_ano)

# Calcular promedio PP por mes-año y quincena
resumen_quincena_5101 <- aggregate(
  PP ~ mes_ano + quincena,
  data = base_5101,
  FUN = function(x) mean(x, na.rm = TRUE)
)

# Ordenar quincenas cronológicamente
resumen_quincena_5101 <- resumen_quincena_5101[order(resumen_quincena_5101$mes_ano, resumen_quincena_5101$quincena), ]
resumen_quincena_5101$quincena_index <- seq_len(nrow(resumen_quincena_5101))

# (Opcional) Crear etiquetas para eje x
resumen_quincena_5101$label <- paste0(
  gsub("-", "-", resumen_quincena_5101$mes_ano),
  " Q", resumen_quincena_5101$quincena
)

# Graficar
library(ggplot2)

ggplot(resumen_quincena_5101, aes(x = quincena_index, y = PP)) +
  geom_line(color = "blue") +
  geom_point(color = "blue") +
  scale_x_continuous(
    breaks = resumen_quincena_5101$quincena_index,
    labels = resumen_quincena_5101$label
  ) +
  labs(
    x = "Quincena (Enero 2020 - Diciembre 2024)",
    y = "Proporción de PP = 1",
    title = "Proporción de PP por Quincena en COMUNA 5101 (2020-2024)"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))




ggplot(resumen_quincena_5101, aes(x = quincena_index, y = PP)) +
  geom_line(color = "blue") +
  geom_point(color = "blue") +
  geom_smooth(method = "lm", se = TRUE, color = "orchid", linewidth = 1.2) +  # línea lila
  scale_x_continuous(
    breaks = resumen_quincena_5101$quincena_index,
    labels = resumen_quincena_5101$label
  ) +
  labs(
    x = "Quincena (Enero 2020 - Diciembre 2024)",
    y = "Proporción de PP = 1",
    title = "Proporción de PP por Quincena en COMUNA 5101 (2020-2024)"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))





# Filtrar solo COMUNA 5101 y meses enero-marzo 2024
base_5101_2024 <- base_partoigualigualuno[
  base_partoigualigualuno$COMUNA == 5101 &
    base_partoigualigualuno$ANO_NAC == 2024 &
    base_partoigualigualuno$MES_NAC %in% c(1, 2, 3),
]

# Crear variable fecha
base_5101_2024$fecha <- as.Date(
  paste(
    base_5101_2024$ANO_NAC,
    base_5101_2024$MES_NAC,
    base_5101_2024$DIA_NAC,
    sep = "-"
  )
)

# Ordenar base por fecha
base_5101_2024 <- base_5101_2024[order(base_5101_2024$fecha), ]

# Calcular proporción diaria de PP
resumen_diario <- aggregate(
  PP ~ fecha,
  data = base_5101_2024,
  FUN = function(x) mean(x, na.rm = TRUE)
)

# Ordenar también la tabla resumen por fecha
resumen_diario <- resumen_diario[order(resumen_diario$fecha), ]

# Mostrar tabla ordenada
print(resumen_diario)

