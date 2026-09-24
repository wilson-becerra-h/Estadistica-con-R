library(ggplot2)
library(dplyr)

# 1. Preparación de los datos
datos <- data.frame(
  Edad = c(15, 16, 17, 17, 18, 19,
           14, 15, 15, 18, 19, 21,
           13, 14, 15, 17, 20, 23),
  Grupo = factor(rep(c("Grupo 1 (σ = 1.41)", "Grupo 2 (σ = 2.76)", "Grupo 3 (σ = 3.85)"), each = 6))
)

# 2. Parámetros para las curvas normales (Media = 17)
parametros <- data.frame(
  Grupo = factor(c("Grupo 1 (σ = 1.41)", "Grupo 2 (σ = 2.76)", "Grupo 3 (σ = 3.85)")),
  media = 17,
  sd = c(1.414, 2.757, 3.847)
)

# Generar puntos para dibujar las campanas de Gauss
curvas <- do.call(rbind, lapply(1:nrow(parametros), function(i) {
  x <- seq(10, 24, length.out = 200)
  y <- dnorm(x, mean = parametros$media[i], sd = parametros$sd[i])
  data.frame(x = x, y = y, Grupo = parametros$Grupo[i])
}))

# 3. Construcción del gráfico con ggplot2
ggplot() +
  # Campanas de Gauss (sombra y línea)
  geom_area(data = curvas, aes(x = x, y = y, fill = Grupo), alpha = 0.25, show.legend = FALSE) +
  geom_line(data = curvas, aes(x = x, y = y, color = Grupo), linewidth = 1) +
  
  # Puntos individuales de los datos (similares a la imagen de la pizarra)
  geom_point(data = datos, aes(x = Edad, y = 0, color = Grupo), 
             position = position_jitter(width = 0.15, height = 0.005), 
             size = 3.5, alpha = 0.85) +
  
  # Línea vertical indicando la media común (x = 17)
  geom_vline(xintercept = 17, linetype = "dashed", color = "#E41A1C", linewidth = 0.8) +
  annotate("text", x = 17.2, y = 0.25, label = "Media = 17", color = "#E41A1C", fontface = "bold", hjust = 0) +
  
  # Separación por páneles (un panel por grupo)
  facet_wrap(~ Grupo, ncol = 1, scales = "free_y") +
  
  # Escalas y límites
  scale_x_continuous(breaks = seq(11, 23, by = 1), limits = c(10.5, 23.5)) +
  scale_color_manual(values = c("#2B5C8F", "#D95F02", "#7570B3")) +
  scale_fill_manual(values = c("#2B5C8F", "#D95F02", "#7570B3")) +
  
  # Formato visual
  labs(
    title = "Comparación de Dispersión y Campana de Gauss",
    subtitle = "Tres distribuciones con la misma media (x̄ = 17) pero distinta Desviación Típica (σ)",
    x = "Edad (años)",
    y = "Densidad de Probabilidad"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
    plot.subtitle = element_text(size = 10, hjust = 0.5, color = "gray30"),
    strip.text = element_text(face = "bold", size = 11),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "none"
  )
