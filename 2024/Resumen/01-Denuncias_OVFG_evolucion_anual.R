# Limpiar todo
rm(list = ls())

# Funciones
`%ni%` <- Negate(`%in%`)

# Librerías
library(ggplot2)
library(dplyr)
library(stringr)
library(directlabels)
library(forecast)
library(tseries)

# Fuentes
library(showtext)
font_add_google("Barlow", "font")
showtext_auto()

# Crear datos
Raw <- read.csv(paste0(dirname(rstudioapi::getActiveDocumentContext()$path),"/Datos/Denuncias_OVFG.csv"))

# Crear serie temporal
Data_trimestral <- Raw %>%
  mutate(Año = factor(formatC(Año, big.mark=".", decimal.mark=",", format="d"))) %>%
  filter(Tipo %in% c("Familiar","Género")) %>%
  group_by(Año, Trimestre) %>%
  summarise(Cantidad = sum(Frecuencia)) %>%
  ungroup

Data_ts <- ts(Data_trimestral$Cantidad, start=c(2020, 1), frequency=4)

# Crear modelo ARIMA
Modelo_ARIMA <- auto.arima(Data_ts)

# Crear proyección
Prediccion <- forecast(Modelo_ARIMA, h=1)

# Añadir nuevo dato
Data <- Data_trimestral %>%
  group_by(Año) %>%
  summarise(Cantidad = sum(Cantidad)) %>%
  ungroup %>%
  add_row(Año = c("2.016", "2.017", "2.018", "2.019"), Cantidad = c(14695, 13619, 14584, 21707))

Estimacion <- (Data %>% filter(Año == "2.024"))$Cantidad + round(as.numeric(Prediccion$mean))

# Colores
Colores <- c("#6e3169", "#ec6489")

# Gráfico
grafico <- ggplot(Data, aes(x=Año, y=Cantidad)) +
  geom_col(aes(fill=Cantidad), width=0.9) +
  annotate(geom="rect", xmin=8.55, xmax=9.45, ymin=(Data %>% filter(Año == "2.024"))$Cantidad, ymax=Estimacion,
           linetype=2, color="gray", fill="gray", alpha=0.5) +
  geom_text(aes(y=Cantidad, label=formatC(Cantidad, big.mark = ".", decimal.mark = ",", format="d")), family="font", fontface="bold",
            color="white", size=6, vjust=2) +
  annotate(geom="text", x=9, y=Estimacion,
           label = formatC(Estimacion, big.mark = ".", decimal.mark=",", format="d"),
           vjust=2, family="font", fontface="bold", color="black", size=5) +
  annotate(geom="text", x=9, y=Estimacion,
           label = str_wrap("Proyección del número total de denuncias para el año 2.024 completo", width=25),
           vjust=-0.4, family="font", fontface="italic", color="gray", size=3) +
  labs(title="",
       x="Año", y="Cantidad") +
  scale_y_continuous(limits=c(0,max(Data$Cantidad+7000)), labels = function(z) formatC(z, big.mark=".", decimal.mark=",", format="d")) +
  theme_light() +
  scale_fill_gradient2(low="#f2904c", high="#c93131", mid="#e55a3e", midpoint=mean(Data$Cantidad)) +
  theme(text=element_text(family="font"), legend.position="none",
        plot.title = element_text(size=20, family="font", face="bold"),
        plot.subtitle = element_text(size=15, family="font"),
        plot.caption = element_text(size=12, family="font", face="italic"),
        panel.grid.major = element_line(colour = "#F5F5F5"),
        panel.grid.major.x = element_blank(),
        axis.text.x = element_text(size=20, margin = margin(t=10,r=0,b=5,l=0)),
        axis.text.y = element_text(size=15, margin = margin(t=0,r=10,b=0,l=5)),
        axis.title.x = element_text(size=20),
        axis.title.y = element_text(size=20))

# Guardar gráfico
filename <- str_sub(basename(rstudioapi::getSourceEditorContext()$path), 1,
                    str_length(unlist(basename(rstudioapi::getSourceEditorContext()$path)))-2)

ggsave(filename = paste0(filename, ".png"),
       path = paste0(dirname(rstudioapi::getActiveDocumentContext()$path),"/Graficos/PNG/"),
       plot=grafico, dpi=100, width=12, height=7)
ggsave(filename = paste0(filename, ".svg"), path=paste0(dirname(rstudioapi::getActiveDocumentContext()$path),"/Graficos/SVG/"),
       plot=grafico, dpi=72, width=12, height=7)

# Fuente: Elaboración propia a partir de la información remitida por la Oficina de Violencia Familiar y de Género. Corte de Justicia de Salta