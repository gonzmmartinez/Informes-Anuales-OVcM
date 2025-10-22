# Limpiar todo
rm(list = ls())

# Funciones
`%ni%` <- Negate(`%in%`)

# Librer?as
library(ggplot2)
library(dplyr)
library(stringr)
library(directlabels)
library(forecast)
library(tseries)
library(googlesheets4)

# Fuentes
library(showtext)
font_add_google("Source Sans 3", "font_sans")
font_add_google("Source Serif 4", "font_serif")
showtext_auto()

# Leer datos
Raw <- read_sheet(ss = "https://docs.google.com/spreadsheets/d/1Cfbecjc5DLo3uGsMEHscsfUC9YOtnKtFvt1bOZI_B4c/edit?usp=sharing",
                  sheet = "Ingresadas") %>%
  filter(Tipo != "No configura VFG")

# Crear serie temporal
Data_trimestral <- Raw %>%
  mutate(Año = factor(formatC(Año, big.mark=".", decimal.mark=",", format="d"))) %>%
  filter(Tipo %in% c("Familiar","Género")) %>%
  group_by(Año, Trimestre) %>%
  summarise(Cantidad = sum(Frecuencia)) %>%
  ungroup %>%
  add_row(Año = rep(c("2.016", "2.017", "2.018", "2.019"), each=4),
          Trimestre = rep(1:4, 4),
          Cantidad = round(c(7255*0.52, 7255*0.48, 7440*0.48, 7440*0.52, 7253*0.52, 7253*0.48, 6366*0.48, 6366*0.52,
                             5876*0.52, 5876*0.48, 8708*0.48, 8708*0.52, 10538*0.52, 10538*0.48, 11169*0.48, 11169*0.52),0)) %>%
  arrange(Año, Trimestre)

Data_ts <- ts(Data_trimestral$Cantidad, start=c(2016, 1), frequency=4)

# Crear modelo ARIMA
Modelo_ARIMA <- auto.arima(Data_ts)

# Crear proyección
Prediccion <- forecast(Modelo_ARIMA, h=2)

# Añadir nuevo dato
Data <- Data_trimestral %>%
  group_by(Año) %>%
  summarise(Cantidad = sum(Cantidad))

Estimacion <- (Data %>% filter(Año == "2.025"))$Cantidad + round(sum(as.numeric(Prediccion$mean)))

# Colores
Paleta <- c("#206170", "#5ec5d4", "#a782ec", "#852f8c", "#0f216d", "#2b42a0",
            "#ff9d27", "#ff621d", "#f93e35", "#d3335e", "#cbc2ce")

# Gráfico
grafico <- ggplot(Data, aes(x=Año, y=Cantidad)) +
  geom_col(aes(fill=Cantidad), width=0.9) +
  annotate(geom="rect", xmin=9.55, xmax=10.45, ymin=(Data %>% filter(Año == "2.025"))$Cantidad, ymax=Estimacion,
           linetype=2, color="gray", fill="gray", alpha=0.5) +
  geom_text(aes(y=Cantidad, label=formatC(Cantidad, big.mark = ".", decimal.mark = ",", format="d")), family="font_sans", fontface="bold",
            color="white", size=6, vjust=2) +
  annotate(geom="text", x=10, y=Estimacion,
           label = formatC(Estimacion, big.mark = ".", decimal.mark=",", format="d"),
           vjust=2, family="font_sans", fontface="bold", color="black", size=6) +
  annotate(geom="text", x=10, y=Estimacion+750,
           label = str_wrap("Proyección del número total de denuncias para el año 2.025 completo", width=20),
           vjust=0, family="font_sans", fontface="italic", color="gray", size=4) +
  annotate(geom="segment", y=29037, yend=29037, x=8, xend=9.45, linetype=1, color="#f93e35", linewidth=1) +
  annotate(geom="text", x=8, y=29037+750,
           label = "Estimación realizada\nen 2.024:\n 29.037 denuncias",
           vjust=0, hjust=0, family="font_sans", fontface="italic", color="#f93e35", size=4) +
  labs(title="",
       x="Año", y="Cantidad") +
  scale_y_continuous(limits=c(0,max(Data$Cantidad+7000)), labels = function(z) formatC(z, big.mark=".", decimal.mark=",", format="d")) +
  theme_light() +
  scale_fill_gradient(low="#8790b6", high="#0f216d") +
  theme(text=element_text(family="font_sans"), legend.position="none",
        plot.title = element_blank(),
        plot.subtitle = element_blank(),
        plot.caption = element_text(size=12, family="font_sans", face="italic"),
        panel.grid.major = element_line(colour = "#F5F5F5"),
        panel.grid.major.x = element_blank(),
        axis.text.x = element_text(size=20, family="font_sans", margin = margin(t=10,r=0,b=5,l=0)),
        axis.text.y = element_text(size=15, family="font_sans", margin = margin(t=0,r=10,b=0,l=5)),
        axis.title.x = element_text(size=20, family="font_sans"),
        axis.title.y = element_text(size=20, family="font_sans"))

# Guardar gráfico
filename <- str_sub(basename(rstudioapi::getSourceEditorContext()$path), 1,
                    str_length(unlist(basename(rstudioapi::getSourceEditorContext()$path)))-2)

ggsave(filename = paste0(filename, ".png"),
       path = paste0(dirname(rstudioapi::getActiveDocumentContext()$path),"/Graficos/PNG/"),
       plot=grafico, dpi=100, width=12, height=7)
ggsave(filename = paste0(filename, ".pdf"),
       path=paste0(dirname(rstudioapi::getActiveDocumentContext()$path),"/Graficos/PDF/"),
       plot=grafico, dpi=72, width=12, height=7)
