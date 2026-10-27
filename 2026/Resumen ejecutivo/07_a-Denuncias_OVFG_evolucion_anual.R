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
library(sysfonts)
library(showtext)
dir <- paste0(dirname(dirname(rstudioapi::getActiveDocumentContext()$path)), "/Fonts/")
font_add(family = "font_title",
         bold = file.path(dir, "CreatoDisplay-ExtraBold.otf"),
         regular = file.path(dir, "CreatoDisplay-Regular.otf"))
font_add("font_subtitle", file.path(dir, "CreatoDisplay-Regular.otf"))
font_add(family = "font_body",
         regular = file.path(dir, "RobotoSlab-Regular.ttf"),
         bold = file.path(dir, "RobotoSlab-Bold.ttf"))
showtext_auto()

# Leer datos
Raw <- read_sheet(ss = "https://docs.google.com/spreadsheets/d/1Cfbecjc5DLo3uGsMEHscsfUC9YOtnKtFvt1bOZI_B4c/edit?usp=sharing",
                  sheet = "Ingresadas") %>%
  filter(Tipo != "No configura VFG")

# Crear serie temporal
Data_trimestral <- Raw %>%
  mutate(Año = factor(Año)) %>%
  filter(Tipo %in% c("Familiar","Género")) %>%
  group_by(Año, Trimestre) %>%
  summarise(Cantidad = sum(Frecuencia)) %>%
  ungroup %>%
  add_row(Año = rep(c("2016", "2017", "2018", "2019"), each=4),
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

Estimacion <- (Data %>% filter(Año == "2026"))$Cantidad + round(sum(as.numeric(Prediccion$mean)))

# Colores
Paleta <- c("#1e7b34", "#119ca0", "#b8d6ac", "#6963aa",
            "#7c428a", "#4c2158", "#c72a29", "#ec6230", "#cbc2ce")

# Gráfico
grafico <- ggplot(Data, aes(x=Año, y=Cantidad)) +
  geom_col(aes(fill=Cantidad), width=0.85) +
  annotate(geom="rect", xmin=10.55, xmax=11.45, ymin=(Data %>% filter(Año == "2026"))$Cantidad, ymax=Estimacion,
           linetype=2, color="gray", fill="gray", alpha=0.5) +
  geom_text(aes(y=Cantidad, label=formatC(Cantidad, big.mark = ".", decimal.mark = ",", format="d")),
            family="font_body", fontface="bold",
            color="white", size=6, vjust=2) +
  annotate(geom="text", x=11, y=Estimacion,
           label = formatC(Estimacion, big.mark = ".", decimal.mark=",", format="d"),
           vjust=2, family="font_body", fontface="bold", color="black", size=6) +
  annotate(geom="text", x=11, y=Estimacion+750,
           label = str_wrap("Proyección del número total de denuncias para el año 2026 completo", width=20),
           vjust=0, family="font_title", fontface="bold", color="gray", size=4,
           lineheight=0.9) +
  labs(title="",
       x="Año", y="Cantidad") +
  scale_y_continuous(limits=c(0,max(Data$Cantidad)*1.15),
                     labels = function(z) formatC(z, big.mark=".", decimal.mark=",", format="d"),
                     expand = c(0.025, 0)) +
  theme_light() +
  scale_fill_gradient(low="#8AB68C", high="#1e7b34") +
  theme(text=element_text(family="font_title"), legend.position="none",
        plot.title = element_blank(),
        plot.subtitle = element_blank(),
        plot.caption = element_blank(),
        panel.grid = element_blank(),
        panel.grid.major = element_line(colour = "grey95"),
        panel.grid.major.x = element_blank(),
        panel.border = element_blank(),
        axis.line.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.line.x = element_line(color="darkgrey", linewidth=0.5),
        axis.title.x = element_blank(),
        axis.text.x = element_text(size=20, family="font_sans", margin = margin(t=10,r=0,b=5,l=0)))

# Guardar gráfico
filename <- str_sub(basename(rstudioapi::getSourceEditorContext()$path), 1,
                    str_length(unlist(basename(rstudioapi::getSourceEditorContext()$path)))-2)

ggsave(filename = paste0(filename, ".png"),
       path = paste0(dirname(rstudioapi::getActiveDocumentContext()$path),"/Graficos/PNG/"),
       plot=grafico, dpi=100, width=15, height=6)
ggsave(filename = paste0(filename, ".pdf"),
       path=paste0(dirname(rstudioapi::getActiveDocumentContext()$path),"/Graficos/PDF/"),
       plot=grafico, dpi=72, width=15, height=6)

