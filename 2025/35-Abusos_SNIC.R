# Limpiar todo
rm(list = ls())

# Funciones
`%ni%` <- Negate(`%in%`)

# Librer?as
library(ggplot2)
library(dplyr)
library(stringr)
library(scales)
library(googlesheets4)
library(tidyr)

# Fuentes
library(showtext)
font_add_google("Source Sans 3", "font_sans")
font_add_google("Source Serif 4", "font_serif")
showtext_auto()

# Crear datos
Data <- data.frame(matrix(c(2014, 318, 503, 821,
                            2015, 367, 558, 925,
                            2016, 250, 716, 966,
                            2017, 290, 870, 1160,
                            2018, 272, 840, 1112,
                            2019, 446, 1196, 1642,
                            2020, 328, 1113, 1441,
                            2021, 336, 1297, 1633,
                            2022, 365, 1409, 1774,
                            2023, 356, 1443, 1799,
                            2024, 298, 1315, 1613),
                          nrow=11, byrow=TRUE)) %>%
  select(-X4) %>%
  rename(Año = X1) %>%
  pivot_longer(cols = c(X2, X3), names_to = "Tipo", values_to = "Cantidad") %>%
  mutate(Tipo = ifelse(Tipo == "X2", "Violaciones", "Otros delitos contra la integridad sexual"))

# Colores
Paleta <- c("#206170", "#5ec5d4", "#a782ec", "#852f8c", "#0f216d", "#2b42a0",
            "#ff9d27", "#ff621d", "#f93e35", "#d3335e", "#cbc2ce")

# Definir colores
Colores <- c("Violaciones" = "#f93e35",
             "Otros delitos contra la integridad sexual" = "#a782ec")

# Gráfico
grafico <- ggplot(Data, aes(x=Año, y=Cantidad, fill=Tipo)) +
  geom_col(position="stack", width=0.75) +
  geom_text(aes(label = formatC(Cantidad, big.mark = ".", decimal.mark=",", format="fg")),
            family="font_sans", position = position_stack(vjust = 0.5), size=5, color="white") +
  geom_text(data = Data %>% group_by(Año) %>% summarise(Cantidad = sum(Cantidad)), inherit.aes=FALSE,
            aes(x=Año, y=Cantidad, label = formatC(Cantidad, big.mark = ".", decimal.mark=",", format="fg")),
            family="font_sans", size=7.5, fontface="bold", color="black", nudge_y=75) +
  labs(y="Cantidad de casos", x="Año") +
  scale_fill_manual(name = str_wrap("Tipo de delito", width=20),
                    values = Colores) +
  scale_y_continuous(labels = function(x) formatC(x, big.mark = ".", decimal.mark = ",", format="fg"),
                     limits= c(0, 2000)) +
  scale_x_continuous(labels = function(x) formatC(x, big.mark = ".", decimal.mark = ",", format="fg"),
                     expand = c(0.01, 0.01), breaks = 2014:2024) +
  theme_light() +
  theme(text=element_text(family="font_sans"),
        legend.position="top",
        legend.justification = "right",
        legend.title = element_text(size=10, family="font_serif"),
        legend.text = element_text(size=12, family="font_sans"),
        legend.key.spacing.x = unit(1, "cm"),
        plot.title = element_blank(),
        plot.subtitle = element_blank(),
        plot.caption = element_blank(),
        panel.grid = element_blank(),
        panel.grid.major = element_line(color="grey95", linewidth=0.5),
        axis.text.x = element_text(size=15, margin = margin(t=10,r=0,b=5,l=0)),
        axis.text.y = element_text(size=15, margin = margin(t=0,r=10,b=0,l=5)),
        axis.title.x = element_text(size=20),
        axis.title.y = element_text(size=20))

# Guardar gr?fico
filename <- str_sub(basename(rstudioapi::getSourceEditorContext()$path), 1,
                    str_length(unlist(basename(rstudioapi::getSourceEditorContext()$path)))-2)

ggsave(filename = paste0(filename, ".png"),
       path = paste0(dirname(rstudioapi::getActiveDocumentContext()$path),"/Graficos/PNG/"),
       plot=grafico, dpi=100, width=12, height=7)
ggsave(filename = paste0(filename, ".pdf"),
       path=paste0(dirname(rstudioapi::getActiveDocumentContext()$path),"/Graficos/PDF/"),
       plot=grafico, dpi=72, width=12, height=7)