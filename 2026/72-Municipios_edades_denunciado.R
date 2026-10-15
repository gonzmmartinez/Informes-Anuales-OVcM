# Limpiar todo
rm(list = ls())

# Librerías
library(tidyverse)
library(ggtext)

# Funciones
`%nin%` = Negate(`%in%`)

# Fuentes
library(sysfonts)
library(showtext)
dir <- paste0(dirname(rstudioapi::getActiveDocumentContext()$path), "/Fonts/")
font_add(family = "font_title",
         bold = file.path(dir, "CreatoDisplay-ExtraBold.otf"),
         regular = file.path(dir, "CreatoDisplay-Regular.otf"))
font_add("font_subtitle", file.path(dir, "CreatoDisplay-Regular.otf"))
font_add(family = "font_body",
         regular = file.path(dir, "RobotoSlab-Regular.ttf"),
         bold = file.path(dir, "RobotoSlab-Bold.ttf"))
showtext_auto()

# Cargar datos
Raw <- read.csv(file=paste0(dirname(rstudioapi::getActiveDocumentContext()$path), "/Datos/Registro_Asistencias_Completo_2026.csv")) %>%
  rename(Edad_old = "Edad",
         Edad = "Edad.1")

Tabla <- data.frame(
  Rango_etario = c("0-4", "5-9", "10-14", "15-19", "20-24", "25-29", "30-34",
                   "35-39", "40-44", "45-49", "50-54", "55-59", "60-64", "65-69",
                   "70-74", "75-79", "80-84", "85-89", "90-94", "Sin dato"),
  Rango_ord = 1:20)

# Modificar datos
Data <- Raw %>%
  mutate(Edad = as.numeric(Edad)) %>%
  mutate(
    Rango_etario = cut(
      Edad,
      breaks = c(0, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70, 75, 80, 85, 90, 95),
      labels = c(
        "0-4", "5-9", "10-14", "15-19", "20-24", "25-29", "30-34",
        "35-39", "40-44", "45-49", "50-54", "55-59", "60-64",
        "65-69", "70-74", "75-79", "80-84", "85-89", "90-94"
      ),
      right = FALSE
    ),
    Rango_etario = as.character(Rango_etario),
    Rango_etario = ifelse(is.na(Edad), "Sin dato", Rango_etario),
    Rango_etario = factor(
      Rango_etario,
      levels = c(
        "0-4", "5-9", "10-14", "15-19", "20-24", "25-29", "30-34",
        "35-39", "40-44", "45-49", "50-54", "55-59", "60-64",
        "65-69", "70-74", "75-79", "80-84", "85-89", "90-94",
        "Sin dato"
      )
    )
  ) %>%
  left_join(Tabla, by = "Rango_etario") %>%
  group_by(Rango_etario, Rango_ord) %>%
  summarise(Cantidad = n())

# Colores
Paleta <- c("#1e7b34", "#119ca0", "#b8d6ac", "#6963aa",
            "#7c428a", "#4c2158", "#c72a29", "#ec6230", "#cbc2ce")

# Gráfico
grafico <- ggplot(subset(Data, Rango_etario != "Sin dato"),
                  aes(x=reorder(Rango_etario, Rango_ord), y=Cantidad)) +
  geom_col(aes(fill = Cantidad), width=0.9) +
  geom_text(aes(y=Cantidad, label=Cantidad), family="font_body", color="black",
            size=5, nudge_y=max(subset(Data, Rango_etario != "Sin dato")$Cantidad)*0.05) +
  annotate(geom="text", x=14, y=max(subset(Data, Rango_etario != "Sin dato")$Cantidad)*1.05,
           label=paste0("Sin dato: ", formatC(subset(Data, Rango_etario == "Sin dato")$Cantidad, format="fg", big.mark=".", decimal.mark=",")),
           family="font_body", color="grey50", size=5, hjust=1, vjust=0.5) +
  labs(title="",
       x="Rango etario", y="Cantidad") +
  theme_light() +
  scale_fill_gradient(low="#D3BFD9", high="#4c2158") +
  scale_y_continuous(limits=c(0, max(subset(Data, Rango_etario != "Sin dato")$Cantidad)*1.1)) +
  theme(text=element_text(family="font_title"), legend.position="none",
        plot.title = element_blank(),
        plot.subtitle = element_blank(),
        plot.caption = element_text(size=12, family="font_title", face="italic"),
        panel.grid.major = element_line(colour = "grey95", linewidth = 0.5),
        panel.grid = element_blank(),
        axis.text.x = element_text(family="font_title", size=12.5, margin = margin(t=10,r=0,b=5,l=0)),
        axis.text.y = element_text(family="font_title", size=10, margin = margin(t=0,r=10,b=0,l=5)),
        axis.title.x = element_text(family="font_title", size=15),
        axis.title.y = element_text(family="font_title", size=15))

# Guardar gráfico
filename <- str_sub(basename(rstudioapi::getSourceEditorContext()$path), 1,
                    str_length(unlist(basename(rstudioapi::getSourceEditorContext()$path)))-2)

ggsave(filename = paste0(filename, ".png"),
       path = paste0(dirname(rstudioapi::getActiveDocumentContext()$path),"/Graficos/PNG/"),
       plot=grafico, dpi=100, width=12, height=6)
ggsave(filename = paste0(filename, ".pdf"),
       path = paste0(dirname(rstudioapi::getActiveDocumentContext()$path),"/Graficos/PDF/"),
       plot=grafico, dpi=72, width=12, height=6)