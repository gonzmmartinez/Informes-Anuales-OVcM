# Limpiar todo
rm(list = ls())

# Funciones
`%ni%` <- function(x, table) !(x %in% table)

# Librerías
library(ggplot2)
library(dplyr)
library(stringr)
library(cowplot)
library(magick)
library(ggtext)
library(googlesheets4)
library(forcats)
library(tidyr)

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

# Leer datos
Raw <- read_sheet(ss = "https://docs.google.com/spreadsheets/d/1cl0-rAT-ARDgQQQIjCApJ_hXtrWXjImAGrFnmzMDukw/edit?usp=sharing",
                  sheet = "Edades",
                  range = "E3:G9")

# Crear datos
Data <- Raw %>%
  filter(Edad != "Sin dato") %>%
  mutate(Edad = factor(Edad, levels = c("Menor de 15 años", "15 a 19 años",
                                        "20 a 25 años", "26 a 35 años",
                                        "Mayor de 35 años"))) %>%
  pivot_longer(
    cols = c("2024", "2025"),
    names_to = "Año",
    values_to = "Cantidad"
  ) %>%
  mutate(Año = as.character(Año)) %>%
  group_by(Edad) %>%
  arrange(Año, .by_group = TRUE) %>%
  mutate(
    Anterior = lag(Cantidad),
    Variacion = ifelse(
      Año == "2025",
      (Cantidad - Anterior) / Anterior * 100,
      NA_real_
    )
  ) %>%
  ungroup() %>%
  mutate(
    Label = ifelse(
      is.na(Variacion) | is.infinite(Variacion),
      paste0("**", Cantidad, "**"),
      paste0(
        "**", Cantidad, "** (",
        ifelse(Variacion > 0, "+", ""),
        formatC(round(Variacion, 1), format = "fg", decimal.mark = ","),
        "%)"
      )
    )
  )

Sin_dato <- Raw %>%
  filter(Edad == "Sin dato") %>%
  mutate(Texto = paste0("Sin dato:<br>2024: **", `2024`, "**<br>2025: **", `2025`, "** (+",
                        formatC(round(100 * (`2025` - `2024`)/`2024`, 1), format="fg", decimal.mark=","), "%)")) %>%
  pull(Texto)

# Colores
Paleta2 <- c("#1e7b34", "#119ca0", "#b8d6ac", "#6963aa",
             "#7c428a", "#4c2158", "#c72a29", "#ec6230", "#cbc2ce")

# Definir colores
Colores <- c("2024" = "#ec6230",
             "2025" = "#c72a29")

# Gráfico
grafico <- ggplot(Data, aes(x = Edad, y = Cantidad)) +
  geom_col(aes(fill = Año, group = Año),
    width = 0.85, position = position_dodge(width = 0.85)) +
  geom_richtext(
    aes(y = Cantidad + round(max(Data$Cantidad) * 0.025, 1), label = Label, group = Año),
    family = "font_body", color = "black", size = 5, vjust = 0, fill = NA,
    label.color = NA, position = position_dodge(width = 0.85)) +
  geom_richtext(aes(x=4.5, y=60, label=Sin_dato), size=5, vjust=0.5, hjust=0, family="font_title",
                fill = NA, label.color = NA, color="grey", lineheight = 1.15) +
  labs(y="Cantidad", x="Edad de la persona gestante") +
  scale_y_continuous(expand = c(0.05, 0), limits=c(0, max(Data$Cantidad)*1.2)) +
  scale_fill_manual(values = Colores) +
  theme_light() +
  theme(text=element_text(family="font_body"),
        legend.position="top",
        legend.justification = "right",
        legend.title = element_text(size=12, family="font_title", face="bold", margin=margin(r=15)),
        legend.text = element_text(size=12, family="font_subtitle"),
        legend.key.spacing.x = unit(0.5, "cm"),
        plot.title = element_blank(),
        plot.subtitle = element_blank(),
        plot.caption = element_blank(),
        panel.grid = element_blank(),
        panel.grid.major = element_line(color="grey98", linewidth=0.5),
        axis.text.x = element_text(family="font_subtitle", size=15, margin = margin(t=10,r=0,b=5,l=0)),
        axis.text.y = element_text(family="font_subtitle", size=10, margin = margin(t=0,r=5,b=0,l=5)),
        axis.title.x = element_text(family="font_subtitle", size=15),
        axis.title.y = element_text(family="font_subtitle", size=15))

# Guardar gr?fico
filename <- str_sub(basename(rstudioapi::getSourceEditorContext()$path), 1,
                    str_length(unlist(basename(rstudioapi::getSourceEditorContext()$path)))-2)

ggsave(filename = paste0(filename, ".png"),
       path = paste0(dirname(rstudioapi::getActiveDocumentContext()$path),"/Graficos/PNG/"),
       plot=grafico, dpi=100, width=10, height=5)
ggsave(filename = paste0(filename, ".pdf"),
       path=paste0(dirname(rstudioapi::getActiveDocumentContext()$path),"/Graficos/PDF/"),
       plot=grafico, dpi=72, width=10, height=5)
