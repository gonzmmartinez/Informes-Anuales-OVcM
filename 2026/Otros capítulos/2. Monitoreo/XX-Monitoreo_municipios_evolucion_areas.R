# Limpiar todo
rm(list = ls())

# Funciones
`%ni%` <- Negate(`%in%`)

# Librerías
library(ggplot2)
library(ggtext)
library(dplyr)
library(stringr)
library(cowplot)
library(magick)
library(googlesheets4)
library(tidyr)
library(ggimage)
library(fontawesome)
library(grid)

# Fuentes
library(sysfonts)
library(showtext)
dir <- paste0(str_sub(getwd(), 1, -30), "/Fonts/")
font_add(family = "font_title",
         bold = file.path(dir, "CreatoDisplay-ExtraBold.otf"),
         regular = file.path(dir, "CreatoDisplay-Regular.otf"))
font_add("font_subtitle", file.path(dir, "CreatoDisplay-Regular.otf"))
font_add(family = "font_body",
         regular = file.path(dir, "RobotoSlab-Regular.ttf"),
         bold = file.path(dir, "RobotoSlab-Bold.ttf"))
showtext_auto()

tryCatch(
  {
    font_add(
      family = "fa",
      regular = "C:/Users/Usuario/AppData/Local/Microsoft/Windows/Fonts/Font Awesome 7 Free-Solid-900.otf"
    )
  },
  error = function(e) {
    font_add(
      family = "fa",
      regular = "C:/Users/user/AppData/Local/Microsoft/Windows/Fonts/Font Awesome 7 Free-Solid-900.otf"
    )
  }
)

showtext_auto()

# Función para obtener unicode del ícono
icon_unicode <- function(icon) {
  intToUtf8(
    strtoi(
      fa_metadata()[[icon]]$unicode,
      16L
    )
  )
}

# Cargar datos
Raw <- read.csv(
  paste0(
    dirname(rstudioapi::getActiveDocumentContext()$path),
    "/Datos/Mapa_poseen_area.csv"
  )
) %>%
  mutate(ID = as.character(ID)) %>%
  rename(in1 = "ID") %>%
  mutate(Icono_2024 = case_when(Area_2024 == "Con área"    ~ "\uf00c",
                                Area_2024 == "Sin área"    ~ "\uf00d",
                                Area_2024 == "Absorbida"   ~ "\uf021",
                                Area_2024 == "No responde" ~ "\uf068"),
         Icono_2026 = case_when(
           Area_2026 == "Con área"    ~ "\uf00c",
           Area_2026 == "Sin área"    ~ "\uf00d",
           Area_2026 == "Absorbida"   ~ "\uf021",
           Area_2026 == "No responde" ~ "\uf068"))

plot_width_cm <- 8 * 2.54
x_range <- 3.1

Data <- Raw %>%
  arrange(Departamento, Nombre) %>%
  mutate(Level = -1 * row_number()) %>%
  rowwise() %>%
  mutate(ancho_cm = grid::convertWidth(grobWidth(textGrob(Nombre,gp = gpar(fontfamily = "font", fontsize = 3 * ggplot2::.pt))), "cm", valueOnly = TRUE),
         x_fin_nombre = 1 + (ancho_cm / plot_width_cm) * x_range + 0.1) %>%
  ungroup() %>%
  mutate(Area_2024 = factor(Area_2024, levels=c("Con área", "Absorbida", "Sin área", "No responde")),
         Area_2026 = factor(Area_2026, levels=c("Con área", "Absorbida", "Sin área", "No responde")))

Deptos <- Data %>%
  group_by(Departamento) %>%
  mutate(Level_depto = (min(Level) + max(Level)) / 2,
         Level_max = min(Level) -0.3,
         Level_min = max(Level) +0.3) %>%
  distinct(Departamento, .keep_all = TRUE)

# Colores
Colores <- c("Con área" = "#1e7b34",
             "Absorbida" = "#119ca0",
             "Sin área" = "#ec6230",
             "No responde" = "#cbc2ce")

# Gráfico
grafico <- ggplot(Data, aes(x=0, y=Level)) +
  geom_text(aes(x=1, label=Nombre), color="black", size=5, hjust=0, family="font_title") +
  geom_text(data=Deptos, aes(x=1-0.2, y=Level_depto, label=str_wrap(Departamento, 15)),
            color="black", size=6, hjust=1, family="font_title", lineheight=0.75) +
  geom_segment(aes(x = x_fin_nombre, xend = 2.25, yend = Level, color = Area_2024),
               linewidth = 2.5, lineend = "round") +
  geom_segment(aes(x = 2.25, xend = 3.1, yend = Level, color = Area_2026),
               linewidth = 2.5, lineend = "round") +
  geom_point(aes(x=2.25, y=Level, color=Area_2024), size=6) +
  geom_point(aes(x=3, y=Level, color=Area_2026), size=6) +
  geom_text(aes(x = 2.25, label = Icono_2024), family = "fa", size = 3, color="white") +
  geom_text(aes(x = 3,label = Icono_2026),family = "fa", size = 3, color="white") +
  annotate(geom="text", x=2.25, y=0.5, label="2024", family="font_title", fontface="bold", color="black", size=8) +
  annotate(geom="text", x=3, y=0.5, label="2026", family="font_title", fontface="bold", color="black", size=8) +
  geom_segment(data=Deptos, aes(x=1-0.1, xend=1-0.1, y=Level_min, yend=Level_max),
               color = "black", linewidth=1.5) +
  scale_x_continuous(limits=c(0.4, 3.1)) +
  scale_y_continuous(breaks=-1:-60, expand=c(0.025,0)) +
  scale_color_manual(values=Colores) +
  theme_void() +
  theme(legend.position="bottom",
        legend.justification = "center",
        legend.title = element_blank(),
        legend.text = element_text(family="font_title", size=15),
        legend.key.spacing.x = unit(1, "cm"),
        legend.box.margin = margin(t=-15),
        plot.background = element_rect(fill="white", color=NA),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(color="grey95"),
        panel.grid.major.x = element_blank(),
        axis.title.y = element_blank(),
        axis.title.x.top = element_text(family="font_title", size=20, margin=margin(t=0, r=0, b=15, l=0)),
        axis.text.y = element_blank(),
        axis.text.x.top = element_markdown(family="font_title", size=15, margin=margin(b=10), lineheight = 1),
        axis.ticks.y = element_blank(),
        plot.margin = unit(c(0, 0, 0, 0), "cm"))

# Guardar gráfico
filename <- str_sub(basename(rstudioapi::getSourceEditorContext()$path), 1,
                    str_length(unlist(basename(rstudioapi::getSourceEditorContext()$path)))-2)

ggsave(filename = paste0(filename, ".png"),
       path = paste0(dirname(rstudioapi::getActiveDocumentContext()$path),"/Graficos/PNG/"),
       plot=grafico, dpi=100, width=12, height=16)
ggsave(filename = paste0(filename, ".pdf"),
       path=paste0(dirname(rstudioapi::getActiveDocumentContext()$path),"/Graficos/PDF/"),
       plot=grafico, dpi=72, width=12, height=16)