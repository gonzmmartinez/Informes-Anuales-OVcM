# Limpiar todo
rm(list = ls())

# Funciones
`%ni%` <- Negate(`%in%`)

# Librerías
library(ggplot2)
library(dplyr)
library(stringr)
library(directlabels)
library(googlesheets4)
library(lubridate)

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
Raw <- read_sheet(ss = "https://docs.google.com/spreadsheets/d/1Cfbecjc5DLo3uGsMEHscsfUC9YOtnKtFvt1bOZI_B4c/edit?usp=sharing",
                  sheet = "Ingresadas") %>%
  filter(Tipo != "No configura VFG") %>%
  rename(Cantidad = "Frecuencia")

Data <- Raw %>%
  filter(Año %in% c(2024, 2025, 2026), Tipo %ni% c("Penal")) %>%
  group_by(Año, Trimestre) %>%
  summarise(Cantidad = sum(Cantidad), .groups = "drop") %>%
  mutate(Ord = match(Trimestre, c("1° trimestre", "2° trimestre", "3° trimestre", "4° trimestre"))) %>%
  arrange(Año, Ord) %>%
  mutate(Label = formatC(Cantidad, big.mark = ".", decimal.mark = ",", format = "fg"),
         Pos = row_number())

Data_complete <- Raw %>%
  filter(Tipo %ni% c("Penal")) %>%
  group_by(Año, Trimestre) %>%
  summarise(Cantidad = sum(Cantidad), .groups = "drop") %>%
  mutate(Ord = match(Trimestre, c("1° trimestre", "2° trimestre", "3° trimestre", "4° trimestre"))) %>%
  arrange(Año, Ord) %>%
  mutate(Label = formatC(Cantidad, big.mark = ".", decimal.mark = ",", format = "fg"),
         Pos = row_number())


# Colores
Paleta <- c("#1e7b34", "#119ca0", "#b8d6ac", "#6963aa",
            "#7c428a", "#4c2158", "#c72a29", "#ec6230", "#cbc2ce")

# Gráfico 1
grafico1 <- ggplot(Data, aes(x = Pos, y = Cantidad)) +
  geom_col(aes(fill = Cantidad), width = 0.9) +
  geom_text(aes(label = Label), size = 6, family = "font_body",
            fontface = "bold", color = "white", hjust = 0.5, nudge_y = -400) +
  scale_y_continuous(labels = function(z) formatC(z, big.mark = ".", decimal.mark = ",", format = "fg")) +
  scale_x_continuous(breaks = Data$Pos,
                     labels = paste0(str_extract(Data$Trimestre, "^[1-4]"), "° T")) +
  scale_fill_gradient(low = "#F9CDBE", high = "#ec6230") +
  labs(x = "Trimestre/Año", y = "Cantidad") +
  annotate(geom = "text", y = -1200, x = c(2.5, 6.5, 9.5), label = 2024:2026,
           size = 8, color = "black", family = "font_title", fontface = "bold") +
  annotate(geom = "segment", x = c(0.5, 4.5, 8.5, 10.5), xend = c(0.5, 4.5, 8.5, 10.5),
           y = -500, yend = -1500, color = "grey", linewidth = 0.25) +
  theme_light() +
  coord_cartesian(ylim = c(-100, 10250), xlim = c(0.4, 10.6), clip = "off", expand = FALSE) +
  scale_alpha_continuous(range = c(0.5, 1)) +
  theme(text = element_text(family = "font_body"), legend.position = "none",
        plot.title = element_blank(),
        plot.subtitle = element_blank(),
        plot.caption = element_text(size = 12, family = "font_title"),
        panel.grid = element_blank(),
        panel.grid.major = element_line(colour = "grey95"),
        axis.text.x = element_text(family = "font_title", size = 15, margin = margin(t = 10)),
        axis.text.y = element_text(family = "font_title", size = 15, margin = margin(r = 10)),
        axis.title.x = element_text(family = "font_title", size = 20, margin = margin(t = 40)),
        axis.title.y = element_text(family = "font_title", size = 20),
        plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"))


# Gráfico 2
grafico2 <- ggplot(Data_complete, aes(x = Pos, y = Cantidad)) +
  annotate(geom = "rect", xmin = 17, xmax = 26, ymin = 0, ymax = 10000,
           color = NA, fill = "grey", alpha = 0.5) +
  geom_vline(xintercept = c(5, 9, 13, 17, 21, 25), linewidth = 1, color = "grey") +
  geom_line(linewidth = 2, color = "#ec6230") +
  scale_x_continuous(breaks = 1:26, expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0), limits = c(-1500, 10000)) +
  annotate(geom = "text", y = -1500,
           x = c(3, 7, 11, 15, 19, 23, 25.5),
           label = 2020:2026,
           size = 4, color = "black", family = "font_title",
           hjust = 0.5, vjust = 0.5) +
  coord_cartesian(ylim = c(0, 10000), xlim = c(1, 26),
                  clip = "off", expand = FALSE) +
  theme_light() +
  theme(axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        panel.grid.minor = element_blank(),
        margins = margin(r = 150, l = 225, b = 30))

# Layout
grafico <- plot_grid(grafico1, grafico2, ncol=1,
                     rel_heights = c(5,1)) +
  theme(plot.background = element_rect(fill = "white", colour = NA))

# Guardar gráfico
filename <- str_sub(basename(rstudioapi::getSourceEditorContext()$path), 1,
                    str_length(unlist(basename(rstudioapi::getSourceEditorContext()$path)))-2)

ggsave(filename = paste0(filename, ".png"),
       path = paste0(dirname(rstudioapi::getActiveDocumentContext()$path),"/Graficos/PNG/"),
       plot=grafico, dpi=100, width=14, height=9)
ggsave(filename = paste0(filename, ".pdf"),
       path=paste0(dirname(rstudioapi::getActiveDocumentContext()$path),"/Graficos/PDF/"),
       plot=grafico, dpi=72, width=14, height=9)
