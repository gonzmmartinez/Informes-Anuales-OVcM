# Limpiar todo
rm(list = ls())

# Funciones
`%ni%` <- Negate(`%in%`)

# Librer?as
library(ggplot2)
library(dplyr)
library(stringr)
library(cowplot)
library(magick)
library(ggtext)

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

# Años
Año_1 <- 2026
Año_2 <- 2025

# Leer datos
Raw <- read_sheet(ss = "https://docs.google.com/spreadsheets/d/1Cfbecjc5DLo3uGsMEHscsfUC9YOtnKtFvt1bOZI_B4c/edit?usp=sharing",
                  sheet = "Modalidad")

Levels <- (Raw %>%
  filter(Año == Año_1, Modalidad != "Sin especificar") %>%
  group_by(Año, Modalidad) %>%
  summarise(Cantidad = sum(Frecuencia)) %>%
  mutate(Porcentaje = 100 * Cantidad / sum(Cantidad)) %>%
  arrange(desc(Porcentaje)) %>%
  ungroup %>%
  filter(Modalidad != "Otras") %>%
  add_row(Modalidad = "Otras"))$Modalidad

Data1 <- Raw %>%
  filter(Año == Año_1, Modalidad != "Sin especificar") %>%
  mutate(Modalidad = factor(Modalidad, levels=Levels)) %>%
  group_by(Año, Modalidad) %>%
  summarise(Cantidad = sum(Frecuencia)) %>%
  mutate(Porcentaje = 100 * Cantidad / sum(Cantidad)) %>%
  mutate(Label = ifelse(Porcentaje >= 10,
                        paste0("<span style='font-size:20pt'>**",
                               formatC(round(Porcentaje,1), big.mark=".", decimal.mark = ",", format="fg"),
                               "%**</span><br><span style='font-size:10pt'>",
                               formatC(Cantidad, big.mark = ".", decimal.mark = ",", format="fg"),
                               "</span>"),
                        NA)) %>%
  mutate(ymax = cumsum(Porcentaje)) %>%
  mutate(ymin = c(0, head(ymax, n=-1))) %>%
  rowwise() %>%
  mutate(ymid = ymax - (ymax - ymin)/2) %>%
  ungroup() %>%
  mutate(Leyenda = ifelse(Porcentaje >= 10, Levels[1],
                          paste0(Modalidad, " (", formatC(Porcentaje, format="g", digits=1, big.mark=".", decimal.mark = ","), "%)")))

Data2 <- Raw %>%
  filter(Año == Año_2, Modalidad != "Sin especificar") %>%
  mutate(Modalidad = factor(Modalidad, levels=Levels)) %>%
  group_by(Año, Modalidad) %>%
  summarise(Cantidad = sum(Frecuencia)) %>%
  mutate(Porcentaje = 100 * Cantidad / sum(Cantidad)) %>%
  mutate(Label = ifelse(Porcentaje >= 10,
                        paste0("<span style='font-size:15pt'>**",
                               formatC(round(Porcentaje,1), big.mark=".", decimal.mark=",", format="fg"),
                               "%**</span><br><span style='font-size:10pt'>",
                               formatC(Cantidad, big.mark = ".", decimal.mark = ",", format="fg"),
                               "</span>"),
                        NA)) %>%
  mutate(ymax = cumsum(Porcentaje)) %>%
  mutate(ymin = c(0, head(ymax, n=-1))) %>%
  rowwise() %>%
  mutate(ymid = ymax - (ymax - ymin)/2) %>%
  ungroup()

Paleta <- c("#1e7b34", "#119ca0", "#b8d6ac", "#6963aa",
            "#7c428a", "#4c2158", "#c72a29", "#ec6230", "#cbc2ce",
            "#d6a62e", "#3f6f9f", "#b05c7a")

Colores <- c("Doméstica" = "#6963aa",
             "Acoso callejero" = "#3f6f9f",
             "Institucional" = "#ec6230",
             "Laboral" = "#119ca0",
             "Mediática" = "#d6a62e",
             "Obstétrica" = "#1e7b34",
             "Política" = "#c72a29",
             "Digital" = "#b05c7a",
             "Otras" = "#cbc2ce")

# Gráfico1
grafico1 <- ggplot(Data1, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=2.25, fill=Modalidad)) +
  geom_rect() +
  geom_richtext(aes(x = 3, y=ymid, label=Label),
                color = "white", hjust=0.5, lineheight=1.25,
                label.color = NA, family="font_body",
                show.legend=FALSE, fill=NA) +
  coord_polar(theta="y") +
  xlim(c(1.5, 4)) +
  theme_void() +
  scale_fill_manual(name = str_wrap("Modalidad de violencia", width = 30),
                    values = Colores,
                    labels = Data1$Leyenda) +
  labs(title=as.character(Año_1),
       subtitle = "primer semestre") +
  theme(text=element_text(family="font_body"),
        legend.position = "right",
        plot.title = element_text(family="font_title", size=25, face="bold", hjust=0.5),
        plot.subtitle = element_text(family="font_title", size=10, hjust=0.5),
        legend.title = element_text(size=10, family="font_title", face="bold"),
        legend.text = element_text(size=7.5, family="font_title"),
        legend.key.spacing.y = unit(0.2, "cm"),
        plot.background = element_rect(fill = "white", colour = NA))

# Gráfico2
grafico2 <- ggplot(Data2, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=2.25, fill=Modalidad)) +
  geom_rect() +
  geom_richtext(aes(x = 3, y=ymid, label=Label),
                color = "white", hjust=0.5, lineheight=1.25,
                label.color = NA, family="font_body",
                show.legend=FALSE, fill=NA) +
  coord_polar(theta="y") +
  xlim(c(1.5, 4.5)) +
  theme_void() +
  scale_fill_manual(values = Colores) +
  labs(title=as.character(Año_2),
       subtitle="Todo el año") +
  theme(text=element_text(family="font_body"),
        legend.position = "none",
        plot.title = element_text(family="font_title", size=25, face="bold", hjust=0.5),
        plot.subtitle = element_text(family="font_title", size=10, hjust=0.5),
        legend.box.margin=margin(5,5,5,5))

# Layout
grafico <- plot_grid(grafico2, grafico1, ncol=2,
                     rel_widths = c(1,2)) +
  theme(plot.background = element_rect(fill = "white", colour = NA))

# Guardar gráfico
filename <- str_sub(basename(rstudioapi::getSourceEditorContext()$path), 1,
                    str_length(unlist(basename(rstudioapi::getSourceEditorContext()$path)))-2)

ggsave(filename = paste0(filename, ".png"),
       path = paste0(dirname(rstudioapi::getActiveDocumentContext()$path),"/Graficos/PNG/"),
       plot=grafico, dpi=100, width=7, height=3.5)
ggsave(filename = paste0(filename, ".pdf"),
       path=paste0(dirname(rstudioapi::getActiveDocumentContext()$path),"/Graficos/PDF/"),
       plot=grafico, dpi=72, width=7, height=3.5)

