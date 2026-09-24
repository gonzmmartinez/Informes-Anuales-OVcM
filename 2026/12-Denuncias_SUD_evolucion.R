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

# Diccionarios
Mes_ord <- data.frame(Mes = c("Enero", "Febrero", "Marzo", "Abril", "Mayo", "Junio", "Julio",
                     "Agosto", "Septiembre", "Octubre", "Noviembre", "Diciembre"),
             Ord = 1:12)

# Leer datos
Raw <- read_sheet(ss = "https://docs.google.com/spreadsheets/d/1mUMxGbv3x1hoVxWbTfAquSDR25YWnSDTL8T5MFkllvU/edit?usp=sharing",
                  sheet = "SUD_db_completa")

Data <- Raw %>%
  filter(Año %in% c(2025, 2026), Tipo %ni% c("Penal")) %>%
  group_by(Año, Mes) %>%
  summarise(Cantidad = sum(Cantidad), .groups = "drop") %>%
  left_join(Mes_ord, by = "Mes") %>%
  arrange(Año, Ord) %>%
  mutate(Label = formatC(Cantidad, big.mark = ".", decimal.mark = ",", format = "fg"),
         Pos = row_number())

Data_complete <- Raw %>%
  filter(Tipo %ni% c("Penal")) %>%
  group_by(Año, Mes) %>%
  summarise(Cantidad = sum(Cantidad), .groups = "drop") %>%
  left_join(Mes_ord, by = "Mes") %>%
  arrange(Año, Ord) %>%
  mutate(Label = formatC(Cantidad, big.mark = ".", decimal.mark = ",", format = "fg"),
         Pos = row_number())

# Colores
Paleta <- c("#1e7b34", "#119ca0", "#b8d6ac", "#6963aa",
            "#7c428a", "#4c2158", "#c72a29", "#ec6230", "#cbc2ce")

# Gráfico
grafico1 <- ggplot(Data, aes(x = Pos, y=Cantidad)) +
  geom_col(aes(fill=Cantidad), width=0.9) +
  geom_text(aes(label=Label), size=5, family="font_body",
            fontface="bold", color="white", hjust=0.5, nudge_y=-200) +
  scale_y_continuous(labels = function(z) formatC(z, big.mark = ".", decimal.mark=",", format="fg")) +
  scale_x_continuous(breaks = Data$Pos,
                     labels = str_to_title(month(c(1:12, 1:6), label = TRUE, abbr = TRUE, locale = "Spanish_Argentina.utf8"))) +
  scale_fill_gradient(low="#b8d6ac", high="#1e7b34") +
  labs(x="Mes/Año", y="Cantidad") +
  annotate(geom="text", y=-700, x=c(6.5, 15.5), label=c(2025,2026),
           size=8, color="black", family="font_title", fontface="bold") +
  annotate(geom="segment", x=c(0.5,12.5,18.5), xend=c(0.5,12.5,18.5), y=-500, yend=-900, color="grey", linewidth=0.25) +
  theme_light() +
  coord_cartesian(ylim = c(-100, 5100), xlim=c(0.25, 18.75), clip="off", expand=FALSE) +
  scale_alpha_continuous(range=c(0.5, 1)) +
  theme(text=element_text(family="font_body"), legend.position="none",
        plot.title = element_blank(),
        plot.subtitle = element_blank(),
        plot.caption = element_text(size=12, family="font_title"),
        panel.grid.major = element_line(colour = "grey95"),
        axis.text.x = element_text(family="font_title", size=15, margin = margin(t=5,r=0,b=5,l=0)),
        axis.text.y = element_text(family="font_title", size=15, margin = margin(t=0,r=10,b=0,l=5)),
        axis.title.x = element_text(family="font_title", size=20, margin=margin(t=40)),
        axis.title.y = element_text(family="font_title", size=20),
        plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm"))

grafico2 <-ggplot(Data_complete, aes(x = Pos, y=Cantidad)) +
  annotate(geom="rect", xmin=13, xmax=30, ymin=0, ymax=5000,
           color=NA, fill="grey", alpha=0.5) +
  geom_vline(xintercept = c(13, 25), linewidth = 1, color="grey") +
  geom_line(linewidth = 2, color="#1e7b34") +
  scale_x_continuous(breaks=1:30, expand=c(0, 0)) +
  scale_y_continuous(expand=c(0,0), limits = c(-1000, 5000)) +
  annotate(geom="text", y=-750, x=c(7, 19, 27.5), label=2024:2026,
           size=5, color="black", family="font_title", hjust=0.5, vjust=0.5) +
  coord_cartesian(ylim = c(0, 5000), xlim=c(1, 30),
                  clip="off", expand=FALSE) +
  theme_light() +
  theme(axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        panel.grid.minor = element_blank(),
        margins = margin(r=150, l=225, b=30))
  

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

