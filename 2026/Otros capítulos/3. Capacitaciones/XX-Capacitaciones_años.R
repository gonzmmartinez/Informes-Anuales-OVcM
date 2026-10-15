# Limpiar todo
rm(list = ls())

# Librerías
library(ggplot2)
library(dplyr)
library(stringr)
library(cowplot)
library(magick)

# Fuentes
library(sysfonts)
library(showtext)
dir <- paste0(str_sub(getwd(), 1, -19), "/Fonts/")
font_add(family = "font_title",
         bold = file.path(dir, "CreatoDisplay-ExtraBold.otf"),
         regular = file.path(dir, "CreatoDisplay-Regular.otf"))
font_add("font_subtitle", file.path(dir, "CreatoDisplay-Regular.otf"))
font_add(family = "font_body",
         regular = file.path(dir, "RobotoSlab-Regular.ttf"),
         bold = file.path(dir, "RobotoSlab-Bold.ttf"))
showtext_auto()

# Leer datos
Data <- data.frame(Año = 2024:2026,
                   Cantidad = c(34, 40, 50)) %>%
  mutate(Porcentaje = 100 * Cantidad / sum(Cantidad)) %>%
  arrange(desc(Año)) %>%
  mutate(Ord = row_number())

Paleta <- c("#1e7b34", "#119ca0", "#b8d6ac", "#6963aa",
            "#7c428a", "#4c2158", "#c72a29", "#ec6230", "#cbc2ce",
            "#d6a62e", "#3f6f9f", "#b05c7a")

# Gráfico
grafico <- ggplot(Data, aes(x=Cantidad, y=reorder(Año, Ord))) +
  geom_col(aes(fill=Cantidad)) +
  scale_fill_gradient(low="#7CDF93", high="#1e7b34") +
  theme_light() +
  labs(y=str_wrap("Año", 25), x="Cantidad") +
  geom_text(aes(label = formatC(Cantidad, big.mark = ".", decimal.mark = ",")), color = "black",
            size=7.5, family="font_body", vjust=0.5, hjust = 0, nudge_x = 0.75) +
  scale_x_continuous(limits = c(0, round(max(Data$Cantidad) * 1.1, 0)),
                     expand = c(0.01, 0)) +
  scale_y_discrete(labels = function(z) str_wrap(z, 30)) +
  theme(text=element_text(family="font_body"), legend.position="none",
        plot.title = element_blank(),
        plot.subtitle = element_blank(),
        panel.grid = element_blank(),
        panel.grid.major.x = element_line(color="grey95", linewidth = 0.5),
        axis.text.x = element_text(family="font_title", size=12.5, margin = margin(t=10,r=0,b=0,l=0)),
        axis.text.y = element_text(family="font_title", size=22.5, margin = margin(t=0,r=10,b=0,l=0)),
        axis.title.x = element_text(family="font_title", size=20, margin = margin(t=15, r=0, b=0, l=0)),
        axis.title.y = element_text(family="font_title", size=20, margin = margin(t=0, r=15, b=0, l=0)))

# Guardar gráfico
filename <- str_sub(basename(rstudioapi::getSourceEditorContext()$path), 1,
                    str_length(unlist(basename(rstudioapi::getSourceEditorContext()$path)))-2)

ggsave(filename = paste0(filename, ".png"),
       path = paste0(dirname(rstudioapi::getActiveDocumentContext()$path),"/Graficos/PNG/"),
       plot=grafico, dpi=100, width=10, height=4)
ggsave(filename = paste0(filename, ".pdf"),
       path=paste0(dirname(rstudioapi::getActiveDocumentContext()$path),"/Graficos/PDF/"),
       plot=grafico, dpi=72, width=10, height=4)
