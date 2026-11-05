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
                  sheet = "Vinculo") %>%
  filter(Año == 2026)

Data1 <- Raw %>%
  group_by(Vínculo) %>%
  summarise(Cantidad = sum(Cantidad)) %>%
  ungroup %>%
  mutate(Porcentaje = 100 * Cantidad / sum(Cantidad)) %>%
  group_by(Vínculo) %>%
  summarise(Cantidad = sum(Cantidad)) %>%
  ungroup %>%
  mutate(Porcentaje = 100 * Cantidad / sum(Cantidad)) %>%
  arrange(Cantidad) %>%
  mutate(Ord = row_number()) %>%
  mutate(Ord = ifelse(Vínculo == "Otro", max(Ord) + 1, Ord))

Paleta <- c("#1e7b34", "#119ca0", "#b8d6ac", "#6963aa",
            "#7c428a", "#4c2158", "#c72a29", "#ec6230", "#cbc2ce",
            "#d6a62e", "#3f6f9f", "#b05c7a")

# Gráfico
grafico <- ggplot(Data1, aes(x=Porcentaje, y=reorder(Vínculo, Ord))) +
  geom_col(data = subset(Data1, Vínculo == "Otro"), fill = "#cbc2ce") +
  geom_col(data = subset(Data1, Vínculo != "Otro"), aes(fill=Cantidad)) +
  scale_fill_gradient(low="#BEF6F8", high="#119ca0") +
  theme_light() +
  labs(y=str_wrap("Vínculo con la persona que resultó denunciada", 25), x="Porcentaje") +
  geom_text(aes(label = formatC(Cantidad, big.mark = ".", decimal.mark = ",")), color = "black",
            size=4, family="font_body", vjust=0, hjust = 0, nudge_x = 0.75, nudge_y = -0.35) +
  geom_text(aes(label = paste0(formatC(round(Porcentaje,1), big.mark = ".", decimal.mark = ","), "%")), color = "black",
            size=7, family="font_body", fontface="bold", vjust=1, hjust = 0, nudge_x = 0.75, nudge_y = 0.35) +
  scale_x_continuous(limits = c(0, round(max(Data1$Porcentaje) * 1.15, 0)),
                     expand = c(0.025, 0),
                     labels = function(z) paste0(z, "%")) +
  scale_y_discrete(labels = function(z) str_wrap(z, 30)) +
  theme(text=element_text(family="font_body"), legend.position="none",
        plot.title = element_blank(),
        plot.subtitle = element_blank(),
        panel.grid = element_blank(),
        panel.grid.major.x = element_line(color="grey95", linewidth = 0.5),
        axis.text.x = element_text(family="font_title", size=15, margin = margin(t=10,r=0,b=0,l=0)),
        axis.text.y = element_text(family="font_title", size=20, margin = margin(t=0,r=10,b=0,l=0)),
        axis.title.x = element_text(family="font_title", size=20, margin = margin(t=15, r=0, b=0, l=0)),
        axis.title.y = element_text(family="font_title", size=20, margin = margin(t=0, r=15, b=0, l=0)))

# Guardar gráfico
filename <- str_sub(basename(rstudioapi::getSourceEditorContext()$path), 1,
                    str_length(unlist(basename(rstudioapi::getSourceEditorContext()$path)))-2)

ggsave(filename = paste0(filename, ".png"),
       path = paste0(dirname(rstudioapi::getActiveDocumentContext()$path),"/Graficos/PNG/"),
       plot=grafico, dpi=100, width=12, height=6)
ggsave(filename = paste0(filename, ".pdf"),
       path=paste0(dirname(rstudioapi::getActiveDocumentContext()$path),"/Graficos/PDF/"),
       plot=grafico, dpi=72, width=12, height=6)
