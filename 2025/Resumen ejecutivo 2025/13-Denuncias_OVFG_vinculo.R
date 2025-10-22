# Limpiar todo
rm(list = ls())

# Librer?as
library(ggplot2)
library(dplyr)
library(stringr)
library(cowplot)
library(magick)

# Fuentes
library(showtext)
font_add_google("Source Sans 3", "font_sans")
font_add_google("Source Serif 4", "font_serif")
showtext_auto()

# Leer datos
Raw <- read_sheet(ss = "https://docs.google.com/spreadsheets/d/1Cfbecjc5DLo3uGsMEHscsfUC9YOtnKtFvt1bOZI_B4c/edit?usp=sharing",
                  sheet = "Vinculo") %>%
  filter(Año == 2025)

Data <- Raw %>%
  group_by(Vínculo) %>%
  summarise(Cantidad = sum(Cantidad)) %>%
  ungroup %>%
  mutate(Porcentaje = 100 * Cantidad / sum(Cantidad)) %>%
  group_by(Vínculo) %>%
  summarise(Cantidad = sum(Cantidad)) %>%
  ungroup %>%
  mutate(Porcentaje = 100 * Cantidad / sum(Cantidad)) %>%
  arrange(Cantidad) %>%
  mutate(Ord = row_number()+1) %>%
  mutate(Ord = ifelse(Vínculo == "Otro", min(Ord) - 1, Ord))

# Definir colores
Paleta <- c("#206170", "#5ec5d4", "#a782ec", "#852f8c", "#0f216d", "#2b42a0",
            "#ff9d27", "#ff621d", "#f93e35", "#d3335e", "#cbc2ce")

# Gráfico
grafico <- ggplot(Data, aes(y=Porcentaje, x=reorder(Vínculo, -Ord))) +
  geom_col(data = subset(Data, Vínculo == "Otro"), aes(x=8), fill = "#cbc2ce") +
  geom_col(data = subset(Data, Vínculo != "Otro"), aes(fill=Cantidad)) +
  scale_fill_gradient(low="#fc9f9a", high="#f93e35") +
  theme_light() +
  labs(x="Vínculo con la persona que resultó denunciada", y="Porcentaje") +
  geom_text(aes(label = formatC(Cantidad, big.mark = ".", decimal.mark = ",")), color = "black",
            size=4, family="font_sans", hjust = 0.5, nudge_y = 1) +
  geom_text(aes(label = paste0(formatC(round(Porcentaje,1), big.mark = ".", decimal.mark = ","), "%")), color = "black",
            size=7, family="font_sans", fontface="bold", hjust = 0.5, nudge_y = 4) +
  scale_y_continuous(limits = c(0, round(max(Data$Porcentaje) * 1.2, -1)),
                     labels = function(z) paste0(z, "%")) +
  scale_x_discrete(labels = function(z) str_wrap(z, 30)) +
  theme(text=element_text(family="font_sans"), legend.position="none",
        plot.title = element_text(size=40, family="font_sans", face="bold"),
        plot.subtitle = element_text(size=25, family="font_sans", face="italic", margin=margin(t=5,r=0,b=20,l=0)),
        legend.background = element_blank(), legend.box.background = element_rect(color = "black"),
        legend.box.margin=margin(5,5,5,5),
        panel.grid = element_blank(),
        panel.grid.major.x = element_line(color="grey95", linewidth = 0.5),
        axis.text.x = element_text(size=15, margin = margin(t=10,r=0,b=0,l=0)),
        axis.text.y = element_text(size=15, margin = margin(t=0,r=10,b=0,l=0)),
        axis.title.x = element_text(size=20, margin = margin(t=15, r=0, b=0, l=0)),
        axis.title.y = element_text(size=20, margin = margin(t=0, r=15, b=0, l=0)))

# Guardar gráfico
filename <- str_sub(basename(rstudioapi::getSourceEditorContext()$path), 1,
                    str_length(unlist(basename(rstudioapi::getSourceEditorContext()$path)))-2)

ggsave(filename = paste0(filename, ".png"),
       path = paste0(dirname(rstudioapi::getActiveDocumentContext()$path),"/Graficos/PNG/"),
       plot=grafico, dpi=100, width=12, height=6)
ggsave(filename = paste0(filename, ".pdf"),
       path=paste0(dirname(rstudioapi::getActiveDocumentContext()$path),"/Graficos/PDF/"),
       plot=grafico, dpi=72, width=12, height=6)