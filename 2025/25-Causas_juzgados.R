# Limpiar todo
rm(list = ls())

# Librer?as
library(ggplot2)
library(dplyr)
library(stringr)
library(cowplot)
library(magick)
library(ggtext)
library(googlesheets4)

# Fuentes
library(showtext)
font_add_google("Barlow", "font")
showtext_auto()

# Leer datos
Raw <- read_sheet(ss = "https://docs.google.com/spreadsheets/d/1AO8SmJ45quqCCvg9nZ0vZqRLvxZMfAqB7uzOtXmu9Ro/edit?usp=sharing",
                  sheet = "Causas")

Data1 <- Raw %>%
  filter(Año == 2025) %>%
  group_by(Motivo) %>%
  summarise(Cantidad = sum(Cantidad)) %>%
  mutate(Porcentaje = 100 * Cantidad / sum(Cantidad)) %>%
  mutate(Motivo = factor(Motivo,
                         levels = c("Violencia familiar", "Violencia de género", "No violencia y otros"))) %>%
  arrange(Motivo) %>%
  mutate(Label = paste0("<span style='font-size:10pt'>**",
                        formatC(round(Porcentaje,1), big.mark=".", decimal.mark=","),
                        "%**</span><br><span style='font-size:6pt'>",
                        formatC(Cantidad, big.mark = ".", decimal.mark = ",", format="fg"),
                        "</span>")) %>%
  mutate(ymax = cumsum(Porcentaje)) %>%
  mutate(ymin = c(0, head(ymax, n=-1))) %>%
  rowwise() %>%
  mutate(ymid = ymax - (ymax - ymin)/2) %>%
  ungroup()

Data2 <- Raw %>%
  filter(Año == 2024) %>%
  group_by(Motivo) %>%
  summarise(Cantidad = sum(Cantidad)) %>%
  mutate(Porcentaje = 100 * Cantidad / sum(Cantidad)) %>%
  mutate(Motivo = factor(Motivo,
                         levels = c("Violencia familiar", "Violencia de género", "No violencia y otros"))) %>%
  arrange(Motivo) %>%
  mutate(Label = paste0("<span style='font-size:8pt'>**",
                        formatC(round(Porcentaje,1), big.mark=".", decimal.mark=","),
                        "%**</span><br><span style='font-size:4pt'>",
                        formatC(Cantidad, big.mark = ".", decimal.mark = ",", format="fg"),
                        "</span>")) %>%
  mutate(ymax = cumsum(Porcentaje)) %>%
  mutate(ymin = c(0, head(ymax, n=-1))) %>%
  rowwise() %>%
  mutate(ymid = ymax - (ymax - ymin)/2) %>%
  ungroup()

# Definir colores
Colores <- c("Violencia familiar" = "#f2904c",
             "Violencia de género" = "#1daa6a",
             "No violencia y otros" = "#ec6489")

# Total
Total1 <- paste0(paste0("<span style='font-size:20pt'>",
                        "Total",
                        "</span><br><span style='font-size:30pt'>**",
                        formatC(sum(Data1$Cantidad), big.mark = ".", decimal.mark = ",", format="fg"),
                        "**</span>"))

Total2 <- paste0(paste0("<span style='font-size:15pt'>",
                        "Total",
                        "</span><br><span style='font-size:20pt'>**",
                        formatC(sum(Data2$Cantidad), big.mark = ".", decimal.mark = ",", format="fg"),
                        "**</span>"))

# Gráfico1
grafico1 <- ggplot(Data1, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=Motivo)) +
  geom_rect() +
  geom_richtext(y=0, x=1.5,
                label=Total1, size=6,
                color = "black",
                label.color = NA, family="font",
                show.legend=FALSE, fill=NA) +
  geom_richtext(aes(x = 3.5, y=ymid, label=Label), size=3,
                color = "black",
                label.color = NA, family="font",
                show.legend=FALSE, fill=NA) +
  coord_polar(theta="y") +
  xlim(c(1.5, 4)) +
  theme_void() +
  scale_fill_manual(name = str_wrap("Motivo de la causa judicial", width=30),
                    values = Colores, labels=function(z) str_wrap(z, width=20)) +
  labs(title="2.025",
       subtitle = str_wrap("primer semestre", 20)) +
  theme(text=element_text(family="font"),
        legend.position = "right",
        plot.title = element_text(family="font", size=25, face="bold", hjust=0.5),
        plot.subtitle = element_text(family="font", size=15, face="italic", hjust=0.5),
        legend.title = element_text(size=10, family="font"),
        legend.text = element_text(size=15),
        legend.box.margin=margin(5,5,5,5),
        legend.key.spacing.y = unit(0.5, "cm"),
        plot.background = element_rect(fill = "white", colour = NA))

# Gráfico2
grafico2 <- ggplot(Data2, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=Motivo)) +
  geom_rect() +
  geom_richtext(y=0, x=1.5,
                label=Total2, size=5,
                color = "black",
                label.color = NA, family="font",
                show.legend=FALSE, fill=NA) +
  geom_richtext(aes(x = 3.5, y=ymid, label=Label), size=2,
                color = "black",
                label.color = NA, family="font",
                show.legend=FALSE, fill=NA) +
  coord_polar(theta="y") +
  xlim(c(1.5, 4)) +
  theme_void() +
  scale_fill_manual(values = Colores) +
  labs(title="2.024",
       subtitle = str_wrap("enero-diciembre", 20)) +
  theme(text=element_text(family="font"),
        legend.position = "none",
        plot.title = element_text(family="font", size=25, face="bold", hjust=0.5),
        plot.subtitle = element_text(family="font", size=12, face="italic", hjust=0.5),
        legend.title = element_blank(),
        legend.text = element_text(size=15),
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
       plot=grafico, dpi=100, width=9, height=4.5)
ggsave(filename = paste0(filename, ".pdf"), path=paste0(dirname(rstudioapi::getActiveDocumentContext()$path),"/Graficos/PDF/"),
       plot=grafico, dpi=72, width=9, height=4.5)