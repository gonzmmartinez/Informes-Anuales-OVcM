# Limpiar todo
rm(list = ls())

# Librerías
library(ggplot2)
library(dplyr)
library(stringr)
library(cowplot)
library(magick)

# Fuentes
library(showtext)
font_add_google("Barlow", "font")
showtext_auto()

# Crear datos
Raw <- read.csv(file=paste0(dirname(rstudioapi::getActiveDocumentContext()$path),"/Datos/Denuncias_OVFG_vinculos.csv"))

Data1 <- Raw %>%
  mutate(Vínculo = ifelse(Vínculo %in% c("Padre", "Madre", "Hermano/a", "Hijo/a"), "Familiar nuclear", Vínculo)) %>%
  mutate(Vínculo = ifelse(Vínculo %in% c("Familiar ", "Familiar"), "Familiar extendido", Vínculo)) %>%
  mutate(Vínculo = ifelse(Vínculo == "Pareja/Ex pareja", "Pareja", Vínculo)) %>%
  group_by(Vínculo) %>%
  summarise(Cantidad = sum(Cantidad)) %>%
  ungroup %>%
  mutate(Porcentaje = 100 * Cantidad / sum(Cantidad)) %>%
  mutate(Vínculo = ifelse(Porcentaje < 1.5, "Otro", Vínculo)) %>%
  group_by(Vínculo) %>%
  summarise(Cantidad = sum(Cantidad)) %>%
  ungroup %>%
  mutate(Porcentaje = 100 * Cantidad / sum(Cantidad)) %>%
  arrange(Cantidad) %>%
  mutate(Ord = row_number()) %>%
  mutate(Ord = ifelse(Vínculo == "Otro", 0, Ord))

# Gráfico
grafico <- ggplot(Data1, aes(x=Porcentaje, y=reorder(Vínculo, Ord))) +
  geom_col(aes(fill=Cantidad)) +
  scale_fill_gradient(low="#1daa6a", high="#e54c7c") +
  theme_classic() +
  labs(y="Vínculo con la persona que ejerció la agresión", x="Cantidad") +
  geom_text(aes(label = formatC(Cantidad, big.mark = ".", decimal.mark = ",")), color = "white",
            size=4, family="font", hjust = 1.3) +
  geom_text(aes(label = paste0(formatC(round(Porcentaje,1), big.mark = ".", decimal.mark = ","), "%")), color = "black",
            size=7, family="font", fontface="bold", hjust = -0.2) +
  scale_x_continuous(limits = c(0, round(max(Data1$Porcentaje) * 1.2, -1))) +
  scale_y_discrete(labels = function(z) str_wrap(z, 30)) +
  theme(text=element_text(family="font"), legend.position="none",
        plot.title = element_text(size=40, family="font", face="bold"),
        plot.subtitle = element_text(size=25, family="font", face="italic", margin=margin(t=5,r=0,b=20,l=0)),
        legend.background = element_blank(), legend.box.background = element_rect(color = "black"),
        legend.box.margin=margin(5,5,5,5),
        axis.text.x = element_text(size=15, margin = margin(t=10,r=0,b=0,l=0)),
        axis.text.y = element_text(size=20, margin = margin(t=0,r=10,b=0,l=0)),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size=20, margin = margin(t=0, r=15, b=0, l=0)))

# Guardar gráfico
filename <- str_sub(basename(rstudioapi::getSourceEditorContext()$path), 1,
                    str_length(unlist(basename(rstudioapi::getSourceEditorContext()$path)))-2)

ggsave(filename = paste0(filename, ".png"),
       path = paste0(dirname(rstudioapi::getActiveDocumentContext()$path),"/Graficos/PNG/"),
       plot=grafico, dpi=100, width=8, height=8)
ggsave(filename = paste0(filename, ".pdf"), path=paste0(dirname(rstudioapi::getActiveDocumentContext()$path),"/Graficos/PDF/"),
       plot=grafico, dpi=72, width=8, height=8)