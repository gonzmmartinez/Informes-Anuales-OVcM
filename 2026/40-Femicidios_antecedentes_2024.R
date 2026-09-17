# Limpiar todo
rm(list = ls())

# Librer?as
library(ggplot2)
library(dplyr)
library(stringr)
library(cowplot)
library(magick)
library(ggtext)

# Fuentes
library(showtext)
font_add_google("Source Sans 3", "font_sans")
font_add_google("Source Serif 4", "font_serif")
showtext_auto()

# Crear datos
Raw <- data.frame(Categorias = factor(c("Sí existían antecedentes de violencia", "Sin dato"),
                                      levels=c("Sí existían antecedentes de violencia", "Sin dato")),
                   Cantidad = c(4, 7))

Data <- Raw %>%
  mutate(Porcentaje = 100 * Cantidad / sum(Cantidad)) %>%
  mutate(Label = ifelse(Porcentaje < 10,
                        paste0("<span style='font-size:12pt'>**",
                               formatC(round(Porcentaje,1), big.mark=".", decimal.mark=",", format="fg"),
                               "%**</span><br><span style='font-size:10pt'>",
                               formatC(Cantidad, big.mark = ".", decimal.mark = ",", format="fg"),
                               "</span>"),
                        paste0("<span style='font-size:20pt'>**",
                               formatC(round(Porcentaje,1), big.mark=".", decimal.mark=",", format="fg"),
                               "%**</span><br><span style='font-size:15pt'>",
                               formatC(Cantidad, big.mark = ".", decimal.mark = ",", format="fg"),
                               "</span>"))) %>%
  mutate(ymax = cumsum(Porcentaje)) %>%
  mutate(ymin = c(0, head(ymax, n=-1))) %>%
  rowwise() %>%
  mutate(ymid = ymax - (ymax - ymin)/2) %>%
  ungroup() %>%
  add_row(Categorias = "Existía una denuncia previa", Cantidad = 3, Porcentaje = 27.27273,
          Label = "<span style='font-size:20pt'>**27,3%**</span><br><span style='font-size:15pt'>3</span>",
          ymax = 27.27273, ymin = 0, ymid = 27.27273/2) %>%
  mutate(Outside = c(FALSE, FALSE, TRUE)) %>%
  mutate(ymid = ifelse(Categorias == "Sí existían antecedentes de violencia", 32, ymid))

# Total
Total <- paste0( "<span style='font-size:20pt'>Total</span><br>",
                  "**", formatC(sum(Data$Cantidad)-3, big.mark = ".", decimal.mark = ",", format = "fg"),
                  "**")

# Colores
Paleta <- c("#206170", "#5ec5d4", "#a782ec", "#852f8c", "#0f216d", "#2b42a0",
            "#ff9d27", "#ff621d", "#f93e35", "#d3335e", "#cbc2ce")

# Definir colores
Colores <- c("Sí existían antecedentes de violencia" = "#a782ec",
             "Existía una denuncia previa" = "#852f8c",
             "Sin dato" = "#cbc2ce")

# Gr?fico
grafico <- ggplot(Data, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=Categorias)) +
  geom_rect(data=Data %>% filter(Categorias != "Existía una denuncia previa")) +
  geom_rect(data=Data %>% filter(Categorias == "Existía una denuncia previa"),
            aes(xmin=3.5)) +
  geom_textbox(x = 1.5, y = 0, label = Total, hjust = 0.5,
               halign = 0.5, fill = NA, size=20, box.color=NA,
               family = "font_sans", lineheight = 0.25) +
  geom_richtext(aes(x = ifelse(Outside, 4.4, 3.5), y=ymid, label=Label),
                color = ifelse(Data$Outside, "black", "white"), lineheight=1.5,
                label.color = NA, family="font_sans",
                show.legend=FALSE, fill=NA) +
  coord_polar(theta="y") +
  xlim(c(1.5, 4.5)) +
  theme_void() +
  scale_fill_manual(values = Colores, labels = function(z) str_wrap(z, width=15)) +
  theme(text=element_text(family="font"),
        legend.position = "right",
        plot.title = element_text(family="font", size=25, face="bold", hjust=0.5),
        plot.subtitle = element_text(size=12, family="font"),
        legend.title = element_blank(),
        legend.text = element_text(size=15),
        legend.box.margin = margin(t=5,b=5,l=-40,r=40),
        legend.key.spacing.y = unit(0.5, "cm"),
        plot.margin = unit(c(-3,0,-3,0), "cm"),
        plot.background = element_rect(fill = "white", colour = NA))

# Layout
grafico <- grafico +
  theme(plot.background = element_rect(fill = "white", colour = NA))


# Guardar gr?fico
filename <- str_sub(basename(rstudioapi::getSourceEditorContext()$path), 1,
                    str_length(unlist(basename(rstudioapi::getSourceEditorContext()$path)))-2)

ggsave(filename = paste0(filename, ".png"),
       path = paste0(dirname(rstudioapi::getActiveDocumentContext()$path),"/Graficos/PNG/"),
       plot=grafico, dpi=100, width=9, height=6)
ggsave(filename = paste0(filename, ".pdf"),
       path=paste0(dirname(rstudioapi::getActiveDocumentContext()$path),"/Graficos/pdf/"),
       plot=grafico, dpi=72, width=9, height=6)