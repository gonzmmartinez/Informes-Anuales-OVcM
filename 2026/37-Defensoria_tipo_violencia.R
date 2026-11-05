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
library(tibble)

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

# Crear datos
Data <- data.frame(Tipo = c("Económica/ Patrimonial", "Física", "Psicológica",
                            "Sexual", "Simbólica", "No configura VIF", "Sin identificar"),
                   Cantidad = c(26, 588, 2050, 231, 54, 466, 87)) %>%
  mutate(Porcentaje = 100 * Cantidad / sum(Cantidad),
         Tipo = factor(Tipo)) %>%
  mutate(Tipo = fct_reorder(Tipo, Porcentaje, .desc = TRUE)) %>%
  mutate(Label = ifelse(Porcentaje > 10,
                        paste0("<span style='font-size:15pt'>**",
                               round(Porcentaje,1),
                               "%**</span><br><span style='font-size:10pt'>",
                               formatC(Cantidad, big.mark = ".", decimal.mark = ",", format="fg"),
                               "</span>"),
                        "")) %>%
  arrange(desc(Porcentaje)) %>%
  mutate(ymax = cumsum(Porcentaje)) %>%
  mutate(ymin = c(0, head(ymax, n=-1))) %>%
  rowwise() %>%
  mutate(ymid = ymax - (ymax - ymin)/2) %>%
  ungroup() %>%
  mutate(Leyenda = ifelse(Porcentaje >= 10, as.character(Tipo),
                          paste0(as.character(Tipo), " (", formatC(round(Porcentaje,1), big.mark=".", decimal.mark = ",", format="fg"), "%)")))

Leyenda <- Data %>%
  mutate(Leyenda = ifelse(Porcentaje >= 10, as.character(Tipo),
      paste0(as.character(Tipo), " (",
             formatC(round(Porcentaje, 1),big.mark = ".", decimal.mark = ",", format = "fg"), "%)"))) %>%
  select(Tipo, Leyenda) %>% 
  deframe()

Paleta <- c("#1e7b34", "#119ca0", "#b8d6ac", "#6963aa",
            "#7c428a", "#4c2158", "#c72a29", "#ec6230", "#cbc2ce",
            "#d6a62e", "#3f6f9f", "#b05c7a")

Colores <- c("Física" = "#c72a29",
             "Psicológica" = "#ec6230",
             "Simbólica" = "#3f6f9f",
             "Económica/ Patrimonial" = "#7c428a",
             "Sexual" = "#1e7b34",
             "Sin identificar" = "#119ca0",
             "No configura VIF" = "#cbc2ce")
# Gr?fico1
grafico <- ggplot(Data, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=2.25, fill=Tipo)) +
  geom_rect() +
  geom_richtext(aes(x = 3, y=ymid, label=Label),
                color = "white", hjust=0.5, lineheight=1.25,
                label.color = NA, family="font_body",
                show.legend=FALSE, fill=NA) +
  coord_polar(theta="y") +
  xlim(c(1.5, 4)) +
  theme_void() +
  scale_fill_manual(name = "Tipo de violencia",
                    values = Colores,
                    labels = Leyenda) +
  theme(text=element_text(family="font_body"),
        legend.position = "right",
        plot.title = element_blank(),
        plot.subtitle = element_blank(),
        legend.title = element_text(size=10, family="font_title", face="bold"),
        legend.text = element_text(size=10, family="font_title"),
        legend.key.spacing.y = unit(0.25, "cm"),
        plot.background = element_rect(fill = "white", colour = NA))

# Guardar gráfico
filename <- str_sub(basename(rstudioapi::getSourceEditorContext()$path), 1,
                    str_length(unlist(basename(rstudioapi::getSourceEditorContext()$path)))-2)

ggsave(filename = paste0(filename, ".png"),
       path = paste0(dirname(rstudioapi::getActiveDocumentContext()$path),"/Graficos/PNG/"),
       plot=grafico, dpi=100, width=6, height=3.5)
ggsave(filename = paste0(filename, ".pdf"),
       path=paste0(dirname(rstudioapi::getActiveDocumentContext()$path),"/Graficos/PDF/"),
       plot=grafico, dpi=72, width=6, height=3.5)