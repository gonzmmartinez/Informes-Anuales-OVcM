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

# Cargar datos
Raw <- read_sheet(ss = "https://docs.google.com/spreadsheets/d/1_n2tTaEXNYTv7fGRLLXt65W49wvFmE3eDpxiBZqmMZk/edit?usp=sharing",
                  sheet = "REGISTRO")

Data <- Raw %>%
  filter(Año == 2026) %>%
  select(Vinculo) %>%
  group_by(Vinculo) %>%
  summarise(Cantidad = n()) %>%
  ungroup() %>%
  mutate(Porcentaje = 100 * Cantidad / sum(Cantidad)) %>%
  mutate(Label = ifelse(Porcentaje < 10,
                        paste0("<span style='font-size:15pt'>**",
                               formatC(round(Porcentaje,1), big.mark=".", decimal.mark=",", format="fg"),
                               "%**</span><br><span style='font-size:10pt'>",
                               formatC(Cantidad, big.mark = ".", decimal.mark = ",", format="fg"),
                               "</span>"),
                        paste0("<span style='font-size:20pt'>**",
                               formatC(round(Porcentaje,1), big.mark=".", decimal.mark=",", format="fg"),
                               "%**</span><br><span style='font-size:15pt'>",
                               formatC(Cantidad, big.mark = ".", decimal.mark = ",", format="fg"),
                               "</span>"))) %>%
  ungroup()

Data <- Data %>%
  mutate(Vinculo = factor(Vinculo, levels = (Data %>% arrange(desc(Porcentaje)))$Vinculo)) %>%
  arrange(desc(Porcentaje)) %>%
  mutate(ymax = cumsum(Porcentaje)) %>%
  mutate(ymin = c(0, head(ymax, n=-1))) %>%
  rowwise() %>%
  mutate(ymid = ymax - (ymax - ymin)/2)

# Colores
Paleta <- c("#1e7b34", "#119ca0", "#b8d6ac", "#6963aa",
            "#7c428a", "#4c2158", "#c72a29", "#ec6230", "#cbc2ce")

Colores <- c("Pareja" = "#4c2158",
             "Ex pareja" = "#7c428a",
             "Familiar afín" = "#ec6230",
             "Hijo" = "#c72a29",
             "Sin dato" = "#cbc2ce",
             "Sin vínculo" = "#1e7b34",
             "Otro" = "#119ca0")

# Gr?fico
grafico <- ggplot(Data, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=2.5, fill=Vinculo)) +
  geom_rect() +
  geom_richtext(aes(x = ifelse(Porcentaje <= 5, 4.4, 3.3), y=ymid, label=Label),
                color = "white", lineheight=1.5,
                label.color = NA, family="font_body",
                show.legend=FALSE, fill=NA) +
  coord_polar(theta="y") +
  xlim(c(1.5, 4.5)) +
  theme_void() +
  scale_fill_manual(name = str_wrap("Vínculo con la persona que ejerció la agresión", width=20),
                    values = Colores, labels = function(z) str_wrap(z, width=15)) +
  theme(text=element_text(family="font_title"),
        legend.position = "right",
        plot.title = element_blank(),
        plot.subtitle = element_blank(),
        legend.title = element_text(size=15, family="font_title", face="bold", margin=margin(b=10)),
        legend.text = element_text(size=15, family="font_title"),
        legend.box.margin = margin(t=5,b=5,l=-50,r=50),
        legend.key.spacing.y = unit(0.5, "cm"),
        plot.margin = margin(t=-50, r=-50, b=-50, l=-70),
        plot.background = element_rect(fill = "white", colour = NA))

# Layout
grafico <- grafico +
  theme(plot.background = element_rect(fill = "white", colour = NA))

# Guardar gr?fico
filename <- str_sub(basename(rstudioapi::getSourceEditorContext()$path), 1,
                    str_length(unlist(basename(rstudioapi::getSourceEditorContext()$path)))-2)

ggsave(filename = paste0(filename, ".png"),
       path = paste0(dirname(rstudioapi::getActiveDocumentContext()$path),"/Graficos/PNG/"),
       plot=grafico, dpi=100, width=7, height=5)
ggsave(filename = paste0(filename, ".pdf"),
       path=paste0(dirname(rstudioapi::getActiveDocumentContext()$path),"/Graficos/PDF/"),
       plot=grafico, dpi=72, width=7, height=5)