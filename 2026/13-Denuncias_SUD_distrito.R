# Limpiar todo
rm(list = ls())

# Librerías
library(ggplot2)
library(dplyr)
library(stringr)
library(cowplot)
library(magick)
library(ggtext)
library(googlesheets4)

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
Raw <- read_sheet(ss = "https://docs.google.com/spreadsheets/d/1mUMxGbv3x1hoVxWbTfAquSDR25YWnSDTL8T5MFkllvU/edit?usp=sharing",
                  sheet = "SUD_db_completa") %>%
  filter(Año %in% c(2025, 2026), Tipo %in% c("Género", "Familiar", "No penal"))

Data <- Raw %>%
  group_by(Año, Distrito) %>%
  summarise(Cantidad = sum(Cantidad)) %>%
  mutate(Porcentaje = 100 * Cantidad / sum(Cantidad)) %>%
  ungroup %>%
  arrange(desc(Cantidad)) %>%
  mutate(Orden = row_number()) %>%
  mutate(Distrito = case_when(Distrito == "Centro" ~ "Centro",
                              Distrito == "Centro-Cafayate" ~ "Centro Cafayate",
                              Distrito == "Norte-Orán" ~ "Norte Orán",
                              Distrito == "Norte-Tartagal" ~ "Norte Tartagal",
                              Distrito == "Sur-Anta" ~ "Sur Anta",
                              Distrito == "Sur-Metán" ~ "Sur Metán"),
         Año = factor(case_when(Año == 2025 ~ "2025 (todo el año)",
                                Año == 2026 ~ "2026 (primer semestre)"),
                      levels=c("2026 (primer semestre)", "2025 (todo el año)")))

# Colores
Paleta <- c("#1e7b34", "#119ca0", "#b8d6ac", "#6963aa",
            "#7c428a", "#4c2158", "#c72a29", "#ec6230", "#cbc2ce")

Colores <- c("Centro" = "#6963aa",
             "Centro Cafayate" = "#7c428a",
             "Norte Orán" = "#119ca0",
             "Norte Tartagal" = "#095e80",
             "Sur Anta" = "#ec6230",
             "Sur Metán" = "#c72a29")

# Grafico
grafico <- ggplot(Data, aes(x=reorder(Distrito, Orden), y=Porcentaje, fill=Distrito)) +
  geom_col() +
  geom_richtext(aes(label = paste0("<span style='font-size:15pt'>**",formatC(Porcentaje, big.mark=".", decimal.mark=",", digits=1, format="f"),
                                   "%**</span><br><span style='font-size:8pt'>",
                                   formatC(Cantidad, big.mark = ".", decimal.mark = ",", format="fg"),
                                   "</span>")),
                color = "black", nudge_y=15, lineheight=1,
                label.color = NA, family="font_body",
                show.legend=FALSE, fill=NA) +
  facet_wrap(~Año, nrow=2, scales='free') +
  labs(x="Distrito judicial", y="Porcentaje") +
  scale_fill_manual(values = Colores) +
  scale_y_continuous(labels=function(z) paste0(abs(z), "%"), limits=c(0, 100), breaks=seq(0, 100, by=25)) +
  scale_x_discrete(labels = function(z) str_wrap(z, width=3)) +
  theme_light() +
  theme(text=element_text(family="font_body"), legend.position="none",
        plot.title = element_blank(),
        plot.subtitle = element_blank(),
        plot.caption = element_blank(),
        panel.grid = element_blank(),
        panel.grid.major = element_line(linewidth=0.5, colour = "grey95"),
        axis.text.x = element_text(size=12, family="font_title", margin = margin(t=5,r=0,b=0,l=0)),
        axis.text.y = element_text(size=10, family="font_title", margin = margin(t=0,r=5,b=0,l=5)),
        axis.title.x = element_text(size=12, family="font_title", margin = margin(t=10,r=0,b=0,l=0)),
        axis.title.y = element_text(size=12, family="font_title", margin = margin(t=0,r=5,b=0,l=0)),
        strip.background = element_rect(color=NA, fill="#cbc2ce"),
        strip.text = element_text(size=15, color="black", family="font_title", face="bold",
                                  margin=margin(t=10, b=10)))

# Imagen
# imagen <- ggdraw() +  draw_image("https://www.justiciasalta.gov.ar/media/images/distritos_judiciales_web-2024.jpg?timestamp=20240312114917")
imagen <- ggdraw() +
  draw_image(paste0(dirname(rstudioapi::getActiveDocumentContext()$path), "/Anexos/Mapa_distritos_judiciales.jpg"))

# Arrange
grafico <- plot_grid(imagen, grafico, ncol=2) +
  theme(plot.background = element_rect(fill = "white", color=NA),
        panel.border = element_blank())

# Guardar gráfico
filename <- str_sub(basename(rstudioapi::getSourceEditorContext()$path), 1,
                    str_length(unlist(basename(rstudioapi::getSourceEditorContext()$path)))-2)

ggsave(filename = paste0(filename, ".png"),
       path = paste0(dirname(rstudioapi::getActiveDocumentContext()$path),"/Graficos/PNG/"),
       plot=grafico, dpi=100, width=15, height=6)
ggsave(filename = paste0(filename, ".pdf"),
       path=paste0(dirname(rstudioapi::getActiveDocumentContext()$path),"/Graficos/PDF/"),
       plot=grafico, dpi=72, width=15, height=6)

