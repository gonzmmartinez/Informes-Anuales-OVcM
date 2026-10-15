# Limpiar todo
rm(list = ls())

# Librerías
library(tidyverse)
library(ggtext)

# Funciones
`%nin%` = Negate(`%in%`)

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
Raw <- read.csv(file=paste0(dirname(rstudioapi::getActiveDocumentContext()$path), "/Datos/Registro_Asistencias_Completo_2026.csv")) %>%
  rename(Originarias = "Perteneciente.a.comunidades.originarias")

# Modificar datos
Data <- Raw %>%
  mutate(Originarias = case_when(Originarias == "Sí" ~ "Si",
                                 Originarias == "Si" ~ "Si",
                                 Originarias == "No" ~ "No",
                                 Originarias == "NO" ~ "No",
                                 Originarias == "Sin dato" ~ "Sin dato")) %>%
  filter(Originarias != "Sin dato") %>%
  mutate(Originarias = factor(Originarias, levels=c("Si", "No"))) %>%
  group_by(Originarias) %>%
  summarise(Cantidad = n()) %>%
  mutate(Porcentaje = 100 * Cantidad / sum(Cantidad)) %>%
  ungroup %>%
  mutate(Label = ifelse(Porcentaje > 3,
                        paste0("<span style='font-size:12.5pt'>**",
                               formatC(round(Porcentaje,1), big.mark = ".", decimal.mark=","),
                               "%**</span><br><span style='font-size:10pt'>",
                               formatC(Cantidad, big.mark = ".", decimal.mark = ",","</span>")),
                        NA)) %>%
  mutate(ymax = cumsum(Porcentaje)) %>%
  mutate(ymin = c(0, head(ymax, n=-1))) %>%
  rowwise() %>%
  mutate(ymid = ymax - (ymax - ymin)/2)

Total <- Raw %>%
  filter(Originarias %in% c("Sin dato", "")) %>%
  mutate(Originarias = "Sin dato") %>%
  group_by(Originarias) %>%
  summarise(Cantidad = n()) %>%
  mutate(Texto = paste0("<span style='font-size:15pt'>",
                        "Sin dato: **",
                        formatC(Cantidad, big.mark=".", decimal.mark=",", format="fg"),
                        "**</span>")) %>%
  pull(Texto)

Leyenda <- Data %>%
  mutate(Leyenda = ifelse(Porcentaje >= 5,
                          as.character(Originarias),
                          paste0(as.character(Originarias), " (",
                                 formatC(round(Porcentaje, 1), big.mark = ".", decimal.mark = ",", format = "fg"), "%)"))) %>%
  select(Originarias, Leyenda) %>% 
  deframe()

# Colores
Paleta <- c("#1e7b34", "#119ca0", "#b8d6ac", "#6963aa",
            "#7c428a", "#4c2158", "#c72a29", "#ec6230", "#cbc2ce")

Colores <- c("Si" = "#ec6230",
             "No" = "#cbc2ce")

# Gráfico
grafico <- ggplot(Data, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=2.75, fill=Originarias)) +
  geom_rect() +
  geom_richtext(y=0, x=1.5,
                label=Total, size=9,
                color = "darkgrey",
                label.color = NA, family="font_title",
                show.legend=FALSE, fill=NA) +
  geom_richtext(aes(x = 3.5,
                    y=ymid, label=Label),
                color = "white",
                label.color = NA, family="font_body",
                show.legend=FALSE, fill=NA) +
  coord_polar(theta="y") +
  xlim(c(1.5, 4.5)) +
  theme_void() +
  scale_fill_manual(name = str_wrap("¿Es una persona perteneciente a comunidades originarias?", 30),
                    values = Colores,
                    labels = Leyenda) +
  theme(text=element_text(family="font_title"),
        legend.position = "right",
        plot.title = element_blank(),
        plot.subtitle = element_blank(),
        legend.title = element_text(family="font_title", face="bold", size=12, margin=margin(b=10)),
        legend.text = element_text(family="font_title", size=12),
        legend.box.margin = margin(t=5,b=5,l=-50,r=40),
        legend.key.spacing.y = unit(0.5, "cm"),
        plot.margin = margin(t=-50, b=-50, l=-50, r=-25),
        plot.background = element_rect(fill = "white", colour = NA))

# Guardar gráfico
filename <- str_sub(basename(rstudioapi::getSourceEditorContext()$path), 1,
                    str_length(unlist(basename(rstudioapi::getSourceEditorContext()$path)))-2)

ggsave(filename = paste0(filename, ".png"),
       path = paste0(dirname(rstudioapi::getActiveDocumentContext()$path),"/Graficos/PNG/"),
       plot=grafico, dpi=100, width=7, height=4.5)
ggsave(filename = paste0(filename, ".pdf"),
       path = paste0(dirname(rstudioapi::getActiveDocumentContext()$path),"/Graficos/PDF/"),
       plot=grafico, dpi=72, width=7, height=4.5)