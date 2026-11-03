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
dir <- paste0(dirname(dirname(dirname(rstudioapi::getActiveDocumentContext()$path))), "/Fonts/")
font_add(family = "font_title",
         bold = file.path(dir, "CreatoDisplay-ExtraBold.otf"),
         regular = file.path(dir, "CreatoDisplay-Regular.otf"))
font_add("font_subtitle", file.path(dir, "CreatoDisplay-Regular.otf"))
font_add(family = "font_body",
         regular = file.path(dir, "RobotoSlab-Regular.ttf"),
         bold = file.path(dir, "RobotoSlab-Bold.ttf"))
showtext_auto()

# Cargar datos
Raw_2024 <- read.csv(paste0(dirname(rstudioapi::getActiveDocumentContext()$path), "/Datos/Respuestas_municipios_2024.csv"))
Raw_2026 <- read.csv(paste0(dirname(rstudioapi::getActiveDocumentContext()$path), "/Datos/Respuestas_municipios_2026.csv"))


Data <- Raw_2026 %>%
  filter(X.Este.municipio.cuenta.con.Área.de.Mujeres..Géneros.y.Diversidades. == "Sí") %>%
  select(X.El.área.cuenta.con.un.presupuesto.asignado.anualmente.) %>%
  rename(Variable = "X.El.área.cuenta.con.un.presupuesto.asignado.anualmente.") %>%
  group_by(Variable) %>%
  summarise(Cantidad = n()) %>%
  mutate(Porcentaje = 100 * Cantidad / sum(Cantidad)) %>%
  mutate(Variable = factor(Variable,
                         levels = c("Sí", "No", "No responde"))) %>%
  mutate(Label = paste0("<span style='font-size:30pt'>**",
                        formatC(round(Porcentaje,1), big.mark=".", decimal.mark=","),
                        "%**</span><br><span style='font-size:20pt'>(",
                        formatC(Cantidad, big.mark = ".", decimal.mark = ","),
                        ")</span>")) %>%
  mutate(Label = ifelse(Porcentaje >= 3, Label, "")) %>%
  mutate(ymax = cumsum(Porcentaje)) %>%
  mutate(ymin = c(0, head(ymax, n=-1))) %>%
  rowwise() %>%
  mutate(ymid = ymax - (ymax - ymin)/2) %>%
  ungroup()

# Total
Total <- paste0(paste0("<span style='font-size:25pt'>",
                       "Municipios</span><br><span style='font-size:25pt'>",
                       "con AMGyD",
                       "</span><br><span style='font-size:60pt'>**",
                       formatC(sum(Data$Cantidad), big.mark = ".", decimal.mark = ","),
                       "**</span>"))

# Colores
Paleta <- c("#1e7b34", "#119ca0", "#b8d6ac", "#6963aa",
            "#7c428a", "#4c2158", "#c72a29", "#ec6230", "#cbc2ce",
            "#d6a62e", "#3f6f9f", "#b05c7a")

# Definir colores
Colores <- c("Sí" = "#119ca0",
             "No" = "#cbc2ce",
             "No responde" = "#cbc2ce")

# Gráfico1
grafico <- ggplot(Data, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=2.75, fill=Variable)) +
  geom_rect() +
  geom_richtext(y=0, x=1.5,
                label=Total, size=10,
                color = "black", lineheight=1,
                label.color = NA, family="font_title",
                show.legend=FALSE, fill=NA) +
  geom_richtext(aes(x = 3.375, y=ymid, label=Label), size = 7,
                color = "white", label.color = NA,
                family="font_body", show.legend=FALSE, fill=NA) +
  coord_polar(theta="y") +
  xlim(c(1.5, 5)) +
  theme_void() +
  scale_fill_manual(values = Colores) +
  theme(text=element_text(family="font_title"),
        legend.position = "right",
        plot.title = element_text(family="font_title", size=25, face="bold", hjust=0.5),
        plot.subtitle = element_text(size=12, family="font_title"),
        legend.title = element_blank(),
        legend.text = element_text(size=35, family="font_title"),
        plot.margin = margin(t=-150, b=-150, r=0, l=-150),
        legend.box.margin = margin(t=0, r=0, b=0, l=-150),
        legend.key.spacing.y = unit(0.5, "cm"),
        plot.background = element_rect(fill = "white", colour = NA))

# Guardar gráfico
filename <- str_sub(basename(rstudioapi::getSourceEditorContext()$path), 1,
                    str_length(unlist(basename(rstudioapi::getSourceEditorContext()$path)))-2)

ggsave(filename = paste0(filename, ".png"),
       path = paste0(dirname(rstudioapi::getActiveDocumentContext()$path),"/Graficos/PNG/"),
       plot=grafico, dpi=100, width=7, height=6.5)
ggsave(filename = paste0(filename, ".pdf"),
       path=paste0(dirname(rstudioapi::getActiveDocumentContext()$path),"/Graficos/PDF/"),
       plot=grafico, dpi=72, width=7, height=6.5)
