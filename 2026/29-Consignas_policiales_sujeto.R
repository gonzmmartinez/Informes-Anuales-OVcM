# Limpiar todo
rm(list = ls())

# Librer?as
library(ggplot2)
library(dplyr)
library(stringr)
library(cowplot)
library(magick)
library(ggfittext)
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
Raw <- read_sheet(ss = "https://docs.google.com/spreadsheets/d/1rfuD4W7yQsjPiIXeAwo0Hh8nmHlg0NTDpIozghgGsGw/edit?usp=sharing",
                  sheet = "Consignas")

Data <- Raw %>%
  mutate(Año = as.character(Año),
         Tipo = factor(Tipo, levels=c("Fija","Ambulatoria","Personalizada"))) %>%
  mutate(Año = ifelse(Año == "2026", "2026*", Año))

# Colores
Paleta <- c("#1e7b34", "#119ca0", "#b8d6ac", "#6963aa",
            "#7c428a", "#4c2158", "#c72a29", "#ec6230", "#cbc2ce")

# Definir colores
Colores <- c("Fija" = "#119ca0",
             "Ambulatoria" = "#1e7b34",
             "Personalizada" = "#6963aa")

# Titulo
titulo <- ggplot() +
  labs() +
  theme_void() +
  theme(plot.title=element_text(family="font_title", size=20, face="bold"),
        plot.subtitle=element_text(family="font_title", size=15),
        plot.margin = margin(t=15, r=0, b=0, l=10))

# Grafico 1
grafico1 <- ggplot(Data %>% filter(Sujeto == "Agresor"), aes(x=Año, y=Cantidad, fill=Tipo)) +
  geom_col() +
  geom_text(aes(label = formatC(Cantidad, big.mark = ".", decimal.mark = ",", format="fg")),
            vjust=0, size=3, nudge_y=max(Data %>% filter(Sujeto == "Agresor") %>% select(Cantidad))*0.05,
            color="black", family="font_body") +
  facet_wrap(~Tipo, ncol=1, scales="free_x") +
  labs(title="Personas denunciadas", y ="Cantidad de consignas policiales",
       caption = "") +
  scale_fill_manual(values=Colores) +
  scale_y_continuous(limits=c(0,700)) +
  theme_light() +
  theme(legend.position = "none",
        plot.title = element_text(size= 20, family= "font_title", face="bold", hjust = 0.5, margin = margin(t=0,r=0,b=10,l=0)),
        plot.caption = element_text(size=8, family="font_title", face="italic", margin=margin(t=10)),
        axis.title.x = element_text(size=15, family="font_title", margin=margin(t=5)),
        axis.title.y = element_text(size=15, family="font_title", margin=margin(r=10)),
        axis.text.x = element_text(size=10, family="font_title", margin=margin(t=5)),
        axis.text.y = element_text(size=10, family="font_title"),
        panel.grid = element_blank(),
        panel.grid.major = element_line(colour = "grey95", linewidth = 0.5),
        strip.background = element_rect(color=NA, fill="#cbc2ce"),
        strip.text = element_text(size=15, color="black", family="font_title", face="bold",
                                  margin=margin(t=10, b=10)))

# Grafico 2
grafico2 <- ggplot(Data %>% filter(Sujeto == "Víctima"), aes(x=Año, y=Cantidad, fill=Tipo)) +
  geom_col() +
  geom_text(aes(label = formatC(Cantidad, big.mark = ".", decimal.mark = ",", format="fg")),
            vjust=0, size=3, color="black", family="font_body",
            nudge_y=max(Data %>% filter(Sujeto == "Víctima") %>% select(Cantidad))*0.05) +
  facet_wrap(~Tipo, ncol=1, scales="free_x")  +
  labs(title="Víctimas",
       caption="* las proporciones se calculan en base a los datos correspondientes al primer semestre únicamente.") +
  scale_fill_manual(values=Colores) +
  scale_y_continuous(labels = function(z) formatC(z, big.mark = ".", decimal.mark = ",", format="fg"),
                     limits=c(0, 18000)) +
  theme_light() +
  theme(legend.position = "none",
        plot.title = element_text(size= 20, family= "font_title", face="bold", hjust = 0.5, margin = margin(t=0,r=0,b=10,l=0)),
        plot.caption = element_text(size=8, family="font_title", face="italic", margin=margin(t=10)),
        axis.title.y = element_blank(),
        axis.title.x = element_text(size=15, family="font_title", margin=margin(t=5)),
        axis.text.x = element_text(size=10, family="font_title", margin=margin(t=5)),
        axis.text.y = element_text(size=10, family="font_title"),
        panel.grid = element_blank(),
        panel.grid.major = element_line(colour = "grey95", linewidth = 0.5),
        strip.background = element_rect(color=NA, fill="#cbc2ce"),
        strip.text = element_text(size=15, color="black", family="font_title", face="bold",
                                  margin=margin(t=10, b=10)))

grafico <- plot_grid(grafico1, grafico2, ncol=2)

# Guardar gráfico
filename <- str_sub(basename(rstudioapi::getSourceEditorContext()$path), 1,
                    str_length(unlist(basename(rstudioapi::getSourceEditorContext()$path)))-2)

ggsave(filename = paste0(filename, ".png"),
       path = paste0(dirname(rstudioapi::getActiveDocumentContext()$path),"/Graficos/PNG/"),
       plot=grafico, dpi=100, width=10, height=8)
ggsave(filename = paste0(filename, ".pdf"),
       path=paste0(dirname(rstudioapi::getActiveDocumentContext()$path),"/Graficos/PDF/"),
       plot=grafico, dpi=72, width=10, height=8)