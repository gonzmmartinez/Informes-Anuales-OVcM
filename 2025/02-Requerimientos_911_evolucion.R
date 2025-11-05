# Limpiar todo
rm(list = ls())

# Funciones
`%ni%` <- Negate(`%in%`)

# Librer?as
library(ggplot2)
library(dplyr)
library(stringr)
library(scales)
library(googlesheets4)

# Fuentes
library(showtext)
font_add_google("Source Sans 3", "font_sans")
font_add_google("Source Serif 4", "font_serif")
showtext_auto()

# Leer datos
Raw <- read_sheet(ss = "https://docs.google.com/spreadsheets/d/1fX8iWndJKs_UTTcB1SoU5tpTK7ysVvxJeyVAE0C5gro/edit?usp=sharing",
                  sheet = "Mes") %>%
  mutate(Accion = ifelse(Accion %in% c("Llamadas SAMEC", "Intervenciones SAMEC"), "Intervenciones SAMEC", Accion))

# Diccionarios
Mes <- data.frame(Mes = c("Enero", "Febrero", "Marzo", "Abril", "Mayo", "Junio",
                 "Julio", "Agosto", "Septiembre", "Octubre", "Noviembre", "Diciembre"),
                 Mes_num = 1:12,
                 Semestre_num = rep(c(1,2), each=6))

Data <- Raw %>%
  filter(Tipo != "Abuso sexual") %>%
  mutate(Año = formatC(Año, big.mark = ".", decimal.mark = ",")) %>%
  left_join(Mes, by="Mes") %>%
  group_by(Año,Semestre_num) %>%
  summarise(Cantidad = sum(Cantidad)) %>%
  ungroup %>%
  rbind(c("2.023", 2, NA)) %>%
  mutate(Cantidad = as.numeric(Cantidad),
         Semestre_año = paste0(str_sub(Año, 4,5), "-", Semestre_num)) %>%
  arrange(Semestre_año) %>%
  mutate(Label = ifelse(is.na(Cantidad), "", formatC(Cantidad, big.mark=".", decimal.mark=",", format="d"))) %>%
  mutate(Año = ifelse(Año %in% c("2.023", "2.025"), paste0(Año, "*"), Año))

Totales <- Data %>%
  mutate(Cantidad = ifelse(is.na(Cantidad), 0, Cantidad)) %>%
  group_by(Año) %>%
  summarise(Cantidad = sum(Cantidad))

# Colores
Paleta <- c("#206170", "#5ec5d4", "#a782ec", "#852f8c", "#0f216d", "#2b42a0",
            "#ff9d27", "#ff621d", "#f93e35", "#d3335e", "#cbc2ce")

# Gr?fico
grafico <- ggplot(Data, aes(x=Semestre_año, y=Cantidad, group=1)) +
  geom_col(aes(fill=Cantidad)) +
  geom_text(aes(label=Label), family="font_sans", size=5,
            fontface="bold", color="white", nudge_y=-7500, hjust=0.5) +
  geom_point(data=Totales, aes(x=c(1.5, 3.5, 5.5, 7.5, 9.5, 11), y=150000, size=Cantidad, color=Cantidad)) +
  geom_text(data=Totales, aes(x=c(1.5, 3.5, 5.5, 7.5, 9.5, 11), y=147500,
                              label=formatC(Cantidad, big.mark = ".", decimal.mark = ",", format="fg")),
            size=5, color="white", family="font_sans", fontface="bold") +
  geom_text(data=Totales, aes(x=c(1.5, 3.5, 5.5, 7.5, 9.5, 11), y=154000, label=Año),
            size=4, color="white", family="font_serif") +
  annotate(geom="text", y=-25000, x=c(seq(from=1.5, to=9.5, by=2), 11), label=formatC(seq(2020,2025), big.mark=".", decimal.mark=",", format="d"),
           size=8, color="black", family="font_sans") +
  annotate(geom="segment", x=c(0.25, 2.5, 4.5, 6.5, 8.5, 10.5, 11.75), xend=c(0.25, 2.5, 4.5, 6.5, 8.5, 10.5, 11.75),
           y=-30000, yend=175000, color="grey", linewidth=0.25) +
  labs(title="",
       x="Semestre/Año", y="Cantidad",
       caption="* las proporciones se calculan en base a los datos correspondientes al primer semestre únicamente.") +
  scale_x_discrete(labels = rep(c("1°", "2°"), 6)) +
  scale_y_continuous(labels = function(z) formatC(z, big.mark = ".", decimal.mark = ",", format="d"),
                     breaks = seq(from=0, to=100000, by=25000)) +
  scale_fill_gradient2(high="#2b42a0", low="#95a1d0", mid="#2b42a0", midpoint=mean(Data$Cantidad, na.rm=TRUE)) +
  scale_color_gradient2(high="#2b42a0", low="#95a1d0", mid="#2b42a0", midpoint=mean(Totales$Cantidad, na.rm=TRUE)) +
  scale_size_continuous(range=c(25, 40)) +
  theme_light() +
  coord_cartesian(ylim = c(-5000, 175000), xlim=c(0.25, 11.75), clip="off", expand=FALSE) +
  theme(text=element_text(family="font"),
        legend.position="none",
        legend.title = element_blank(),
        legend.text = element_text(size=12, family="font_sans"),
        plot.title = element_text(size=20, family="font_sans", face="bold"),
        plot.subtitle = element_text(size=15, family="font_sans"),
        plot.caption = element_text(size=10, family="font_sans", face="italic", margin=margin(t=10)),
        axis.text.x = element_text(size=15, margin = margin(t=5,r=0,b=5,l=0)),
        axis.text.y = element_text(size=15, margin = margin(t=0,r=10,b=0,l=5)),
        axis.title.x = element_text(size=15, margin=margin(t=40)),
        axis.title.y = element_text(size=15),
        plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm"),
        panel.grid = element_blank(),
        panel.grid.major = element_line(linewidth=0.5, color="grey95"),
        panel.grid.major.x = element_blank())

# Guardar gráfico
filename <- str_sub(basename(rstudioapi::getSourceEditorContext()$path), 1,
                    str_length(unlist(basename(rstudioapi::getSourceEditorContext()$path)))-2)

ggsave(filename = paste0(filename, ".png"),
       path = paste0(dirname(rstudioapi::getActiveDocumentContext()$path),"/Graficos/PNG/"),
       plot=grafico, dpi=100, width=14, height=7)
ggsave(filename = paste0(filename, ".pdf"),
       path=paste0(dirname(rstudioapi::getActiveDocumentContext()$path),"/Graficos/pdf/"),
       plot=grafico, dpi=72, width=14, height=7)
