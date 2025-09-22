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
font_add_google("Barlow", "font")
showtext_auto()

# Leer datos
Raw <- read_sheet(ss = "https://docs.google.com/spreadsheets/d/1fX8iWndJKs_UTTcB1SoU5tpTK7ysVvxJeyVAE0C5gro/edit?usp=sharing",
                  sheet = "Mes")

# Diccionarios
Mes <- data.frame(Mes = c("Enero", "Febrero", "Marzo", "Abril", "Mayo", "Junio",
                 "Julio", "Agosto", "Septiembre", "Octubre", "Noviembre", "Diciembre"),
                 Mes_num = 1:12,
                 Semestre_num = rep(c(1,2), each=6))

Data <- Raw %>%
  filter(Tipo != "Abuso sexual") %>%
  mutate(Año = as.factor(formatC(Año, big.mark = ".", decimal.mark = ","))) %>%
  left_join(Mes, by="Mes") %>%
  group_by(Año,Semestre_num) %>%
  summarise(Cantidad = sum(Cantidad)) %>%
  ungroup %>%
  rbind(c("2.023", 2, NA)) %>%
  mutate(Cantidad = as.numeric(Cantidad),
         Semestre_año = paste0(str_sub(Año, 4,5), "-", Semestre_num)) %>%
  arrange(Semestre_año) %>%
  mutate(Label = ifelse(is.na(Cantidad), "", formatC(Cantidad, big.mark=".", decimal.mark=",", format="d")))

# Colores
Colores <- c("#6e3169", "#ec6489")

# Gr?fico
grafico <- ggplot(Data, aes(x=Semestre_año, y=Cantidad, group=1)) +
  geom_col(aes(fill=Cantidad)) +
  geom_text(aes(label=Label, size=Cantidad), family="font",
            fontface="bold", color="white", nudge_y=-5000, hjust=0.5) +
  annotate(geom="text", y=-20000, x=c(seq(from=1.5, to=9.5, by=2), 11), label=formatC(seq(2020,2025), big.mark=".", decimal.mark=",", format="d"),
           size=8, color="black", family="font") +
  annotate(geom="segment", x=c(0.25, 2.5, 4.5, 6.5, 8.5, 10.5, 11.75), xend=c(0.25, 2.5, 4.5, 6.5, 8.5, 10.5, 11.75),
           y=-25000, yend=120000, color="grey", linewidth=0.25) +
  labs(title="",
       x="Semestre/Año", y="Cantidad") +
  scale_x_discrete(labels = rep(c("1°", "2°"), 6)) +
  scale_y_continuous(labels = function(z) formatC(z, big.mark = ".", decimal.mark = ",", format="d")) +
  scale_fill_gradient2(high="#6e3169", low="#ec6489", mid="#6e3169", midpoint=mean(Data$Cantidad, na.rm=TRUE)) +
  scale_size_continuous(range=c(4, 6)) +
  theme_light() +
  coord_cartesian(ylim = c(-5000, 120000), xlim=c(0.25, 11.75), clip="off", expand=FALSE) +
  theme(text=element_text(family="font"),
        legend.position="none",
        legend.title = element_blank(),
        legend.text = element_text(size=12, family="font"),
        plot.title = element_text(size=20, family="font", face="bold"),
        plot.subtitle = element_text(size=15, family="font"),
        plot.caption = element_text(size=12, family="font", face="italic"),
        axis.text.x = element_text(size=15, margin = margin(t=5,r=0,b=5,l=0)),
        axis.text.y = element_text(size=15, margin = margin(t=0,r=10,b=0,l=5)),
        axis.title.x = element_text(size=15, margin=margin(t=50)),
        axis.title.y = element_text(size=15),
        plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm"),
        panel.grid = element_line(color="grey95"),
        panel.grid.major.x = element_blank())

# Guardar gráfico
filename <- str_sub(basename(rstudioapi::getSourceEditorContext()$path), 1,
                    str_length(unlist(basename(rstudioapi::getSourceEditorContext()$path)))-2)

ggsave(filename = paste0(filename, ".png"),
       path = paste0(dirname(rstudioapi::getActiveDocumentContext()$path),"/Graficos/PNG/"),
       plot=grafico, dpi=100, width=14, height=7)
ggsave(filename = paste0(filename, ".pdf"), path=paste0(dirname(rstudioapi::getActiveDocumentContext()$path),"/Graficos/pdf/"),
       plot=grafico, dpi=72, width=14, height=7)

