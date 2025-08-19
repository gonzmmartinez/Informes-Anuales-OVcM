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
  arrange(Semestre_año)

# Colores
Colores <- c("#6e3169", "#ec6489")

# Gr?fico
grafico <- ggplot(Data, aes(x=Semestre_año, y=Cantidad, group=1)) +
  geom_col(aes(alpha=Cantidad), fill=Colores[1]) +
  geom_line(linewidth = 4, color=Colores[2]) +
  geom_point(size = 5, color=Colores[2]) +
  geom_text(aes(y=45000, label=formatC(Cantidad, big.mark=".", decimal.mark=",", format="d")),
            size=10, family="font", fontface="bold", color="white", angle=90) +
  annotate(geom="text", y=-0.1, x=2, label="Hola", size=30, color="black") +
  labs(title="",
       x="Año", y="Cantidad") +
  scale_alpha_continuous(range=c(0.6,1)) +
  scale_fill_gradient(labels = function(x) str_wrap(x, width = 20)) +
  scale_x_discrete(breaks = Data$Semestre_año, labels = ifelse(Data$Semestre_num == 1, Data$Año, "")) +
  scale_y_continuous(limits = c(0, 120000), labels = function(z) formatC(z, big.mark = ".", decimal.mark = ",", format="d")) +
  theme_light() +
  coord_cartesian(xlim=c(1, 11), ylim=c(0,120000), expand=FALSE, clip="off") +
  theme(text=element_text(family="font"),
        legend.position="none",
        legend.title = element_blank(),
        legend.text = element_text(size=12, family="font"),
        plot.title = element_text(size=20, family="font", face="bold"),
        plot.subtitle = element_text(size=15, family="font"),
        plot.caption = element_text(size=12, family="font", face="italic"),
        panel.grid = element_blank(),
        axis.text.x = element_text(size=20, margin = margin(t=10,r=0,b=5,l=0), hjust=-1),
        axis.text.y = element_text(size=15, margin = margin(t=0,r=10,b=0,l=5)),
        axis.title.x = element_text(size=20),
        axis.title.y = element_text(size=20))

# Guardar gráfico
filename <- str_sub(basename(rstudioapi::getSourceEditorContext()$path), 1,
                    str_length(unlist(basename(rstudioapi::getSourceEditorContext()$path)))-2)

ggsave(filename = paste0(filename, ".png"),
       path = paste0(dirname(rstudioapi::getActiveDocumentContext()$path),"/Graficos/PNG/"),
       plot=grafico, dpi=100, width=14, height=7)
ggsave(filename = paste0(filename, ".pdf"), path=paste0(dirname(rstudioapi::getActiveDocumentContext()$path),"/Graficos/pdf/"),
       plot=grafico, dpi=72, width=14, height=7)