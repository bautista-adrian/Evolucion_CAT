library(magrittr)
library(gganimate)
library(scales)
library(ggthemes)
library(hrbrthemes)

getwd()
setwd("C:/Users/Adrian Bautista/Desktop/CAT_Tarjetas_clasicas")
dir()

cattcc <- read.csv("cattcc.csv")
cattcc <- cattcc[-9,]

str(cattcc)

cattcc %<>% gather(key = "Fecha",value = "Tasa", 2:17)
cattcc$Fecha <- as.character(cattcc$Fecha)
cattcc$Fechas <- gsub("_"," ",cattcc$Fecha)
cattcc$Fechasss <- gsub("_"," ",cattcc$Fecha)
cattcc <- separate(cattcc,Fechasss, into = c("Mes","A?o"),sep = " ")

levels(cattcc$Fechas)
cattcc$Fechas %<>% as.factor()
levels(cattcc$Fechas)

cattcc$Fechas <- factor(cattcc$Fechas, levels = c("Abril 2016",
                                                  "Junio 2016",
                                                  "Agosto 2016",
                                                  "Octubre 2016",
                                                  "Diciembre 2016",
                                                  "Febrero 2017",
                                                  "Abril 2017",
                                                  "Junio 2017",
                                                  "Agosto 2017",
                                                  "Octubre 2017",
                                                  "Diciembre 2017",
                                                  "Febrero 2018",
                                                  "Abril 2018",
                                                  "Junio 2018",
                                                  "Agosto 2018",
                                                  "Octubre 2018"))

class(cattcc$Fecha)
levels(cattcc$Fecha)

cattcc %<>% group_by(Fechas)
cattcc %<>% mutate(rank = min_rank(-Tasa) * 1)
cattcc %<>% ungroup()
cattcc %<>% filter(rank <= 10)

levels(cattcc$Entidad)
levels(cattcc$Entidad)
abr2016 <- as.data.frame(levels(cattcc$Entidad)) 
names(abr2016) <- (c("Entidad"))


cat_plot <- ggplot(cattcc, aes(rank, group = Entidad, fill = Entidad, color = Entidad)) +
  geom_tile(aes(y = Tasa/2,height = Tasa,width = 0.9)) + 
  geom_text(aes(y = 0, label = paste(Entidad, " ")), vjust = 0.2, hjust = 1) +
  coord_flip(clip = "off", expand = FALSE) +
  scale_y_continuous(labels = percent_format(),limits = c(0,1.05),breaks = c(0,.1,.2,.3,.4,.5,.6,.7,.8,.9,1)) +
  scale_x_reverse() + 
  scale_color_brewer(palette = "Paired") + scale_fill_brewer(palette = "Paired")+
  guides(color = FALSE, fill = FALSE) +
  labs(title = "CAT promedio de las tarjetas de credito clasicas por entidad emisora",
       subtitle="Promedio calculado al mes de {closest_state}", x = element_blank(),
       y = element_blank(), caption = "Source: COFECE / Elaboracion: @4drian.bautista") + 
  theme_ft_rc()+
  theme(plot.title = element_text(hjust = 0, size = 22),
        axis.ticks.y = element_blank(),  
        axis.text.y  = element_blank(),  
        plot.margin = margin(1,1,1,4, "cm")) +
  transition_states(Fechas, transition_length = 4, state_length = 1) +
  ease_aes('cubic-in-out')

animate(cat_plot, fps = 25 ,duration = 40, width = 1280, height = 720)

anim_save("CATTCC.gif")
