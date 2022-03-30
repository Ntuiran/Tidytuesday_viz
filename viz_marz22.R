#Paquete para descargar las tablas
tuesdata <- tidytuesdayR::tt_load(2022, week = 12)
#Selección tabla para nombre de bebes
babynames <- tuesdata$babynames

library(tidyverse)
library()
data<-babynames%>%
  group_by(year)%>%
  summarise(count=sum(n[name=='Neil']),
            total=sum(n))%>%
  mutate(perc_total = count/total)


note<-data.frame(x=c(1953),
                 y=c(0.00047),
                 label= c("Peak Year \n 1953")
)

library(emoji)
max(data$perc_total)
points<-data.frame(x=1953, y=0.0005038643)
ggplot(data, aes(x=year, y=perc_total))+
  geom_area(color = "#7dd386", fill = "#99e8a2")+
  geom_text(data=note, mapping=aes(x=x,y=y,label=label), size=3, color="#1a5276")+
  geom_point(data=points, mapping=aes(x=x,y=y), color=col_bl, shape = emoji('baby'), size = 4)+
  scale_y_continuous(labels=scales::label_percent(accuracy=0.01))+
  labs(title="Popularity of my name over time", 
       subtitle="Popularity of Neil baby name in the United States from 1880 to 2017",
       caption="Data from babynames R package | Plot by @Neiltuirant",
       y="% of Total", x="Year")+
  theme_minimal()+
  theme(text=element_text(family = "chivo", color="#525b88"),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(color="#03971b", size=0.08),
        axis.title=element_text(face="bold"),
        axis.text =element_text(color="#525b88"),
        axis.title.y=element_text(margin=margin(r=10)),
        plot.title=element_text(face="bold"),
        plot.background = element_rect(fill="#d8e8ac"),
        plot.margin  = margin(t=20, b=20, r=10, l=10),
        axis.line = element_line(colour = "grey50"))

ggsave("neil_names.png", height=5, width=7.5)
