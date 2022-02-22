library(tidytuesdayR)
library(tidyverse)
tuesdata <- tidytuesdayR::tt_load('2022-02-22')
tuesdata$freedom->freedom
freedom%>%
  distinct()%>%
  drop_na()%>%
  select(-c(CL,PR,Region_Code))%>%
  group_by(Region_Name)->data
  
data

str_replace_all(data$Status,"PF","Partly Free")->data$Status
str_replace_all(data$Status,"NF","Not Free")->data$Status
str_replace_all(data$Status,"F","Free")->data$Status
str_replace_all(data$Status,"Partly Freeree","Partly Free")->data$Status
str_replace_all(data$Status,"Not Freeree","Not Free")->data$Status

ggplot(data,aes(year,Status))+
  geom_jitter(aes(colour=as.factor(is_ldc)))+
  facet_wrap(~Region_Name,scales = "free")+
  scale_x_continuous(breaks=c(1995,2000,2005,2010,2015,2020))+
  scale_colour_manual(values=c("gray10","white"))+
  labs(colour=" ")+
  theme(plot.margin=unit(c(0.5,1.5,0.5,1.5),"cm"),
        plot.background = element_rect(fill="black"),
        panel.background = element_rect(fill="black"),
        axis.text = element_text(colour="white",size=10,face="bold"),
        axis.title=element_blank(),
        panel.grid = element_blank(),
        legend.position = "none",
        plot.title.position = "plot",
        plot.caption.position = "plot",
        plot.title=element_text(size=14, face="bold",colour="white",margin=margin(b=20)),
        plot.subtitle = element_text(size=12, colour="white",margin=margin(b=30)),
        plot.caption=element_text(size=10,colour="white",hjust=0,margin=margin(t=30)))+
  labs(title= str_wrap("THE LEAST DEVELOPED COUNTRIES IN AFRICA, AMERICAS AND ASIA ARE EITHER NOT FREE OR ONLY PARTLY FREE",100),
       subtitle=str_wrap("Most of the least developed countries in Africa, Americas and Asia are either not free or only partially free, based on data from Freedom House. On the contrary, in Oceania, most of the least developed countries are free, as can be seen from the data visualization below",140),
       caption = "Data from Freedom House and the United Nations by way of Arthur Cheib for Tidy Tuesday| Analysis and design: @annapurani93")->plot


ggsave("LDC.png",plot,width=12.5,height=8.4)
ggsave("LDC.pdf",plot,width=12.5,height=8.4)



