library(dplyr)
library(ggplot2)
library(ggthemes)
library(extrafont)
loadfonts(device = "win")

seasons <- 2016:2019
pbp <- purrr::map_df(seasons, function(x) {
  readRDS(
    url(
      glue::glue("https://raw.githubusercontent.com/guga31bb/nflfastR-data/master/data/play_by_play_{x}.rds")
    )
  )
})

dak<-pbp %>% filter(
  play_type %in% c('pass'),
  !is.na(epa),
  !is.na(wp),
  posteam == 'DAL',
  season_type=='REG',
  passer_player_name == 'D.Prescott') %>%
  mutate(
    wp = round(wp,2)
  )%>%group_by(wp) %>%
  summarise(
    epa = mean(epa),
  ) %>% ungroup() %>% mutate(
    group='D.Prescott',
    level = 'team'
  )%>% filter(
    wp>=.10,
    wp<=.90)

wentz<-pbp %>% filter(
  play_type %in% c('pass'),
  !is.na(epa),
  !is.na(wp),
  posteam == 'PHI',
  season_type=='REG',
  passer_player_name == 'C.Wentz') %>%
  mutate(
    wp = round(wp,2)
  )%>%group_by(wp) %>%
  summarise(
    epa = mean(epa),
  ) %>% ungroup() %>% mutate(
    group='C.Wentz',
    level = 'team'
  )%>% filter(
    wp>=.10,
    wp<=.90)

nfl<-pbp %>% filter(
  play_type %in% c('pass'),
  !is.na(epa),
  !is.na(wp),
  season_type=='REG')  %>%
  mutate(
    wp = round(wp,2)
  )%>%
  group_by(wp) %>%
  summarise(
    epa = mean(epa)
  ) %>% ungroup()%>% mutate(
    group='NFL AVG',
    level = 'nfl'
  ) %>% filter(
    wp>=.10,
    wp<=.90)
plot <- rbind(dak,wentz,nfl)

#Just making sure I have the order right for the colors
plot$group<-factor(plot$group, levels = c("D.Prescott","C.Wentz","NFL AVG"))
#Same here
plot$level<-factor(plot$level, levels = c('team','nfl'))


# I do a lot of try and error with sizes and stuff (for content purposes)
#Just try diff things, see what you like
plot %>% ggplot(aes(x=wp,y=epa)) + 
  geom_smooth(aes(color=group,linetype=level),se=F,size=.6)+
  theme_fivethirtyeight() +
  scale_color_manual(values = c('#acc0c6',"#004953","#a71930" ))+
  scale_linetype_discrete(name="level",
                          breaks=c("team","team","team","team","nfl"),
                          labels= NULL,guide=FALSE)+
  theme(
    plot.title = element_text(size = 8, family = "Trebuchet MS",color = "grey20",hjust = .5),
    plot.subtitle = element_text(size = 5, family = "Trebuchet MS",color = "grey20",hjust = 0.5),
    axis.title = element_text(size = 4.5, family = "Trebuchet MS",color = "grey20"),
    plot.caption = element_text(size=4.25, family = "Trebuchet MS",color = "grey20",hjust = 0),
    legend.text = element_text(size = 5, family = "Trebuchet MS",color = "grey20"),
    legend.title = element_blank(),
    legend.direction = "horizontal",
    legend.position = "top",
    legend.key.size =unit(.6,"line"),
    axis.text = element_text(size = 3.5, family = "Trebuchet MS",color = "grey20"),
    panel.grid = element_line(size=.11)) + 
  annotate(
    geom = "curve", x = .55, y = .35,xend = .5, yend = .1, 
    curvature = -.1, size= unit(.15, "mm"),arrow = arrow(length = unit(1.3, "mm")),
  )+
  annotate(geom = "label", x = .55, y = .35,size=1.5,family = "Trebuchet MS",color = "grey20",fill="#F0F0F0",
           label = 'Close Game Situation')+
  annotate(geom = "text", x = .15, y = .38,size=1.6,family = "Trebuchet MS",color = "grey20",
           label = 'Seasons: 2016 - 2019') +
  xlim(.1, .9)+ coord_cartesian(ylim = c(-.05, .4)) +
  labs(x='Win Probability', y='Passing Efficiency (EPA/Dropback)',
       title = "During his Career, Dak Prescott has Delivered in Key Moments",
       subtitle = 'Filtering-out "garbage" time | Dak has remained above average regardless of win-probability situation',
       caption = "Win Probability between 10% and 90% | Pass plays | Regular Season Games: 2016 - 2019 
Data: nflfastR | Chart by Adrian Cadena @adrian_cadem" )

ggsave('file_namet.png', dpi=1200, width = 11, height = 8, units = "cm")

##------------------------------------------------------------------------------------------------------


