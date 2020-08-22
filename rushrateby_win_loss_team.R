library(dplyr)
library(ggplot2)
library(ggthemes)
library(extrafont)
loadfonts(device = "win")
round(1.23,2)

seasons <- 2017:2019
pbp <- purrr::map_df(seasons, function(x) {
  readRDS(
    url(
      glue::glue("https://raw.githubusercontent.com/guga31bb/nflfastR-data/master/data/play_by_play_{x}.rds")
    )
  )
})

plot<-pbp %>% filter(
  play_type %in% c('pass','run'),
  !is.na(epa),
  !is.na(wp),
  season_type=='REG') %>%
  mutate(
    wp = round(wp,1),
    pass = if_else(play_type=='pass',1,0),
    run = if_else(play_type=='run',1,0),
    hometeam_won = if_else(result >0,'Winning Team','Losing Team'),
    awayteam_won = if_else(result <0,'Winning Team','Losing Team'),
    posteam_won = if_else(posteam==home_team,hometeam_won,awayteam_won)
  ) %>% group_by(posteam_won,wp) %>%
  summarise(
    epa = mean(epa,na.rm = T),
    pass = mean(pass,na.rm = T),
    run = round(mean(run,na.rm = T),2),
  ) %>% ungroup() %>% select(wp,rate=run,posteam_won) 


#Sizes here will depend a lot on your computer. I made the file small so it looks better on twitter, try and error is the key here
#Sorry

plot%>% ggplot(aes(x=wp,y=rate)) + 
  geom_smooth(aes(color=posteam_won),se=F,size=.4)+
  theme_fivethirtyeight() + 
  scale_color_manual(values = c('#002244','#a71930' )) +
  theme(
    plot.title = element_text(size = 8, family = "Trebuchet MS",color = "grey20",hjust = 0.5),
    plot.subtitle = element_text(size = 6, family = "Trebuchet MS",color = "grey20",hjust = 0,vjust=1.5),
    axis.title = element_text(size = 6, family = "Trebuchet MS",color = "grey20"),
    plot.caption = element_text(size=4.5, family = "Trebuchet MS",color = "grey20",hjust = 0),
    legend.text = element_text(size = 5, family = "Trebuchet MS",color = "grey20"),
    legend.position = "none",
    axis.text = element_text(size = 5, family = "Trebuchet MS",color = "grey20"),
    panel.grid = element_line(size=.075))+ coord_cartesian(ylim = c(.15, .58)) +
  labs(x='Win Probability', y='Rush Rate',
       title = "Winning Teams Pass Often... Then They Rush",
       subtitle = 'Winning teams tend  to rush at a higher rate once they are already likely to win',
       caption = "Seasons: 2017 - 2019
Data: nflfastR | Analysis by Adrian Cadena @adrian_cadem | Analysis suggested by @Landeros_p33" ) +
  annotate(geom = "text", x = .22, y = .43,size=2.2,family = "Trebuchet MS",color = '#002244',
           label = 'Team ended-up losing')+
  annotate(geom = "text", x = .6, y = .37,size=2.2,family = "Trebuchet MS",color = '#a71930',
           label = 'Team ended-up winning') + 
  scale_y_continuous(labels = scales::percent_format(accuracy = 1))+ 
  scale_x_continuous(labels = scales::percent_format(accuracy = 1))+ 
  annotate(
    geom = "curve", x = .25, y = .215,xend = .1, yend = .2, 
    curvature = -.1, size= unit(.16, "mm"),arrow = arrow(length = unit(1.3, "mm")),
  )+
  annotate(geom = "text", x = .26, y = .224,size=1.55,family = "Trebuchet MS",color = "grey20",
           label = 'Winning teams relied on the
passing-game during key situations',
           hjust=0) +
  annotate(
    geom = "curve", x = .75, y = .53 ,xend = .85, yend = .5405, 
    curvature = 0.05, size= unit(.16, "mm"),arrow = arrow(length = unit(1.3, "mm")),
  ) +
  annotate(geom = "text", x = .75, y = .53,size=1.55,family = "Trebuchet MS",color = "grey20",
           label = 'Winning teams rushed more once 
                    win probability was above 75%',
           hjust=1)

#It will look nice once you save it like this, I think
ggsave('file_name.png', dpi=1200, width = 12.5, height = 6.5, units = "cm")


