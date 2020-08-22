library(lme4)
library(ggplot2)
library(ggthemes)
library(dplyr)

seasons <- 2000:2019
pbp <- purrr::map_df(seasons, function(x) {
  readRDS(
    url(
      glue::glue("https://raw.githubusercontent.com/guga31bb/nflfastR-data/master/data/play_by_play_{x}.rds")
    )
  )
})

pbp_mut<-pbp %>%
  mutate(
    away = if_else(home_team == posteam, 0,1),
    team = paste(season,posteam),
    oppteam = paste(season,defteam),
    poscoach = if_else(home_team==posteam,home_coach,away_coach),
    defcoach = if_else(home_team==defteam,home_coach,away_coach),
    wind = ifelse(is.na(wind)==T,min(wind,na.rm=T),wind),
    temp = ifelse(is.na(temp)==T,75,temp),
    id = passer_player_id,
    passer_player_name = if_else(passer_player_name == 'Jos.Allen','J.Allen', #fixing some weird bugs I found with names
                                 if_else(passer_player_name =='R.Griffin','R.Griffin III', #bugs don't affect model, but mess with plot  
                                         if_else(passer_player_name =='Matt.Moore','M.Moore',passer_player_name)))
    ) %>% filter(
    wp <= .85,
    wp >= .15,
    play_type == 'pass',
    !is.na(cpoe)
  ) %>% select(id,wind,temp,poscoach,defcoach,team,oppteam,away,stadium_id,cpoe,away,passer_player_name,epa,season,cpoe)

#If you want to look at EPA instead of CPOE, just change repsonse variable, also maybe take stadium_id out
mixed_model<-pbp_mut %>% 
  lmer(formula=
         cpoe ~
         temp +
         wind + 
         away +
         (1|id)+
         (1|team)+
         (1|oppteam)+
         (1|defcoach)+
         (1|poscoach)+
         (1|stadium_id) #This is my proxy for altitude
       ,
       control=lmerControl(optimizer="bobyqa",
                           optCtrl=list(maxfun=2e5)))

#I'm a super messy coder, so I'm sorry for this in advance
coef<-broom.mixed::tidy(mixed_model,effects="ran_vals") %>% filter(group=='id')

solid<-pbp_mut %>% group_by(id) %>% summarise(first_seas = min(season),last_seas = max(season)) %>% filter(first_seas<2018 & last_seas == 2019) #change this at will
players = pbp_mut %>% group_by(id) %>% summarise(Quarterback = unique(passer_player_name)) %>% select(level=id,Quarterback)
plays <- pbp_mut %>%
  group_by(id) %>% 
  summarise(
    num_plays = n()
  )

coef<-inner_join(coef,plays, by = c("level"='id'), all.x=T) %>% filter(num_plays >= 500)
plot<-left_join(coef,players, by = "level", all.x=T) %>% mutate(t = estimate/std.error) %>% filter(level %in% (solid$id)) %>% arrange(std.error) %>% tail(15)

# You'll probably want to do some "hard-coding here, to select which players are expected to be starters coming into 2020 
#Change titles if using EPA/play of course
plot %>%
  ggplot(aes(x=factor(Quarterback, level = Quarterback),estimate)) + 
  geom_point() +
  geom_linerange(aes(ymin=estimate - 1.96*std.error,
                     ymax=estimate + 1.96*std.error))+
  coord_flip() + theme_fivethirtyeight () +
  labs(x = "Quarterback",
       y = "CPOE Intercept and 95% Conf. Intervals (Range)",
       caption = "Data from nflfastR",
       title = "Quarterbacks with Largest Range of Outcomes Going into 2020",
       subtitle = 'In terms of CPOE')+
  theme(text = element_text(),
        plot.title = element_text(family = "Trebuchet MS", hjust =.5,color = "grey20"),
        plot.subtitle = element_text(family = "Trebuchet MS",color = "grey20", hjust =.5),
        axis.title = element_text(family = "Trebuchet MS",color = "grey20"),
        plot.caption = element_text(color = "grey20", hjust = 0),
        axis.text = element_text(face = "bold",family = "Trebuchet MS",color = "grey20")) 


ggsave('file_name.png', dpi=1200) #width = 16, height = 13, units = "cm" <- I like to play around with those when posting on twitter
