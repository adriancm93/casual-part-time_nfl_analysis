library(dplyr)
library(ggplot2)
library(ggthemes)
library(extrafont)
library(tidyverse)
loadfonts(device = "win")
library(scales)

seasons <- 2016:2019
pbp <- purrr::map_df(seasons, function(x) {
  readRDS(
    url(
      glue::glue("https://raw.githubusercontent.com/guga31bb/nflfastR-data/master/data/play_by_play_{x}.rds")
    )
  )
})

npass <- pbp %>%filter( play_type == 'pass') %>%
  group_by(passer_player_id) %>% 
  summarise(
    num_plays = n(),
    last_seas = max(season)
  )

pbp2<-merge(pbp,npass,by='passer_player_id',all.x = T,no.dups = T)

#I'll use wather data from nflfastT, but ideally you want to use @DataWithBliss (check hsi profile). 
#Won't do becas I don't have time to map time of the day

pbp_mut <- pbp2 %>% 
  filter(
    season_type == 'REG',
    wp <= .85,
    wp >= .15,
    play_type == 'pass',
    !is.na(complete_pass),
    penalty == 0,
    num_plays > 450) %>%
  mutate(
    ayard_is_zero = if_else(air_yards==0,1,0),
    tosticks = (air_yards - ydstogo),
    era1 = if_else(season %in% 2014:2017,1,0),
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
                                         if_else(passer_player_name =='Matt.Moore','M.Moore',
                                                 if_else(passer_player_name=='G.Minshew II','G.Minshew',passer_player_name)))),
    outdoor = if_else(roof %in% c('outdoors','open'),1,0)) %>% 
  select(id,passer_player_name,outdoor,era1,season,away,wind,temp,complete_pass,
         air_yards,qb_hit,ayard_is_zero,yardline_100,ydstogo,tosticks,down,
         team,oppteam,poscoach,defcoach)

names<-pbp_mut %>% group_by(id) %>% summarise(Quarterback=unique(passer_player_name),last_seas = max(season),num_plays=n())


library(lme4)

#Here the right thing to do is a GAMM (Generalized Aditive Mixed Model) since air_yards has a non-linear relationship with completion%
#as @StatsbyLopez Michael Lopez has explaned here: https://statsbylopez.netlify.app/post/plotting-air-yards/

#But I'm lazy so let's do just the logistic mixed model
#This will take around 30 minutes.
mod<-pbp_mut %>% 
  glmer(formula=
          complete_pass ~
          ydstogo*as.factor(down) + 
          ydstogo+
          down +
          wind +
          temp +
          away +
          outdoor + 
          qb_hit +
          ayard_is_zero + 
          air_yards + 
          (1|id)+
          (1|oppteam)+
          (1|team),
      family = binomial,
      control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
mod %>% summary()

getME(mod,'theta') %>% bind_rows() %>% t() 

est<-broom.mixed::tidy(mod,effects="ran_vals") %>% 
  filter(group=='id') %>% rename('id'='level')

logit2prob <- function(logit){
  odds <- exp(logit)
  prob <- odds / (1 + odds)
  return(prob)
}
plot
plot<-merge(est,names,by= 'id',all.x = T,no.dups = T) %>% arrange(estimate) %>% mutate(
  prob = logit2prob(estimate),
) 


plot  %>%
  ggplot(aes(x=factor(Quarterback, level = Quarterback),estimate)) + 
  geom_point(size=.6) +
  geom_linerange(size=.4,aes(ymin=estimate - 1.96*std.error,
                     ymax=estimate + 1.96*std.error))+
  coord_flip() +
  theme_fivethirtyeight()+
  labs(y="iLog-Odds of Completion per QB (by how much QB's ability changes the existing odds of completion)",
       title = 'Individual Log-Odds of Completion per Quartbeack',
       subtitle = 'This time it can be negative becase it changes existing odds | ABOVE 0 IS POSITIVE CPOE',
       caption = "Data: nflfastR | Analysis by Adrian Cadena @adrian_cadem" ) +
  theme(
    axis.title.y = element_blank(),
    plot.title = element_text(size=12,family = "Trebuchet MS",color = "grey20",hjust = .5),
    plot.subtitle = element_text(size=9,family = "Trebuchet MS",color = "grey20",hjust = 0.5),
    axis.title = element_text(size=8,family = "Trebuchet MS",color = "grey20"),
    plot.caption = element_text(size=6.5,family = "Trebuchet MS",color = "grey20",hjust = 0),
    axis.text = element_text(size=7,family = "Trebuchet MS",color = "grey20")
  )

ggsave('file_name1.png', dpi=1100,width = 16, height = 14, units = "cm") 

plot  %>% filter(last_seas>2018) %>%
  ggplot(aes(x=factor(Quarterback, level = Quarterback),prob)) + 
  geom_col(fill = "grey20")+
  geom_text(aes(label = Quarterback, y=(((prob-.45)*.5))+.45 ) ,color = 'white',hjust=1,size=1.6)+coord_flip() +
  theme_fivethirtyeight()+
  labs(y="iProbability of Completion per QB (by how much QB's ability increases probability of completion)",
       title = 'Individual Probability of Completion per Quartbeack',
       subtitle = 'Think about it as the "floor" completion % or the "added" completion % by the QB',
       caption = "Data: nflfastR | Analysis by Adrian Cadena @adrian_cadem" ) + 
  scale_y_continuous(limits=c(.45,.54),oob = rescale_none,labels = scales::percent_format(accuracy = 1)) +
  theme(
    axis.ticks.y  = element_blank(),
    axis.text.y = element_blank(),
    axis.title.y = element_blank(),
    plot.title = element_text(size = 8, family = "Trebuchet MS",color = "grey20",hjust = .5),
    plot.subtitle = element_text(size = 6, family = "Trebuchet MS",color = "grey20",hjust = 0.5),
    axis.title = element_text(size = 5, family = "Trebuchet MS",color = "grey20"),
    plot.caption = element_text(size=4, family = "Trebuchet MS",color = "grey20",hjust = 0),
    axis.text = element_text(size = 4.5, family = "Trebuchet MS",color = "grey20"),
    panel.grid = element_line(size=.11)) 
  
ggsave('file_name.png', dpi=1200,width = 14, height = 11, units = "cm")  



v<-plot  %>% filter(last_seas>2018) 




coefficients(mod)
