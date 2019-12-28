
library("rtweet")
library(tidyverse)
library(maps)
library(tidyverse)
library(ggthemes)
library(png)
library(gridGraphics)
library(cowplot)
library(hrbrthemes)
library(ggtext)
library(ggdark)

token <- create_token(
  app = "my_twitter_research_app",
  consumer_key = 'RgivABxfsX9ubdhxLxZBBh5m7',
  consumer_secret = '5TigMInpBwJLNnFed8x8JlfNpvjMU0nOd9u1UPgRA2KOYIsNVU',
  access_token = '496163705-nylSebOzfa5bThbRQHmS40f3eLV0X2z6V514uxcB',
  access_secret = 'OkTZpcW9CJPMuApCQvWpZAlKR38vz8d2g9UStFDX32U3k')

tag = 'صدرالساداتی'
tweets_fazel<- search_tweets(
  tag , n = 70000,include_rts = TRUE,
  retryonratelimit = TRUE,
)
tweets_fazel_2<- search_tweets(
  tag , n = 30000,include_rts = TRUE,
  retryonratelimit = TRUE,
)

ts_plot(tweets_fazel) +
  
  theme_ipsum_pub(base_family  = 'Montserrat')+
  theme(axis.text = element_text(family = "Montserrat",
                                 size = 10),
        axis.title.y = element_text(family = "Montserrat", 
                                    face = "plain",
                                    size = 13,
                                    color = "grey85",
                                    vjust = 1,
                                    hjust = 0.5),
        
        plot.title = element_text(family = "Montserrat Black", 
                                  size = 20, 
                                  color = "grey65"),
        plot.subtitle = element_text(family = "Montserrat Black", 
                                     size = 15, 
                                     color = "grey65"),
        #panel.background = element_rect(fill = "grey"),
        plot.background = element_rect(fill = "transparent", color = "transparent"),
        #strip.text = element_blank(),
        #strip.background = element_blank(),
        #panel.spacing.x = unit(12, "pt"),
        line = element_blank(),
        legend.position = "none",
        rect = element_rect(fill = "#173f50")
  ) 
  

theme_update()

--------------------------------------------------------------
  ------------------------------------------------------------
  
  
  
  bkgrnd <- "#252a32"
fgrnd <- "#617a89"
christmas_colors <-
  c( '#ffd700',
     '#F8B229',
     '#D52B27',
     '#165B33',
     '#146B3A',
     '#BB2528',
     '#27336B',
     '#C0C0C0',
     '#1B294C',
     '#252525'
  )



christmas_songs %>% 
  group_by(year,month) %>% 
  summarise(peak_position = min(peak_position),
            top_song = song[which.min(peak_position)],
            weeks_on_chart =weeks_on_chart[which.min(peak_position)] ) %>% 
  ggplot() +
  geom_rect(aes(xmin = -1, ymin = -1, xmax = 1, ymax = 1, fill = peak_position), color = "white") +
  geom_rect(aes(xmin = -1, ymin = 0.8, xmax = 1, ymax = 1, fill = peak_position), color = "white") +
  geom_text(aes(label = top_song, x = -0.65, y = 0.45),
            size = 5, color = "white") +
  facet_wrap(~ year, ncol = 3) +
  theme(
    panel.grid.major = element_blank(),
    
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.title.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.title.y = element_blank(),
  ) 





```{r fig.width = 15, fig.height = 15}


data <- christmas_songs %>%
  mutate(christmas = ifelse(
    month == 12,
    paste('Christmas', year + 1),
    paste('Christmas', year)
  )) %>%
  group_by(christmas) %>%
  summarise(
    top_song = str_wrap(song[which.min(peak_position)], width = 15),
    
    performer = str_wrap(performer[which.min(peak_position)], width = 15),
    
    peak_position = min(peak_position),
    
    weeks_on_chart = weeks_on_chart[which.min(peak_position)]
  ) %>%
  mutate(peak_position_d = chop(peak_position, breaks = seq(0, 100, 10)))

data %>% 
  ggplot() +
  geom_rect(aes(
    xmin = -4,
    ymin = -4,
    xmax = 4,
    ymax = 4,
    fill = peak_position_d
  ),
  color = "white") +
  #geom_rect(aes(xmin = -1, ymin = 0.8, xmax = 1, ymax = 1, fill = peak_position), color = "white")
  geom_text(
    aes(label = str_wrap(top_song,width = 15 ), x = -1.5, y = 0.5),
    hjust = 0,
    size = 5,
    vjust = 0,
    color = "white",
    check_overlap = TRUE,
    family = 'Montserrat'
  ) +
  
  geom_text(aes(label = performer ),
            x = -1.5,
            y = -1,
            hjust =  "center",
            size = 4,
            vjust = "middle",
            color = "white",
            family = 'Montserrat'
  ) +
  
  geom_text(aes(label = weeks_on_chart ),
            x = -1.5,
            y = -3,
            hjust = "center",
            size = 4,
            #vjust = 0,
            color = "white",
            family = 'Montserrat'
  )  +
  # coord_fixed(xlim = c(-2, 2), ylim = c(-2, 2)) +
  facet_wrap( ~ christmas,
              ncol = 5) +
  labs(title = 'The most Popular Christmas songs') +
  theme_void() +
  scale_fill_manual(values = christmas_colors) +
  #theme_ft_rc() +
  theme(
    panel.grid.major = element_blank(),
    panel.border = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.title.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.title.y = element_blank(),
    plot.background = element_rect(fill = bkgrnd, color = bkgrnd),
    plot.title = element_text( family = 'Montserrat',face = "bold", size = 48,
                               margin = margin(0, 0, 60, 0), hjust = 0),
    strip.text = element_text(family = 'Montserrat',size = 20)
    
  )


```


```{r fig.width = 15, fig.height = 30}
data %>% 
  #filter(christmas != 'Christmas 1959') %>% 
  ggplot() +
  geom_rect(aes(
    xmin = -4,
    ymin = -4,
    xmax = 4,
    ymax = 4,
    #color = peak_position_d,
  ),
  color = "gray80",
  fill = 'white') +
  
  geom_rect(aes(
    xmin = -4,
    ymin = 2,
    xmax = 4,
    ymax = 4,
    fill = peak_position_d
  ),
  
  color = "white") +
  
  geom_text(aes(label = christmas),
            x = 0, y = 3,
            family = 'Montserrat',
            color = "white",
            size = 6) +
  
  #geom_rect(aes(xmin = -1, ymin = 0.8, xmax = 1, ymax = 1, fill = peak_position), color = "white")
  geom_text(
    aes(label = str_wrap(top_song,width = 20 ), x = 0, y = 0.5),
    hjust = 'middle',
    size = 5,
    vjust = 'top',
    color = "gray20",
    check_overlap = TRUE,
    family = 'Montserrat'
  ) +
  
  geom_text(aes(label = str_wrap(performer,width = 20 )  ),
            x = 0,
            y = -2,
            hjust =  'middle',
            size = 4,
            vjust = 'top',
            color = "gray20",
            family = 'Montserrat'
  ) +
  
  geom_text(aes(label = weeks_on_chart ),
            x = -1.5,
            y = -3,
            hjust = "center",
            size = 4,
            vjust = 'top',
            color = "gray80",
            family = 'Montserrat'
  ) +
  facet_wrap( ~ christmas,
              ncol = 4) +
  labs(title = 'The most Popular Christmas songs') +
  theme_void() +
  scale_fill_manual(values = christmas_colors) +
  theme_ft_rc() +
  theme(
    panel.grid.major = element_blank(),
    panel.border = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.title.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.title.y = element_blank(),
    plot.title = element_text( family = 'Montserrat',face = "bold", size = 48,
                               margin = margin(0, 0, 60, 0), hjust = 0),
    #strip.text = element_text(family = 'Montserrat',size = 20)
    strip.text = element_blank()
  )
```

---------------------------------------------------------------
  --------------------------------------------------------