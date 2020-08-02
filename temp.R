
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

```{r}
?simpsons_pal(10)
library(hrbrthemes)
library(ggchicklet)

library(tidyverse)
nrc <- get_sentiments("bing")
simpsons_sentiment <- simpsons %>%
  unnest_tokens(word, episode_title) %>% inner_join(get_sentiments("bing")) %>% 
  filter(guest_star %in% top_guest_stars) %>%   count(guest_star,sentiment,sort = TRUE) %>%  ggplot(aes(guest_star,y =n,fill = sentiment)) +
  geom_chicklet() +
  coord_flip() +
  theme_ipsum_rc(grid="X")
simpsons_sentiment




get_sentiments("nrc")
```



```{r}

simpsons_palette <- c(
  "#FC0209", # bart red
  "#fed90f", # simpsons yellow
  "#46732EFF", # Flanders green 
  "#0363C3", # marge blue
  "#d1b271", # lightbrownbeige
  "#7A491E", # beige 
  "#000000",  # black
  "#424f46"  # greyish-blue
)
simpsons_merged %>% 
  filter(guest_star %in% most_frequent_guest_stars,season.x != 'Movie',!is.na(imdb_rating)) %>% 
  
  mutate(season.x  = parse_number(season.x )) %>% 
  
  ggplot(aes(season.x, y = guest_star)) +
  geom_tile(aes(fill =us_viewers_in_millions ,size = imdb_rating )) +
  #scale_fill_manual(values = rev(simpsons_palette)) +
  #?scale_fill_gradient2( low =  "#0363C3", high = "#FC0209", name = "" ) +
  scale_fill_gradientn(colors = rev(simpsons_palette)) +
  theme_minimal_grid()
#theme_ipsum_pub(font_family = "Akbar") 
#theme_minimal_grid(font_family = "Akbar")



```



simpsons_merged %>% 
  separate_rows(role, sep = ";\\s+") %>% 
  mutate(episode_id = id ,
         raw_character_text = role) %>% 
  left_join(simpson_lines,by=c('episode_id','raw_character_text')) %>% 
  filter(guest_star %in% top_guest_stars,
         !is.na(word_count)) %>%
  unnest_tokens(word, normalized_text) %>% inner_join(get_sentiments("afinn")) %>% 
  group_by(season.x,guest_star) %>% 
  summarise(sentiment_score = mean(value)) %>% 
  
  
  
  ggplot(aes(season.x,y =guest_star)) +
  geom_tile(aes(fill =sentiment_score )) +
  scale_fill_gradientn(colors = rev(simpsons_palette))



test

names(simpson_lines)

%>% 
  filter(guest_star %in% most_frequent_guest_stars,season.x != 'Movie',!is.na(imdb_rating))







```{r fig.height = 5, fig.width = 10,dpi=1000}
simpsons_merged %>% 
  group_by(guest_star) %>% 
  summarize(avg_rating = mean(imdb_rating),
            avg_viewers = mean(us_viewers_in_millions),
            n_appearances = n()) %>% 
  arrange(avg_rating) %>% 
  slice(1:20) %>% 
  mutate(guest_star = fct_reorder(guest_star,avg_viewers,.desc = TRUE)) %>% 
  ggplot(aes(x =guest_star,y = avg_viewers )) +
  geom_col(fill = 'white') +
  #scale_y_continuous(limits = seq(4,8) ) +
  coord_flip() 


```
```{r fig.height = 11, fig.width = 15,dpi=1000}
extrafont::loadfonts(device = "win")
extrafont::font_import()
import_simpsons()  

simpsons_palette <- c(
  "#FC0209", # bart red
  "#FFCC00",
  "#fed90f", # simpsons yellow
  "#46732EFF" # Flanders green 
  #"#0363C3", # marge blue
  #"#d1b271", # lightbrownbeige
  #"#7A491E" # beige 
  #"#000000",  # black
  #"#424f46"  # greyish-blue
)
simpsons_merged %>% 
  filter(guest_star %in% most_frequent_guest_stars,season.x != 'Movie',!is.na(imdb_rating)) %>% 
  
  mutate(season.x  = parse_number(season.x )) %>% 
  
  group_by(season.x,guest_star) %>% 
  summarize(avg_rating = mean(imdb_rating),
            avg_viewers = mean(us_viewers_in_millions),
            n_appearances = n()) %>% 
  mutate(n_viewers = cut(avg_viewers,breaks  = c(0,10,20,30,40,50), labels = c(2,4,6,8,9)),
         ratings = cut(avg_rating, breaks = c(5,6,7,8,9,10), labels = c(5,6,7,8,9))) %>% 
  filter(!is.na(n_viewers)) %>% 
  ggplot(aes(season.x, y = guest_star,fill =ratings )) +
  geom_point(aes(size = n_viewers),shape = 23) +
  
  #geom_point(shape = 23,size =5) +
  #scale_fill_manual(values = rev(simpsons_palette)) +
  #scale_color_gradientn(colors = rev(simpsons_palette)) +
  #scale_fill_gradientn(colors = rev(simpsons_palette)) +
  scale_x_discrete(position = "bottom", limits = seq(1,28,10)) +
  labs(x = NULL, y = NULL,
       title = "The Most Frequent Simpsons Guest Stars by Season",
       caption = "source: Wikipedia
         Visualization @Frau_Dr_Barber"
  ) +
  #scale_size(range = c(3, 8)) +
  #scale_fill_simpsons(5, type = "continuous", reverse = TRUE)
  
  #scale_fill_simpsons()
  scale_fill_manual(values = simpsons_palette) +
  theme_simpsons()+
  theme_minimal_grid(font_family = "Akbar") +
  theme(panel.grid.major = element_blank(),
  )
```


```{r}
df_cpu %>% 
  filter(!is.na(transistor_count)) %>% 
  group_by(date_of_introduction,designer ) %>% 
  summarise(max_tr_count = max(transistor_count )) %>% 
  ungroup() 
```



df_cpu_2 %>% 
  mutate(intel = ifelse(designer != 'Intel','Intel','')) %>% 
  
  ggplot(aes(date_of_introduction,transistor_count,color =intel )) +
  geom_point(size = 4) +
  #geom_text(aes(label = intel))+
  scale_y_log10() +
  theme_cowplot() +
  theme_ipsum_tw()




```{r}
companies <- read_csv('D:/free-7-million-company-dataset/companies_sorted.csv')

head(companies)
```
```{r}
d<-df_cpu_2 %>% 
  mutate(name = str_to_lower(designer)) %>% 
  inner_join(companies,by = 'name')
```




df_cpu %>% 
  filter(!is.na(transistor_count)) %>% 
  mutate(log_transistor_count = log2(transistor_count))  %>% 
  ggplot() +
  geom_boxplot(aes(x=date_of_introduction, y=transistor_count,group = date_of_introduction),fill ='gray')  +
  geom_point(data =df_cpu, aes(x=date_of_introduction, y=transistor_count,color =designer)) +
  scale_y_log10(breaks = c(1, 10^3, 10^6, 10^9, 10^12),
                labels = scales::comma) +
  #geom_point(data =df_cpu, aes(x=date_of_introduction, y=transistor_count))+
  #geom_jitter(aes(color = date_of_introduction)) 
  
  geom_line(data = df_opt,aes(date_of_introduction, y = opt),color = 'indianred') +
  scale_y_log10(breaks = c(1, 10^3, 10^6, 10^9, 10^12),
                labels = scales::comma) +
  theme_ipsum_rc() 


#ggplot(NULL) +
# geom_boxplot(data = df_cpu, aes(x=date_of_introduction, y=log_transistor_count,group = #date_of_introduction))  +
#   geom_point(data = df_opt,aes(date_of_introduction, y = log_transistor_count))



#library(data.table)
#df_cpu_2<-data.table(df_cpu)
#df_cpu_2<-df_cpu_2[ , .SD[which.max(transistor_count)], by = year_cum]


df_opt<-df_opt %>% 
  mutate(log_transistor_count = log2(opt),
         date_of_introduction = year_cum   )


df_cpu_modified <-df_cpu %>% 
  mutate(year_cum = date_of_introduction %/% 2 * 2,
         opt =  2250 * 2^(year_cum / 2 - 1970 / 2),
         pioneers = ifelse(transistor_count>=opt,designer,''),
         pioneers_bool = ifelse(transistor_count>=opt,TRUE,FALSE) ,
         
         #text_position_y = transistor_count/1000 , 
         text_position_y = case_when(succ == 'Microsoft/AMD' ~ 12000000000,
                                     
                                     TRUE ~ transistor_count + transistor_count/5 ),
         text_position_x= case_when(succ == 'Microsoft/AMD' ~ year_cum - 0.5,
                                    succ == 'Oracle' ~ year_cum + 0.2,
                                    TRUE ~ year_cum - 0.81) )

df_cpu_pioneers df_cpu_modified


filter(!is.na(transistor_count)) %>% 
  mutate(log_transistor_count = log2(transistor_count))  %>% 
  ggplot() +
  # geom_boxplot(aes(x=year_cum, y=transistor_count,group = year_cum),fill ='gray')  +
  geom_point( aes(x=year_cum, y=transistor_count,color =pioneers  ),alpha = 0.5) +
  scale_y_log10(breaks = c(1, 10^3, 10^6, 10^9, 10^12),
                labels = scales::comma) +
  #geom_jitter(aes(x=year_cum, y=transistor_count,color =succ_2 ),size = 1,alpha = 0.5,  width = 0.2) +
  geom_text(aes(label = succ,x = text_position_x  ,y=text_position_y  ),size = 2,family = "Montserrat") +
  geom_line(data = df_opt,aes(year_cum, y = opt),color = 'indianred') +
  scale_y_log10(breaks = c(1, 10^3, 10^6, 10^9, 10^12),
                labels = scales::comma) +
  theme_tufte() +
  labs(x = NULL, y = NULL,
       title = "MOORE'S LAW", 
       subtitle = "*Moore's law* is the observation that the **number of transistors** in\n a dense integrated circuit **doubles about every two years**.<br>The observation is named after *Gordon E. Moore*, the co-founder of Fairchild Semiconductor and CEO of Intel.<br>") +
  theme(axis.text = element_text(family = "Montserrat",
                                 size = 10),
        axis.title.y = element_text(family = "Montserrat", 
                                    face = "plain",
                                    size = 13,
                                    color = "grey85"),
        strip.text = element_blank(),
        strip.background = element_blank(),
        panel.spacing.x = unit(12, "pt"),
        line = element_blank()) +
  labs(x = NULL, y = "Number of Transistors")


```{r}
office_person_ent_graph_table <- office_person_ent %>%
  filter(char_name != 'None', character %in% list_characters
  ) %>%
  # dplyr::rowwise() %>% 
  #mutate(char_name      =  as.factor(extract2(char_name,1))) %>% 
  
  mutate(from = character  , to = char_name        ) %>%
  group_by(from, to) %>%
  summarize(n_mentions = n()) %>%
  ungroup() %>%
  as_tbl_graph(directed = TRUE) %>%
  activate(nodes) %>% 
  mutate(
    bet_cent = centrality_betweenness(),
    deg_cent = centrality_degree(),
    
  )
```

```{r warning=FALSE,message=FALSE,fig.height=25,fig.width=25}
ggraph(office_person_ent_graph_table, layout = 'auto', ) +
  geom_edge_link(aes(
    edge_width = n_mentions,
    colour = n_mentions,
    fill = n_mentions
  ),
  arrow = arrow(length = unit(3.5, 'mm')), 
  start_cap = circle(3.5, 'mm'),
  end_cap = circle(3.3, 'mm')) +
  geom_node_point(aes( size = deg_cent)) +
  geom_node_label(aes(label = name),
                  repel = TRUE,
                  size = 8) +
  #scale_fill_manual(values = custom_palette) +
  
  theme_graph(foreground = 'steelblue', fg_text_colour = 'white') +
  labs(title = 'Who Mentioned Whom ') +
  #scale_edge_colour_manual(values = edge_cols) +
  scale_edge_colour_gradient2() +
  scale_edge_fill_gradient2() +
  theme_graph(base_family = 'Montserrat', ) +
  theme(
    plot.title = element_text(
      family = 'Montserrat',
      face = "bold",
      size = 50,
      margin = ggplot2::margin(0, 0, 20, 0),
      hjust = 0.5
    )
  )
```
```{r warning=FALSE,message=FALSE,fig.height=25,fig.width=25}
full_layout <- create_layout(graph = office_person_ent_graph_table, layout = "linear", circular = T)


ggraph(full_layout ) +
  geom_edge_arc(aes(
    edge_width = n_mentions,
    colour = n_mentions,
    #fill = n_mentions
  ),
  arrow = arrow(length = unit(3.5, 'mm')), 
  start_cap = circle(3.5, 'mm'),
  end_cap = circle(3.3, 'mm')) +
  geom_node_point(aes( size = deg_cent,color = bet_cent)) +
  geom_node_label(aes(label = name),
                  #  angle = ifelse(atan(-(x/y))*(180/pi) < 0,
                  #                 90 + atan(-(x/y))*(180/pi),
                  #                 270 + atan(-x/y)*(180/pi)),
                  #  hjust = ifelse(x > 0, 0 ,1),
                  repel = TRUE,
                  size = 8) +
  #scale_fill_manual(values = custom_palette) +
  scale_fill_continuous() + 
  theme_graph(foreground = 'steelblue', fg_text_colour = 'white') +
  labs(title = 'Who Mentioned Whom ') +
  #scale_edge_colour_manual(values = edge_cols) +
  scale_edge_colour_gradient2() +
  scale_edge_fill_gradient2() +
  theme_graph(base_family = 'Montserrat', ) +
  theme(
    plot.title = element_text(
      family = 'Montserrat',
      face = "bold",
      size = 50,
      margin = ggplot2::margin(0, 0, 20, 0),
      hjust = 0.5
    )
  )
```


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
  
  
  passwords_letters  %>%
  mutate(from = let1, to = let2) %>%
  rbind(passwords_letters  %>%
          mutate(from = let2, to = let3)) %>%
  rbind(passwords_letters  %>%
          mutate(from = let3, to = let4)) %>%
  rbind(passwords_letters  %>%
          mutate(from = let4, to = let5)) %>%
  rbind(passwords_letters  %>%
          mutate(from = let5, to = let6)) %>%
  rbind(passwords_letters  %>%
          mutate(from = let6, to = let7)) %>%
  rbind(passwords_letters  %>%
          mutate(from = let7, to = let8)) %>%
  rbind(passwords_letters  %>%
          mutate(from = let8, to = let9)) %>%
  group_by(from, to) %>%
  summarize(n_ = n()/100,
            avg_strength = mean(strength)) %>%
  select(from, to, n_, avg_strength) %>%
  graph_from_data_frame() %>%
  create_layout(layout = "circle") %>%
  ggraph() +
  #geom_edge_fan() +
  geom_edge_link(aes(alpha = n_)) + 
  #geom_edge_arc()+
  geom_node_point(aes(color = name %in% letters)) +
  geom_node_text(aes(label = name), repel = TRUE, size = 10) +
  #geom_edge_loop(aes(alpha = n_),direction = 180,span =90) +
  theme_void()




#count(letters,let,sort = TRUE)
group_by(letters,let) %>% 
  summarize(n_ = n(), 
            lettt = let[which.max(n_)])%>% 
  arrange(desc(n_),desc(letters),desc(let),)


passwords_letters  %>% 
  mutate(from = let2,to = let3) %>% 
  group_by(from,to) %>% 
  summarize(n = n(),
            avg_strength = mean(strength)) %>% 
  select(from,to,n,avg_strength) %>% 
  graph_from_data_frame() %>% 
  #mutate(is_number = if_else(name %in% letters, 0,1)) %>%     
  create_layout( layout = "star") %>% 
  
  ggraph() +
  #geom_edge_link() + 
  geom_edge_fan()+
  geom_node_point(aes(color = name %in% letters )) +
  geom_node_text(aes(label = name),repel = TRUE,size = 5) +
  geom_edge_loop()


g1 <- passwords_letters  %>% 
  mutate(from = let1,to = let2) %>% 
  select(from,to) %>% 
  graph_from_data_frame() %>% 
  create_layout( layout = "fr") %>% 
  ggraph() +
  geom_edge_link() + 
  geom_node_point() +
  geom_node_text(aes(label = name),repel = TRUE,size = 5) 









embedding <- as_tibble(spotify_songs_umap_normed$layout)
colnames(embedding) <- c('element_1','element_2')
embedding$track_name <-tmp$sd 
embedding$playlist_genre <-spotify_songs$playlist_genre 
embedding$playlist_subgenre <- spotify_songs$playlist_subgenre
embedding$track_artist <- spotify_songs$track_artist
embedding$track_popularity <- spotify_songs$track_popularity
embedding <- embedding %>% 
  
  
  
  clust <- kmeans(normed_input, 5)
library(widyr)
library(janeaustenr)
library(dplyr)
library(tidytext)
library(text2vec)
library(ggraph)
library(tidygraph)
library(igraph)

# Comparing Jane Austen novels
austen_words <- austen_books() %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words, by = "word") %>%
  count(book, word) %>%
  ungroup()

# closest books to each other
closest <- austen_words %>%
  pairwise_similarity(book, word, n) %>%
  arrange(desc(similarity))

closest

closest %>%
  filter(item1 == "Emma")

simmm <- normed_input %>%  
  slice(1:100) %>% 
  as.matrix() %>% 
  sim2
simmm 0

g <- graph_from_adjacency_matrix(simmm, mode = "undirected", weighted = TRUE,
                                 diag = FALSE)
simmm <- sim2(as.matrix(normed_input)
              
              
              as.data.frame(simmm) %>% dim()
              spotify_songs %>% 
                pairwise_similarity(track_name,liveness,)
              g 
              lay = create_layout(g, layout = "fr")
              
              # plot with ggraph
              ggraph(lay) + 
                geom_edge_link(aes(edge_width = weight )) + 
                geom_node_point() +
                geom_edge_density()+
                #geom_node_text(aes(label = name),repel = TRUE,size = 5) +
                theme_graph() 
              
              
              
              
              
              embedding <- spotify_songs_umap_normed$layout %>%
                as_tibble() %>%
                dplyr::rename(element_1 = V1, element_2 = V2) %>%
                mutate(
                  track_name = spotify_songs$shorter_names ,
                  playlist_genre =  spotify_songs$playlist_genre,
                  playlist_subgenre = spotify_songs$playlist_subgenre,
                  track_artist = spotify_songs$track_artist,
                  track_popularity = spotify_songs$track_popularity ,
                  selected_artist = if_else(
                    track_artist %in% selected_artists,
                    as.character(track_artist),
                    ""
                  ),
                  point_size_selected_artist = if_else(track_artist %in% selected_artists,
                                                       1,
                                                       0.7),
                  
                  
                  track_name_selected_artist = if_else(track_artist %in% selected_artists,
                                                       track_name,
                                                       NULL),
                  genre_selected_artist = if_else(track_artist %in% selected_artists,
                                                  playlist_genre,
                                                  NULL)) %>%
                distinct(track_name, .keep_all = TRUE)
              write_csv(embedding,'embedding.csv')
              embedding %>%
                slice(1000:2999) %>% 
                ggplot(aes(x = element_1, y = element_2 , color = selected_artist ,size = point_size_selected_artist,
                           shape = playlist_genre )) +
                geom_point(alpha = 0.9) +
                gghighlight(selected_artist != "",unhighlighted_params = list(alpha = 0.2, color = 'gray')) +
                scale_color_ipsum() +
                #scale_color_gradient2_tableau() +
                #scale_shape_few() +
                geom_text_repel(aes(label = track_name_selected_artist),size = 3, family = 'Montserrat',
                                point.padding = 1.2,
                                box.padding = .3,
                                force = 1,
                                min.segment.length = 0) +
                theme_void() +
                theme_ipsum_tw() +
                theme_modern_rc() + 
                theme(legend.position = 'top',
                      strip.background = element_blank(),
                      axis.ticks.x = element_blank() , 
                      axis.ticks.y = element_blank(),
                      panel.grid.major.x =element_blank(),
                      panel.grid.major.y =element_blank(),
                      panel.grid.minor =element_blank(),
                      axis.text.x.bottom  = element_blank(),
                      axis.text.y.left = element_blank()) +
                guides( size = FALSE,
                        color = guide_legend(override.aes = list(alpha = 0.9,size = 5)))
              
              
              
              
              
              
              
              
              
              
              
              
              
              
              
              
              
              
              
              
              
              
              
              
              
              
              ```{r fig.height=31, fig.width=26, message=FALSE, warning=FALSE}
              
              
              
              theme_set(
                theme_minimal() +
                  theme(
                    panel.spacing = unit(7, "lines"),
                    
                    axis.title = element_blank(),
                    axis.text.y = element_blank(),
                    axis.text.x = element_text(
                      family =  'Montserrat',
                      size = 20,
                      color = 'grey10',
                      margin = ggplot2::margin(b = 35)
                    ),
                    text = element_text(family =  'Montserrat'),
                    strip.text = element_text(family =  'Montserrat', size = 25),
                    strip.text.x = element_text(margin = ggplot2::margin(1, 1, 1, 1, "cm")),
                    legend.position = 'none'
                  )
              )
              
              
              labels <- c(
                'bill_length_mm' = "Bill length",
                'bill_depth_mm' = "Bill depth",
                'flipper_length_mm' = "Flipper length",
                'body_mass_g' = "Body mass"
                
              )
              
              
              
              
              make_plot  <- function(type, colors,title,subtitle){
                
                Adelie_plot <- stat_penguins_long %>%
                  filter(!is.na(sex), species == type) %>%
                  ggplot() +
                  geom_polygon(
                    aes(
                      x = measurements,
                      y = value,
                      group = stats,
                      fill = stats,
                      color= stats
                    ),
                    alpha = .84,
                    size = 2.5,
                    
                  ) +
                  scale_fill_manual(values = colors) +
                  scale_color_manual(values = colors) +
                  facet_grid( ~ sex) +
                  coord_polar(clip = 'off') +
                  scale_x_discrete(labels = labels) +
                  
                  ylim(0, 20) +
                  labs(title = title, subtitle = subtitle )+
                  theme(strip.background = element_rect(fill = '#FFB55B',color = '#FFB55B'),
                        plot.title = element_markdown(size = 30,margin = margin(t = 50,b = 50)))
                
              }
              
              
              
              Adelie_plot <- stat_penguins_long %>%
                filter(!is.na(sex), species == 'Adelie') %>%
                ggplot() +
                geom_polygon(
                  aes(
                    x = measurements,
                    y = value,
                    group = stats,
                    fill = stats,
                    color= stats
                  ),
                  alpha = .84,
                  size = 2.5,
                  
                ) +
                scale_fill_manual(values = c('min' = '#925000', 'avg' = '#FF8C00', 'max' = '#FFB55B')) +
                scale_color_manual(values = c('min' = '#925000', 'avg' = '#FF8C00', 'max' = '#FFB55B')) +
                facet_grid( ~ sex) +
                coord_polar(clip = 'off') +
                scale_x_discrete(labels = labels) +
                
                ylim(0, 20) +
                labs(title = '   ' )+
                
                theme(strip.background = element_rect(fill = '#FFB55B',color = '#FFB55B'),
                      plot.title = element_text(size = 50,margin = margin(t = 50,b = 50)))
              
              
              
              Gentoo_plot <- stat_penguins_long %>%
                filter(!is.na(sex), species == 'Gentoo') %>%
                ggplot() +
                geom_polygon(
                  aes(
                    x = measurements,
                    y = value,
                    group = stats,
                    fill = stats,
                    color = stats
                  ),
                  alpha = .84,
                  size = 2.5,
                ) +
                scale_fill_manual(values = c('min' = '#0D5C5C', 'avg' = '#159090', 'max' = '#A8F3F3')) +
                scale_color_manual(values = c('min' = '#0D5C5C', 'avg' = '#159090', 'max' = '#A8F3F3')) +
                facet_grid( ~ sex) +
                coord_polar(clip = 'off') +
                scale_x_discrete(labels = labels) +
                labs(subtitle = '    ' )+
                ylim(0, 20) +
                theme(strip.background = element_rect(fill = '#7DECEC',color =  '#7DECEC'),
                      plot.subtitle = element_text(size = 30,hjust = 0.5,margin = margin(t = 50,b = 50)))
              
              
              Chinstrap_plot <- stat_penguins_long %>%
                filter(!is.na(sex), species == 'Chinstrap') %>%
                ggplot() +
                geom_polygon(
                  aes(
                    x = measurements,
                    y = value,
                    group = stats,
                    fill = stats,
                    color = stats
                  ),
                  alpha = .84,
                  size = 1.5,
                ) +
                scale_fill_manual(values = c('min' = '#510A87', 'avg' = '#A034F0', 'max' = '#DCB6FA')) +
                scale_color_manual(values = c('min' = '#510A87', 'avg' = '#A034F0', 'max' = '#DCB6FA')) +
                facet_grid( ~ sex) +
                coord_polar(clip = 'off') +
                scale_x_discrete(labels = labels) +
                ylim(0, 20)  +
                labs(title = " A Comparison between size measurements of <span style = 'color:#A034F0;'> Chinstrap</span>, <span style = 'color:#159090;'> Gentoo</span> and <span style = 'color:#FF8C00;'> Adelie</span> Penguin Species",
                     subtitle = 'This plot illustrates how three penguin species in  Palmer Archipelago, Antarctica differ from each other based on their the average, the minimum and the maximum recorded size measurements') +
                theme(strip.background = element_rect(color = '#DCB6FA',fill = '#DCB6FA'),
                      plot.title = element_textbox_simple(size = 30,margin = margin(t = 50,b = 50)),
                      plot.subtitle = element_textbox_simple(size = 22,margin = margin(t = 10,b = 60))) 
              
              plot_grid(Chinstrap_plot,Gentoo_plot,Adelie_plot,nrow = 3,labels = c('Chinstrap','Gentoo','Adelie'),label_x = 1)
              
              ```