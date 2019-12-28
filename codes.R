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



