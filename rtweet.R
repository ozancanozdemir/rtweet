## Gerekli kütüphaneleri yükleme ve çaðýrma

#install.packages("rtweet")
#install.packages("httpuv")
#install.packages("tidyverse")
#install.packages("stringr")

library(rtweet)
library(httpuv)
library(tidyverse)
library(stringr)


auth <- rtweet_app()
#Tokeninizi girin
auth_setup_default()



### Twit arama

galatasaray<-search_tweets(q = "galatasaray",include_rts = FALSE, n = 50)
head(galatasaray)

users_data(galatasaray)

### Kullanýcý arama

users_galatasaray<-search_users(q="galatasaray",n = 50)
head(users_galatasaray,2)

### Dil Bazlý Arama

search_tweets("lang:fr" ,n = 5)
search_tweets("lang:en" ,n = 5)

### Lokasyon bazlý arama

ankara_galatasaray<- search_tweets("galatasaray", geocode = lookup_coords("ankara"), n = 5)
ankara_galatasaray

### Bir kullanýcý hesabýna ait twitleri çekme

galatasaray_twit<-get_timeline(c("GalatasaraySK"), n = 50)
head(galatasaray_twit,2)

### Canlý Twit toplama

galatasaray_fatihterim<-stream_tweets("galatasaray,fatih terim",timeout = 60 * 2) #içinde galatasaray
#ve fatih terim kelimeleri geçen ve son 2 dakika içerisinde atýlan tüm twitler
galatasaray_fatihterim

galatasaray_fatihterim<-stream_tweets("galatasaray,fatih terim",timeout = 60 * 2,file_name = "galatasaray_fatihterim.json") #içinde galatasaray

json_to_df <- jsonlite::stream_in(file("galatasaray_fatihterim.json"), verbose = FALSE)

### Takip edilen hesaplarý bulma

takip_edilen <- get_friends("GalatasaraySK")
lookup_users(takip_edilen$ids)

### Takip eden hesaplarý bulma
gs <- lookup_users("GalatasaraySK")
takip_eden<- get_followers("GalatasaraySK",n = gs$followers_count)

### Trendleri elde etme

trend_ankara<- get_trends("ankara")
trend_ankara$trend


trend_turkiye<-get_trends("turkey")
trend_turkiye$trend

### Kiþisel hesabýmýzdan twit atmak

post_tweet("Bu twit R'da yazýlmýþtýr.")


### Bir hesabý takip etmek

post_follow("OzancanOzdemir")

##  Uygulamaa


galatasaray<-get_timeline(c("GalatasaraySK"), n = 500)


galatasaray_tidy<-galatasaray$full_text %>%
str_replace_all("https://t.co/[A-Za-z\\d]+|&amp;", "")%>%
str_replace_all("\n", "")%>%str_replace_all("\"", "")%>%
stringi::stri_replace_last_regex('#\\S+',"")%>%
stringi::stri_replace_last_regex('@\\S+',"")%>%
str_replace_all("https://t.co/[A-Za-z\\d]+|&amp;", "")%>%tibble()%>%
tidytext::unnest_tokens(word,".")%>%
  filter(!word %in% stop_words$word,
         str_detect(word, "[a-z]"))

galatasaray_tidy_frekans <- galatasaray_tidy %>% count(word,sort = T)%>%head(10)
galatasaray_tidy_frekans

library(ggthemes)
galatasaray_tidy_frekans$col<-ifelse(galatasaray_tidy_frekans$n>mean(galatasaray_tidy_frekans$n),"Ortalama üstü","Ortalama altý")
galatasaray_tidy_frekans$word<-factor(galatasaray_tidy_frekans$word,levels = galatasaray_tidy_frekans$word)
galatasaray_tidy_frekans%>%ggplot(aes(x=word,y=n,fill = col))+geom_bar(stat="identity")+scale_fill_manual(values = c("red","yellow"))+
  geom_text(aes(label= n),vjust=-0.25,size=4,col="black",fontface ="bold")+theme_fivethirtyeight()+
  labs(title ="@GalatasaraySK hesabýnýn\nson 500 twitinde en çok kullanýlan kelimeler",caption = "Twitter: @OzancanOzdemir")+
  xlab('Frekans')+ylab('Kelime')+theme(rect = element_rect(fill = "White",linetype = 0, colour = NA),axis.title= element_text(face="bold"),legend.title = element_blank())


