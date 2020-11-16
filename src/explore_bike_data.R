##Explore bike data to see if there is a relationship between weather and ridership

library(tidyverse)

#Load the data ----
df<-read_csv("data/daily_bike_data.csv")

df #got downloaded as an html file at first


##Exploration of data relationships----

##plot data of date as x axis and count of riders as y axis
p<-ggplot(data=df)+
  geom_line(aes(x=dteday,y=cnt))
p

#Relationship between ridership and temperature
ggplot()+
  geom_point(data=df,aes(x=temp,y=cnt))##can use different data set if you put the data= argument here instead of at the main ggplot argument

#can use this syntax if we are using the same data and metrics below
ggplot(data=df,aes(x=temp, y=cnt))+
  geom_point()+
  geom_smooth()

#What is weathersit?
summary (df$weathersit)
unique(df$weathersit)

df2<-df %>% 
  dplyr::mutate(
    weather_fac = factor(weathersit,
                         level=c(1,2,3,4),
                         labels= c("clear","cloudy","rainy","heavy rain"))
  )
df2 %>% dplyr::select(dteday,weathersit,weather_fac)

df2 %>%  
  dplyr::filter(weather_fac=="clear") %>% 
  ggplot(aes(x=temp,y=cnt))+
  geom_point()+
  geom_smooth()


##dplyr:: select, you can drop variables
df3<-df2 %>% 
  dplyr::select(-weathersit)

#It can also use characters lists
keepvars<- c("dteday","weather_fac","temp","cnt")
df4<-df2 %>% select(all_of(keepvars))

#other ways of filtering
weather_factors_we_like<-c("rainy","cloudy")
df2 %>% dplyr::filter(weather_fac =="rainy"| weather_fac=="cloudy")
df2 %>% dplyr::filter(weather_fac%in% weather_factors_we_like)
df2 %>% dplyr::filter(weather_fac!= "rainy")
df2 %>% dplyr::filter(!(weather_fac%in%weather_factors_we_like))

#dplyr::summarize
df2 %>% 
  dplyr::group_by(season,weather_fac) %>% 
  dplyr::summarize(
    cnt_mean=mean(cnt)
  )


##Transforming data format from long to wide or vice versa

#transform to create separate temp variables for each month
months<-c("January","February","March","April","May","June","July","August","September","October","November","December")

df_wide<-df2 %>% 
  dplyr::mutate(mnth=factor(mnth,levels=months,labels=months)) %>% 
  dplyr::rename(year=yr) %>% 
  dplyr::select(year,mnth,temp) %>% 
  dplyr::group_by(year,mnth) %>% 
  dplyr::summarize(temp_mean=mean(temp)) %>% 
  tidyr::pivot_wider(names_prefix="temp_",names_from=mnth,values_from=temp_mean) %>% 
  dplyr::rename_with(tolower)

df_wide<-df2 %>% 
  dplyr::select(mnth,temp,season) %>% 
  dplyr::group_by(season,mnth) %>% 
  dplyr::summarize(temp_mean=mean(temp)) %>% 
  dplyr::select(-season) %>% 
  tidyr::pivot_wider(names_prefix="temp_",names_from = mnth,values_from=temp)

df2 %>% 
  dplyr::select(mnth,temp,season) %>% 
  dplyr::group_by(season,mnth) %>% 
  dplyr::summarize(temp_mean=mean(temp)) %>% 
  dplyr::select(-season)

df2_a<-df2 %>% select(season,mnth,temp)
df2_b<-df2_a %>% 
  dplyr::group_by(season,mnth) %>% 
  dplyr::summarize(temp_mean=mean(temp)) %>% 
  dplyr::ungroup(season)

#pivoting longer
df_long<-df2 %>% 
  tidyr::pivot_longer(cols=c(temp,atemp,hum,windspeed),
                      values_to="value",names_to="variable")
##pivoting wider
df_wide2<-df_long %>% 
  tidyr::pivot_wider(names_prefix="v_",names_from=variable,values_from=value)

#Matt's code
df %>% 
  group_by(weekday) %>% 
  summarize(mean_temp=mean(temp)) %>% 
  pivot_wider(names_from=weekday,
              values_from=mean_temp)
##Facetting
ggplot(data=df2,aes(x=temp,y=cnt)) +
  geom_point(shape=21,color="orangered")+
  geom_smooth(method="lm",formula=y~poly(x,2),color="steelblue",se=FALSE)+
  facet_wrap(~weather_fac,scales="free_y") + #makes the y axis variable in case the scales are different
labs(x="Temperature",y="Ridership Count")+
  ggtitle("Relationship between Temperature and Ridership")
  theme_linedraw()+
  theme(strip.background=element_rect(fill=NA),
          strip.text=element_text(color="black"))
ggsave(plot=p,filename="temp_count_scatter.png")

##Plotting long 
ggplot(data=df2,aes(x=temp,y=cnt)) +
  geom_point(shape=21,color="orangered")+
  geom_smooth(method="lm",formula=y~poly(x,2),color="steelblue",se=FALSE)+
  facet_wrap(~weather_fac,scales="free_y") + #makes the y axis variable in case the scales are different
  labs(x="Temperature",y="Ridership Count")+
  ggtitle("Relationship between Temperature and Ridership")
theme_linedraw()+
  theme(strip.background=element_rect(fill=NA),
        strip.text=element_text(color="black"))
ggsave(plot=p,filename="temp_count_scatter.png")

ggplot(data=df_long,aes(x=value,y=cnt,color=variable))+
  geom_point()+
  geom_smooth(se=FALSE)+
  facet_wrap(~weather_fac,scales="free_y")
