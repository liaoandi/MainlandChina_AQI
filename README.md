---
title: "hw5"
author: "Andi Liao"
date: "February 10, 2019"
output:
  html_document:
    code_folding: hide
---

```{r global_options, include = FALSE}
knitr::opts_chunk$set(fig.width = 12, fig.height = 8, echo = TRUE, warning = FALSE, message = FALSE)
```


# Air Quality Index {.tabset .tabset-fade .tabset-pills}

## Text

### g1
The major source of air polluants in China is sulfur dioxide emitted from factories, because the energy structure is currently dominated by coal. The central government has released a series of policies to control the concentration and emissions of sulfur dioxide. Although AQI is not the best indicator of sulfur dioxide polluation, areas highly polluted by sulfur dioxide are usually suffer from other air pollutants. The paired violin plots illustrate that the air quality index decreases a bit from 2008 to 2011, but then increases greatly from 2011 to 2014. Using the average sulfur dioxide level as cutoff, provinces with higher sulfur dioxide levels have a higher increment in terms of AQI from 2011 to 2014.


### g2
The distribution of air pollution are heterogeneous. The dot plot shows the average air quality index grouped by provinces, and indicates that most southern provinces have lower average air quality index value, except for Tibet, which lies in the west plateau region. This finding can be explained by the fact that southern provinces have much denser population, more prosperous economy, warmer climate and higher coverage of vegetation. However, the air pollution is a nationalwide issue, and according to the different circumstances in each province, local governments are supposed to take different actions to help fight against air pollution.


### g3
To validate the hypothesis that the severity of air pollution is related with economy and environment, the bubble chart connecting popluation, forest coverage and air quality is presented. It seems that population doesn't affect air quality as much as we supposed, but the forest coverage rate does. Interestingly, provinces with worse air quality all have relatively less forest coverage, but their population vary from low to high. The natrual environment factor plays a key role in air quality here.


### g4
Now let's concentrate on the capital city, Beijing for the next graphs. Beijing lies in the northern part of China, and it is the most populated city in the country. It is also well-known for sandstrom and haze, but the local government has made efforts to reduce the air pollution for the past decades. The polar line chart clearly shows that the weekly average AQI of Beijing from 2014 to 2018 is improving, as the circle is getting smaller. However, there always some terrible days within a single month in the winter, when the local government often take actions to control pollution by published regulations.


### g5
Beijing government started a seris of campagin. The one of the largest scale is  called "changing from coal to gas" in the winter of 2017. The air quality was largerly improved after the campagin, compared to previous years. However, Beijing still has a long way to go to reduce the days with heavy pollution.


### g6
If we look at the average PM2.5 concentration of Beijing from 2014 to 2018, breaking down by month and hour, the winter has serious air pollution problem. During the winter, the air quality in the mornings and evenings are worse than afternoons, partically because people need more heat before the sun comes out and after the sun comes down. In the mornings of January, the problem is extremely eye-catching. This might due to the fact that January is the last working month before the Spring Festical, and it is usually the coldest month in Beijing. Actually, it is the horrible pollution in the winter of 2016 that pushes Beijing government to launch the "changing from coal to gas" in the winter of 2017.


### g7
Decomposing the air pollution gas source(i.e., exclusing PM2.5 and PM10), SO2 is not the major pollution source as most other places in China. This is the result of previous efforts of Beijing government of controlling SO2 emission, and this practive can be generalized to other northern cities facing similar air pollution issues.


### g8
Zooming in Beijing again, the distribution of air population is uneven in Beijing, but things are getting better. The southern part of Beijing experienced more serious air pollution than the other parts. But the uneven distribution are being migigated since 2017.


### g9
Viewing from a sptial angle, the dominant polluant in each dsitrict of Beijing is different. The southern part of Beijing have higher PM2.5 and PM10. The middle part have higher NO2 and S02. The northern part have higher O3.
    

## Grpah

```{r include = FALSE}
library(tidyverse)
library(dplyr) 
library(lubridate)
library(ggplot2)
library(RColorBrewer)
library(sf)

ggpreview = function(...) {
     fname = tempfile(fileext = ".pdf")
     ggsave(filename = fname, ...)
     system2("open", fname)
     invisible(NULL)
}

work_dir = "C:/Users/liaoa/Desktop/CAPP 30239/air"
setwd(work_dir)
```


```{r include = FALSE}
wrapper = function(x, ...) {paste(strwrap(x, ...), collapse = "\n")}

theme_new = function(base_size = 12,
                      base_family = "Georgia",
                      base_line_size = base_size / 170,
                      base_rect_size = base_size / 170){
  theme_minimal(base_size = base_size, 
                base_family = base_family,
                base_line_size = base_line_size) %+replace%
    theme(
      plot.title = element_text(
        color = "#525252", 
        size = 18,
        face = "bold",
        hjust = 0,
        margin = margin(0,10,5,5)),
      plot.subtitle = element_text(
        color = "#525252",
        size = 14,
        hjust = 0,
        margin = margin(5,10,20,5)),
      plot.caption = element_text(
        color = "#696969",
        size = 8,
        hjust = 1),
      plot.margin = unit(c(1,1,1,1), "cm"),
      axis.title = element_text(
        color = "#525252",
        size = 12),
      axis.text = element_text(
        color = "#525252",
        size = 10),
      axis.line = element_line(
        color = "#525252",
        linetype = "solid",
        size = 1),
      axis.ticks = element_line(
        size = 1), 
      legend.title = element_text(
        size = 10),
      legend.position = "top",
      panel.grid.major = element_line(
        color = "#696969",
        linetype = "dotted"),   
      panel.grid.minor = element_blank(),
      complete = TRUE
    )
}
```


```{r include = FALSE}
data = read_csv("environment_index_province.csv", skip = 1)
data = data[, -1]

data_long = gather(data, key = "metrics", value = "value", water_2011:aqi_2014)
data_long = cbind(data_long, year = 0)
data_long = cbind(data_long, measurement = 0)

for(i in 1:dim(data_long)[1]){
  tmp = unlist(strsplit(as.character(data_long[i, "metrics"]),"_"))
  data_long[i, 5] = tmp[2]
  data_long[i, 6] = tmp[1]
}

south_prov = c(1,0,1,1,1,1,1,1,1,1,1,1,0,0,0,0,1,1,1,0,0,0,0,0,0,0,1,0,0,0,0)
```


```{r out.width = "1000px", out.height = "800px"}
g1 <-
  data_long %>%
  select(-("metrics")) %>%
  filter(measurement == "aqi" | measurement == "sulfur") %>%
  spread(measurement, value) %>%
  mutate(category = cut(sulfur, breaks = c(0, 70, Inf),labels = c("low", "high"))) %>%
  group_by(category) %>%

  ggplot(aes(x = year, y = aqi, fill = category)) + 
  scale_fill_manual(values = c("#92AAC7", "#EC96A4")) +
  geom_violin(position = position_dodge(width = 0.6), width = 0.9, alpha = 0.5) +
  geom_boxplot(position = position_dodge(width = 0.6), width = 0.1, alpha = 1.0) +
  ylim(30, 150) +
  theme_new() + 
  labs(
    title = wrapper("Air quality of provinces with high sulfur dioxide levels become worse from 2011 to 2014.", width = 70),
    subtitle = wrapper("From 2008 to 2011, AQI drops a little, but from 2011 to 2014, AQI significantly increase. Split by sulfur dioxide levels, provinces with higher sulfur dioxide levels have worse air quality.", width = 110),
    caption = "Peking University Open Research Data Platform",
    x = "Year",
    y = "Air Quality Index",
    fill = "Sulfur Dioxide Level"
  )

g1
ggsave(filename = "g1.pdf", g1, width = 10, height = 8, device = cairo_pdf)
```


```{r out.width = "1000px", out.height = "800px"}
p <-
  data_long %>%
  select(-("metrics")) %>%
  filter(measurement == "aqi") %>%
  spread(measurement, value) %>%
  group_by(region) %>%
  summarise(aqi = mean(aqi)) %>% 
  rownames_to_column("province") %>%
  arrange(aqi) %>%
  mutate(region = factor(region, levels = .$region)) %>%
  mutate(is_south = south_prov)

g2 <-
  p %>%
  ggplot(aes(x = aqi, y = region)) +
  geom_segment(aes(x = 0, xend = aqi, y = region, yend = region, 
                   color = factor(is_south)), alpha = 0.9) +
  geom_point(aes(color = factor(is_south)), size = 1, alpha = 0.9) +
  scale_x_continuous(expand = c(0, 0), limits = c(0, 120)) +
  scale_y_discrete(limits = rev(levels(p$region))) +
  scale_color_manual(labels = c("No","Yes"),
                     values =  c("#92AAC7", "#EC96A4")) +
  theme_new() + 
  theme(panel.grid.major.y = element_blank()) +
  labs(
    title = wrapper("Provinces in the southern part of China have better air quality.", width = 70),
    subtitle = wrapper("Calculate the average AQI grouped by province of year 2008, 2011 and 2014, and the ordered result shows that most southern provinces have lower average AQI. The only exception is Tibet, the plateau area in the western part.", width = 90),
    caption = "Peking University Open Research Data Platform",
    x = "Air Quality Index", 
    y = "Province",
    color = "Southern Part") 

g2
ggsave(filename = "g2.pdf", g2, width = 10, height = 8, device = cairo_pdf)
```


```{r out.width = "1000px", out.height = "800px"}
p <-
  data_long %>%
  select(-("metrics")) %>%
  filter(measurement == "aqi" | measurement == "forest" | measurement == "population") %>%
  spread(measurement, value) %>%
  group_by(region) %>%
  summarise(aqi = mean(aqi), forest = mean(forest), population = mean(population)) 

g3 <-  
  p %>%
  ggplot(aes(x = forest, y = aqi, size = population)) + 
  geom_jitter(width = 0.8, height = 0.8, color = "#92AAC7", fill = "#92AAC7", alpha = 0.5) +
  scale_size_continuous(range = c(5, 20)) +
  ylim(25, 120) +
  geom_text(label = p$region, position = position_jitter(width = 2, height = 2), size = 3, color = "#525252") +
  theme_new() + 
  labs(
    title = wrapper("Provinces with higher forest coverage have better air quality.", width = 70),
    subtitle = wrapper("With the increase of forest coverage rate, the air quality becomes better despite the changes of population.", width = 100),
    caption = "Peking University Open Research Data Platform",
    x = "Forest Coverage",
    y = "Air Quality Index",
    size = "Population"
  ) 

g3
ggsave(filename = "g3.pdf", g3, width = 10, height = 8, device = cairo_pdf)
```


```{r include = FALSE, eval = FALSE}
work_dir = "C:/Users/liaoa/Desktop/CAPP 30239/air/beijing_20140101-20141231"
setwd(work_dir)

  files <- list.files() 
  old = data.frame()
  for (i in 1:length(files)) {
    if(file.info(files[i])$size > 200){
      new <- read_csv(files[i]) %>% 
        filter(!grepl("_24h$", type)) %>%
        ungroup() %>%
        mutate(avg = rowMeans(.[,4:38], na.rm = TRUE)) %>% 
        select(-c(4:38))
      new = data.frame(matrix(unlist(new), nrow = dim(new)[1], byrow = F), stringsAsFactors = FALSE)
      old = rbind(new, old)
    }
  }
  
  old = as.data.frame(matrix(unlist(old), nrow = dim(old)[1], byrow = F), stringsAsFactors = FALSE)
  old = as.data.frame(old, col.names = c("date", "hour", "metrics", "value"))
  
  colnames(old) = c("date", "hour", "metrics", "value")
  old$value = as.numeric(old$value)

write.csv(old, file = "2014_AQI.csv", row.names = FALSE)
```


```{r include = FALSE, eval = FALSE}
setwd("C:/Users/liaoa/Desktop/CAPP 30239/air/beijing_20140101-20141231")
write.csv(old, file = "2014_AQI.csv", row.names = FALSE)

setwd("C:/Users/liaoa/Desktop/CAPP 30239/air/beijing_20150101-20151231")
write.csv(old, file = "2015_AQI.csv", row.names = FALSE)

setwd("C:/Users/liaoa/Desktop/CAPP 30239/air/beijing_20160101-20161231")
write.csv(old, file = "2016_AQI.csv", row.names = FALSE)

setwd("C:/Users/liaoa/Desktop/CAPP 30239/air/beijing_20170101-20171231")
write.csv(old, file = "2017_AQI.csv", row.names = FALSE)

setwd("C:/Users/liaoa/Desktop/CAPP 30239/air/beijing_20180101-20181231")
write.csv(old, file = "2018_AQI.csv", row.names = FALSE)
```


```{r include = FALSE}
setwd("C:/Users/liaoa/Desktop/CAPP 30239/air")

AQI_2018 = read_csv("2018_AQI.csv")
AQI_2017 = read_csv("2017_AQI.csv")
AQI_2016 = read_csv("2016_AQI.csv")
AQI_2015 = read_csv("2015_AQI.csv")
AQI_2014 = read_csv("2014_AQI.csv")

p2018 <-
  AQI_2018 %>%
    filter(metrics == "AQI") %>% 
    group_by(week = week(as.Date(date, origin = "2018-01-01"))) %>% 
    summarise(AQI = mean(value, na.rm = TRUE)) %>%
    mutate(year = 2018)

p2017 <-
  AQI_2017 %>%
    filter(metrics == "AQI") %>% 
    group_by(week = week(as.Date(date, origin = "2017-01-01"))) %>% 
    summarise(AQI = mean(value, na.rm = TRUE)) %>%
    mutate(year = 2017)

p2016 <-
  AQI_2016 %>%
    filter(metrics == "AQI") %>%
    group_by(week = week(as.Date(date, origin = "2016-01-01"))) %>% 
    summarise(AQI = mean(value, na.rm = TRUE)) %>%
    mutate(year = 2016)

p2015 <-
  AQI_2015 %>%
    filter(metrics == "AQI") %>%
    group_by(week = week(as.Date(date, origin = "2015-01-01"))) %>% 
    summarise(AQI = mean(value, na.rm = TRUE)) %>%
    mutate(year = 2015)

p2014 <-
  AQI_2014 %>%
    filter(metrics == "AQI") %>%
    group_by(week = week(as.Date(date, origin = "2014-01-01"))) %>% 
    summarise(AQI = mean(value, na.rm = TRUE)) %>%
    mutate(year = 2014)

p = rbind(p2014, p2015, p2016, p2017, p2018)
```


```{r out.width = "1000px", out.height = "800px"}
g4 <-
  p %>% 
  ggplot(aes(x = week, y = AQI, color = factor(year))) +
  geom_line(group = 1) +
  facet_grid(cols = vars(year)) +
  coord_polar(start = 0, theta = "x") +
  scale_x_continuous(breaks = seq(1, 52, 13)) +
  theme_new() +
  theme(
    axis.line = element_blank(),
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank()) +
  labs(
    title = wrapper("The air quality in Beijing is gradually improving, but it is still terrible in winter.", width = 80),
    subtitle = wrapper("This is the weekly average air quality index value of Beijing from 2014 to 2018. The overall trend is that the circle is getting smaller, especailly after 2017.", width = 110),
    caption = "National Urban Air Quality in Real-Time Publishing Platform",
    x = "Week", 
    y = "Air Quality Index",
    color = "Year") 

g4
ggsave(filename = "g4.pdf", g4, width = 10, height = 8, device = cairo_pdf)
```


```{r include = FALSE}
q2018 <-
  AQI_2018 %>%
    filter(metrics == "PM2.5") %>%
    group_by(week = week(as.Date(date, origin = "2018-01-01"))) %>% 
    summarise(PM2.5 = mean(value, na.rm = TRUE)) %>%
    mutate(year = 2018)

q2017 <-
  AQI_2017 %>%
    filter(metrics == "PM2.5") %>%
    group_by(week = week(as.Date(date, origin = "2017-01-01"))) %>% 
    summarise(PM2.5 = mean(value, na.rm = TRUE)) %>%
    mutate(year = 2017)

q2016 <-
  AQI_2016 %>%
    filter(metrics == "PM2.5") %>%
    group_by(week = week(as.Date(date, origin = "2016-01-01"))) %>% 
    summarise(PM2.5 = mean(value, na.rm = TRUE)) %>%
    mutate(year = 2016)

q2015 <-
  AQI_2015 %>%
    filter(metrics == "PM2.5") %>%  
    group_by(week = week(as.Date(date, origin = "2015-01-01"))) %>% 
    summarise(PM2.5 = mean(value, na.rm = TRUE)) %>%
    mutate(year = 2015)

q2014 <-
  AQI_2014 %>%
    filter(metrics == "PM2.5") %>% 
    group_by(week = week(as.Date(date, origin = "2014-01-01"))) %>% 
    summarise(PM2.5 = mean(value, na.rm = TRUE)) %>%
    mutate(year = 2014)

q = rbind(q2014, q2015, q2016, q2017, q2018)
q["seq"] = row.names(q)
q$seq = as.numeric(q$seq)
```


```{r out.width = "1000px", out.height = "800px"}
g5 <-
  q %>%
  ggplot(aes(x = seq, y = PM2.5)) +
  geom_step(size = 0.5, color = "#92AAC7") + 
  ylim (20, 400) + 
  scale_x_continuous(breaks = c(1, 53, 106, 159, 211), labels = c("2014", "2015","2016", "2017","2018")) +
  geom_hline(yintercept = 100, size = 0.5, color = "#EC96A4") +
  annotate("text", x = 20, y = 160, alpha = 0.7, fill = "#929292", size = 4, label = "Air Pollution Prevention\n and Control Regulations") +
  annotate("text", x = 45, y = 25, alpha = 0.7, fill = "#929292", size = 4, label = "APEC") +
  annotate("text", x = 80, y = 25, alpha = 0.7, fill = "#929292", size = 4, label = "National Day\n Military Parade") +
  annotate("text", x = 150, y = 25, alpha = 0.7, fill = "#929292", size = 4, label = " Suspension\n Order") +
  annotate("text", x = 211, y = 120, alpha = 0.7, fill = "#929292", size = 4, label = "Blue Sky Defence War\n 2018 Program") +
  theme_new() +
  theme(
    panel.grid.major.x = element_blank()) +
  labs(
    title = wrapper("The concentration of PM 2.5 used to be above unhealthy threshold frequently, but it is getting better since 2017.", width = 70),
    subtitle = wrapper("This is the weekly average PM2.5 value of Beijing from 2014 to 2018. Though there are fluctations in terms of polluant concentration, the overall trend is that the air quality is being improved.", width = 100),
    caption = "National Urban Air Quality in Real-Time Publishing Platform",
    x = "Year", 
    y = "PM 2.5 Concentration") 

g5
ggsave(filename = "g5.pdf", g5, width = 10, height = 8, device = cairo_pdf)
```


```{r out.width = "1000px", out.height = "800px"}
p = rbind(AQI_2018, AQI_2017, AQI_2016, AQI_2015, AQI_2014)
p$date = ymd(p$date)

g6 <-
  p %>%
  filter(metrics == "PM2.5") %>%
  group_by(hour, bigmonth = month(date)) %>%
  summarise(PM2.5 = mean(value, na.rm = TRUE)) %>%
  ggplot(aes(x = as.factor(hour), y = as.factor(bigmonth), fill = PM2.5)) + 
  geom_tile() + 
  scale_fill_gradient(low = "white",high = "#92AAC7") +
  theme_new() +
  labs(
    title = wrapper("The concentration of PM 2.5 is highest in the mornings of January.", width = 70),
    subtitle = wrapper("This is the average PM2.5 concentration of Beijing from 2014 to 2018, breaking down by month and hour. The winter has worse air quality, especially in the mornings of January", width = 100),
    caption = "National Urban Air Quality in Real-Time Publishing Platform",
    x = "Hour", 
    y = "Month") 

g6
ggsave(filename = "g6.pdf", g6, width = 10, height = 8, device = cairo_pdf)
```


```{r include = FALSE, eval = FALSE}
work_dir = "C:/Users/liaoa/Desktop/CAPP 30239/air/2018_extra"
setwd(work_dir)

  files <- list.files() 
  old = data.frame()
  for (i in 1:length(files)) {
    if(file.info(files[i])$size > 200){
      new <- read_csv(files[i]) %>% 
        filter(!grepl("_24h$", type)) %>%
        ungroup() %>%
        mutate(avg = rowMeans(.[,4:38], na.rm = TRUE)) %>% 
        select(-c(4:38))
      new = data.frame(matrix(unlist(new), nrow = dim(new)[1], byrow = F), stringsAsFactors = FALSE)
      old = rbind(new, old)
    }
  }
  
  old = as.data.frame(matrix(unlist(old), nrow = dim(old)[1], byrow = F), stringsAsFactors = FALSE)
  old = as.data.frame(old, col.names = c("date", "hour", "metrics", "value"))
  
  colnames(old) = c("date", "hour", "metrics", "value")
  old$value = as.numeric(old$value)

write.csv(old, file = "2018_extra.csv", row.names = FALSE)
```


```{r include = FALSE, eval = FALSE}
setwd("C:/Users/liaoa/Desktop/CAPP 30239/air/2014_extra")
write.csv(old, file = "2014_extra.csv", row.names = FALSE)

setwd("C:/Users/liaoa/Desktop/CAPP 30239/air/2015_extra")
write.csv(old, file = "2015_extra.csv", row.names = FALSE)

setwd("C:/Users/liaoa/Desktop/CAPP 30239/air/2016_extra")
write.csv(old, file = "2016_extra.csv", row.names = FALSE)

setwd("C:/Users/liaoa/Desktop/CAPP 30239/air/2017_extra")
write.csv(old, file = "2017_extra.csv", row.names = FALSE)

setwd("C:/Users/liaoa/Desktop/CAPP 30239/air/2018_extra")
write.csv(old, file = "2018_extra.csv", row.names = FALSE)
```


```{r include = FALSE}
setwd("C:/Users/liaoa/Desktop/CAPP 30239/air")

extra_2018 = read_csv("2018_extra.csv")
extra_2017 = read_csv("2017_extra.csv")
extra_2016 = read_csv("2016_extra.csv")
extra_2015 = read_csv("2015_extra.csv")
extra_2014 = read_csv("2014_extra.csv")

e2018 <-
  extra_2018 %>%
    group_by(metrics, week = week(as.Date(date, origin = "2018-01-01"))) %>% 
    summarise(value = mean(value, na.rm = TRUE)) %>%
    mutate(year = 2018)

e2017 <-
  extra_2017 %>%
    group_by(metrics, week = week(as.Date(date, origin = "2017-01-01"))) %>% 
    summarise(value = mean(value, na.rm = TRUE)) %>%
    mutate(year = 2017)

e2016 <-
  extra_2016 %>%
    group_by(metrics, week = week(as.Date(date, origin = "2016-01-01"))) %>% 
    summarise(value = mean(value, na.rm = TRUE)) %>%
    mutate(year = 2016)

e2015 <-
  extra_2017 %>%
    group_by(metrics, week = week(as.Date(date, origin = "2015-01-01"))) %>% 
    summarise(value = mean(value, na.rm = TRUE)) %>%
    mutate(year = 2015)

e2014 <-
  extra_2014 %>%
    group_by(metrics, week = week(as.Date(date, origin = "2014-01-01"))) %>% 
    summarise(value = mean(value, na.rm = TRUE)) %>%
    mutate(year = 2014)

e = rbind(e2014, e2015, e2016, e2017, e2018)
e["seq"] = row.names(e)
e$seq = as.numeric(e$seq)
```


```{r out.width = "1000px", out.height = "800px"}
g7 <-
  e %>% 
  group_by(metrics, week) %>%
  summarize(value = mean(value, na.rm = TRUE)) %>%
  mutate(newvalue = ifelse(metrics == "CO" |metrics == "SO2", value * 5, value)) %>%
  ggplot(aes(x = week, y = newvalue)) +
  facet_grid(cols = vars(metrics)) +
  geom_bar(stat = "identity", fill = "#EC96A4", color = "#929292") +
  coord_polar(start = 0) +
  scale_x_continuous(breaks = seq(1, 52, 13)) +
  theme_new() +
  theme(
    axis.line = element_blank(),
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank()) +
  labs(
    title = wrapper("The major polluant source in Beijing from 2014 to 2018 is not S02.", width = 80),
    subtitle = wrapper("Although SO2 is considered as the most serious source of air pollution in China, it is not the case in Beijing.", width = 110),
    caption = "National Urban Air Quality in Real-Time Publishing Platform",
    x = "Time", 
    y = "Value",
    fill = "Pollutant") 

g7
ggsave(filename = "g7.pdf", g7, width = 10, height = 8, device = cairo_pdf)
```


```{r include = FALSE}
theme_map <- function(base_size = 12,
                      #base_family = "Bell MT",
                      base_family = "Georgia",
                      base_line_size = base_size / 170,
                      base_rect_size = base_size / 170){
  theme_minimal(base_size = base_size, 
                base_family = base_family,
                base_line_size = base_line_size) %+replace%
    theme(
      plot.title = element_text(
        color = "#525252", 
        size = 18,
        face = "bold",
        hjust = 0,
        margin = margin(0,10,5,5)),
      plot.subtitle = element_text(
        color = "#525252",
        size = 14,
        hjust = 0,
        margin = margin(5,10,20,5)),
      plot.caption = element_text(
        color = "#696969",
        size = 8,
        hjust = 1),
      plot.margin = unit(c(1,1,1,1), "cm"),
      axis.title = element_text(
        color = "#525252",
        size = 12),
      axis.text = element_text(
        color = "#525252",
        size = 10),
      axis.line = element_line(
        color = "#525252",
        linetype = "solid",
        size = 1),
      axis.ticks = element_line(
        size = 1), 
      legend.title = element_text(
        size = 10),
      legend.position = "top",
      panel.grid.major = element_line(
        color = "#696969",
        linetype = "dotted"),   
      panel.grid.minor = element_blank(),
      panel.border = element_blank(),
      complete = TRUE
    )
}
```


```{r include = FALSE, eval = FALSE}
work_dir = "C:/Users/liaoa/Desktop/CAPP 30239/air/beijing_20140101-20141231"
setwd(work_dir)

  files <- list.files() 
  old = data.frame()
  for (i in 1:length(files)) {
    if(file.info(files[i])$size > 200){
      new <- read_csv(files[i])
      names(new)[4:38] <- seq(1, 35, by = 1)
      new <- filter(new, type == "AQI") %>%
            summarise_if(.predicate = function(x) is.numeric(x), .funs = funs(mean(., na.rm = TRUE))) %>%
            gather("metrics", "value", -c("date":"hour")) %>%
            select(-hour)
      
      new = data.frame(matrix(unlist(new), nrow = dim(new)[1], byrow = F), stringsAsFactors = FALSE)
      old = rbind(new, old)
    }
  }
  
  old = as.data.frame(matrix(unlist(old), nrow = dim(old)[1], byrow = F), stringsAsFactors = FALSE)
  old = as.data.frame(old, col.names = c("date", "metrics", "value"))
  
  colnames(old) = c("date", "metrics", "value")
  old$value = as.numeric(old$value)

write.csv(old, file = "2014_place.csv", row.names = FALSE)
```


```{r include = FALSE, eval = FALSE}
setwd("C:/Users/liaoa/Desktop/CAPP 30239/air/beijing_20140101-20141231")
write.csv(old, file = "2014_AQI.csv", row.names = FALSE)

setwd("C:/Users/liaoa/Desktop/CAPP 30239/air/beijing_20150101-20151231")
write.csv(old, file = "2015_AQI.csv", row.names = FALSE)

setwd("C:/Users/liaoa/Desktop/CAPP 30239/air/beijing_20160101-20161231")
write.csv(old, file = "2016_AQI.csv", row.names = FALSE)

setwd("C:/Users/liaoa/Desktop/CAPP 30239/air/beijing_20170101-20171231")
write.csv(old, file = "2017_AQI.csv", row.names = FALSE)

setwd("C:/Users/liaoa/Desktop/CAPP 30239/air/beijing_20180101-20181231")
write.csv(old, file = "2018_AQI.csv", row.names = FALSE)
```


```{r include = FALSE}
setwd("C:/Users/liaoa/Desktop/CAPP 30239/air")

place_2018 = read_csv("2018_place.csv")
place_2017 = read_csv("2017_place.csv")
place_2016 = read_csv("2016_place.csv")
place_2015 = read_csv("2015_place.csv")
place_2014 = read_csv("2014_place.csv")

r2018 <-
  place_2018 %>%
    group_by(metrics) %>%
    summarise(AQI = mean(value, na.rm = TRUE)) %>%
    mutate(year = 2018)

r2017 <-
  place_2017 %>%
    group_by(metrics) %>%
    summarise(AQI = mean(value, na.rm = TRUE)) %>%
    mutate(year = 2017)

r2016 <-
  place_2016 %>%
    group_by(metrics) %>%
    summarise(AQI = mean(value, na.rm = TRUE)) %>%
    mutate(year = 2016)

r2015 <-
  place_2015 %>%
    group_by(metrics) %>%
    summarise(AQI = mean(value, na.rm = TRUE)) %>%
    mutate(year = 2015)

r2014 <-
  place_2014 %>%
    group_by(metrics) %>%
    summarise(AQI = mean(value, na.rm = TRUE)) %>%
    mutate(year = 2014)

r = rbind(r2014, r2015, r2016, r2017, r2018)
```

```{r include = FALSE}
#beijing_shp = readOGR(dsn = "C:/Users/liaoa/Desktop/CAPP #30239/air/bj_shp/bj_shp/?????????_region.shp")
beijing_shp = st_read("C:/Users/liaoa/Desktop/CAPP 30239/air/bj_shp/bj_shp/qxj_region.shp")

beijing_station = read_csv("beijing_station.csv")[1:35, 3:4] 
colnames(beijing_station) = c("lon", "lat")
beijing_station["district"] = c("dcq", "dcq", "xcq", "xcq", "cyq", "cyq", "hdq", "hdq", "hdq", "ftq", "ftq", "sjsq", "fsq", "dxq", "dxq", "tzq", "syq","cpq", "mtgq", "pgq", "hrq", "myx", "yqx", "cpq", "yqx", "myx", "pgq", "tzq", "dxq", "fsq", "dcq", "dcq", "xcq", "ftq", "cyq")
beijing_station["seq"] = rownames_to_column(beijing_station)
beijing_station["seq"] = lapply(beijing_station["seq"], as.numeric)
  
merged_data = left_join(beijing_station, r, by = c("seq" = "metrics"))
merged_data = left_join(merged_data, beijing_shp, by = c("district"="PYNAME"))
```


```{r out.width = "1000px", out.height = "800px"}
g8<-
  merged_data %>%
  ggplot() + 
  geom_sf(aes(fill = AQI), alpha = 0.8, color = "#929292") +
  geom_point(aes(x = lon, y = lat, color = "#EC96A4"), fill = "#EC96A4", alpha = 0.5) +
  scale_fill_gradient(low = "white", high = "#92AAC7") +
  scale_colour_manual("Station", values = "#EC96A4", labels = NULL) +
  facet_grid(cols = vars(year)) +
  theme_map() +
  labs(
    title = wrapper("The distribution of air population is uneven in Beijing,but things are getting better.", width = 100),
    subtitle = wrapper("The southern part of Beijing experienced more serious air pollution than the other parts. But the uneven distribution are being migigated since 2017.", width = 100),
    caption = "National Urban Air Quality in Real-Time Publishing Platform",
    x = "Longtitude", 
    y = "Latitude",
    fill = "AQI") +
  guides(fill = guide_legend(order = 1), color = guide_legend(order = 2))

g8
ggsave(filename = "g8.pdf", g8, width = 15, height = 5, device = cairo_pdf)
```

```{r, include = FALSE, eval = FALSE}
library(spData)
# for the purpose of zooming
world_asia = world[world$continent == "Asia", ]
china = world[world$name_long == "China", ]
plot(st_geometry(china), expandBB = c(0, 0.2, 0.1, 1), col = "gray", lwd = 3)
plot(world_asia[0], add = TRUE)
```

```{r include = FALSE, eval = FALSE}
library(ggmap)
library(mapdata)
ggplot(map("china", plot = F), aes(long, lat, group = group, fill = region)) +
  geom_path(show.legend = F) +
  ggtitle("Map of China") +
  theme_map()
```


```{r include = FALSE, eval = FALSE}
work_dir = "C:/Users/liaoa/Desktop/CAPP 30239/air/2018_extra"
setwd(work_dir)

  files <- list.files() 
  old = data.frame()
  for (i in 1:length(files)) {
    if(file.info(files[i])$size > 200){
      new <- read_csv(files[i])
      names(new)[4:38] <- seq(1, 35, by = 1)
      new <- 
        filter(new, !grepl("_24h$", type)) %>%
        group_by(type) %>%
        summarise_if(.predicate = function(x) is.numeric(x), .funs = funs(mean(., na.rm = TRUE))) %>%
        select(-hour) %>%
        gather("metrics", "value", -c("date":"type")) 
      
      new = data.frame(matrix(unlist(new), nrow = dim(new)[1], byrow = F), stringsAsFactors = FALSE)
      old = rbind(new, old)
    }
  }
  
  old = as.data.frame(matrix(unlist(old), nrow = dim(old)[1], byrow = F), stringsAsFactors = FALSE)
  old = as.data.frame(old, col.names = c("type", "date", "metrics", "value"))
  
  colnames(old) = c("type", "date", "metrics", "value")
  old$value = as.numeric(old$value)

write.csv(old, file = "2018_polluant_part2.csv", row.names = FALSE)
```


```{r include = FALSE, eval = FALSE}
work_dir = "C:/Users/liaoa/Desktop/CAPP 30239/air/beijing_20180101-20181231"
write.csv(old, file = "2018_polluant_part1.csv", row.names = FALSE)

work_dir = "C:/Users/liaoa/Desktop/CAPP 30239/air/2018_extra"
write.csv(old, file = "2018_polluant_part2.csv", row.names = FALSE)
```


```{r include = FALSE}
setwd("C:/Users/liaoa/Desktop/CAPP 30239/air")

polluant1_2018 = read_csv("2018_polluant_part1.csv")
polluant2_2018 = read_csv("2018_polluant_part2.csv")

t1 <-
  polluant1_2018 %>%
    group_by(metrics, type) %>%
    summarise(value = mean(value, na.rm = TRUE))

t2 <-
  polluant2_2018 %>%
    group_by(metrics, type) %>%
    summarise(value = mean(value, na.rm = TRUE))

t = rbind(t1, t2) 
t = filter(t, type != "AQI")

merged_data = left_join(beijing_station, t, by = c("seq" = "metrics"))
merged_data = left_join(merged_data, beijing_shp, by = c("district"="PYNAME"))
merged_data <- filter(merged_data, type != "NA") 

tmp <- group_by(merged_data,type) 
merged_data <- mutate(tmp, value = (value-mean(value))/sd(value))
```


```{r out.width = "1000px", out.height = "800px"}
g9<-
  merged_data %>%
  ggplot() + 
  geom_sf(aes(fill = value), alpha = 0.8, color = "#929292") +
  geom_point(aes(x = lon, y = lat, color = "#EC96A4"), fill = "#EC96A4", alpha = 0.5) +
  scale_fill_gradient(low = "white", high = "#92AAC7") +
  scale_colour_manual("Station", values = "#EC96A4", labels = NULL) +
  facet_grid(cols = vars(type)) +
  theme_map() +
  labs(
    title = wrapper("The dominant polluant in each dsitrict of Beijing is different.", width = 100),
    subtitle = wrapper("The southern part of Beijing have higher PM2.5 and PM10. The middle part have higher NO2 and S02. The northern part have higher O3.", width = 100),
    caption = "National Urban Air Quality in Real-Time Publishing Platform",
    x = "Longtitude", 
    y = "Latitude",
    fill = "Standardized Concentration") +
  guides(fill = guide_legend(order = 1), color = guide_legend(order = 2))

g9
ggsave(filename = "g9.pdf", g9, width = 15, height = 5, device = cairo_pdf)
```

