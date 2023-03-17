# Использование информации о сетевом трафике для обнаружения подозрительной и вредоносной активности

## Описание

1. Описание полей датасета: timestamp,src,dst,port,bytes
2. IP адреса внутренней сети начинаются с 12-14
3. Все остальные IP адреса относятся к внешним узлам

RStudio

## Исходные данные

RStudio
traffic_security.csv

## Код программы

```{r}
install.packages("dplyr")
```

### Задание 1

Найти утечку данных из сети.
Один из хостов в нашей сети используется для пересылки этой информации – он пересылает гораздо больше информации на внешние ресурсы в Интернете, чем остальные компьютеры нашей сети. Определите его IP-адрес.

```{r}
library (arrow)
library (dplyr)
library("stringr") 

df <- read.csv('traffic_security.csv')
colnames(df) <- c('ts', 'src', 'dst', 'port', 'bytes')
#df - 105747729 obs. of 5 variables 

df$ts <- as.POSIXct(df$ts, origin = "1900-01-01", tz = "UTC")
df$bytes <- as.numeric(df$bytes)

toMatch <- c("12.","13.","14.")
df$src_info <- with (df, str_detect(df$src, paste(toMatch,collapse="|")))
df$dst_info <- with (df, str_detect(df$dst, paste(toMatch,collapse="|")))

#Поиск IP с наибольшим исходящим трафиком
df_2 <- df %>% 
  group_by(src) %>%
  summarize(bytes_sum = sum(bytes)) %>%
  arrange(desc(bytes_sum))

cat ("Answer: IP -", df_2$src[[1]])

## Answer: IP - 13.37.84.125
```

### Задание 2

Поиск утечки данных 2

```{r}
library (dplyr)

df$hour <- with (df,format(as.POSIXct(df$ts), format = "%H"))
df$minutes <- with (df,format(as.POSIXct(df$ts), format = "%M"))

#Поиск рабочих часов
activhours <- df %>% group_by(hour) %>% summarise(N = n())
select(arrange(activhours,desc(N)),N,hour)
#часы 22,19,18,21,17,20,16,23 (16-23) рабочие

answer2 <- df %>% 
  filter(src != "13.37.84.125") %>% #не адрес из 1 пункта
  filter(src_info == TRUE) %>% #исходящий трафик
  filter(dst_info == FALSE) %>%
  filter(hour >= 0) %>% #нерабочие часы
  filter(hour < 16) %>%
  group_by(src) %>%
  summarise(bytes = mean(bytes))
select(arrange(answer2,desc(bytes)) %>% top_n(1),src)  

## 12.55.77.96
```
