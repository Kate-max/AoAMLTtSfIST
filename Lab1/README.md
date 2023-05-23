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

#Чтение csv файла + описываем поля датасета
df <- read.csv('traffic_security.csv')
colnames(df) <- c('ts', 'src', 'dst', 'port', 'bytes')
#df - 105747729 obs. of 5 variables 


#Столбец timestamp приводим к нормальному виду
df$ts <- as.POSIXct(df$ts, origin = "1900-01-01", tz = "UTC")
df$bytes <- as.numeric(df$bytes)

#Оставим только IP адреса внутренней сети (начинаются с 12-14)
toMatch <- c("12.","13.","14.")
df$src_info <- with (df, str_detect(df$src, paste(toMatch,collapse="|")))
df$dst_info <- with (df, str_detect(df$dst, paste(toMatch,collapse="|")))

#Поиск IP с наибольшим исходящим трафиком
df_2 <- df %>% 
  group_by(src) %>%
  summarize(bytes_sum = sum(bytes)) %>%
  arrange(desc(bytes_sum))
cat ("Answer: IP -", df_2$src[[1]])

## Answer1: IP - 13.37.84.125
```
