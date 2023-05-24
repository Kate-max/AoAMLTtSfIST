# Использование информации о сетевом трафике для обнаружения подозрительной и вредоносной активности
## AoAMLTtSfIST

## Описание
1. Описание полей датасета: timestamp,src,dst,port,bytes
2. IP адреса внутренней сети начинаются с 12-14
3. Все остальные IP адреса относятся к внешним узлам
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

### Задание 2
Поиск утечки данных 2
Другой атакующий установил автоматическую задачу в системном планировщике cron для экспорта содержимого внутренней wiki системы. Эта система генерирует большое количество траффика в нерабочие часы больше, чем остальные хосты. Определите IP этой системы. Известно, что ее IP адрес отличается от нарушителя из предыдущей задачи.
```{r}
library (dplyr)

#Извлекаем часы и минуты
df$hour <- with (df,format(as.POSIXct(df$ts), format = "%H"))
df$minutes <- with (df,format(as.POSIXct(df$ts), format = "%M"))

#Поиск рабочих часов
activhours <- df %>% group_by(hour) %>% summarise(N = n())
select(arrange(activhours,desc(N)),N,hour)
#часы 22,19,18,21,17,20,16,23 => 16-23 рабочие

answer2 <- df %>% 
  filter(src != "13.37.84.125") %>% #не адрес из 1 пункта
  filter(src_info == TRUE) %>% #исходящий трафик
  filter(dst_info == FALSE) %>%
  filter(hour >= 0) %>% #нерабочие часы
  filter(hour < 16) %>%
  group_by(src) %>%
  summarise(bytes = mean(bytes))
select(arrange(answer2,desc(bytes)) %>% top_n(1),src)  

## Answer2: IP - 12.55.77.96

##Графический вид

#Поиск рабочих часов
activhours <- df %>% group_by(hour) %>% summarise(N = n())

plot(activhours$hour,activhours$N,xlab = "часы",ylab = "кол-во",type = "h",main = "Рабочие часы")

select(arrange(activhours,desc(N)),N,hour)
#часы 22,19,18,21,17,20,16,23 (16-23) - рабочие часы

```

### Задание 3
Поиск утечки данных 3
Еще один нарушитель собирает содержимое электронной почты и отправляет в Интернет используя порт, который обычно используется для другого типа трафика. Атакующий пересылает большое количество информации используя этот порт, которое нехарактерно для других хостов, использующих этот номер порта. Определите IP этой системы. Известно, что ее IP адрес отличается от нарушителей из предыдущих задач.

```{r}
library (dplyr)

# Z оценка (стандартная оценка) - этот балл помогает понять, больше или меньше значение данных, чем среднее, и насколько оно далеко от среднего. 
# Z баллов = (среднее значение x) / std. отклонение

#находим "аномальный" порт с помощью стандартной оценки (находим max)
ports <- df %>%
  group_by (port) %>%
  summarise(Z_оценка = ((bytes-mean(bytes))/sd(bytes)))

select(arrange(ports,desc(Z_оценка)) %>% top_n(1),port)  
#port 124

port_src <- df %>%
  filter(src != "13.37.84.125") %>% #не адрес из 1 пункта
  filter(src != "12.55.77.96") %>% #не адрес из 2 пункта
  filter(port == 124) %>% #порт 124
  group_by (src) %>%
  summarise(bytes_sum = sum(bytes))

select(arrange(port_src,desc(bytes_sum)) %>% top_n(1),src)

## 12.30.96.87

##Графический вид

#ports %>% group_by(port)%>%summarise(max=max(Z_оценка)) %>% View

plot((ports %>% group_by(port)%>% summarise(max=max(Z_оценка)))$port, (ports %>% group_by (port) %>% summarise (max=max(Z_оценка)))$max,xlab = "порт",ylab = "Z_оценка",type = "h",main = "Z_оценка")
 
select(arrange(port_src,desc(bytes_sum)) %>% top_n(1),src) 
#12.30.96.87
port_src2 <- select(arrange(port_src,desc(bytes_sum)) %>% top_n(10),src,bytes_sum) 

plot(as.factor(port_src2$src),port_src2$bytes_sum, xlab = "порт",ylab = "байты",type = "h",main = "Z_оценка")

```
