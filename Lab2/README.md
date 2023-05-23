# Использование информации о сетевом трафике для обнаружения подозрительной и вредоносной активности
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
