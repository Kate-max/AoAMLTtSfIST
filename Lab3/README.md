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
