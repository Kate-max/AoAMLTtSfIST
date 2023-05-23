# Исследование метаданных DNS трафика

## Цель

1. Закрепить практические навыки использования языка программирования R для обработки данных
2. Закрепить знания основных функций обработки данных экосистемы tidyverse языка R
3. Закрепить навыки исследования метаданных DNS трафика

RStudioCloud

## Исходные данные

RStudioCloud

## Код программы

```{r}
install.packages("dplyr")
```

### Задания 2

```{r}
library(dplyr)
#2. Добавьте пропущенные данные о структуре данных (назначении столбцов)
dns <- read.csv("dns.log",header = TRUE,sep="")
colnames(dns)<-c(
  'ts',
  'uid',
  'ip_host',
  'port_host',
  'ip_recipient',
  'port_recipient',
  'protocol',
  'trans_id',
  'query',
  'qclass',
  'qclass_name',
  'qtype',
  'qtype_name',
  'rcode',
  'rcode_name',
  'QR',
  'AA',
  'TC RD',
  'RA',
  'Z',
  'answers',
  'TTLs',
  'rejected'
)
```

### Задание 4

```{r}
#4. Сколько участников информационного обмена в сети Доброй Организации?
library(dplyr)
#s<-head(dns,1000)
d1<-length(unique((grep(":", dns$ip_host, value = TRUE)),(grep(":", dns$ip_recipient, value = TRUE))))
d2<-length(unique(dns$ip_host, dns$ip_recipient))
d2-d1

## [1] 123352
```

### Задание 5

```{r}
#5. Какое соотношение участников обмена внутри сети и участников обращений к внешним ресурсам?
library(dplyr)
#внутри сети
toMatch <- c("192.168.","10.10","100.","172.")
b <- length(unique (grep(paste(toMatch,collapse="|"), dns$ip_recipient, value=TRUE)))
#внешние
a <- d2-d1-b
print("Отношение участников обмена внутри сети и участников обращений к внешним ресурсам: ")

## [1] "Отношение участников обмена внутри сети и участников обращений к внешним ресурсам: "

b/a*100

## [1] 0.9625458
```

### Задание 6

```{r}
#6. Найдите топ-10 участников сети, проявляющих наибольшую сетевую активность.
library(dplyr)
dns.1 <- dns %>%
group_by(ip_host) %>%
summarise(N = n())
select(arrange(dns.1,desc(N)) %>% top_n(10),ip_host)

## # A tibble: 10 × 1
##    ip_host        
##    <chr>          
##  1 10.10.117.210  
##  2 192.168.202.93 
##  3 192.168.202.103
##  4 192.168.202.76 
##  5 192.168.202.97 
##  6 192.168.202.141
##  7 10.10.117.209  
##  8 192.168.202.110
##  9 192.168.203.63 
## 10 192.168.202.106
```

### Задание 7

```{r}
#7. Найдите топ-10 доменов, к которым обращаются пользователи сети и соответственное количество обращений
library(dplyr)
library(stringr)
net.1 <- c(".net",".com",".org")
net.2 <- unique (grep(paste(net.1,collapse="|"), dns$query, value=TRUE))
dns.2 <- as.data.frame(str_extract(net.2, "\\w*.[a-z]*$"))
names(dns.2)[1] <- "query"
dns.3 <- dns.2 %>%
group_by(query) %>%
summarise(N = n())
select(arrange(dns.3,desc(N)) %>% top_n(10),query)

## # A tibble: 15 × 1
##    query          
##    <chr>          
##  1 dropbox.com    
##  2 apple.com      
##  3 sorbs.net      
##  4 google.com     
##  5 hec.net        
##  6 microsoft.com  
##  7 fireeye.com    
##  8 org.localdomain
##  9 ahbl.org       
## 10 apews.org      
## 11 nszones.com    
## 12 spamcop.net    
## 13 spamhaus.org   
## 14 spamrats.com   
## 15 tornevall.org
```

### Задание 8

```{r}
#8. Опеределите базовые статистические характеристики (функция summary()) интервала времени между последовательным обращениями к топ-10 доменам.
library(dplyr)
dns.3%>%summary()

##     query                 N          
##  Length:397         Min.   :  1.000  
##  Class :character   1st Qu.:  1.000  
##  Mode  :character   Median :  1.000  
##                     Mean   :  3.108  
##                     3rd Qu.:  2.000  
##                     Max.   :104.000
```

### Задание 9

```{r}
#9. Часто вредоносное программное обеспечение использует DNS канал в качестве канала управления, периодически отправляя запросы на подконтрольный злоумышленникам DNS сервер. По периодическим запросам на один и тот же домен можно выявить скрытый DNS канал. Есть ли такие IP адреса в исследуемом датасете?
library(dplyr)
dns.4<-data.frame(dns$ip_host,dns$query)
names(dns.4)[names(dns.4) == 'dns.ip_host'] <- 'ip_host'
names(dns.4)[names(dns.4) == 'dns.query'] <- 'query'

dns.4%>%group_by(dns.4$query)

## # A tibble: 428,292 × 3
## # Groups:   dns.4$query [5,176]
##    ip_host        query                                                  dns.4…¹
##    <chr>          <chr>                                                  <chr>  
##  1 192.168.202.76 "HPE8AA67"                                             "HPE8A…
##  2 192.168.202.76 "HPE8AA67"                                             "HPE8A…
##  3 192.168.202.76 "HPE8AA67"                                             "HPE8A…
##  4 192.168.202.76 "WPAD"                                                 "WPAD" 
##  5 192.168.202.76 "WPAD"                                                 "WPAD" 
##  6 192.168.202.76 "WPAD"                                                 "WPAD" 
##  7 192.168.202.89 "EWREP1"                                               "EWREP…
##  8 192.168.202.89 "EWREP1"                                               "EWREP…
##  9 192.168.202.89 "EWREP1"                                               "EWREP…
## 10 192.168.202.89 "*\\x00\\x00\\x00\\x00\\x00\\x00\\x00\\x00\\x00\\x00\… "*\\x0…
## # … with 428,282 more rows, and abbreviated variable name ¹​`dns.4$query`

dns.4<-dns.4%>%group_by(dns.4$ip_host)%>%count(dns.4$query,sort=TRUE)
dns.4<-dns.4%>%count(dns.4$ip_host,)

dns.4<-dns.4[which(dns.4$n==1),]
dns.4<-dns.4[which(nchar(dns.4$`dns.4$ip_host`)<16),]
dns.4<-dns.4[which(nchar(dns.4$`dns.4$ip_host`)>2),]

nrow(dns.4)

## [1] 73
```

### Задание 10

```{r}
# 10. Определите местоположение (страну, город) и организацию-провайдера для топ-10 доменов. Для этого можно использовать сторонние сервисы, например https://v4.ifconfig.co/.


#install.packages("devtools")
#require(devtools)
#library(devtools)
#install_github("ip2location/ip2location-r",force=TRUE)
#ip2location::open("~/IP-COUNTRY-REGION-CITY-LATITUDE-LONGITUDE-ZIPCODE-TIMEZONE-ISP-DOMAIN-NETSPEED-AREACODE-WEATHER-MOBILE-ELEVATION-USAGETYPE-SAMPLE.BIN")
```
