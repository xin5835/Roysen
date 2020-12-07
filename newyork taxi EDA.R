#数据维度
#id - ID
#vendor_id - 不同的出租车公司
#pickup_datetime - 上车时间
#dropoff_datetime - 下车时间
#passenger_count - 乘客人数
#pickup_longitude - 上车经度
#pickup_latitude - 上车纬度
#dropoff_longitude - 下车经度
#dropoff_latitude - 下车维度
#store_and_fwd_flag - 是否分享行程记录 Y=是，N= 不
#trip_duration - 旅行时间（秒）


#加载包
library(tidyverse)
library(forcats)
library(lubridate)
library(geosphere)
library(patchwork)


#读取数据并检查缺失值
test <- read_csv("~/workshop/train.csv")
glimpse(test)
summary(test)
sum(is.na(test))

# vender_ID可能是两家出租车公司；上下车时间为字符型向量（上车起止日期2016-1-1--2016-6-30），需转化，经纬度坐标需要转换为距离，进而可以算出速度，数据共有观测145万多行，变量11个，vender_id需转化为引子，字符型日期需转化为数字型 --------------------------------
3526282/3600#最大旅行时长352682s是一个异常值，意味着打车时长为979h，这是几乎不可能的。



#按行随机抽样10，000人
set.seed(1110)
test1 <- sample_n(test, 10000)
glimpse(test1)


#提取经纬度变量，将经纬度转换为距离（km），并添加到数据框中
pickup_location <- test1 %>%
    select(pickup_longitude,pickup_latitude)
dropoff_location <- test1 %>% 
    select(dropoff_longitude,dropoff_latitude)

test1 <- test1 %>%
    mutate(distance = distHaversine(pickup_location,dropoff_location)/1000) 
glimpse(test1)

#日期格式转换，将vendor_id作为因子，添加速度（km/h）列
test1 <- test1 %>% 
    mutate(store_and_fwd_flag = factor(store_and_fwd_flag),
           pickup_datetime = ymd_hms(pickup_datetime),
           dropoff_datetime = ymd_hms(dropoff_datetime),
           vendor_id = factor(vendor_id),
           speed = distance/trip_duration*3600)
glimpse(test1)

#日期处理，将日期转换为数据型，并按年月周日拆分日期
test1 <- test1 %>% 
    mutate(month = as.integer(month(pickup_datetime)),
           hour = hour(pickup_datetime),
           wday = wday(pickup_datetime))

summary(test1)
glimpse(test1)
#查看样本数据类别分布
test1 %>% 
    count(vendor_id )
test1 %>% 
    count(passenger_count )
#每辆车乘客人数直方图（单人超过7000人，其次2人，4人最少，多人出行也有一定的份额）
test1 %>% 
ggplot(aes(passenger_count ))+
    geom_bar(fill = I('blue'))+
    labs(title = "barplot of passenger")+
    theme(plot.title = element_text(hjust = 0.5))
test1 %>% 
ggplot()+
  geom_bar(aes(passenger_count, fill = vendor_id),  position = "dodge")
#多人出行主要在2组出租车公司
test1 %>% 
ggplot(aes(passenger_count ))+
    geom_bar(fill = "skyblue", binwidth = 0.7)+
    facet_grid(.~vendor_id)+
    labs(title = "barplot of 1 and 2 company passenger")+
    theme(plot.title =element_text(hjust = 0.5))

#多人出行旅游、商务还是其他？
filter(test1,passenger_count>3) %>% 
ggplot()+
    geom_bar(aes(passenger_count, fill = "red"))+
    labs(title = "barplot of passenger")+
    theme(plot.title = element_text(hjust = 0.5))

#多人出行年度分布(1-6中5人和6人乘车人数波动增长，4月份人数达到946人，约占样本的1/10，多人出行值得投入资源吗？)
 filter(test1, passenger_count>4) %>% 
    select(month,passenger_count) %>% 
    group_by(month) %>% 
    summarise(sum(passenger_count)) 
 
#可视化  
 filter(test1, passenger_count>4) %>% 
     select(month,passenger_count) %>% 
     group_by(month) %>% 
     summarise(sum_count = sum(passenger_count)) %>% 
     unnest(cols = 2)%>% 
     ggplot(aes(month,sum_count))+
     geom_point(aes(size = sum_count))+
     ylim(0,1000)
 
 
#转发行程人数远小于100人，其中6人乘客无人转发行程，
ggplot(test1,aes(passenger_count,fill = store_and_fwd_flag))+
    geom_histogram(binwidth = 0.5)+
    facet_grid(.~store_and_fwd_flag)+
    labs(title = "male and female histgram of passenger")+
    theme(plot.title = element_text(hjust = 0.5))
#行驶距离（可能服从右偏的正态分布）
ggplot(data = test1,aes(x = distance ))+
    geom_histogram(bins = 300,fill = "skyblue",alpha = 0.8)+
    xlim(0,40)+
    labs(title = "histgram of trip_duration")+
    theme(plot.title = element_text(hjust = 0.5))
#依照出租车公司分组行驶距离（1、2人行驶距离更长，二组出租车公司行驶距离优势更明显？）
ggplot(data = test1,aes(x=distance))+
    geom_histogram(fill = "skyblue",alpha = 0.6)+
    facet_grid(.~vendor_id)+
    labs(title = "histgram of distance")+
    theme(plot.title = element_text(hjust = 0.5))
#上下车时间大体正常，1月底左右无人打车，1组和2组分布差异不大
ggplot(data = test1,aes(speed))+
    geom_histogram(bins = 300, fill = "skyblue")+
    labs(title = "histgram of speed between 1 and 2 group")+
    facet_grid(.~vendor_id)+
    theme(plot.title = element_text(hjust = 0.5))

#pickup_datetime(一月底二月初异常)，谷歌说因为暴风雪，上车时间和下车时间分布正常
p1 <- ggplot(test1,aes(pickup_datetime,fill = "red"))+
    geom_histogram( bins = 200)
p2 <- ggplot(test1,aes(dropoff_datetime,fill = "red"))+
    geom_histogram(bins = 200)
p1+p2+plot_layout(nrow = 2)
#从星期天到星期六打车人数变化不大
test1 %>% 
    ggplot(aes(wday))+
    geom_histogram()
#除了凌晨（2:00-6:00）都是高峰，晚19:00-21:00为打车人数最多的时间，夜生活丰富
test1 %>%
  mutate(hpick = hour(pickup_datetime),
         Month = factor(month(pickup_datetime, label = TRUE))) %>% 
group_by(hpick, Month) %>%
  count() %>%
  ggplot(aes(hpick, n, color = Month)) +
  geom_line(size = 1.5) +
  labs(x = "Hour of one day", y = "count")


#打车时长(norm?),有异常值 
filter(test1,trip_duration<50000) %>% 
ggplot(aes(trip_duration))+
    geom_histogram(bins = 500,fill = "orange")
filter(test1,speed>50000) %>% 
    glimpse()
   
50000/3600#打车时长超过13.8小时不大可能
2500/3600#打车时长超过0.69h的人数很少，主要集中在0.27h左右，短途居多（场景上下班地铁换出租？）
#平均车速(speed=15?), 右偏正态分布
filter(test1, trip_duration>120) %>% 
    glimpse()

filter(test1, speed<120) %>% 
ggplot(aes(speed))+
    geom_histogram(bins = 300,fill = I("blue"))

#车速与乘客人数、月份、周无关，与每日具体几点钟有关（凌晨车少，车速快）
filter(test1, speed<120) %>% 
    ggplot(aes(speed,color = factor(passenger_count)))+
    geom_freqpoly(size = 1.5)

filter(test1, speed<120) %>% 
    ggplot(aes(speed,color = factor(wday)))+
    geom_freqpoly(size = 1.5)

filter(test1, speed<120) %>% 
    ggplot(aes(speed,color = factor(month)))+
    geom_freqpoly(size = 1.5)

filter(test1, speed<120) %>% 
    ggplot(aes(speed,color = factor(hour)))+
    geom_freqpoly(size = 1.5)  
filter(test1, speed<120) %>% 
    ggplot(aes(hour, speed, color = factor(hour)))+
    geom_boxplot() #超过40km/h都算离群值
filter(test1, speed<120) %>% 
  ggplot(aes(month, speed,color = factor(hour)))+
  geom_boxplot() 

#大体距离与速度无关，意味着不管开多远，速度变化不大
ggplot(test1,aes(speed, distance))+
    geom_point()+
    geom_jitter()

#距离的远近和行驶速度之间看不到线性关系
ggplot(test1,aes(speed, distance))+
  geom_point()+
  geom_smooth()+
  scale_x_log10()+
  scale_y_log10()
 
#行驶距离以5km内居多
ggplot(test1,aes(wday, distance,color = distance))+
    geom_point()+
    geom_jitter()
ggplot(test1,aes(hour, distance,color = distance))+
    geom_point()+
    geom_jitter()
ggplot(test1,aes(month, distance,color = distance))+
    geom_point()+
    geom_jitter()

# 结论 --------------------------------------------------------------------------------
#纽约是一个繁忙的城市，出租车业务不论哪一个月份，哪一个星期，哪一天没有大的差异；

#纽约是一个不夜城。仅仅在每日凌晨2:00-5:00乘客人数较少，但并不是没有，其他每日时间段打车人数都很多，区别不大；

#纽约是一个拥堵的城市，平均车速15km/h，一年中车速集中在0-50km/h，可以说不论什么地方什么时间，要想让车速超过50km/h是一件不容易的事；

#短程车的乘客占比极高，行程以5km内居多，这与纽约拥堵的路况也是分不开的，从场景上看打车的范围距离都很近，可能是上下班更换交通工具等情况；

#多人（大于4人）出行具有一定的市场，6个月内呈现小范围波动的增长趋势。

#异常数据。 一月底二月初，基本无人打车，谷歌说因为暴风雪天气。
   
  
  

