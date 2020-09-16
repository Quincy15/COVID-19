library(lubridate) # for working with dates
library(scales)    # to access breaks/formatting functions
library(gridExtra) # for arranging plots
library(ggplot2)
library(ggthemes)
library(dplyr)
library(broom)
library(openxlsx)
# read data 2020-01-23--now
data<-read.xlsx("Russia.xlsx",1)

r_length<-nrow(data)
t <- seq.Date(from = as.Date("20/01/23",format = "%y/%m/%d"),by = "day", length.out = r_length)
t<-as.Date(t,"%Y/%m/%d")
data[,1]<-t

observations<-data$现有确诊
observations<-as.numeric(observations)


#现存确诊人数和增长率描述性统计
summary(observations)
newdata<-data.frame(t,observations,data$rate)
newdata<-newdata[-c(1:38),]
m1<-ggplot(newdata,aes(x=t))+scale_x_date(labels=date_format("%d/%m"))+
  xlab("time(day/month)")+ylab("the number of existing confirmed cases")+
  #ggtitle("Residuals vs time")+
  geom_line(aes(y=observations),color="cyan")+
  geom_point(aes(y=observations),size=1) +  
  theme_minimal(base_size=10)+ 
  theme(plot.title = element_text(lineheight=.8, face="bold"),legend.position = "none",
        axis.title.y = element_text(size = 8))
m1

quyi=newdata
m2<-ggplot(quyi,aes(x=quyi$t))+scale_x_date(labels=date_format("%d/%m"))+
  xlab("time(day/month)")+ylab("Day-on-day change rate")+
  #ggtitle("Residuals vs time")+
  geom_line(aes(y=quyi$data.rate),color="cyan")+ 
  geom_point(aes(y=quyi$data.rate),size=1) + 
  theme_minimal(base_size=10)+ 
  theme(plot.title = element_text(lineheight=.8, face="bold"),legend.position = "none",
        axis.title.y = element_text(size = 8))
m2
cowplot::plot_grid(m1,m2,nrow = 2,labels=c("a","b"))
# 模型
start<-which(data[,1]=="2020-04-06")
end<-r_length
x<-start:end
lm<-lm(data$rate[start:end]~x)
summary(lm)

#残差检验
newdata1<-data.frame(t[start:r_length],lm$residuals)
colnames(newdata1)<-c("time","residuals")
Res<-ggplot(newdata1,aes(x=time))+scale_x_date(labels=date_format("%d/%m"))+
  xlab("time(day/month)")+ylab("residuals")+
  ggtitle("Residuals vs time")+
  geom_point(aes(y=residuals)) + geom_line(aes(y=residuals))+ 
  theme_minimal(base_size=10)+ 
  theme(plot.title = element_text(lineheight=.8, face="bold"),legend.position = "none")
Res

p <- (rank(lm$residuals) -0.5)/length(lm$residuals)
q <- qnorm(p)
newdata1[,3]<-q
newdata1[,4]<-scale(lm$residuals)
colnames(newdata1)[3:4]<-c("q","scale_residuals")
QQ<-ggplot(newdata1,aes(x=q))+
  ggtitle("Normal Q-Q")+xlab("theoretical quantiles")+
  ylab("standardized residuals")+
  geom_point(aes(y=scale_residuals),shape=1,size=2) + 
  geom_line(aes(y=q))+
  theme_minimal(base_size=10)+ 
  theme(plot.title = element_text(lineheight=.8, face="bold"),legend.position = "none",
        axis.title.y = element_text(size = 8))
QQ
cowplot::plot_grid(Res,QQ,nrow = 2, labels = c("a","b"))

shapiro.test(lm$residuals)
library(DistributionTest)
za.test(lm$residuals,"norm")

newdata<-data.frame(data$日期,data$现有确诊,data$rate)
y<-lm$coefficients[1]+lm$coefficients[2]*x
y1<-c(rep(NA,start-1),y)
newdata[,4]<-y1
colnames(newdata)[4]<-"p_rate"

sanyue<-newdata[-c(1:54),]

# 绘制模型图
p1<-ggplot(sanyue,aes(x=sanyue$data.日期))+scale_x_date(labels=date_format("%d/%m"))+
  xlab("time(day/month)")+ylab(NULL)+
  ggtitle(NULL)+
  geom_point(aes(y=sanyue$data.rate)) + geom_line(aes(y=sanyue$data.rate, color="red")) + 
  geom_line(aes(y=sanyue$p_rate,color="cyan")) + theme_minimal(base_size=10)+ 
  theme(plot.title = element_text(lineheight=.8, face="bold"),legend.position = "none") +
  geom_vline(xintercept = t[start],color="pink")+
  geom_text(mapping=aes(x=t[start], y=0.25, label=t[start]),size=3, 
            angle=90, vjust=-0.5, hjust=-0.2, col="red") 
p1

# 预测当前日期起后n1天的变化率
n1<-150
pre<-data.frame()
for (i in 1:(r_length+n1)) {
  data1<-data.frame(x=i)
  p<-predict(lm,data1,interval="prediction",level=0.95)
  pre[i,1]<-p[1]
  pre[i,2]<-p[2]
  pre[i,3]<-p[3]
}
colnames(pre)<-c("fit","lwr","upr")

#绘制区间
t2<-r_length-start+1   
m<-t2+5  #时间总长度
t <- seq.Date(from = as.Date("2020/05/02",format = "%Y/%m/%d"),by = "day", length.out = m)
t<-as.Date(t,"%Y/%m/%d")

rate1<-c(data$rate[start:r_length],rep(NA,m-t2))
rate2<-pre$fit[start:(r_length+5)]
rate3<-c(rep(NA,(r_length-start+1)),rate2[(r_length-start+2):(r_length-start+6)])
u<-pre$upr[start:(r_length+5)]
l<-pre$lwr[start:(r_length+5)]
newdata2<-data.frame(t,rate1,rate2,rate3,u,l)

#================================================
# 绘制环比变化率图+置信区间+预测
p2<-ggplot(newdata2,aes(x=t))+scale_x_date(labels=date_format("%d/%m"))+
  xlab("time(day/month)")+ylab(NULL)+
  ggtitle(NULL)+
  geom_point(aes(y=rate1)) + geom_line(aes(y=rate1))+
  geom_line(aes(y=rate2),color="red")+
  geom_point(aes(y=rate3),color="red")+
  theme_minimal(base_size=10)+ 
  theme(plot.title = element_text(lineheight=.8, face="bold"),legend.position = "none")+
  geom_vline(xintercept = t[t2],color="pink")+
  geom_text(mapping=aes(x=t[t2], y=0, label=t[t2]),size=3, 
            angle=90, vjust=-0.5, hjust=-1.5, col="red")+
  geom_ribbon(aes(x=t,ymin = l, ymax = u),color = "cyan", alpha = .15)
p2
# 预测全国接受医学观测人数
newdata3<-data.frame(NULL)
t <- seq.Date(from = as.Date("2020/01/23",format = "%Y/%m/%d"),by = "day", length.out = r_length+n1)
t<-as.Date(t,"%Y/%m/%d")

newdata3<-data.frame(t,pre)
newdata3[1,2:4]<-c(NA,NA,NA)
newdata3[,5]<-c(observations,rep(NA,n1))
newdata3[,6]<-c(observations,rep(NA,n1))
newdata3[,7]<-c(observations,rep(NA,n1))
newdata3[,8]<-c(observations,rep(NA,n1))
newdata3[,9]<-c(rep(1,r_length),rep(2,n1))
colnames(newdata3)<-c("time","fit","lwr","upr","observations","obs_fit","obs_lwr","obs_upr","group")
L<-nrow(newdata3)
# 计算预测值
for(i in r_length:L){
  newdata3[i+1,6]<-(1+newdata3$fit[i+1])*newdata3[i,6]# 计算预测值
  newdata3[i+1,7]<-(1+newdata3$lwr[i+1])*newdata3[i,7]# 计算下限
  newdata3[i+1,8]<-(1+newdata3$upr[i+1])*newdata3[i,8]# 计算上限
}


attach(newdata3)
t_fit<-min(which(round(obs_fit,0)==0))
newdata3$obs_fit[t_fit:L]<-NA
t_lwr<-min(which(round(obs_lwr,0)==0))
newdata3$obs_lwr[t_lwr:L]<-NA
t_upr<-min(which(round(obs_upr,0)==0))
t<-t[1:t_upr]
detach(newdata3)
newdata3<-newdata3[-(t_upr+1:nrow(newdata3)),]
newdata3[,10]<-c(rep(NA,r_length),newdata3[(r_length+1):t_upr,6])

point_fit<-which.max(newdata3$obs_fit)

#预测
p3<-ggplot(newdata3,aes(x=t,y=observations),color="black")+
  scale_x_date(labels=date_format("%d/%m"))+
  xlab("time(day/month)")+ylab("the number of existing confirmed cases")+
  ggtitle(NULL)+
  geom_point(aes(y=observations),pch=16,size=1)+
  geom_line(aes(y=obs_lwr))+geom_line(aes(y=obs_upr),color="lightblue")+
  geom_ribbon(aes(x=t,ymin = obs_lwr, ymax = obs_upr),color = "lightblue", alpha = .15)+
  theme_minimal(base_size=10)+ 
  theme(plot.title = element_text(lineheight=.8, face="bold"),legend.position = "none",
        axis.title.y = element_text(size = 8))+
  geom_vline(xintercept = t[r_length+1],color="green")+
  geom_text(mapping=aes(x=t[r_length+1], y=30000, label=t[r_length+1]),size=3, 
            angle=90, vjust=-0.5, hjust=-0.2, col="red")+
  geom_vline(xintercept = t[t_fit],color="pink",linetype="dotted")+
  geom_text(mapping=aes(x=t[t_fit], y=30000, label=t[t_fit]),size=3, 
            angle=90, vjust=1, hjust=-0.2, col="red")+
  geom_vline(xintercept = t[t_lwr],color="pink")+
  geom_text(mapping=aes(x=t[t_lwr], y=30000, label=t[t_lwr]),size=3, 
            angle=90, vjust=1, hjust=-0.2, col="red")+
  geom_vline(xintercept = t[t_upr],color="pink")+
  geom_text(mapping=aes(x=t[t_upr], y=30000, label=t[t_upr]),size=3, 
            angle=90, vjust=1, hjust=-0.2, col="red")
p4<-p3+geom_line(aes(y=V10),color="red")+  geom_line(aes(y=observations))+
  geom_point(aes(y=V10),color="#FF6666",size=0.5)
p4



# 导出人数预测值
DD<-data.frame(t,round(newdata3$obs_fit,0),round(newdata3$obs_lwr,0),
               round(newdata3$obs_upr,0))
colnames(DD)<-c("日期","预测值","95%预测下限","95%预测上限")
write.csv(DD,"C:\\Users\\zj\\Desktop\\Prediction.csv")