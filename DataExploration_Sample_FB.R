f<- read.csv('C:/Users/desai/Google Drive/sample_facebook .tsv', sep = '\t')
library(ggplot2)

# Which gender makes more likes on www?
by(fb$www_likes, fb$gender, sum)



#plotting the investigation of this
qplot(x=www_likes, data=subset(fb, !is.na(gender)), geom='freqpoly', color=gender)+
  scale_x_continuous(breaks = seq(0,30,1), limits = c(0,30))

qplot(x=www_likes, data=subset(fb, !is.na(gender)), geom='freqpoly', color=gender)+
  scale_x_log10()

#plotting a box-plot
qplot(x=gender, y=friend_count, data= subset(fb, !is.na(gender)), geom='boxplot')+
  scale_y_continuous(limits = c(0,200), breaks =seq(0,200,10) )

#using the scale layer, removes the data and thus shows the wrong results
qplot(x=gender, y=friend_count, data= subset(fb, !is.na(gender)), geom='boxplot')+
  coord_cartesian(ylim = c(0,250))

# Result show that females more likes on the www




######   Who initiated the Friendship?
colnames(fb)
by(fb$friendships_initiated, fb$gender, summary)
# Results shows that Females initiated more friendships


#plot1: Histogram
qplot(x=friendships_initiated, data=subset(fb, !is.na(gender)), binwidth=25)+
  coord_cartesian(xlim=c(0,500))+
  scale_x_continuous(breaks = seq(0,500,25))

#plot2: Frequency Polygon, gender
qplot(x=friendships_initiated, data=subset(fb, !is.na(gender)), binwidth=25, geom='freqpoly', color=gender)+
  coord_cartesian(xlim=c(0,100))+
  scale_x_continuous(breaks = seq(0,100,10))

#plot3: BOx_plot
qplot(x=gender, y=friendships_initiated, data=subset(fb, !is.na(gender)), geom='boxplot', ylab = 'Friendships Initiated', xlab = 'Sex')+
  coord_cartesian(ylim = c(0,150))+
  scale_y_continuous(breaks = seq(0,300,25))

# ####### Result: It is females from the data which suggests that the friendship is initiated more by them.




#Analysis on Likes made by different gender
qplot(likes,data=fb, color=gender, binwidth=500)+
  coord_cartesian(xlim = c(0,1000))


# Analyzing how many users use mobile phones:

# First, creating a new column "mobile_check_in"
fb$mobile_check_in<- ifelse(fb$mobile_likes > 0, 1 , 0)
# COnverting it to factor to understand and plot the data i a better way.
fb$mobile_check_in<- factor(fb$mobile_check_in)
colnames(fb)
str(fb$mobile_check_in)

#Graphical Count
qplot(mobile_check_in, data=fb)

#Table count
summary(fb$mobile_check_in)

#### Result:
# Since we learn that likes made from mobile are quite more, we can say that more many people are using the mobile app.
# So more focus should be made on developing and maintaining the Mobile app.

