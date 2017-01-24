#plot b/w national_rank and quality_of_education differed by patents for all universities
l <- read.csv("cwurData.csv", header=TRUE)
qplot(l$national_rank,l$quality_of_education,data=l,color = patents,main = "National_rank vs Quality_of_education",geom = "auto",xlim = c(1,230),ylim = c(1,350),xlab = "national_rank",ylab = "quality_of_education")
library(ggplot2)
#plot b/w year and avg_num_of_studs_per_year
k <- read.csv("timesData.csv", header=TRUE)
k
str(timesData)
library(ggplot2)
subset(k,k$year=="2011")
i <- subset(k,k$year=="2011")
i
i$num_students
t <- i$num_students
t
i$num_students <- as.numeric(as.factor(i$num_students))
b <- mean(i$num_students, na.rm = TRUE)
b
c <- subset(k,k$year == "2012")
c
c$num_students <- as.numeric(as.factor(c$num_students))
d <- mean(c$num_students, na.rm = TRUE)
d
e <- subset(k,k$year == 2013)
e
e$num_students <- as.numeric(as.factor(e$num_students))
f <- mean(e$num_students, na.rm = TRUE)
f
g <- subset(k,k$year == 2014)
g
g$num_students <- as.numeric(as.factor(g$num_students))
h <- mean(g$num_students, na.rm = TRUE)
h
i <- subset(k,k$year == 2015)
i
i$num_students <- as.numeric(as.factor(i$num_students))
j <- mean(i$num_students, na.rm = TRUE)
j
m <- subset(k,k$year == 2016)
m
m$num_students <- as.numeric(as.factor(m$num_students))
k <- mean(m$num_students)
k
year <- c(2011,2012,2013,2014,2015,2016)
mean_of_num_studs_per_year <- c( 409.165,394.6468,403.335,406.6725,405.2718,398.615)
qplot(year,mean_of_num_studs_per_year,main = "Year vs Avg no.of students per year")
#3D plot b/w avg_of_score_for w.r.t year for all universities
l <- read.csv("cwurData.csv", header=TRUE)
subset(l,l$year == 2012)
m <- subset(l,l$year == 2012)
mean(m$score)
subset(l,l$year == 2013)
n <- subset(l,l$year == 2013)
mean(n$score)
subset(l,l$year == 2014)
u <- subset(l,l$year == 2014)
mean(u$score)
subset(l,l$year == 2015)
v <- subset(l,l$year == 2015)
mean(v$score)

slices <- c(54.9409,55.2712,47.27141,46.86385)
slices
lbls <- c("2012(54.9409)", "2013(55.2712)", "2014(47.27141)", "2015(46.86385)")
lbls

pie3D(slices,labels = lbls, col=rainbow(length(lbls)),
      main="Score wrt year for all Universities")
library(plotrix)
l <- read.csv("cwurData.csv", header=TRUE)
#regression
k <- read.csv("timesData.csv", header=TRUE)
y=c(k$research)
x=c(k$income)
relation <- lm(y ~ x)
print(relation)
print(summary(relation))
a <- data.frame(x=95)
result <- predict(relation,a)
print(result)
plot(x,y,main = "income & research - Regression",abline(lm(y~x)),col = year,cex = 1.3,pch = 16,xlab = "income",ylab = "research")
#plot for number of schools in a country
data2 = read.csv("school_and_country_table.csv",header = TRUE)
names(data2)
x <- table(data2$country)
x
y <- data.frame(x)
y
names(y)
library(ggplot2)
country_names <- y$Var1
no.of_schools <- y$Freq
ggplot(y, aes(x=country_names, y=no.of_schools,fill = country_names)) +
  geom_bar(stat="identity")+                        
  # Use a larger dot
  theme_bw() +
  theme(axis.text.x = element_text(angle=60, hjust=1),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.x = element_line(colour="grey60", linetype="dotted"))+
  ggtitle(("   No.of schools in each country   "))
#Plots for countries expenditure wrt year for all Universities
data6 = read.csv("education_expenditure_supplementary_data.csv",header = TRUE)
data6
library(ggplot2)
qplot(X1995,country,data = data6,color = institute_type,size = direct_expenditure_type,ylab = "expenditure wrt country",main = "year vs expenditure_wrt_country")
qplot(X2000,country,data = data6,color = institute_type,size = direct_expenditure_type,ylab = "expenditure wrt country",main = "year vs expenditure_wrt_country")
qplot(X2005,country,data = data6,color = institute_type,size = direct_expenditure_type,ylab = "expenditure wrt country",main = "year vs expenditure_wrt_country")
qplot(X2009,country,data = data6,color = institute_type,size = direct_expenditure_type,ylab = "expenditure wrt country",main = "year vs expenditure_wrt_country")
qplot(X2010,country,data = data6,color = institute_type,size = direct_expenditure_type,ylab = "expenditure wrt country",main = "year vs expenditure_wrt_country")
qplot(X2011,country,data = data6,color = institute_type,size = direct_expenditure_type,ylab = "expenditure wrt country",main = "year vs expenditure_wrt_country")
