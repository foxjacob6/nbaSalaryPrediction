
library(data.table)
library(corrplot)
library(GGally)
library(tidyverse)
library(PerformanceAnalytics)
library(plotly)
#load data
salary <- read.csv("NBA-Salary.csv")
stats <- read.csv("stats.csv")

str(stats)
str(salary)

#filter data

stats <- 
  stats %>% filter(Year >= 2017) %>% 
  select(Year:G, MP, PER, FG:PTS) %>% 
  distinct(Player, .keep_all = TRUE) %>% 
  mutate(MPG = MP/G, PPG = PTS/G, APG = AST/G, 
         RPG = TRB/G, TOPG = TOV/G, BPG = BLK/G, 
         SPG = STL/G) 
print(stats)

#merge data 

stats_salary <- merge(stats, salary, by.x = "Player", by.y = "Player")
names(stats_salary)[40] <- "salary"
stats_salary <- stats_salary[-39]

#correlation plot of variables 
 

corrplot(cor(stats_salary %>% 
               select(salary, MPG:SPG, 
                      Age, PER, contains("%")), 
             use = "complete.obs"), 
         method = "number",type = "upper")

#correlation of variables against variables

stats_salary_cor <- 
  stats_salary %>% 
  select(salary, PPG, MPG, TOPG, RPG, PER, SPG, APG, BPG, Age, PF)
ggpairs(stats_salary_cor)

#points per game vs salary
  
names(stats_salary)[5] <- "Team"
plot_ly(data = stats_salary, x = ~salary, y = ~PPG, color = ~Team,
        hoverinfo = "text",
        text = ~paste("Player: ", Player,
                      "<br>Salary: ", format(salary, big.mark = ","),"$",
                      "<br>PPG: ", round(PPG, digits = 3),
                      "<br>Team: ", Team)) %>% 
  layout(
    title = "Salary vs Point Per Game",
    xaxis = list(title = "Salary "),
    yaxis = list(title = "Points Per Game")
  )

#linear plot of salary and points per game


stats_salary %>% 
  ggplot(aes(x = salary, y = PPG)) + 
  geom_point() + 
  geom_smooth(method = "lm") 



#coeficciants
#Each coeficciants represents $$

stats_salary_regression <- 
  stats_salary %>% select(salary, MPG:SPG, Age, PF, PER)
lm(salary~., data=stats_salary_regression)

summary(stats_salary_regression)
 
 ##salary prediction

salary_prediction <- function(m, rebounds, point, minutes, turn_over, personal){
  pre_new <- predict(m, data.frame(RPG = rebounds, PPG = point, MPG = minutes, TOPG = turn_over, PF = personal ))
  msg <- paste("RPG: ", rebounds, "PPG:", point, ",MPG:", minutes, ",TOPG:", turn_over,", PF: ", personal," ==> Expected Salary: $", format(round(pre_new), big.mark = ","), sep = "")
  print(msg)}

model <- lm(formula = salary~ RPG + PPG + MPG + TOPG + PF, data = stats_salary_regression)
salary_prediction(model, 11.8, 18, 34, 1.6, 179) #Trevor Ariza

salary_prediction(model, 3.2, 22.1, 35, 3.1, 242) #Devin Booker
salary_prediction(model, 2.82, 6.7, 15.5, .82, 50) #Alec Burke


#Validation
head(stats_salary)
set.seed(111)
rowId <- sample(nrow(stats_salary), size=300)
training<-stats_salary[rowId,]

test<-stats_salary[-rowId,]
#results from training data
object1<-lm(formula = salary~ RPG + PPG + MPG + TOPG + PF, data = stats_salary_regression)


#average predictions

avg<-predict(object1,newdata=test,interval="confidence")
#This is single point predictions
pre<-predict(object1,newdata=test,interval="prediction")


plot(test[,10],type="p",ylim=c(min(avg[,2],test[,10]),max(avg[,3],test[,10])),ylab="Y", main="Prediction Accuracy")
lines(avg[,1],col="red")
lines(avg[,2],col="green")
lines(avg[,3],col="blue")

#error rate
error<-(test$PPG>avg[,3])|(test$PPG<avg[,2])
error
sum(error)/ (dim(test)[1])

