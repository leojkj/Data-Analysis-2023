#I am cleaning the data by first inspecting for NA
library(dplyr)
library(ggplot2)
library(corrplot)
library(tidyverse)
set_plot_dimensions = function(width_choice, height_choice) {
  options(repr.plot.width=width_choice, repr.plot.height=height_choice)
}
df = data.frame(Life_Expectancy_Data)
missing.rows = dim(df)[1] - dim(na.omit(df))[1]
sprintf("Dataset size: [%s]", toString(dim(df)))
sprintf("Missing rows: %s (%s%%)", missing.rows, round((missing.rows*100)/dim(df)[1], 2))
#43.87% of the data set is missing we must calculate the mean and the median to fill the dataset
na_df = data.frame(type =c("missing", "non missing"), count = c(missing.rows, dim(na.omit(df))[1]))
dev.off()
set_plot_dimensions(16,4)
ggplot(na_df, aes(fill=type, y="", x=count)) +
  geom_bar(position="stack", stat="identity") + 
  ggtitle("Missing vs Non-missing row counts") +
  xlab("Missing Count") + ylab("") +
  theme(text = element_text(size = 18)) + 
  scale_fill_brewer(palette="Set2")
na_counts = data.frame(feature = factor(names(df)),
                       counts=sapply(df, function(x) sum(is.na(x))))
set_plot_dimensions(16, 8)
ggplot(na_counts,
       aes(x=reorder(feature, -counts), y=counts, fill=counts)) +
                                  geom_bar(stat="identity") +
                                  ggtitle("Missing Counts In Each Feature") +
                                  xlab("Feature") + ylab("NA Counts") +
                                  theme(axis.text.x=element_text(angle=20, hjust=1))+
                                  theme(text = element_text(size=18))+
                                  scale_fill_continuous(trans='reverse')
#we now see the major na counts in each variables
#we will proceeed to apply data imputation by checking outliers in each variable
#boxplot method that contains na 
#high outlier = median
#low outlier = mean
dev.off()
set_plot_dimensions(20, 10)
par(mfrow = c(2,5))
boxplot(df$Life.expectancy,
          ylab = "Life Expectancy",
          main = "Boxplot of Life Expectancy",
          col = "#FF6666", 
          outcol = "#FF6666")
boxplot(df$Adult.Mortality,
        ylab = "Adult Mortality",
        main = "Boxplot of Adult Mortality",
        col = c("red"),
        outcol = c("red"))
boxplot(df$Alcohol,
        ylab = "Alcohol",
        main = "Boxplot of Alcohol",
        col= "#008080",
        outcol="#008080")
boxplot(df$Hepatitis.B,
        ylab = "Hepatitis B",
        main = "Boxplot of Hepatitis B",
        col= "#FF6666",
        outcol="#FF6666")
boxplot(df$BMI,
        ylab = "BMI",
        main = "Boxplot of BMI",
        col= "#008080",
        outcol="#008080")
boxplot(df$Polio,
        ylab = "Polio",
        main = "Boxplot of Polio",
        col= "#FF6666",
        outcol="#FF6666")
boxplot(df$Total.expenditure,
        ylab = "Total Expenditure",
        main = "Boxplot of Total Expenditure",
        col= "#FF6666",
        outcol="#FF6666")

summary(df)
Life.expectancy_median <- median(df$Life.expectancy,  na.rm = TRUE)
Adult.Mortality_median <- median(df$Adult.Mortality,  na.rm = TRUE)
Hepatitis.B_median <- median(df$Hepatitis.B,  na.rm = TRUE)
Polio_median <- median(df$Polio,  na.rm = TRUE)
Diphtheria_median <- median(df$Diphtheria,  na.rm = TRUE)
Total.expenditure_median <- median(df$Total.expenditure,  na.rm = TRUE)
GDP_median <- median(df$GDP,  na.rm = TRUE)
Population_median <- median(df$Population,  na.rm = TRUE)
thinness..1.19.years_median <- median(df$thinness..1.19.years,  na.rm = TRUE)
thinness.5.9.years_median <- median(df$thinness.5.9.years,  na.rm = TRUE)
Schooling_median <- median(df$Schooling,  na.rm = TRUE)

Alcohol_mean <- mean(df$Alcohol,  na.rm = TRUE)
BMI_mean <- mean(df$BMI,  na.rm = TRUE)
Income.composition.of.resources_mean <- mean(df$Income.composition.of.resources,  na.rm = TRUE)

#time to replace the missings with the averages and median

df$Life.expectancy[is.na(df$Life.expectancy)] <- Life.expectancy_median
df$Adult.Mortality[is.na(df$Adult.Mortality)] <- Adult.Mortality_median
df$Hepatitis.B[is.na(df$Hepatitis.B)] <- Hepatitis.B_median
df$Polio[is.na(df$Polio)] <- Polio_median
df$Diphtheria[is.na(df$Diphtheria)] <- Diphtheria_median
df$Total.expenditure[is.na(df$Total.expenditure)] <- Total.expenditure_median
df$GDP[is.na(df$GDP)] <- GDP_median
df$Population[is.na(df$Population)] <- Population_median
df$thinness..1.19.years[is.na(df$thinness..1.19.years)] <- thinness..1.19.years_median
df$thinness.5.9.years[is.na(df$thinness.5.9.years)] <- thinness.5.9.years_median
df$Schooling[is.na(df$Schooling)] <- Schooling_median
#means
df$Alcohol[is.na(df$Alcohol)] <- Alcohol_mean
df$BMI[is.na(df$BMI)] <- BMI_mean
df$Income.composition.of.resources[is.na(df$Income.composition.of.resources)] <- Income.composition.of.resources_mean
df
head(df)
#data is now cleaned and able to be analyzed

df$Status = as.factor(df$Status)

#Categorical Variables x = Status y = country Mosaic Plot
dev.off()
mosaicplot(Country ~ Status, data = df, col=c("Blue", "Red"))

#There seems to be no significant change within the year of 2000-2015. No strong associations. Will look for correlation
#Chisquare test in order to see correlation status and year
table(df$Status)
chisq.test(df$Status, df$Year)
#p value = 1 X^2 = .10287
#variables x = status and y = life expectancy
set_plot_dimensions(12, 10)
ggplot(df, aes(x=Status, y=Life.expectancy, fill=Status)) +
  geom_boxplot() +
  ggtitle("Life expectancy per country Status") +
  theme(text=element_text(size = 16)) + 
  scale_fill_brewer(palette="Set 2")
#anova to test the two variables for statistical significance
one.way = aov(Life.expectancy ~ Status, data = df)
summary(one.way)
tukey.plot.aov = aov(Life.expectancy ~ Status, data = df)
tukey.plot.test = TukeyHSD(tukey.plot.aov)
plot(tukey.plot.test, las = 2)
tukey.two.way = TukeyHSD(tukey.plot.aov)
tukey.two.way
two.way.plot <- ggplot(df, aes(x = Status, y = Life.expectancy, group=Status)) +
  geom_point(cex = 1.5, pch = 1.0,position = position_jitter(w = 0.1, h = 0))
two.way.plot
#life expectancy for developed countries are alot higher than developing ones
#Variables adult mortality vs GDP Scatter plot
ggplot(df, aes(x = Adult.Mortality, y = GDP)) + 
    geom_point(size=2, shape=23) +
    geom_smooth()
#I see potential for a 2d density estimation 
sp = ggplot(df, aes(x=Adult.Mortality, y=GDP)) +
  geom_point()
sp + geom_density_2d()
sp + stat_density_2d(aes(fill = ..level..), geom="polygon")+
  scale_fill_gradient(low="blue", high="red")
#I want to see the corr between two variables 
cor(df$Adult.Mortality, df$GDP)
#an ok correlation between gdp and life expectancy
#using a correlation matrix for finding good predictors
library(ggcorrplot)
library(Metrics)
corr <- round(cor(subset(df, select =-c(Status, Year, Country))), 3)
ggcorrplot(corr,type = "upper", lab = TRUE, outline.color = "black", lab_size = 4, legend.title = "Correlation") +
  ggtitle("Correlation Matrix")
#strong correlation of GDP and percentage.expenditure
#lets check the VIF in order to check colinearity making sure some of these varaibles VIF < 3
#Linear Regression Model
data = subset(df, select = -c(Country, Status))
M = lm(Life.expectancy ~ Schooling, data = data)
dev.off()
plot(M)
summ
#signficant but the R squared is .56 and my RSME is 6.1 which is greater than .2 -.5
#I might approach using multiple regression
M = lm(Life.expectancy ~ GDP + Schooling + BMI + Adult.Mortality, data = data)
summary(M)
anova(M)
set.seed(123)
plot(M)

sample = sample(c(TRUE, FALSE), nrow(data), replace=TRUE, prob=c(0.80, 0.20))
train = data[sample, ]
x.test = data[!sample, ]
y.test = data[!sample, ]$Life.expectancy
model.df = lm(Life.expectancy~., data = train)
summary(model.df)
#model seems to have a accurate R^2 and is significant
pred <- predict(model.df, newdata=x.test)
rmse(pred,y.test)
summary(model.df)$adj.r.squared
par(mfrow=c(2,2))
plot(model.df)