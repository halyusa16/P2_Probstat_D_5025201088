#1

#X 
X<-c(78, 75, 67, 77, 70, 72, 28, 74, 77)
#Y
Y<-c(100, 95, 70, 90, 90, 90, 89, 90, 100)

#1a
difff <- c(22, 20, 3, 13, 20, 18, 11, 16, 23)
sd(difff)

#1b
meanX <- mean(X)
meanY <- mean(Y)

sdX <- sd(X)
sdY <- sd(Y)

variansX <- sdX ^ 2
variansY <- sdY ^ 2

abs(meanX - meanY) / sqrt((variansX/9) + (variansY/9))

#1c
t.test(X, Y)



#2
install.packages("BSDA")
library(BSDA)

tsum.test(mean.x=23500, sd(3900), n.x=100)

#3
bandung <- list("saham" = 19, "mean" = 3.64, "sd" = 1.67)
bali <- list("saham" = 27, "mean" = 2.79, "sd" = 1.32)

tsum.test(
  n.x = bandung$saham,
  n.y = bali$saham,
  mean.x = bandung$mean,
  mean.y = bali$mean,
  s.x = bandung$sd,
  s.y = bali$sd,
  var.equal = TRUE,
  alternative = "two.sided",
)

install.packages("mosaic")
library(mosaic)

plotDist(dist='t', df=2, col="blue")


qchisq(p = 0.05, df = 2, lower.tail=FALSE)


#4a
dataoneway <- read.table("https://rstatisticsandresearch.weebly.com/uploads/1/0/2/6/1026585/onewayanova.txt",h=T)
attach(dataoneway)
names(dataoneway)

dataoneway$Group <- as.factor(dataoneway$Group)
dataoneway$Group = factor(dataoneway$Group,labels = c("Orange", "Hitam", "Putih"))

class(dataoneway$Group)

Group1 <- subset(dataoneway, Group == "Orange")
Group2 <- subset(dataoneway, Group == "Hitam")
Group3 <- subset(dataoneway, Group == "Putih")

qqnorm(Group1$Length)
qqline(Group1$Length)

qqnorm(Group2$Length)
qqline(Group2$Length)

qqnorm(Group3$Length)
qqline(Group3$Length)

#4b
bartlett.test(Length ~ Group, data = dataoneway)

#4c
model1 = lm(Length ~ Group, data = dataoneway)
anova(model1)

#4d

#4e
TukeyHSD(aov(model1))

#4f
library(ggplot2)
ggplot(dataoneway, aes(x = Group, y = Length)) + geom_boxplot(fill = "grey80", colour = "blue") + 
  scale_x_discrete() + xlab("Treatment Group") +  ylab("Length (cm)")



#5
#5a
install.packages("multcompView")
library(readr)
library(ggplot2)
library(multcompView)
library(dplyr)

my.data <- read_csv("GTL.csv")

qplot(x = Temp, y = Light, geom = "point", data = my.data) + facet_grid(.~Glass, labeller = label_both)


#5b
my.data$Glass <- as.factor(my.data$Glass)
my.data$Temp_Factor <- as.factor(my.data$Temp)

anova <- aov(Light ~ Glass*Temp_Factor, data = my.data)
summary(anova)

#5c
data.sum <- group_by(my.data, Glass, Temp) %>%
  summarise(mean = mean(Light), sd = sd(Light)) %>%
  arrange(desc(mean))

print(data.sum)

#5d
TukeyHSD(anova)

#5e
tukey.cld <- multcompLetters4(anova, tukey)
print(tukey.cld)

cld <- as.data.frame.list(tukey.cld$`Glass:Temp_Factor`)
data.sum$Tukey <- cld$Letters
print(data.sum)

write.csv("GTL_sum.csv")



