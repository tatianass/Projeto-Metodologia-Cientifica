data <- read.csv("result.csv", header = TRUE, sep = ";")

#visualizing data
stripchart(time~type,
           data=data,
           main="Different strip chart for each type",
           xlab="Type",
           ylab="Time",
           col="brown3",
           vertical=TRUE,
           pch=19,
           method = "jitter", jitter = 0.004
)

#fitting data to anova
analysis <- lm(time~type,data=data)

#anova
#h0: they are equal, h1: they are not (p<0.05)
anova <- aov(analysis)
summary(anova)

#Caso queira visualizar o gráfico
plot(analysis, which = 1)

#can se that there's a positive skewness in the data
plot(analysis, which = 2)

sresids <- rstandard(analysis)
hist(sresids)

#to know the variable that are significantly different
# significant difference p < 0.05
#there's just two types, so no need for turkey's test
#TukeyHSD(anova)

