#1
my_data <- ToothGrowth
#install.packages("dplyr")
library(dplyr)
set.seed(1234)
dplyr::sample_n(my_data, 10)

#2
str(my_data)
# Convert dose jadi factor dan recode levelnya
# as "D0.5", "D1", "D2"
my_data$dose <- factor(my_data$dose, 
                       levels = c(0.5, 1, 2),
                       labels = c("D0.5", "D1", "D2"))
head(my_data)
table(my_data$supp, my_data$dose)

#2.5
require("dplyr")
group_by(my_data, supp, dose) %>%
  summarise(
    count = n(),
    mean = mean(len, na.rm = TRUE),
    sd = sd(len, na.rm = TRUE)
  )

#3
#visualisasi data -1-
# Box plot dengan dua variabel faktor
boxplot(len ~ supp * dose, data=my_data, frame = FALSE, 
        col = c("#00AFBB", "#E7B800"), ylab="Tooth Length")
# Two-way interaction plot
interaction.plot(x.factor = my_data$dose, trace.factor = my_data$supp, 
                 response = my_data$len, fun = mean, 
                 type = "b", legend = TRUE, 
                 xlab = "Dose", ylab="Tooth Length",
                 pch=c(1,19), col = c("#00AFBB", "#E7B800"))

#visualisasi data -2-
# Install
#install.packages("ggpubr")
# Box plot dengan multiple groups
library("ggpubr")
ggboxplot(my_data, x = "dose", y = "len", color = "supp",
          palette = c("#00AFBB", "#E7B800"))
# Line plots dengan multiple groups
ggline(my_data, x = "dose", y = "len", color = "supp",
       add = c("mean_se", "dotplot"),
       palette = c("#00AFBB", "#E7B800"))

#4
res.aov2 <- aov(len ~ supp + dose, data = my_data)
summary(res.aov2)
# Two-way ANOVA with interaction effect
# These two calls are equivalent
res.aov3 <- aov(len ~ supp * dose, data = my_data)
res.aov3 <- aov(len ~ supp + dose + supp:dose, data = my_data)
summary(res.aov3)

#5
TukeyHSD(res.aov3, which = "dose")
pairwise.t.test(my_data$len, my_data$dose,
                p.adjust.method = "BH")

#6
#Homogenitas varians
plot(res.aov3, 1)
#Bartlett lavene
library(car)
leveneTest(len ~ supp*dose, data = my_data)

#7
#Normalitas
plot(res.aov3, 2)
#saphiro-wilk test
# Ekstrak residu
aov_residuals <- residuals(object = res.aov3 )
# Uji Shapiro-Wilk
shapiro.test(x = aov_residuals )

#8
library(car)
my_anova <- aov(len ~ supp * dose, data = my_data)
Anova(my_anova, type = "III")
