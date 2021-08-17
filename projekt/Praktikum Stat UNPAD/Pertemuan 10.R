#1
#install.packages("dplyr")
#library(dplyr)

my_data <- PlantGrowth

#2
# lihat random sample
set.seed(1234)
dplyr::sample_n(my_data, 10)

#3
# melihat atribut data
levels(my_data$group)

my_data$group <- ordered(my_data$group,
                         levels = c("ctrl", "trt1", "trt2"))

#4
library(dplyr)
group_by(my_data, group) %>%
  summarise(
    count = n(),
    mean = mean(weight, na.rm = TRUE),
    sd = sd(weight, na.rm = TRUE)
  )

#5
# Install 1
#if(!require(devtools)) install.packages("devtools")
#devtools::install_github("kassambara/ggpubr")
# Install 2
#install.packages("ggpubr")

# Box plots
# ++++++++++++++++++++
# Plot bobot by group dan warna by group
library("ggpubr")
ggboxplot(my_data, x = "group", y = "weight", 
          color = "group", palette = c("#00AFBB", "#E7B800", "#FC4E07"),
          order = c("ctrl", "trt1", "trt2"),
          ylab = "Weight", xlab = "Treatment")

#6
# Mean plots
# ++++++++++++++++++++
# Plot bobot by group
# Tambah error bars: mean_se
library("ggpubr")
ggline(my_data, x = "group", y = "weight", 
       add = c("mean_se", "jitter"), 
       order = c("ctrl", "trt1", "trt2"),
       ylab = "Weight", xlab = "Treatment")

#7
# Box plot
boxplot(weight ~ group, data = my_data,
        xlab = "Treatment", ylab = "Weight",
        frame = FALSE, col = c("#00AFBB", "#E7B800", "#FC4E07"))
# plotmeans
install.packages("gplots")
library("gplots")
plotmeans(weight ~ group, data = my_data, frame = FALSE,
          xlab = "Treatment", ylab = "Weight",
          main="Mean Plot with 95% CI")

#8
# Hitung ANOVA
res.aov <- aov(weight ~ group, data = my_data)
# Summary analysis
summary(res.aov)

#9
TukeyHSD(res.aov)

#10
pairwise.t.test(my_data$weight, my_data$group,
                p.adjust.method = "BH")

#11
# 1. homogenitas varians
plot(res.aov, 1)

#12
install.packages("car")
library(car)
leveneTest(weight ~ group, data = my_data)

#13
#anova
oneway.test(weight ~ group, data = my_data)
#pairwise
pairwise.t.test(my_data$weight, my_data$group,
                p.adjust.method = "BH", pool.sd = FALSE)

#14
# 2. Normality
plot(res.aov, 2)

#15
# Exkstrak residu
aov_residuals <- residuals(object = res.aov )
# Uji Shapiro-Wilk
shapiro.test(x = aov_residuals )

#16
kruskal.test(weight ~ group, data = my_data)