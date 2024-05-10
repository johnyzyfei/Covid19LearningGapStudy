# Read ME
# Import STA304_Group2_Clean_Dataset.csv from Google Sheet
# Please follow the instructions in the terminal and give full access of your Google account
# If you failed to authorize TidyVerse API, you can reopen RStudio and run the file again and choose 0 in
the terminal to generate a new access token
library(readr)
library(googlesheets4)
library(foreign)
library(MASS)
library(Hmisc)
library(reshape2)
library(car)
library(broom)
library(VGAM)
library(forestplot)
library(dplyr)
library(ggplot2)
library(ggpubr)
theme_set(theme_pubr())
path =
  "https://docs.google.com/spreadsheets/d/1mSdhVUqRZIgQaqskdE_cbvpxD29HAk2WNod3Rxxu2PQ/ed
it?usp=sharing"
dataset <- read_sheet(path)
# Define Variables
avg_skip_in_person = dataset$`Avg Freq of Skipping In-Person Lec (Weekly)`
avg_skip_online = dataset$`Avg Freq of Skipping Online Lec (Weekly)`
# Build linear regression model
lr = lm(avg_skip_in_person ~ avg_skip_online)
summary(lr)
anova(lr)

dataset = read.csv("STA304_Group2_Clean_Dataset.csv")
cgpa.during = dataset$cGPA.during.the.Pandemic..2019.Winter...2021.Winter.
cgpa.after = dataset$cGPA.after.the.Pandemic..2021.Spring...2022.Summer.
during <- data.frame(gpa = cgpa.during)
after <- data.frame(gpa = cgpa.after)
during <- transform(
  during,
  category =
    ifelse(
      gpa %in% c("3.0 - 3.5", "3.6 - 4.0"),
      "high",
      "low"
    )
)
after <- transform(
  after,
  category =
    ifelse(
      gpa %in% c("3.0 - 3.5", "3.6 - 4.0"),
      "high",
      "low"
    )
)
during.category <- during$category
after.category <- after$category
#figure 6
barplot(table(during.category),
        ylim = c(0,50),
        xlab = "cGPA",
        ylab = "counts",
        col = "dodgerblue")
abline(h = 0)
#figure 7
barplot(table(after.category),
        ylim = c(0,50),
        xlab = "cGPA",
        ylab = "counts",
        col = "dodgerblue")
abline(h = 0)

dataset <- dataset %>% mutate(cGPA =
                                case_when((cGPA.during.the.Pandemic..2019.Winter...2021.Winter. %in% c("3.0 - 3.5", "3.6 - 4.0"))
                                          & (cGPA.after.the.Pandemic..2021.Spring...2022.Summer. %in% c("3.0 -
3.5", "3.6 - 4.0")) ~ "high to high",
                                          (cGPA.during.the.Pandemic..2019.Winter...2021.Winter. %in% c("1.0 - 1.5",
                                                                                                       "2.0 - 2.5", "2.6 - 2.9"))
                                          & (cGPA.after.the.Pandemic..2021.Spring...2022.Summer. %in% c("3.0 -
3.5", "3.6 - 4.0")) ~ "low to high",
                                          (cGPA.during.the.Pandemic..2019.Winter...2021.Winter. %in% c("3.0 - 3.5",
                                                                                                       "3.6 - 4.0"))
                                          & (cGPA.after.the.Pandemic..2021.Spring...2022.Summer. %in% c("1.0 -
1.5", "2.0 - 2.5", "2.6 - 2.9")) ~ "high to low",
                                          (cGPA.during.the.Pandemic..2019.Winter...2021.Winter. %in% c("1.0 - 1.5",
                                                                                                       "2.0 - 2.5", "2.6 - 2.9"))
                                          & (cGPA.after.the.Pandemic..2021.Spring...2022.Summer. %in% c("1.0 -
1.5", "2.0 - 2.5", "2.6 - 2.9")) ~ "low to low"))
frequency.table <- as.data.frame(table(dataset$cGPA))
frequencies <- frequency.table$Freq
two.by.two.table <- matrix(frequencies, nrow = 2,
                           dimnames = list("After Pandemic" = c("High", "Low"),
                                           "During Pandemic" = c("High", "Low")))
mcnemar.test(two.by.two.table, correct = TRUE)

# Re-leveling
dataset$`Distraction Lv. of Online Lec Compared to In-Person Lec`<- factor(dataset$`Distraction Lv. of
                                                                           Online Lec Compared to In-Person Lec`, levels = c('1', '2', '3', '4', '5' ), labels = c("Poor", "Fair", "Good",
                                                                                                                                                                   "Very good", "Excellent"), ordered = TRUE)
dataset$`Comprehension Lv. of Materials of Online Lecture`<-factor(dataset$`Comprehension Lv. of
                                                                   Materials of Online Lecture`, levels = c('1', '2', '3', '4', '5' ), labels = c("Poor", "Fair", "Good", "Very good",
                                                                                                                                                  "Excellent"), ordered = TRUE)
ggplot(dataset, aes(x = factor(dataset$`Lec Delivery Mode Preference`),
                    fill = factor(dataset$`Study Environment Preference`))) +
  geom_bar(position = "dodge") +
  labs(fill = "Study Environment Preference") +
  xlab("Lec Delivery Mode Preference") +
  ggtitle("Bar Plot of Lec Delivery Mode Preference vs. Study Environment Preference") +
  scale_x_discrete(guide = guide_axis(angle = 10))
factor(dataset$`Comprehension Lv. of Materials of Online Lecture`),
fill = factor(dataset$`Better Lec Quality Period`))) +
  geom_bar(position = "dodge") +
  labs(fill = "Better Lec Quality Period") +
  xlab("Comprehension Lv. of Materials of Online Lecture") +
  ggtitle("Bar Plot of Comprehension Lv. of Materials of Online Lecture and Better Lec Quality Period")
+ scale_x_discrete(guide = guide_axis(angle = 10))

# Linear Reg Plot
plot(avg_skip_in_person ~ avg_skip_online, data = dataset, main="Linear Regression", xlab="Avg Freq
of Skipping Online Lec", ylab="Avg Freq of Skipping In-Person Lec")
abline(lr)
table(dataset$`Better Lec Quality Period`)
Period = c("Pandemic", "Pre-Pandemic", "Same")
Frequency = c(11, 30, 20)
df <- data.frame(Period, Frequency)
ggplot(df, aes(Period, Frequency)) +
  geom_linerange(
    aes(x = Period, ymin = 0, ymax = Frequency),
    color = "lightgray", size = 1.5
  )+
  geom_point(aes(color = Period), size = 2)+
  ggpubr::color_palette("jco")+
  theme_pubclean()

# Read ME
# Import STA304_Group2_Clean_Dataset.csv from Google Sheet
# Please follow the instructions in the terminal and give full access of your Google account
# If you failed to authorize TidyVerse API, you can reopen RStudio and run the file again and choose 0 in
the terminal to generate a new access token
library(readr)
library(googlesheets4)
library(foreign)
library(ggplot2)
library(MASS)
library(Hmisc)
library(reshape2)
library(car)
library(broom)
library(VGAM)
library(forestplot)
library(dplyr)
path =
  "https://docs.google.com/spreadsheets/d/1mSdhVUqRZIgQaqskdE_cbvpxD29HAk2WNod3Rxxu2PQ/ed
it?usp=sharing"
dataset <- read_sheet(path)
# Response Variable
cGPA = dataset$`cGPA during the Pandemic (2019 Winter - 2021 Winter)`
# Predictor Variable
avg_study_time = dataset$`Avg Study Time (During Pandemic)`
avg_freq_skip = dataset$`Avg Freq of Skipping Online Lec (Weekly)`
avg_freq_OH = dataset$`Avg Freq of Participating OH (During Pandemic)`
comp_lv = dataset$`Comprehension Lv. of Materials of Online Lecture`
# Initialize training set
training_set <- data.frame(cGPA, avg_freq_OH, avg_freq_skip, avg_study_time, comp_lv)
training_set$cGPA <- factor(training_set$cGPA, levels = c('3.6 - 4.0', '3.0 - 3.5', '2.6 - 2.9', '2.0 - 2.5', '1.6
- 1.9', '1.0 - 1.5', '0.6 - 0.9', '0.0 - 0.5'), labels = c("Excellent", "Good", "Adeqate", "Poor", "Poor", "Poor",
                                                           "Poor", "Poor"), ordered = TRUE)
training_set$avg_study_time <- factor(training_set$avg_study_time, levels = c('< 3 Hours', '3 - 4 Hours',
                                                                              '4 - 5 Hours', '5 - 6 Hours', '> 6 Hours'), labels = c(': < 3 Hours', ': 3 - 4 Hours', ': 4 - 5 Hours', ': > 5 Hours',
                                                                                                                                     ': > 5 Hours'))
# Check assumption of No Multi-collinearity
olr <- polr(as.factor(cGPA) ~ avg_study_time + avg_freq_skip + avg_freq_OH, data = training_set,
            Hess=TRUE)
vif(olr)
# Single Factor Analysis
study_time_fit = polr(as.factor(cGPA) ~ avg_study_time, data = training_set)
freq_skip_fit = polr(as.factor(cGPA) ~ avg_freq_skip, data = training_set)
freq_OH_fit = polr(as.factor(cGPA) ~ avg_freq_OH, data = training_set)
drop1(study_time_fit,test = "Chi")
drop1(freq_skip_fit,test = "Chi")
drop1(freq_OH_fit,test = "Chi")
# Confidence Interval of Coef
confint(study_time_fit, level = 0.95)
coef(summary(study_time_fit))
confint(freq_skip_fit, level = 0.95)
coef(summary(freq_skip_fit))
confint(freq_OH_fit, level = 0.95)
coef(summary(freq_OH_fit))
base_data <- tibble::tibble(mean = c(0.3766106, 1.0517438, -2.0211190, 0.1033543, 0.2040784),
                            lower = c(-0.8527867, -0.1937557, -3.6680004, -0.1368487, -0.2317010),
                            upper = c(1.6107616, 2.3431600, -0.5102267, 0.3374198, 0.6477571),
                            coef_name = c("avg_study_time: 3 - 4 Hours", "avg_study_time: 4 - 5 Hours",
                                          "avg_study_time: > 5 Hours", "avg_freq_skip",
                                          "avg_freq_OH"),
                            coef = c('0.3766106', '1.0517438', '-2.0211190', '0.1033543', '0.2040784'),
                            LCL = c('-0.8527867', '-0.1937557', '-3.6680004', '-0.1368487', '0.2317010'),
                            UCL = c('1.6107616', '2.3431600', '-0.5102267', '0.3374198', '0.6477571'))
base_data |>
  forestplot(labeltext = c(coef_name, LCL, coef, UCL),
             xlog = FALSE) |>
  fp_set_style(box = "royalblue",
               line = "darkblue",
               summary = "royalblue") |>
  fp_add_header(coef_name = c("", "Variable"),
                coef = c("", "Coefficient"),
                LCL = c("", "LCL"),
                UCL = c("", "UCL")) |>
  fp_set_zebra_style("#EFEFEF")
# Proportional Odds Assumption
olr <- polr(as.factor(cGPA) ~ avg_study_time, data = training_set, Hess=TRUE)
brant::brant(olr)
summary(olr)
exp(cbind(OR = coef(olr), confint(olr)))


#Hypothesis Test
STA304_Group2_Clean_Dataset <- read.csv("~/Desktop/STA304_Group2_Clean_Dataset.csv",
                                        header=TRUE)
online_avg=c(STA304_Group2_Clean_Dataset$Avg.Freq.of.Skipping.Online.Lec..Weekly.)
inperson_avg=c(STA304_Group2_Clean_Dataset$Avg.Freq.of.Skipping.In.Person.Lec..Weekly.)
difference=online_avg-inperson_avg
#Histogram
hist(difference_in_means, prob = TRUE, main = "Histogram with normal curve")
x = seq(min(difference_in_means), max(difference_in_means), length = 40)
f = dnorm(x, mean = mean(difference_in_means), sd = sd(difference_in_means))
lines(x, f, col = "red", lwd = 2)
  
