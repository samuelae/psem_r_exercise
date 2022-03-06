# Dependencies -----
library(tidyverse)

# Aufgabe 1 -----
df <- read.table("practice_data.csv", sep = ";")
df <- read_delim("practice_data.csv", delim = ";")
df

# Aufgabe 2 -----

# a) Stichprobengrösse
nrow(df)
df %>% 
  count()

# b) Prozentualer Anteil Frauen / Männer
table(df$gender)
prop.table(table(df$gender))*100
df %>% 
  group_by(gender) %>% 
  count() %>% 
  mutate(percent = n / nrow(df) * 100)

# c) NAs
sum(is.na(df$gender))
sum(is.na(df$gender)) / nrow(df) * 100
df %>% 
  group_by(gender) %>% 
  count() %>% 
  mutate(percent = n / nrow(df) * 100)

# Aufgabe 3 -----

# a) how many did not finish
sum(df$progress < 100)
df %>% 
  filter(progress < 100) %>% 
  count()

# b) dataset of those that finished
dc <- df[df$progress == 100, ]
dim(dc)
dc <- df %>% 
  filter(progress == 100)
dc

# Aufgabe 4 -----

# a) Geschlecht
table(dc$gender, useNA = "always")
dc %>% 
  group_by(gender) %>% 
  count()

# b) language
table(dc$language, useNA = "always")
dc %>% 
  group_by(language) %>% 
  count()

# c) email
table(dc$success, useNA = "always")
dc %>% 
  group_by(success) %>% 
  count()

# d) Verteilung Alter
mean(dc$age)
median(dc$age)
sd(dc$age)
min(dc$age)
max(dc$age)
dc %>% 
  summarise(mean = mean(age),
            median = median(age),
            sd = sd(age),
            min = min(age),
            max = max(age))

# Aufgabe 5 -----

# a) Probanden pro Bedingung
table(dc$condition)
dc %>% 
  group_by(condition) %>% 
  count()

# b) Geschlecht pro Bedingung
table(dc$condition, dc$gender)
dc %>% 
  group_by(condition, gender) %>% 
  count()

# c) E-Mail pro Bedingung
table(dc$condition, dc$success)
dc %>% 
  group_by(condition, success) %>% 
  count()

# d) Klicks auf Seite 1
mean(dc$clicks_p1)
mean(dc[dc$condition == "music", ]$clicks_p1)
mean(dc[dc$condition == "silence", ]$clicks_p1)
sd(dc[dc$condition == "music", ]$clicks_p1)
sd(dc[dc$condition == "silence", ]$clicks_p1)
median(dc[dc$condition == "music", ]$clicks_p1)
median(dc[dc$condition == "silence", ]$clicks_p1)
for (fun in c("mean", "sd", "median")) {
  print(tapply(FUN = fun,
               X = dc$clicks_p1,
               INDEX = dc$condition))
}
dc %>% 
  summarize(mean_clicks_p1 = mean(clicks_p1))
dc %>% 
  group_by(condition) %>% 
  summarize(mean = mean(clicks_p1),
            sd = sd(clicks_p1),
            median = median(clicks_p1))

# Aufgabe 6 -----

# a) test ob Musik Einfluss auf E-Mail 
table(dc$condition, dc$success)
chisq.test(x = dc$condition, 
           y = dc$success)

# b) test ob Gender Einfluss auf E-Mail
table(dc$gender, dc$success)
chisq.test(x = dc$gender, 
           y = dc$success)

# c) test ob Condition Einfluss auf Clicks Seite 1
dc %>% 
  group_by(condition) %>% 
  summarize(mean(clicks_p1))
t.test(clicks_p1 ~ condition, data = dc)

# Aufgabe 7 -----
# Plot mit Anzahl Personen die Email geschickt haben pro bedingung
df %>% 
  ggplot() +
  geom_col(mapping = aes(x = condition,
                         y = success),
           fill = "#651e3e") +
  labs(title = "Aufgabe 7",
       subtitle = "Anzahl versendete E-Mails pro Versuchsbedingung",
       x = "Versuchsbedingung",
       y = "Anzahl versendete E-Mails") +
  theme_minimal()




