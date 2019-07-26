library(ggplot2)
library(dplyr)

Stroke <- read.csv("/Users/chenlin/Downloads/train_2v.csv")
summary(Stroke)
Stroke <- Stroke %>% filter(gender !='Other') %>% select(-id)


#convert some variables to factors
Stroke$gender <- as.factor(Stroke$gender)
Stroke$hypertension <- as.factor(Stroke$hypertension)
Stroke$heart_disease <- as.factor(Stroke$heart_disease)
Stroke$ever_married <- as.factor(Stroke$ever_married)
Stroke$work_type <- as.factor(Stroke$work_type)
Stroke$Residence_type <- as.factor(Stroke$Residence_type)
Stroke$stroke <- as.factor(Stroke$stroke)

attach(Stroke)

ggplot(Stroke, aes(x=stroke)) + 
  geom_bar() + 
  theme_bw()

#First question? Which factor is related to stroke(just to test single factor)
#gender?
ggplot(Stroke, aes(x=gender, fill=stroke)) +
  theme_bw() +
  geom_bar() +
  labs(y = 'stroke people count',
       x = 'gender',
       title ='gender distribution for stroke')

#age?
ggplot(Stroke, aes(x=age, fill=stroke)) +
  theme_bw() +
  geom_density(alpha=0.5) +
  labs(y = 'stroke people count',
       x = 'age',
       title ='age distribution for stroke')

#hypertension
ggplot(Stroke, aes(x=hypertension, fill=stroke)) +
  theme_bw() +
  geom_bar() +
  labs(y = 'stroke people count',
       x = 'hypertension',
       title ='hypertension distribution for stroke')

#heart_disease
ggplot(Stroke, aes(x=heart_disease, fill=stroke)) +
  theme_bw() +
  geom_bar() +
  labs(y = 'stroke people count',
       x = 'heart_disease',
       title ='heart_disease distribution for stroke')

percent_heart_disease=prop.table(table(Stroke$stroke, Stroke$heart_disease))
percent_heart_disease[2, ] / percent_heart_disease[1, ] 



#ever_married
ggplot(Stroke, aes(x=ever_married, fill=stroke)) +
  theme_bw() +
  geom_bar() +
  labs(y = 'stroke people count',
       x = 'ever_married',
       title ='ever_married distribution for stroke')

percent_ever_married=prop.table(table(Stroke$stroke, Stroke$ever_married))
percent_ever_married[2, ] / percent_ever_married[1, ] #

#work_type
ggplot(Stroke, aes(x=work_type, fill=stroke)) +
  theme_bw() +
  geom_bar() +
  labs(y = 'stroke people count',
       x = 'work_type',
       title ='work_type distribution for stroke')

percent_work_type =prop.table(table(Stroke$stroke, Stroke$work_type))
percent_work_type[2, ] / percent_work_type[1, ]

#Residence_type
#No difference
ggplot(Stroke, aes(x=Residence_type, fill=stroke)) +
  theme_bw() +
  geom_bar() +
  labs(y = 'stroke people count',
       x = 'Residence',
       title ='Residence distribution for stroke')

percent_Residence_type =prop.table(table(Stroke$stroke, Stroke$Residence_type))
percent_Residence_type[2, ] / percent_Residence_type[1, ]

#avg_glucose_level
#eyeballing: no difference
ggplot(Stroke, aes(x=avg_glucose_level, fill=stroke)) +
  theme_bw() +
  geom_histogram(binwidth = 5, alpha = 0.5) +
  labs(y = 'stroke people count',
       x = 'avg_glucose_level',
       title ='Glucose distribution for stroke')

#bmi
#eyeballing: no difference
ggplot(Stroke, aes(x=bmi, fill=stroke)) +
  theme_bw() +
  geom_histogram(binwidth = 5, alpha = 0.5) +
  labs(y = 'stroke people count',
       x = 'bmi_glucose_level',
       title ='bmi distribution for stroke')

#Age with gender together
ggplot(Stroke, aes(x = age, fill=stroke)) + 
  theme_bw() + 
  facet_wrap(~gender) +
  geom_histogram(binwidth = 5) +
  labs(y ='Passenger Count',
       x = 'Age (binwidth = 5)', 
       title = 'Titanic Age Distribution')


#################
# distribution
distribution_ratio  <- function(x){
  ## Function creates a stacked bar plots for stroke outcome and ratio by binomial variables (0/1)  
  plt <- Stroke %>% 
    select(stroke, x, gender) %>%
    group_by(stroke, var = eval(parse(text = x))) %>% 
    summarise(count = length(gender)) %>%
    group_by(stroke) %>%
    mutate(ratio = round(count*100/sum(count), 1)) %>%
    ggplot(aes(y = ratio, x = stroke, fill = var)) + 
    geom_bar(stat="identity") +
    labs(title=paste0("Ratio of stroke outcome by ", x), fill = x) +
    theme_bw()
  return(plt)
}
distribution_ratio("hypertension")
distribution_ratio("ever_married")
distribution_ratio("Residence_type")
distribution_ratio("work_type")
distribution_ratio("smoking_status")














