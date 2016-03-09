setwd("C:/Recovered Files/Krishna/PGPBA/Data Mining/Assignment 1")

Attrition = read.csv("HR_Employee_Attrition_Data.csv", header = TRUE, stringsAsFactors = FALSE)

library(ggplot2)

ggplot(Attrition, aes(MonthlyIncome, Age)) + geom_point(aes(color=Attrition))

tab1 = prop.table(table(Attrition$YearsAtCompany, Attrition$Attrition))  
tab1 = as.data.frame(tab1)
tab2 = as.data.frame(tab2)

tab2 = prop.table(table(Attrition$MonthlyIncome, Attrition$Attrition))  

ggplot(Attrition, aes(tab1, tab2)) + geom_point(aes(color=Attrition))


### Multiple Scatter Plots

ggplot(Attrition, aes(MonthlyIncome, Age)) +
  geom_point(aes(color=JobRole)) +
  geom_point(aes(color=Attrition)) +
  facet_wrap(~JobRole)


### Histogram

ggplot(Attrition, aes(x=MonthlyIncome, fill=Attrition)) +
  geom_histogram(binwidth = 1000)


### Histogram for MonthlyIncome
ggplot(Attrition, aes(x=MonthlyIncome, fill=Attrition)) +
  geom_histogram(aes(y = 100*((..count..)/sum(..count..))), binwidth = 1000, color ="black") +
  labs(y="Percent")

### Histogram for Age
ggplot(Attrition, aes(x=Age, fill=Attrition)) +
  geom_histogram(aes(y = 100*((..count..)/sum(..count..))), binwidth = 5, color="black") +
  scale_x_continuous(breaks = seq(0,80,10)) +
  labs(y="Percent")

### Heat Map
ggplot(Attrition, aes(x=Age, y=JobRole)) +
  geom_raster(aes(fill=MonthlyIncome)) +
  labs(title = "Heat Map", x="Age", y = "Job Role") +
  scale_fill_continuous(name="MonthlyIncome")

## Correlogram
corrgram(Attrition, order=NULL,
         panel=panel.shade, text.panel = panel.txt,
         main="Correlogram")

## Tabplot
Attr <- as.data.frame(sapply(Attrition,function(col){
  if(class(col)=="character"){
    col <- as.factor(col)
  }
  col
})
)
tableplot(Attr)


data("iris")
tableplot(diamonds)

library(tabplot)
install_github("tabplot", username="mtennekes", subdir="pkg")
