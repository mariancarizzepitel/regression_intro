library(tidyverse)
library(apaTables)

#I. Load population data
population_data <- read_csv("population_data.csv")

# View data 
glimpse(population_data) # 2 variables (performance, IQ), N = 10000

#II. Get sample data - random sample from population 
set.seed(1) # only b/c we're doing activity as group, makes sure my sample is same as David's 
sample1_analytic_data <- sample_n(population_data, size=200)
glimpse(sample1_analytic_data) 

#III. Run a regression analysis 
sample1_lm_results <- lm(performance ~ IQ +1, data=sample1_analytic_data)
## IQ indicates that IQ is predictor 
## +1 is to tell R, "btw, I want an intercept". It is implicit so  OKAY TO LEAVE OFF.  
## -1 is asking for no intercept 
summary(sample1_lm_results) # shows regression results
## intercept/elevation of line : (Intercept) 50.59668  
## slope/angle of line : IQ           0.24233

# APA format of regression summary
apa.reg.table(sample1_lm_results)
## overall fit (overall R squared) =  R2 = .304**
## b weight (looking at 2nd row): .24 [.19,.29]

#IV. Run regression analysis on population - only for simulation, never possible in reality 
population_lm_results <- lm(performance ~ IQ, data=population_data)
summary(population_lm_results) # no need for apa reg table b/c population parameter (no need for CI)
## b weight : .20 
## compare with sample results - CI captures population effect 



#PREDICTING VALUES using REGRESSION 

# predicted value for a single person 
x_axis_range <- data.frame(IQ=c(120)) # apply regression equation we created (e.g., sample1_lm_results) with new data (e.g., x_axis_range) using predict function
CI_data <- predict(sample1_lm_results, newdata=x_axis_range,interval="confidence", level=.95)
CI_data <- as.data.frame(cbind(x_axis_range, CI_data))
print(CI_data)
# fit : 79.68
# lwr: 78.42
# upr: 80.93
# conclusion : best guess for population Y mean at given X is 79.68

# predicted value for entire X axis range 
min_predictor <- min(sample1_analytic_data$IQ) # 67 : minimum value of IQ column 
max_predictor <- max(sample1_analytic_data$IQ) # 142.3 : maximum value of IQ column 
x_axis_range_line <- data.frame(IQ=seq(min_predictor,max_predictor,by=.5)) # kinda re-arranges values at specific intervals (e.g., .5)
CI_data_line <- predict(sample1_lm_results, newdata=x_axis_range_line,interval="confidence", level=.95)
CI_data_line <- as.data.frame(cbind(x_axis_range_line, CI_data_line))
print(CI_data_line) 
head(CI_data_line) #shows first few upper values 

# prediction intervals 
PI_data_line <- predict(sample1_lm_results, newdata=x_axis_range_line,interval="prediction", level=.95)
PI_data_line <- as.data.frame(cbind(x_axis_range_line, PI_data_line))
head(PI_data_line) #shows first few upper values; compare with CI for entire X axis range 

# plotting the data for sample size we plugged out of pop, with n=200, with CI ON LINE
reg_plot <- ggplot(sample1_analytic_data, aes(x=IQ, y=performance))
reg_plot <- reg_plot + geom_point()
reg_plot <- reg_plot + theme_classic() #apa style 
reg_plot <- reg_plot + geom_smooth(data=CI_data_line,aes(x=IQ, y=fit, ymin=lwr, ymax=upr), stat="identity")
# fit instead of perf, using CI data line set; ymin + ymax for lower and upper limits
print(reg_plot) 
# scatter plot with regression line and CI on the whole regression line 

# plotting EXACT SAME data with simple addition of PI (so CI-line + PI)
reg_plot <- ggplot(sample1_analytic_data, aes(x=IQ, y=performance))
reg_plot <- reg_plot + geom_point()
reg_plot <- reg_plot + theme_classic() #apa style 
reg_plot <- reg_plot + geom_smooth(data=CI_data_line,
                                   aes(x=IQ, y=fit, ymin=lwr, ymax=upr), 
                                   stat="identity") #longer way- sometimes necessary 
                                   ##if David wants us to look into CI line data 
reg_plot <- reg_plot + geom_smooth(data=PI_data_line,aes(x=IQ, y=fit, ymin=lwr, ymax=upr), stat="identity")
print(reg_plot) 

# more simple way of plotting CI 
reg_plot <- ggplot(sample1_analytic_data, aes(x=IQ, y=performance))
reg_plot <- reg_plot + geom_point()
reg_plot <- reg_plot + theme_classic() #apa style 
reg_plot <- reg_plot + geom_smooth(method="lm", se=TRUE) # built in SHORTCUT to get CI-line shading 
reg_plot <- reg_plot + geom_smooth(data=PI_data_line,aes(x=IQ, y=fit, ymin=lwr, ymax=upr), stat="identity")
print(reg_plot) 

