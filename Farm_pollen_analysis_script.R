#############################################################################################################################
###   Paper title:  Quantifying the production of plant pollen at the farm scale - New Phytologist 2024  ####################
#############################################################################################################################

#The following script provides the code for all figures and data analyses included in the farmland pollen paper

##Install required packages
library(tidyverse)
library(readxl)
library(data.table)
library(skimr)
library(openxlsx)
library(reshape2)
library(tidyr)
library(gridExtra)
library(ggplot2)
library("ggpubr")
library(mgcv) #This is the best package for running GAMs 

# Paths (Edit these before running)
input.path <- "YOUR INPUT FILE PATH HERE"
output.path <- "YOUR OUTPUT FILE PATH HERE"


##Import data
pollen_data <- data.table(read_excel(file.path(input.path,"MASTERSHEET_nectar_pollen_data.xlsx"),
                                              sheet = "species_pollen_km2", 
                                              na = c("", "---", NA))) 

nectar_data <- data.table(read_excel(file.path(input.path,"MASTERSHEET_nectar_pollen_data.xlsx"),
                                     sheet = "species_nectar_km2", 
                                     na = c("", "---", NA))) 


species_FU_values <- data.table(read_excel(file.path(input.path,"MASTERSHEET_nectar_pollen_data.xlsx"),
                                           sheet = "species_info", 
                                           na = c("", "---", NA))) 

raw_phenology_data <- data.table(read_excel(file.path(input.path,"MASTERSHEET_nectar_pollen_data.xlsx"),
                                            sheet = "raw_FU_nectar_pollen_km2", 
                                            na = c("", "---", NA))) 
  

habitat_resource_data <- data.table(read_excel(file.path(input.path,"MASTERSHEET_nectar_pollen_data.xlsx"),
                                               sheet = "habitat data", 
                                               na = c("", "---", NA))) 
  


######################################
### Plotting floral longevity values
######################################

# Specify breaks to start from zero with a binwidth of 1
breaks <- seq(0, 10, by = 0.5)

# Plot the histogram with adjusted bins
floral_lonvevity_hist <- ggplot(species_FU_values, aes(x = floral_longevity_days)) +
  geom_histogram(binwidth = 10, fill = "skyblue", color = "black", breaks = breaks) +
  labs(title = "",
       x = "Floral Longevity (Days)",
       y = "Number of plant species")+
  theme_bw() +  # Set the theme to black and white
  theme(panel.border = element_blank(),  # Remove the panel border
        panel.grid.major = element_line(color = "grey"),  # Set major grid lines color
        panel.grid.minor = element_blank(),  # Remove minor grid lines
        axis.line = element_line(color = "black")) +  # Set color of axis lines
  scale_x_continuous(breaks = seq(0, 10, by = 1))+  # Adjust x-axis breaks
  geom_vline(xintercept = 2.6, linetype = "dashed", color = "darkblue", linewidth = 1) +
  geom_text(aes(x = 2.6, y = 17, label = "Mean = 2.6"), color = "darkblue", vjust = -0.5, hjust = -0.1)
  

ggsave(plot=floral_lonvevity_hist, filename= file.path(output.path,"Floral_longevity_hist.svg"), width=4, height=4, dpi=500, bg="white")
ggsave(plot=floral_lonvevity_hist, filename= file.path(output.path,"Floral_longevity_hist.png"), width=4, height=4, dpi=500, bg="white")



##################################################
### Plotting pollen versus nectar values by floral unit
##################################################
#
#Remove all rows with no pollen information
filtered_data <- species_FU_values[!is.na(species_FU_values$pollen_FU_day_µm3), ]
str(filtered_data)

#Testing the relationship between nectar and pollen
#Check normality of each variable - both need log transforming
hist(log(filtered_data$pollen_FU_day_µm3))
hist(log(filtered_data$nectar_FU_day_µg))

# Log-transform the variables and add a constant value (1) to each value to avoid infinite values resulting from zero values
filtered_data$log_pollen <- log(filtered_data$pollen_FU_day_µm3+1)
filtered_data$log_nectar <- log(filtered_data$nectar_FU_day_µg+1)

# Fit a linear regression model
model <- lm(log_pollen ~ log_nectar, data = filtered_data)

# Summary of the regression model
summary(model)

# Extract coefficients and R squared of the model
coefficients <- coef(model)
rsquared <- summary(model)$r.squared

####Plotting the relationship

#Add a very small constant value to all nectar values to ensure no zero values during log transformation
filtered_data$nectar_FU_day_µg <- filtered_data$nectar_FU_day_µg + 1

# Define the species you want to label
species_to_label <- c("Salix.spp.", "Myosotis.arvensis", "Filipendula.ulmaria", "Galium.aparine", "Galium.odoratum","Allium.ursinum", "Rosa.canina")  # Add your desired species names here

pollen_nectar_FU_plot <- ggplot(filtered_data, aes(x = nectar_FU_day_µg/1000, y = pollen_FU_day_µm3/1000000000)) +
  geom_point() +
  geom_text(data = subset(filtered_data, species %in% species_to_label),
            aes(label = species), vjust = -0.5, hjust = 0, size = 3, fontface = "italic") +  # Adjust label position and size
  geom_smooth(method = "lm", fill = "skyblue", color = "blue", alpha = 0.3) +
  labs(title = "",
       x = expression(bold("Nectar sugar (mg/FU/day)")),
       y = expression(bold("Pollen volume (mm"^3/"FU/day)"))) +
  scale_x_log10(labels = scales::comma_format()) +
  scale_y_log10(labels = scales::comma_format()) +
  theme_bw()+
  theme(panel.border = element_blank(),  # Remove panel border
        axis.line = element_line(size = 0.5, color = "black"),  # Set axis lines
        axis.text = element_text(size = 12),  # Set axis text size
        axis.title = element_text(size = 14, face = "bold"))+  # Set axis title size and bold text
# Add annotations
  annotate("text", x = 0.001, y = 1500, label = paste("Regression equation: ",
                                                     "log(pollen) = ",
                                                     round(coefficients[1], 2), " + ",
                                                     round(coefficients[2], 2), " * log(nectar)", sep = ""),
           size = 4, hjust = 0) +
  annotate("text", x = 0.001, y = 800, label = paste("R-squared: ", round(rsquared, 2)), size = 4, hjust = 0)


ggsave(plot=pollen_nectar_FU_plot, filename= file.path(output.path,"Plots/Pollen_nectar_FU_plot.svg"), width=7, height=5, dpi=500, bg="white")
ggsave(plot=pollen_nectar_FU_plot, filename= file.path(output.path,"Pollen_nectar_FU_plot.png"), width=7, height=5, dpi=500, bg="white")


######################################
### Pollen analysis
######################################

##Calculate proportion pollen contribution for each species & farm
pollen_data_subset <- select(pollen_data, species,	farm, annual_pollen_cm3)
  
pollen_summary <- pollen_data_subset %>%
  group_by(farm) %>%
  mutate(total_pollen = sum(annual_pollen_cm3)) %>%
  ungroup() %>%
  # Create the new column with the proportion
  mutate(prop_contribution = annual_pollen_cm3 / total_pollen)

#Calculate mean and SE for each species
pollen_mean_and_se <- pollen_summary %>% group_by(species) %>%
  summarise(mean_proportion = mean(prop_contribution),
    std_error = sd(prop_contribution) / sqrt(n()))


#Filter to top 30 values
top_20_pollen <- pollen_mean_and_se %>%
  arrange(desc(mean_proportion)) %>%
  head(20)

pollen_contributions <- ggplot(top_20_pollen, aes(x = reorder(species, -mean_proportion), y = mean_proportion * 100)) +
  geom_col() +
  geom_errorbar(aes(ymin = (mean_proportion - std_error) * 100, ymax = (mean_proportion + std_error) * 100), width = 0.2) +
  labs(x = "Species",
       y = "Percentage of total annual pollen supply (%)",
       title = "Pollen") +
  theme_minimal() +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 0.5) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, face = "italic"),
    axis.title.x = element_text(face = "bold"),  # Bold x-axis title
    axis.title.y = element_text(face = "bold"))   # Bold y-axis title
 

######################################
### Nectar analysis
######################################

##Calculate proportion pollen contribution for each species & farm
nectar_data_subset <- select(nectar_data, species,	farm, annual_nectar_grams)

nectar_summary <- nectar_data_subset %>%
  group_by(farm) %>%
  mutate(total_nectar = sum(annual_nectar_grams)) %>%
  ungroup() %>%
  # Create the new column with the proportion
  mutate(prop_contribution = annual_nectar_grams / total_nectar)

#Calculate mean and SE for each species
nectar_mean_and_se <- nectar_summary %>% group_by(species) %>%
  summarise(mean_proportion = mean(prop_contribution),
            std_error = sd(prop_contribution) / sqrt(n()))


#Filter to top 30 values
top_20_nectar <- nectar_mean_and_se %>%
  arrange(desc(mean_proportion)) %>%
  head(20)

nectar_contributions <- ggplot(top_20_nectar, aes(x = reorder(species, -mean_proportion), y = mean_proportion * 100)) +
  geom_col() +
  geom_errorbar(aes(ymin = (mean_proportion - std_error) * 100, ymax = (mean_proportion + std_error) * 100), width = 0.2) +
  labs(x = "Species",
       y = "Percentage of total annual nectar supply (%)",
       title = "Nectar") +
  theme_minimal() +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 0.5) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, face = "italic"),
        axis.title.x = element_text(face = "bold"),  # Bold x-axis title
        axis.title.y = element_text(face = "bold"))   # Bold y-axis title 

pollen_nectar_plots <- grid.arrange(pollen_contributions, nectar_contributions,   ncol = 1, nrow = 2)

ggsave(plot=pollen_nectar_plots, filename= file.path(output.path,"species_pollen_nectar_contributions.svg"), width=6, height=10, dpi=500, bg="white")
ggsave(plot=pollen_nectar_plots, filename= file.path(output.path,"species_pollen_nectar_contributions.png"), width=6, height=10, dpi=500, bg="white")


########################################################
### Plotting relative importance of different habitats
########################################################

#Calculate totals for each habitat
total_resources <- habitat_resource_data %>% group_by(habitat) %>%
  summarise(nectar_grams_m2_habitat = sum(nectar_grams_m2_habitat),
            nectar_grams_m2_std_err = sum(nectar_grams_m2_std_err),
            pollen_cm2_m2_habitat = sum(pollen_cm2_m2_habitat),
            pollen_cm2_m2_std_err = sum(pollen_cm2_m2_std_err),
            nectar_grams_km2_habitat = sum(nectar_grams_km2_habitat),
            nectar_grams_km2_std_err = sum(nectar_grams_km2_std_err),
            pollen_cm3_km2_habitat = sum(pollen_cm3_km2_habitat),
            pollen_cm3_km2_std_err = sum(pollen_cm3_km2_std_err))


df <- total_resources

custom_colors <- c("#ee7621ff", "#f5dc83ff",  "#cdd4dcff", "#8fa33fff")  # Add more colors as needed


# Panel 1: Nectar per m2
plot1 <- ggplot(df, aes(x = habitat, y = nectar_grams_m2_habitat, fill = habitat)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_errorbar(aes(ymin = nectar_grams_m2_habitat - nectar_grams_m2_std_err, 
                    ymax = nectar_grams_m2_habitat + nectar_grams_m2_std_err),
                position = position_dodge(0.9), width = 0.25) +
  ggtitle(expression(Nectar~per~m^2))  +
  labs(x = bquote(""), y = bquote(bold("Total annual nectar sugar (grams / m2)"))) +
  theme_classic(base_size = 20) +
  theme(legend.position = "none") +  # Remove legend
  scale_fill_manual(values = custom_colors)  # Use custom colors

# Panel 2: Nectar per km2
plot2 <- ggplot(df, aes(x = habitat, y = nectar_grams_km2_habitat, fill = habitat)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_errorbar(aes(ymin = nectar_grams_km2_habitat - nectar_grams_km2_std_err, 
                    ymax = nectar_grams_km2_habitat + nectar_grams_km2_std_err),
                position = position_dodge(0.9), width = 0.25) +
  ggtitle(expression(Nectar~per~km^2))  +
  labs(x = bquote(""), y = bquote(bold("Total annual nectar sugar (grams / km2)"))) +
  theme_classic(base_size = 20) +
  theme(legend.position = "none") +  # Remove legend
  scale_fill_manual(values = custom_colors)  # Use custom colors

# Panel 3: Pollen per m2
plot3 <- ggplot(df, aes(x = habitat, y = pollen_cm2_m2_habitat, fill = habitat)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_errorbar(aes(ymin = pollen_cm2_m2_habitat - pollen_cm2_m2_std_err, 
                    ymax = pollen_cm2_m2_habitat + pollen_cm2_m2_std_err),
                position = position_dodge(0.9), width = 0.25) +
  ggtitle(expression(Pollen~per~m^2))  +
  labs(x = bquote(""), y = bquote(bold("Total annual pollen volume (cm3 / m2)"))) +
  theme_classic(base_size = 20) +
  theme(legend.position = "none") +  # Remove legend
  scale_fill_manual(values = custom_colors)  # Use custom colors

# Panel 3: Pollen per m2
plot4 <- ggplot(df, aes(x = habitat, y = pollen_cm3_km2_habitat, fill = habitat)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_errorbar(aes(ymin = pollen_cm3_km2_habitat - pollen_cm3_km2_std_err, 
                    ymax = pollen_cm3_km2_habitat + pollen_cm3_km2_std_err),
                position = position_dodge(0.9), width = 0.25) +
  ggtitle(expression(Pollen~per~km^2))  +
  labs(x = bquote(""), y = bquote(bold("Total annual pollen volume (cm3 / km2)"))) +
  theme_classic(base_size = 20) +
  theme(legend.position = "none") +  # Remove legend
  scale_fill_manual(values = custom_colors)  # Use custom colors


habitat_resource_plots <- grid.arrange(plot1, plot2, plot3, plot4, ncol = 2)

ggsave(plot=habitat_resource_plots, filename= file.path(output.path,"habitat_pollen_nectar_contributions.svg"), width=15, height=15, dpi=500, bg="white")
ggsave(plot=habitat_resource_plots, filename= file.path(output.path,"habitat_pollen_nectar_contributions.png"), width=15, height=15, dpi=500, bg="white")


####################################################################
######## Plotting relative importance of different habitats
####################################################################

##Install required packages
#install.packages("tidyverse")


str(habitat_resource_data)

# Define custom fill colors
custom_colors <- c("#ee7621ff", "#f5dc83ff",  "#cdd4dcff", "#8fa33fff")  # Add more colors as needed

# Nectar m2 plot
nectar_m2_plot <- ggplot(habitat_resource_data, aes(x = day, y = nectar_prop_m2, fill = habitat)) +
  theme_classic(base_size = 22) +
  ggtitle(expression(Nectar~per~m^2)) +
  labs(x = bquote(""), y = bquote("Proportion nectar contribution")) +
  theme(axis.text.x = element_text(color = "black"), axis.text.y = element_text(color = "black")) +
  geom_area() +
  scale_x_continuous(
    name = "Date",
    breaks = c(1, 32, 62, 93, 123, 154, 185, 215, 246),
    labels = c("March", "April", "May", "June", "July", "Aug", "Sept", "Oct", "Nov")
  ) +
  scale_fill_manual(values = custom_colors)  # Use custom colors

# Nectar km2 plot
nectar_km2_plot <- ggplot(habitat_resource_data, aes(x = day, y = nectar_prop_km2, fill = habitat)) +
  theme_classic(base_size = 22) +
  ggtitle(expression(Nectar~per~km^2)) +
  labs(x = bquote(""), y = bquote("Proportion nectar contribution")) +
  theme(axis.text.x = element_text(color = "black"), axis.text.y = element_text(color = "black")) +
  geom_area() +
  scale_x_continuous(
    name = "Date",
    breaks = c(1, 32, 62, 93, 123, 154, 185, 215, 246),
    labels = c("March", "April", "May", "June", "July", "Aug", "Sept", "Oct", "Nov")
  ) +
  scale_fill_manual(values = custom_colors)  # Use custom colors

# Pollen m2 plot
pollen_m2_plot <- ggplot(habitat_resource_data, aes(x = day, y = pollen_prop_m2, fill = habitat)) +
  theme_classic(base_size = 22) +
  ggtitle(expression(Pollen~per~m^2)) +
  labs(x = bquote(""), y = bquote("Proportion pollen contribution")) +
  theme(axis.text.x = element_text(color = "black"), axis.text.y = element_text(color = "black")) +
  geom_area() +
  scale_x_continuous(
    name = "Date",
    breaks = c(1, 32, 62, 93, 123, 154, 185, 215, 246),
    labels = c("March", "April", "May", "June", "July", "Aug", "Sept", "Oct", "Nov")
  ) +
  scale_fill_manual(values = custom_colors)  # Use custom colors

# Pollen km2 plot
pollen_km2_plot <- ggplot(habitat_resource_data, aes(x = day, y = pollen_prop_km2, fill = habitat)) +
  theme_classic(base_size = 22) +
  ggtitle(expression(Pollen~per~km^2)) +
  labs(x = bquote(""), y = bquote("Proportion pollen contribution")) +
  theme(axis.text.x = element_text(color = "black"), axis.text.y = element_text(color = "black")) +
  geom_area() +
  scale_x_continuous(
    name = "Date",
    breaks = c(1, 32, 62, 93, 123, 154, 185, 215, 246),
    labels = c("March", "April", "May", "June", "July", "Aug", "Sept", "Oct", "Nov")
  ) +
  scale_fill_manual(values = custom_colors)  # Use custom colors


panel_plot <- grid.arrange(nectar_m2_plot, nectar_km2_plot, pollen_m2_plot, pollen_km2_plot, ncol = 2, nrow = 2)

ggsave(plot=panel_plot, filename= file.path(output.path,"habitat_resource_plots.svg"), width=20, height=10, dpi=500, bg="white")
ggsave(plot=panel_plot, filename= file.path(output.path,"habitat_resource_plots.png"), width=20, height=10, dpi=500, bg="white")




####################################################################
######## Modelling and plotting nectar and pollen phenology
####################################################################

phenology_data <- raw_phenology_data
str(phenology_data)

####################################################################
################## Running the models  ############################
####################################################################

####### Model for Floral Unit data
FU_model <- gam(total_FU_km2/1000000 + 0.0001 ~ s(day, fx = T, k= 8),  # Floral unit values divided by 1,000,000 to make them more readable
                family = Gamma(link = "log"),
                data = phenology_data)

####### Model for Nectar data
nectar_model <- gam(total_nectar_gram_km2 + 0.0001 ~ s(day, fx = T, k= 14), 
                    family = Gamma(link = "log"),
                    data = phenology_data)
AIC(nectar_model)

####### Model for Floral Unit data
pollen_model <- gam(total_pollen_cm3_km2 + 0.0001 ~ s(day, fx = T, k= 12), 
                    family = Gamma(link = "log"),
                    data = phenology_data)
AIC(pollen_model)

#This creates a model of your dependent variable smoothed over time i.e. it predicts how your dependent variable changes over time from the data you give it
#The '+0.0001' shifts all points very slightly so that they don't overlap and get hidden
#k=8 is the degree of smoothing you choose to put in the model. A high value will allow the line to wiggle a lot whereas a low value will constrain the line. 
#The family and link may need to be played around with and checked for goodness of fit by visually inspecting and checking the AIC values (lower = better fit). For more information, see stats course notes on GAMs

AIC(FU_model) #This will report the Akaike information criterion (AIC) value of your model so that you can compare goodness of fit between models. A lower AIC value generally means a better fit

gam.check(FU_model) #This will show you some summary statistics to test the assumptions of your model e.g. normality of residuals


####################################################################
##################  Plotting the data  #############################
####################################################################

############ Setting up the plot 
svg(file.path(output.path,"Floral resource phenology_all farms.svg"), bg="transparent", width=6,height=9) #Start your plot with this code to print the plot as a scalable vector graphic (svg) which can be edited in Inkscape
par(mfrow=c(3,1)) #This sets how many plots you would like in one window. c(1,1) would give you a single plot whereas c(2,1) would give 2 (one above, one below)

############ Plotting FU values
par(mar=c(5,8,1,1)) #sets the size of the margins around the plot. The first number sets the bottom margin, second is the left, third is top and fourth is right

plot(total_FU_km2/1000000 ~ day, # plotting dependent variable by day with a minor adjustment Jitter shifts points slightly so that they don't overlap and get hidden
     data = phenology_data,
     ylab = "",    #labels y axis  - can be left blank for no label    
     xlab = "Date", # labels x axis - can be left blank for no label 
     cex.lab = 1.8, # size of y and x axis titles
     cex.axis = 1.5, #size of y and x axis marking labels
     font.lab=2, #font type of axis labels. 1=plain, 2=bold, 3=italic, 4=bold-italic
     las = 1,  # Ensure that all the axis labels are horizontal. 2 would be vertical
     col = "black", # colour the dots in black
     pch = 19, # changes the type of dot for datapoints. Look online for different dot styles
     xaxt = "n", # suppresses the x axis labels in case you wanted to specify your own - see below
     xlim=c(-1,230), #specifies the range of x-axis values you want to plot (i.e. days). If left blank, will set automatically
     ylim=c(0,10),#specifies the range of y-axis values you want to plot. If left blank, will set automatically
     bty = "L") # changes the box type around plot to an L rather than square. Other options are "O" "7", "C", "U", or "]" which changes the box to resemble each of these upper case letters. A value of "n" suppresses the box.

title(ylab=bquote("Million floral Units/km"^2*"/day"), line=5, cex.lab=1.5)
title("(a) Floral Units", adj = 1, line = 0, cex.main=2)


#Plotting your own x axis labels for date
axis(side = 1, at = c(1,32,62,93,123,154,185,215,246), #side=1 is the bottom. You now list a string of x-axis values where you would like your labels plotted. My data starts on March 1st which I have labelled in my dataset as Day 1. Therefore 1 corresponds to March, 32 to April etc. 
     labels = c("March","April","May","June","July","Aug","Sept","Oct","Nov"), #you now list the actual labels you would like plotted in the correct order
     cex.axis=1.5) #axis label size

##Now you can use the pred function to plot model predictions over the data i.e. plot your smoothed lines
pdat <- expand.grid(day = seq(0,230,1)) #predicts dependent variable values from the model for each day i.e. Day 0 to day 365 in increments of exactly one day
pred <- predict (FU_model, newdata = pdat, na.rm = T, type= "response", se.fit = TRUE)
predframe_FU <- data.frame (pdat,level=0, preds = pred$fit, se = pred$se.fit)

lines(predframe_FU$preds+predframe_FU$se~predframe_FU$day, lwd=1, lty=2, col="black") #plots the upper standard error curve for the model (as a dotted line)
lines(predframe_FU$preds~predframe_FU$day, lwd=3, col="black") #plots a curve based upon the model's predicted values for each timepoint
lines(predframe_FU$preds-predframe_FU$se~predframe_FU$day, lwd=1, lty=2, col="black") #plots the lower standard error curve for the model (as a dotted line)

############ Plotting nectar values
par(mar=c(5,8,1,1)) #sets the size of the margins around the plot. The first number sets the bottom margin, second is the left, third is top and fourth is right

plot(total_nectar_gram_km2 ~ day, # plotting dependent variable by day with a minor adjustment Jitter shifts points slightly so that they don't overlap and get hidden
     data = phenology_data,
     ylab = "",    #labels y axis  - can be left blank for no label    
     xlab = "Date", # labels x axis - can be left blank for no label 
     cex.lab = 1.8, # size of y and x axis titles
     cex.axis = 1.5, #size of y and x axis marking labels
     font.lab=2, #font type of axis labels. 1=plain, 2=bold, 3=italic, 4=bold-italic
     las = 1,  # Ensure that all the axis labels are horizontal. 2 would be vertical
     col = "black", # colour the dots in black
     pch = 19, # changes the type of dot for datapoints. Look online for different dot styles
     xaxt = "n", # suppresses the x axis labels in case you wanted to specify your own - see below
     xlim=c(-1,230), #specifies the range of x-axis values you want to plot (i.e. days). If left blank, will set automatically
     ylim=c(0,17000),#specifies the range of y-axis values you want to plot. If left blank, will set automatically
     bty = "L") # changes the box type around plot to an L rather than square. Other options are "O" "7", "C", "U", or "]" which changes the box to resemble each of these upper case letters. A value of "n" suppresses the box.

title(ylab=bquote("Nectar sugar/km"^2*"/day (grams)"), line=5, cex.lab=1.5)
title("(b) Nectar", adj = 1, line = -1, cex.main=2)



#Plotting your own x axis labels for date
axis(side = 1, at = c(1,32,62,93,123,154,185,215,246), #side=1 is the bottom. You now list a string of x-axis values where you would like your labels plotted. My data starts on March 1st which I have labelled in my dataset as Day 1. Therefore 1 corresponds to March, 32 to April etc. 
     labels = c("March","April","May","June","July","Aug","Sept","Oct","Nov"), #you now list the actual labels you would like plotted in the correct order
     cex.axis=1.5) #axis label size

##Now you can use the pred function to plot model predictions over the data i.e. plot your smoothed lines
pdat <- expand.grid(day = seq(0,230,1)) #predicts dependent variable values from the model for each day i.e. Day 0 to day 365 in increments of exactly one day
pred <- predict (nectar_model, newdata = pdat, na.rm = T, type= "response", se.fit = TRUE)
predframe_nectar <- data.frame (pdat,level=0, preds = pred$fit, se = pred$se.fit)

lines(predframe_nectar$preds+predframe_nectar$se~predframe_nectar$day, lwd=1, lty=2, col="black") #plots the upper standard error curve for the model (as a dotted line)
lines(predframe_nectar$preds~predframe_nectar$day, lwd=3, col="black") #plots a curve based upon the model's predicted values for each timepoint
lines(predframe_nectar$preds-predframe_nectar$se~predframe_nectar$day, lwd=1, lty=2, col="black") #plots the lower standard error curve for the model (as a dotted line)


############ Plotting pollen values
par(mar=c(5,8,1,1)) #sets the size of the margins around the plot. The first number sets the bottom margin, second is the left, third is top and fourth is right

plot(total_pollen_cm3_km2 ~ day, # plotting dependent variable by day with a minor adjustment Jitter shifts points slightly so that they don't overlap and get hidden
     data = phenology_data,
     ylab = "",    #labels y axis  - can be left blank for no label    
     xlab = "Date", # labels x axis - can be left blank for no label 
     cex.lab = 1.8, # size of y and x axis titles
     cex.axis = 1.5, #size of y and x axis marking labels
     font.lab=2, #font type of axis labels. 1=plain, 2=bold, 3=italic, 4=bold-italic
     las = 1,  # Ensure that all the axis labels are horizontal. 2 would be vertical
     col = "black", # colour the dots in black
     pch = 19, # changes the type of dot for datapoints. Look online for different dot styles
     xaxt = "n", # suppresses the x axis labels in case you wanted to specify your own - see below
     xlim=c(-1,230), #specifies the range of x-axis values you want to plot (i.e. days). If left blank, will set automatically
     ylim=c(0,8000),#specifies the range of y-axis values you want to plot. If left blank, will set automatically
     bty = "L") # changes the box type around plot to an L rather than square. Other options are "O" "7", "C", "U", or "]" which changes the box to resemble each of these upper case letters. A value of "n" suppresses the box.

title(ylab=bquote("Pollen volume/km"^2*"/day (cm"^3*")"), line=5, cex.lab=1.5)
title("(c) Pollen", adj = 1, line = -1, cex.main=2)

#Plotting your own x axis labels for date
axis(side = 1, at = c(1,32,62,93,123,154,185,215,246), #side=1 is the bottom. You now list a string of x-axis values where you would like your labels plotted. My data starts on March 1st which I have labelled in my dataset as Day 1. Therefore 1 corresponds to March, 32 to April etc. 
     labels = c("March","April","May","June","July","Aug","Sept","Oct","Nov"), #you now list the actual labels you would like plotted in the correct order
     cex.axis=1.5) #axis label size

##Now you can use the pred function to plot model predictions over the data i.e. plot your smoothed lines
pdat <- expand.grid(day = seq(0,230,1)) #predicts dependent variable values from the model for each day i.e. Day 0 to day 365 in increments of exactly one day
pred <- predict (pollen_model, newdata = pdat, na.rm = T, type= "response", se.fit = TRUE)
predframe_pollen <- data.frame (pdat,level=0, preds = pred$fit, se = pred$se.fit)

lines(predframe_pollen$preds+predframe_pollen$se~predframe_pollen$day, lwd=1, lty=2, col="black") #plots the upper standard error curve for the model (as a dotted line)
lines(predframe_pollen$preds~predframe_pollen$day, lwd=3, col="black") #plots a curve based upon the model's predicted values for each timepoint
lines(predframe_pollen$preds-predframe_pollen$se~predframe_pollen$day, lwd=1, lty=2, col="black") #plots the lower standard error curve for the model (as a dotted line)

dev.off() #End your plot with this code to tell R that you have finished plotting. The plot will now be exported as an svg to your working directory


#################################################################################################
################  Generating outputs of the model predictions (Preds)  ##########################
#################################################################################################

#write.csv(predframe_FU, "Daily FU phenology.csv") #This csv file will save to your working directory. You can change your working directory by clicking Session > Set working directory > Choose directory

#write.csv(predframe_nectar, "Daily nectar phenology.csv")

#write.csv(predframe_pollen, "Daily pollen phenology.csv")


#######################################################################################
#######            Plotting nectar and pollen values per km2 for each habitat          ############
#######################################################################################

pasture_data <- habitat_resource_data[habitat_resource_data$habitat == 'pasture', ]
hedge_data <- habitat_resource_data[habitat_resource_data$habitat == 'hedge', ]
margin_data <- habitat_resource_data[habitat_resource_data$habitat == 'margin', ]
wood_data <- habitat_resource_data[habitat_resource_data$habitat == 'wood', ]

##Pasture plots
pasture_nectar_plot <- ggplot(pasture_data, aes(x=day, y=nectar_grams_km2_habitat)) + 
  geom_line(size=1.5) +
  geom_line(aes(y = nectar_grams_km2_habitat-nectar_grams_km2_std_err), color = "black", linetype = "dashed", size=1) +
  geom_line(aes(y = nectar_grams_km2_habitat+nectar_grams_km2_std_err), color = "black", linetype = "dashed", size=1) +
  geom_line(aes(y = 0), color = "red", linetype = "dotted", size=1) +
  theme_classic(base_size = 22) + 
  labs(x=bquote(""),
       y=bquote("Sugar/km"^2*"/day (grams)")) + #sets plot theme - there are various other themes available
  theme(axis.text.x = element_text(color="black"),
        axis.text.y = element_text(color="black"))+
  ggtitle("Pasture - nectar")+
  ylim(0,3500)+
  scale_x_continuous(name="Date", breaks=c(1,32,62,93,123,154,185,215,246), labels=c( "March","April","May","June","July","Aug","Sept","Oct","Nov"))

pasture_pollen_plot <- ggplot(pasture_data, aes(x=day, y=pollen_cm3_km2_habitat)) + 
  geom_line(size=1.5) +
  geom_line(aes(y = pollen_cm3_km2_habitat-pollen_cm3_km2_std_err), color = "black", linetype = "dashed", size=1) +
  geom_line(aes(y = pollen_cm3_km2_habitat+pollen_cm3_km2_std_err), color = "black", linetype = "dashed", size=1) +
  geom_line(aes(y = 0), color = "red", linetype = "dotted", size=1) +
  theme_classic(base_size = 22) + 
  labs(x=bquote(""),
       y=bquote("Pollen volume/km"^2*"/day (cm"^3*")")) + #sets plot theme - there are various other themes available
  theme(axis.text.x = element_text(color="black"),
        axis.text.y = element_text(color="black"))+
  ggtitle("Pasture - pollen")+
  ylim(0,2500)+
  scale_x_continuous(name="Date", breaks=c(1,32,62,93,123,154,185,215,246), labels=c( "March","April","May","June","July","Aug","Sept","Oct","Nov"))


##hedge plots
hedge_nectar_plot <- ggplot(hedge_data, aes(x=day, y=nectar_grams_km2_habitat)) + 
  geom_line(size=1.5) +
  geom_line(aes(y = nectar_grams_km2_habitat-nectar_grams_km2_std_err), color = "black", linetype = "dashed", size=1) +
  geom_line(aes(y = nectar_grams_km2_habitat+nectar_grams_km2_std_err), color = "black", linetype = "dashed", size=1) +
  geom_line(aes(y = 0), color = "red", linetype = "dotted", size=1) +
  theme_classic(base_size = 22) + 
  labs(x=bquote(""),
       y=bquote("Sugar/km"^2*"/day (grams)")) + #sets plot theme - there are various other themes available
  theme(axis.text.x = element_text(color="black"),
        axis.text.y = element_text(color="black"))+
  ggtitle("Hedgerow - nectar")+
  ylim(0,500)+
  scale_x_continuous(name="Date", breaks=c(1,32,62,93,123,154,185,215,246), labels=c( "March","April","May","June","July","Aug","Sept","Oct","Nov"))

hedge_pollen_plot <- ggplot(hedge_data, aes(x=day, y=pollen_cm3_km2_habitat)) + 
  geom_line(size=1.5) +
  geom_line(aes(y = pollen_cm3_km2_habitat-pollen_cm3_km2_std_err), color = "black", linetype = "dashed", size=1) +
  geom_line(aes(y = pollen_cm3_km2_habitat+pollen_cm3_km2_std_err), color = "black", linetype = "dashed", size=1) +
  geom_line(aes(y = 0), color = "red", linetype = "dotted", size=1) +
  theme_classic(base_size = 22) + 
  labs(x=bquote(""),
       y=bquote("Pollen volume/km"^2*"/day (cm"^3*")")) + #sets plot theme - there are various other themes available
  theme(axis.text.x = element_text(color="black"),
        axis.text.y = element_text(color="black"))+
  ggtitle("Hedgerow - pollen")+
  ylim(0,700)+
  scale_x_continuous(name="Date", breaks=c(1,32,62,93,123,154,185,215,246), labels=c( "March","April","May","June","July","Aug","Sept","Oct","Nov"))


##margin plots
margin_nectar_plot <- ggplot(margin_data, aes(x=day, y=nectar_grams_km2_habitat)) + 
  geom_line(size=1.5) +
  geom_line(aes(y = nectar_grams_km2_habitat-nectar_grams_km2_std_err), color = "black", linetype = "dashed", size=1) +
  geom_line(aes(y = nectar_grams_km2_habitat+nectar_grams_km2_std_err), color = "black", linetype = "dashed", size=1) +
  geom_line(aes(y = 0), color = "red", linetype = "dotted", size=1) +
  theme_classic(base_size = 22) + 
  labs(x=bquote(""),
       y=bquote("Sugar/km"^2*"/day (grams)")) + #sets plot theme - there are various other themes available
  theme(axis.text.x = element_text(color="black"),
        axis.text.y = element_text(color="black"))+
  ggtitle("Margin - nectar")+
  ylim(0,100)+
  scale_x_continuous(name="Date", breaks=c(1,32,62,93,123,154,185,215,246), labels=c( "March","April","May","June","July","Aug","Sept","Oct","Nov"))

margin_pollen_plot <- ggplot(margin_data, aes(x=day, y=pollen_cm3_km2_habitat)) + 
  geom_line(size=1.5) +
  geom_line(aes(y = pollen_cm3_km2_habitat-pollen_cm3_km2_std_err), color = "black", linetype = "dashed", size=1) +
  geom_line(aes(y = pollen_cm3_km2_habitat+pollen_cm3_km2_std_err), color = "black", linetype = "dashed", size=1) +
  geom_line(aes(y = 0), color = "red", linetype = "dotted", size=1) +
  theme_classic(base_size = 22) + 
  labs(x=bquote(""),
       y=bquote("Pollen volume/km"^2*"/day (cm"^3*")")) + #sets plot theme - there are various other themes available
  theme(axis.text.x = element_text(color="black"),
        axis.text.y = element_text(color="black"))+
  ggtitle("Margin - pollen")+
  ylim(0,100)+
  scale_x_continuous(name="Date", breaks=c(1,32,62,93,123,154,185,215,246), labels=c( "March","April","May","June","July","Aug","Sept","Oct","Nov"))


##wood plots
wood_nectar_plot <- ggplot(wood_data, aes(x=day, y=nectar_grams_km2_habitat)) + 
  geom_line(size=1.5) +
  geom_line(aes(y = nectar_grams_km2_habitat-nectar_grams_km2_std_err), color = "black", linetype = "dashed", size=1) +
  geom_line(aes(y = nectar_grams_km2_habitat+nectar_grams_km2_std_err), color = "black", linetype = "dashed", size=1) +
  geom_line(aes(y = 0), color = "red", linetype = "dotted", size=1) +
  theme_classic(base_size = 22) + 
  labs(x=bquote(""),
       y=bquote("Sugar/km"^2*"/day (grams)")) + #sets plot theme - there are various other themes available
  theme(axis.text.x = element_text(color="black"),
        axis.text.y = element_text(color="black"))+
  ggtitle("Woodland - nectar")+
  ylim(0,6000)+
  scale_x_continuous(name="Date", breaks=c(1,32,62,93,123,154,185,215,246), labels=c( "March","April","May","June","July","Aug","Sept","Oct","Nov"))

wood_pollen_plot <- ggplot(wood_data, aes(x=day, y=pollen_cm3_km2_habitat)) + 
  geom_line(size=1.5) +
  geom_line(aes(y = pollen_cm3_km2_habitat-pollen_cm3_km2_std_err), color = "black", linetype = "dashed", size=1) +
  geom_line(aes(y = pollen_cm3_km2_habitat+pollen_cm3_km2_std_err), color = "black", linetype = "dashed", size=1) +
  geom_line(aes(y = 0), color = "red", linetype = "dotted", size=1) +
  theme_classic(base_size = 22) + 
  labs(x=bquote(""),
       y=bquote("Pollen volume/km"^2*"/day (cm"^3*")")) + #sets plot theme - there are various other themes available
  theme(axis.text.x = element_text(color="black"),
        axis.text.y = element_text(color="black"))+
  ggtitle("Woodland - pollen")+
  ylim(0,1500)+
  scale_x_continuous(name="Date", breaks=c(1,32,62,93,123,154,185,215,246), labels=c( "March","April","May","June","July","Aug","Sept","Oct","Nov"))


habitat_resource_plots <- grid.arrange(pasture_nectar_plot, pasture_pollen_plot,
                                  hedge_nectar_plot, hedge_pollen_plot,
                                  margin_nectar_plot, margin_pollen_plot,
                                  wood_nectar_plot, wood_pollen_plot, nrow = 4,ncol=2)



ggsave(plot=habitat_resource_plots, filename= file.path(output.path,"Habitat_phenology_plots.svg"), width=13, height=18, dpi=500)
ggsave(plot=habitat_resource_plots, filename= file.path(output.path,"Habitat_phenology_plots.png"), width=13, height=18, dpi=500)


##################################################################################################################################################
###############################################                END OF SCRIPT                     #################################################
##################################################################################################################################################
