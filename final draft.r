# Load required packages
library(ggplot2)
library(DescTools)

# Read the CSV file
Cars <- read.csv("Cars.csv")

# Randomly sample 1000 rows from the dataset
set.seed(1)
Cars <- Cars[sample(nrow(Cars), 1000), ]

# Clean and transform data
Cars$Body.type <- factor(Cars$Body.type)
Cars$Engine <- as.numeric(gsub("L", "", Cars$Engine))
Cars$Gearbox <- Cars$Gearbox == "Automatic"
Cars$Fuel.type <- factor(Cars$Fuel.type)
Cars$Brand <- gsub(" .*", "", Cars$title)
Cars$Previous.Owners <- ifelse(is.na(Cars$Previous.Owners), 0, Cars$Previous.Owners)
colnames(Cars)[colnames(Cars) == "Gearbox"] <- "Automatic.Gearbox"

# Data exploration and visualization
str(Cars)
table_fuel_type <- table(Cars$Fuel.type)
table_body_type <- table(Cars$Body.type)
Freq(Cars$Mileage.miles)
table(Cars$Brand , Cars$Body.type)

# Descriptive statistics
dim(Cars)
Mode(Cars$Brand)
Mode(Cars$Automatic.Gearbox)
Mode(Cars$Body.type)
Mode(Cars$Fuel.type)
Mode(Cars$Mileage.miles.)
mean(Cars$Mileage.miles.)
median(Cars$Mileage.miles.)
mean(Cars$Price)
median(Cars$Price)
summary(Cars$Price)




# Univariate Plots

#Body type distribution
ggplot(Cars, aes(x = Body.type)) +
  geom_bar(fill = "blue") +
  theme(axis.text.x = element_text(angle = 65, hjust = 1))


# Pie chart: Fuel type distribution
pie(table_fuel_type, labels = paste0(names(table_fuel_type), " (", round(table_fuel_type/sum(table_fuel_type) * 100, 1), "%)"))



# pie plot: Gearbox type distribution
pie(table(Cars$Automatic.Gearbox), labels = paste0(names(table(Cars$Automatic.Gearbox)), " (", round(table(Cars$Automatic.Gearbox)/sum(table(Cars$Automatic.Gearbox)) * 100, 1), "%)"))


# Bar plot: Brand distribution
ggplot(Cars, aes(x = Brand)) +
  geom_bar() +
  theme(axis.text.x = element_text(angle = 65, hjust = 1))

#distribution of miles
ggplot(Cars, aes(x = Mileage.miles.)) +
  geom_histogram(binwidth = 5000, fill = "skyblue", color = "black", alpha = 0.7) +
  labs(title = "Distribution of Mileage",
    x = "Mileage (miles)",
    y = "Frequency") +
  theme_minimal() 


#distribution prices among the data
ggplot(Cars, aes(y = Price)) +
  geom_boxplot(fill = "skyblue", color = "black") +
  labs(title = "Box Plot: Distribution of Price",
       y = "Price") +
  theme_minimal()



# Bivariate Plots

# Bar plot: Automatic Gearbox by Fuel Type
ggplot(Cars, aes(x = Fuel.type, fill = Automatic.Gearbox)) +
  geom_bar(position = "dodge", stat = "count") +
  labs(title = "Clustered Bar Chart: Automatic Gearbox by Fuel Type",
       x = "Fuel Type",
       y = "Count",
       fill = "Automatic Gearbox") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 65, hjust = 1))

# clusterd bar chart: Body type by Fuel type
ggplot(Cars, aes(x = Fuel.type, fill = Body.type)) +
  geom_bar(position = "fill", stat = "count") +
  labs(title = "Clustered Bar Chart: Body Type by Fuel Type",
       x = "Fuel Type",
       y = "Count",
       fill = "Body Type") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 65, hjust = 1))


# Scatter Plot: Price vs. Mileage
ggplot(Cars, aes(x = Mileage.miles., y = Price)) +
  geom_point(color = "blue") +
  labs(title = "Scatter Plot: Price vs. Mileage",
       x = "Mileage (miles)",
       y = "Price") +
  theme_minimal()

# Scatter Plot: Price vs. Mileage
ggplot(Cars, aes(x = Mileage.miles., y = Price, color = as.factor(Previous.Owners))) +
  geom_point(shape = 16, size = 3, alpha = 0.7) +
  labs(title = "Scatter Plot: Price vs. Mileage (colored by previous owners)",
    x = "Mileage",
    y = "Price",
    color = "Previous Owners") +
  theme_minimal()


#Box_plot: Brand Vs price
ggplot(Cars, aes(x = Brand, y = Price)) +
  geom_boxplot(fill = "skyblue", color = "black") +
  labs(title = "Box Plot: Brand Vs Price",
       x = "Brand",
       y = "Price") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 65, hjust = 1))



# Bar plot: Distribution of Price by Body Type
ggplot(Cars, aes(x = Body.type, y = Price, fill = Body.type)) +
  geom_boxplot(alpha = 0.5) +
  labs(title = "Box Plot: Distribution of Price by Body Type",
       x = "Body Type",
       y = "Price") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

  
# Box plot: Fuel Type vs. Engine
ggplot(Cars, aes(x = Fuel.type, y = Engine)) +
  geom_boxplot(fill = "skyblue", color = "black") +
  labs(title = "Box Plot: Fuel Type vs. Engine",
    x = "Fuel Type",
    y = "Engine") +
  theme_minimal()

#scatter plot: Price Vs Registration year 
ggplot(Cars, aes(x = Registration_Year, y = Price)) +
  geom_point(color = "blue") +
  labs(title = "Scatter Plot: Price Vs Year", x = "Registration Year", y = "Price")
