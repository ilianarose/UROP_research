library(readr)
library(ggplot2)
install.packages('textcat')
library(textcat)

# Read CSV file
file_path <- "C:/Users/ilian/Downloads/UROP R Project/LESO_social_capital_2022.csv"
if (!file.exists(file_path)) {
  stop("File not found! Check the path.")
}

# Load data
new_data <- read_csv(file_path)

# Ensure ec_county is a factor
new_data$ec_county <- as.factor(new_data$ec_county)

# (Outliers are now included; no filtering is performed)

# Scatterplot: Economic Connectedness vs. Weapon Items
ggplot(new_data, aes(x = ec_county, y = num_item_weapon_2022)) +
  geom_point(size = 1, shape = 23, color = "black", fill = "white") +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(title = "Economic Connectedness vs. Weapon Items",
       x = "Economic Connectedness (County)",
       y = "Number of Weapons (2022)") +
  theme_minimal()

# Scatterplot: Clustering County vs. Weapon Items
ggplot(new_data, aes(x = clustering_county, y = num_item_weapon_2022)) +
  geom_point(size = 1, shape = 21, color = "black", fill = "black") +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "Clustering Compared to Number of Weapons",
       x = "Friend Clustering (by County)",
       y = "Number of Weapons per county") +
  theme_minimal() +
  theme(
    text= element_text(family = "serif", size = 10),
  plot.title = element_text(size = 11)
  )
png("Clustering Compared to Number of Weapons", width = 8, 
    height = 6, units = "in", res = 350) 

# Scatterplot: Volunteering Rate vs. Weapon Items
#new_data$log_volrate <- log((new_data$num_item_weapon_2022)+1)

png("Volunteering Rate Compared to Weapons Received", width = 8, 
    height = 6, units = "in", res = 350) 

ggplot(new_data, aes(x = volunteering_rate_county, y = num_item_weapon_2022)) +
  geom_point(size = 1, shape = 21, color = "black", fill = "black") +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "Volunteering Rate Compared to Weapons Received",
       x = "Volunteering Rate (by County)",
       y = "Number of Weapons") +
  theme_minimal() +
  scale_y_continuous(breaks = 10,  )
  theme(
    text= element_text(family = "serif", size = 10),
    plot.title = element_text(size = 11)
  )
dev.off()

model <- lm(num_item_weapon_2022 ~ volunteering_rate_county, data=new_data)
print(summary(model))

