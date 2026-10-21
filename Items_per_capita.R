#setting up libraries and making file path 
library(ggplot2)

file_path <- ("C:\\Users\\ilian\\Downloads\\UROP R Project\\LESO_social_capital_2022.csv")
if (!file.exists(file_path)) {
  stop("File not found! Check the path.")
}
descstats <- read.csv(file_path, header = TRUE)

print(colnames(descstats))

#Population variable 
county_population <- (descstats$tot_pop_cty)

#Number of items total variable
num_items <- (descstats$tot_num_item_2022)
#Number of weapons only variable 
num_weapons <- (descstats$num_item_weapon_2022)

#per-capita variable 
#total items 
per_capita_total <- {
  for (i in 1:length(county_population) { 
    total <- num_items[i] / county_population[i]
    return(total)
    }
}
#print(per_capita_total)
#weapons only 
per_capita_weapons <- {
  for (i i 1:length(county_population){
    total_w <- num_weapons[i] / county_population[i]
    return(total_w)
  }
}
#print(per_capita_weapons)
#Making the histogram for total items 
ggplot(data = data.frame(descstats), aes(x = per_capita_total)) +
geom_histogram(binwidth = 0.001, color = 'grey', fill = 'red') + 
scale_x_continuous(limits = c(0, 0.02) , breaks = seq(0, 0.02, by = 0.001)) +
                       labs( x = 'Items Per-Capita', y = 'Frequency') +
                       ggtitle('Common Items Per-Capita') + 
scale_y_continuous(limits = c(0,1000), breaks = seq(0, 1000, by = 100))


#Histogram for weapons per-capita 
ggplot(data = data.frame(descstats), aes(x = per_capita_weapons)) +
  geom_histogram(binwidth = 0.001, color = 'darkgrey', fill = 'red') + 
  scale_x_continuous(limits = c(0, 0.02) , breaks = seq(0, 0.02, by = 0.005)) +
  labs( x = 'Number of Weapons Per-Capita (by County)', y = 'Number of Counties (log scale)') +
  ggtitle('Frequency of Number of Weapons Per-Capita') + 
  scale_y_continuous(trans = "log1p", breaks = c(1, 10, 50, 100, 250, 500),
                     limits = c(0,625) #breaks = seq(0, 625, by = 100)
                     ) +
theme_minimal() +
  theme(
    text= element_text(family = "serif", size = 12),
    plot.title = element_text(size = 13),
    panel.background = element_rect(fill = "lightblue", color = NA),
    plot.background = element_rect(fill = "gray90", color = NA) 
  )
png("Frequency of Number of Weapons Per-Capita", width = 8, 
    height = 6, units = "in", res = 350) 


#Most and least weapons per capita counties 
print(max(per_capita_weapons, na.rm = FALSE))
typeof(per_capita_weapons)

