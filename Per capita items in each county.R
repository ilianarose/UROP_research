#county v weapons scatterplot (per capita items)
library(readr)
#print(file.exists("C:/Users/ilian/Downloads/UROP R Project/LESO_social_capital_2022.csv"))
new_data <-(read_csv("C:/Users/ilian/Downloads/UROP R Project/LESO_social_capital_2022.csv"))
new_data$tot_pop_cty <- as.factor(new_data$tot_pop_cty)
head(new_data)
library(ggplot2)
ggplot(new_data, 
       aes(x=tot_pop_cty, y=num_item_weapon_2022)) +
         geom_point(size=2,shape=23) +
         geom_text(aes(label=state),
                   vjust = -0.5)

#scatterplot clustering_county
library(ggplot2)
ggplot(new_data, 
       aes(x=clustering_county, y=num_item_weapon_2022)) +
  geom_point(size=2,shape=23) +
  geom_text(aes(label=state),
            vjust = -0.5)
#scatterplot volunteering_rate_county
library(ggplot2)
ggplot(new_data, 
       aes(x=volunteering_rate_county, y=num_item_weapon_2022)) +
  geom_point(size=2,shape=23) +
  geom_text(aes(label=state),
            vjust = -0.5)

