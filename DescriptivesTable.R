#Descriptive Statistics Table 
#install.packages("readxl")
#install.packages("modeest")
#install.packages("tableone")
#install.packages("dplyr")
#install.packages("DescTools")
#install.packages("gt")

library(readxl)
library(modeest)
library(tableone)
library(dplyr)
library(DescTools)
library(gt)

#Calling in Data 
file_path <- ("C:\\Users\\ilian\\Downloads\\UROP R Project\\LESO_social_capital_2022.csv")
if (!file.exists(file_path)) {
  stop("File not found! Check the path.")
}
descstats <- read.csv(file_path, header = TRUE)

print(colnames(descstats))

#Averages: 
descstats$tot_pop_cty <- as.numeric(as.character(descstats$tot_pop_cty))
mean_pop <- mean(descstats$tot_pop_cty, na.rm = TRUE)

descstats$num_item_weapon_2022 <- as.numeric(as.character(descstats$num_item_weapon_2022))
mean_weapons <- mean(descstats$num_item_weapon_2022, na.rm = TRUE)

descstats$ec_county <- as.numeric(as.character(descstats$ec_county))
mean_ec <- mean(descstats$ec_county, na.rm = TRUE)

descstats$clustering_county<- as.numeric(as.character(descstats$clustering_county))
mean_clustering <- mean(descstats$clustering_county, na.rm = TRUE)

descstats$support_ratio_county <- as.numeric(as.character(descstats$support_ratio_county))
mean_support_ratio <- mean(descstats$support_ratio_county, na.rm = TRUE)

descstats$volunteering_rate_county <- as.numeric(as.character(descstats$volunteering_rate_county))
mean_vol_rate <- mean(descstats$volunteering_rate_county, na.rm = TRUE)

descstats$civic_organizations_county <- as.numeric(as.character(descstats$civic_organizations_county))
mean_civic_org <- mean(descstats$civic_organizations_county, na.rm = TRUE)


colnames(descstats)
print(descstats$ec_county)
#Descriptive Statistics Table: 
tabledata <- data.frame(
  Variable = c( 'Population', 'Number of Weapons',
                'Economic Connectedness', 'Clustering County',
                'Support Ratio County', 'Volunteering Rate County', 
                'Civic Organizations County'), 
  Mean = c(mean_pop, mean_weapons, mean_ec, mean_clustering,
         mean_support_ratio, mean_vol_rate, mean_civic_org),
  Median = c(med_pop, med_weapons, med_ec, med_clustering, 
           med_support, med_volunteering, med_civic),
  Mode = c(mode_pop, mode_weapons, mode_ec, mode_clustering,
         mode_support, mode_volunteering, mode_civic),
  Standard_Deviation = c(sd_pop, sd_weapons, sd_ec, sd_clustering,
                         sd_support, sd_volunteer, sd_civic),
  Maximum = c(max_pop, max_weapons, max_ec, max_clustering,
              max_support, max_volunteer, max_civic),
  Minimum = c(min_pop, min_weapons, min_ec, min_clustering,
              min_support, min_volunteer, min_civic)
)
print(tabledata)

file.exists(file_path)

#Medians
medians_table <- function(colname) {
  med_var <- median(descstats[[colname]], na.rm = TRUE)
  return(med_var)
}
med_pop <- medians_table("tot_pop_cty")
med_weapons <- medians_table("num_item_weapon_2022")
med_ec <- medians_table("ec_county")
med_clustering <- medians_table("clustering_county")
med_support<- medians_table("support_ratio_county")
med_volunteering <- medians_table("volunteering_rate_county")
med_civic <- medians_table("civic_organizations_county")

#Calculating Mode 
mode_table <- function(colname) { 
  mode_var <- Mode(descstats[[colname]], na.rm = TRUE)
  return(mode_var)
}
mode_pop <- mode_table("tot_pop_cty")
mode_weapons <- mode_table("num_item_weapon_2022")
mode_ec <- mode_table("ec_county")
mode_clustering <- mode_table("clustering_county")
mode_support <- mode_table("support_ratio_county")
mode_volunteering <- mode_table("volunteering_rate_county")
mode_civic <- mode_table("civic_organizations_county")

#Standard Deviation 
sd_table <- function(colname) {
  stan_dev <- sd(descstats[[colname]], na.rm = TRUE)
  return(stan_dev)
}
sd_pop <- sd_table("tot_pop_cty")
sd_weapons <- sd_table("num_item_weapon_2022")
sd_ec <- sd_table("ec_county")
sd_clustering <- sd_table("clustering_county")
sd_support <- sd_table("support_ratio_county")
sd_volunteer <- sd_table("volunteering_rate_county")
sd_civic <- sd_table("civic_organizations_county")

#Maximum 
max_val <- function(colname) {
  max_val <- max(descstats[[colname]], na.rm = TRUE)
  return(max_val)
}
max_pop <- max_val("tot_pop_cty")
max_weapons <- max_val("num_item_weapon_2022")
max_ec <- max_val("ec_county")
max_clustering <- max_val("clustering_county")
max_support <- max_val("support_ratio_county")
max_volunteer <- max_val("volunteering_rate_county")
max_civic <- max_val("civic_organizations_county")
#Minimum
min_val <- function(colname){
  min_val <- min(descstats[[colname]], na.rm = TRUE)
  return(min_val)
}
min_pop <- min_val("tot_pop_cty")
min_weapons <- min_val("num_item_weapon_2022")
min_ec <- min_val("ec_county")
min_clustering <- min_val("clustering_county")
min_support <- min_val("support_ratio_county")
min_volunteer <- min_val("volunteering_rate_county")
min_civic <- min_val("civic_organizations_county")

#Making the table look nice 
#install.packages("gt")
library(gt)
tabledata %>%
  gt() %>%
  tab_header(
    title = "2022 Data Descriptives",
    subtitle = "Data on Weapons Received from the 1033 Program and Social Capital"
  ) %>%
  fmt_number(columns = c(Mean, Median, Mode, Standard_Deviation, Maximum, Minimum), decimals = 2) %>%
  cols_label(Mean = "Mean", Median = "Median", Standard_Deviation = "Standard Deviation", 
             Maximum = "Maximum", Minimum = "Minimum") %>%
  opt_table_font(
    font = list(
      google_font(name = "Serif"),
      default_fonts()  
    )) %>%
  tab_options(
    table.background.color = "lightgray",  # Change table background
    heading.background.color = "lightgray", # Change title & subtitle background
    column_labels.background.color = "lightgray"  # Change column header background
  ) %>%
  tab_style(
    style = list(cell_text(color = "black", weight = "bold")), 
    locations = cells_title(groups = "title")
  ) %>%
  tab_style(
    style = list(cell_text(color = "black")), 
    locations = cells_title(groups = "subtitle")
  ) %>%
  tab_style(
    style = list(cell_text(color = "black", weight = "bold")), 
    locations = cells_column_labels()
  )