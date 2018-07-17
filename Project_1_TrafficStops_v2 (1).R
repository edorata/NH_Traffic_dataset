# Data 801: R Programming
# Group Six (6): Brennan Donnell, Eric Dorata, Anna M. Kot, and Dushyant Kumar
# Last updated: June 19, 2018 by Brennan Donnell

# Call the relevant libraries used in the code below.
library(dplyr)
library(ggplot2)
library(ggmap)
library(maps)
library(mapdata)
library(stringr)
library(lettercase)

# Ensure the working directory is set to the location of the 
# dataset, subsequently import the dataset into the environment, and
# apply the dplyr library to the dataset.
setwd("/users/annakot/desktop/m.s. analytics/data 801/R programming/Project_1")
NH = read.csv("NH_cleaned.csv")
NH <- tbl_df(NH)

# Create a new category to group ages according to generation 
# and include the new category in the dataset as driver_age_category.
NH <- mutate(NH,driver_age_category = cut(driver_age,
                                          c(15,17,34,50,69,87,100),
                                          labels = c(
                                            "Gen Z (15-17)",
                                            "Millennial (18-34)",
                                            "Gen X (35-50)",
                                            "Boomer (51-69)",
                                            "Silent (70-87)",
                                            "Greatest (89+)")))

# Create a new data frame for data columns considered for the analysis.
NH <- NH[c(1,6,16,21,23,24,27)]

# Filter the dataset to include only individuals with speeding violations 
# and exclude drivers with a null driver_age_category.
NH_Speeding <- filter(NH,
                     !is.na(driver_age_category),
                     str_detect(violation, ".*Speeding"))

# Update the violation column to only reflect the term 'Speeding'.
NH_Speeding$violation <- "Speeding"

# Plot the distribution of speeding violations by generation in New Hampshire, 2014-2015.
Plot_1 <- ggplot(NH_Speeding, aes(x=driver_age_category, fill=driver_age_category)) + 
  geom_bar(stat="count") +
  theme_minimal() +
  theme(legend.position="none") +
  theme(plot.background=element_rect(fill="#FBFCFF")) +
  scale_fill_manual(values=c(
    "#f77a05", 
    "#cb4d0b", 
    "#f77a05", 
    "#cb4d0b", 
    "#f77a05", 
    "#cb4d0b")) + 
  labs(title="", x="", y="")

# Return the last plot and save as 8’ x 8’ file named "Plot_1.png" 
# in the working directory and match the file type to the file extension.
Plot_1
ggsave("Plot_1.png", width = 8, height = 8)

# Subset the dataset to include only individuals with speeding violations 
# and exclude drivers with a null driver_age_category.
NH_Speeding_2 <- filter(NH_Speeding,
                      driver_age_category == "Millennial (18-34)" | driver_age_category == "Gen X (35-50)")

# Plot the distribution of speeding violations by county in New Hampshire, 2014-2015
Plot_2 <- ggplot(NH_Speeding_2, aes(x=driver_age_category, fill=county_name)) + 
  geom_bar(position = "stack") +
  coord_flip() +
  theme_minimal() +
  theme(legend.position="top") +
  theme(plot.background=element_rect(fill="#FBFCFF")) +
  scale_fill_manual(values=c(
    "#001d52", 
    "#0044bb", 
    "#cb4d0b", 
    "#f77a05", 
    "#263645", 
    "#5c6874", 
    "#A3A9AC", 
    "#dcdedf", 
    "#f4f4f4", 
    "#003591")) +  
  labs(title="", x="", y="", fill = "County")

# Return the last plot and save as 8’ x 8’ file named "Plot_2.png" 
# in the working directory and match the file type to the file extension.
Plot_2
ggsave("Plot_2.png", width = 8, height = 8)

# Create a new dataframe for Rockingham County
rock <- c("Rockingham County")
rockfinal <- filter(NH_Speeding_2, 
                    county_name %in% rock, 
                    !is.na(lat), 
                    !is.na(lon))

# Create a Google Map illustrating points of Rockingham County 
# speeding violations for Millennials and Gen X.
Plot_3 <- qmplot(lon,lat, 
  zoom    = 10, 
  data    = rockfinal,
  source  = "google", 
  maptype = "roadmap",
  color   = I("blue"))

# Return the last plot and save as 8’ x 8’ file named "Plot_3.png" 
# in the working directory and matches the file type to the file extension.
Plot_3
ggsave("Plot_3.png", width = 8, height = 8)

# Create a new dataframe for Merrimack County
mack <- c("Merrimack County")
mackfinal <- filter(NH_Speeding_2,
                    county_name %in% mack,
                    !is.na(lat), 
                    !is.na(lon))

# Create a Google Map illustrating points of Merrimack County 
# speeding violations Millennials and Gen X.
Plot_4 <- qmplot(lon,lat, 
                 zoom    = 10, 
                 data    = mackfinal,
                 source  = "google", 
                 maptype = "roadmap",
                 color   = I("orange"))

# Return the last plot and save as 8’ x 8’ file named "Plot_4.png" 
# in the working directory and matches the file type to the file extension.
Plot_4
ggsave("Plot_4.png", width = 8, height = 8)

# Create a new dataframe for speeding violations in both Merrimack 
# and Rockingham counties for all generations.
NH_Speeding_3 <- filter(NH_Speeding,
                      str_detect(county_name, ".*Rockingham") | str_detect(county_name, ".*Merrimack"))

# Create a new dataframe for speeding violations in both Merrimack 
# and Rockingham counties for all generations to include ticket 
# and warning outcome totals.
NH_Speeding_4 <- NH_Speeding_3 %>% 
  filter(stop_outcome == "Ticket" | stop_outcome == "Warning") %>% 
  group_by(county_name,driver_age_category) %>% 
  count(stop_outcome)

# Plot the distribution of speeding violation outcomes in 
# both Merrimack and Rockingham counties with highlights for 
# millenials and generation x.
Plot_5 <- ggplot(NH_Speeding_4,aes(x=county_name,y= n)) + 
  geom_segment(aes(x= county_name, xend = county_name, y = 0, yend = 6500)) +
  geom_point(color=ifelse(NH_Speeding_4$driver_age_category %in% c("Gen X (35-50)"), 
  "#003591",ifelse(NH_Speeding_4$driver_age_category %in% c("Millennial (18-34)"),  "#f77a05", "#A3A9AC")),
   size=ifelse(NH_Speeding_4$driver_age_category %in% c("Gen X (35-50)"), 6,
   ifelse(NH_Speeding_4$driver_age_category %in% c("Millennial (18-34)"), 6, 3))) + 
  coord_flip() + 
  xlab("") + ylab("") +
  theme_minimal() +
  theme(plot.background=element_rect(fill="#FBFCFF")) +
  annotate("text", 
               x    = "Rockingham County", 
               y    = 3277, 
           label    = "Tickets: Millennial (18-34)", 
           angle    = 45, 
           hjust    = -.10, 
           fontface = "bold", 
           color    = "#f77a05") +
  annotate("text", 
               x    = "Rockingham County", 
               y    = 2986, 
           label    = "Warnings: Millennial (18-34)", 
           angle    = 45, 
           hjust    = -.10, 
           fontface = "bold", color="#f77a05") +
  annotate("text", 
           x        = "Rockingham County", 
           y        = 1382, 
           label    = "Tickets: Gen X (35-50)", 
           angle    = 45, 
           hjust    = -.10, 
           fontface = "bold", color="#003591") +
  annotate("text", 
           x        = "Rockingham County", 
           y        = 1719, 
           label    = "Warnings: Gen X (35-50)", 
           angle    = 45, 
           hjust    = -.10, 
           fontface = "bold", 
           color    = "#003591") +  
  annotate("text", 
           x        = "Merrimack County", 
           y        = 4899, 
           label    = "Tickets: Millennial (18-34)", 
           angle    = 45, 
           hjust    = -.10, 
           fontface = "bold", 
           color    = "#f77a05") +
  annotate("text", 
           x        = "Merrimack County", 
           y        = 4058, 
           label    = "Warnings: Millennial (18-34)", 
           angle    = 45, 
           hjust    = -.10, 
           fontface = "bold", 
           color    = "#f77a05") +
  annotate("text", 
           x        = "Merrimack County", 
           y        = 2447, 
           label    = "Tickets: Gen X (35-50)", 
           angle    = 45, 
           hjust    = -.10, 
           fontface = "bold", 
           color    = "#003591") +
  annotate("text", 
           x        = "Merrimack County", 
           y        = 2665, 
           label    = "Warnings: Gen X (35-50)", 
           angle    = 45, 
           hjust    = -.10, 
           fontface = "bold", color="#003591")

# Return the last plot and save as 8’ x 8’ file named "Plot_5.png" 
# in the working directory and matches the file type to the file extension.
Plot_5
ggsave("Plot_5.png", width = 8, height = 8)


# Create the a map showing the counties in the state of New Hampshire
states <- map_data("state")

nh_df <- subset(states, region == "new hampshire")
counties <- map_data("county")
nh_county <- subset(counties, region == "new hampshire")

nh_base <- ggplot(data = nh_df, mapping = aes(
  x     = long, 
  y     = lat, 
  fill  = subregion, 
  group = group)) + 
  coord_fixed(1.3) + 
  geom_polygon(
    color = "black", 
    fill  = "gray", aes(
      group = group, 
      fill  = subregion)) +
  theme_nothing()

nh_base <-nh_base + 
  geom_polygon(data = nh_county, aes(fill = subregion),  color = "#263645") +
  geom_polygon(color = "#263645", fill = NA) +
  scale_fill_manual(values=c(
    "#001d52", 
    "#0044bb", 
    "#cb4d0b", 
    "#f77a05", 
    "#263645", 
    "#5c6874", 
    "#A3A9AC", 
    "#dcdedf", 
    "#f4f4f4", 
    "#003591")) +
  theme_minimal() +
  theme(plot.background=element_rect(fill="#FBFCFF")) +
  labs(title="", x="", y="", fill = "County")

# Return the last plot and save as 8’ x 8’ file named "Plot_6.png" 
# in the working directory and matches the file type to the file extension.
nh_base
ggsave("Plot_6.png", width = 8, height = 8)
