# This piece of code produce the bar graph for
# the number of approved vaccine brand in each country

# Load packages
library(ggplot2)
library(dplyr)
library(gghighlight)

# Set a vector on how many brands are approved in each country
approved_vaccine_brand <- c(6, 4, 4, 4)
# Source: https://covid19.trackvaccines.org/

# Set a country vector
country_vector <- c("Chile",
                    "Estonia",
                    "Latvia",
                    "Slovenia")

# Create the frame and configure the column names
df_approved_vaccine <- data.frame(country_vector, approved_vaccine_brand)
colnames(df_approved_vaccine)[1] <- "country"
colnames(df_approved_vaccine)[2] <- "approved_vaccine"

# Plot the bar graph
ggplot(data = df_approved_vaccine, aes(x = country, y = approved_vaccine, fill = country)) +
  geom_bar(stat = "identity", width = 0.5, colour = "black") +
  ggtitle("Total Vaccine Brand Approved") +
  labs(y = "Approved Vaccine Count", x = "Country") + 
  theme_minimal()

# Save the bar graph
ggsave("plots/plot_approved.png", dpi = 320, width = 1500, height = 800, units = "px")