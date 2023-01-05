# clear the environment
rm(list=ls(all=TRUE))


# load some packages
library(rio)
library(tidyverse)
library(ggrepel)
library(forcats)
library(scales)

# load the data
data = import("gaaps_analysis_data.dta")

#Begin lolipop graph construction

#Take GAAP components and put into new df
components <- subset(data, select = c("internal_financial_audit",
                                      "external_financial_audit",
                                      "performance_audit",
                                      "technical_audit",
                                      "procurement",
                                      "internal_oversight",
                                      "other_govt_oversight",
                                      "third_party_oversight",
                                      "complaints_system",
                                      "community_monitoring",
                                      "responsibility",
                                      "timetable",
                                      "info_disclosure",
                                      "extra_sanctions",
                                      "early_warning_indicators"))

# Replace NAs with 0 in components
components <- 
  components %>% 
  mutate_if(is.numeric, ~replace_na(., 0))

#Checking variables
table(components$internal_financial_audit)

# get the count of the components
components_count <- 
  components %>%  
  summarize(across(where(is.numeric),sum))

# reshape it long
components_long <- 
  components_count %>% 
  pivot_longer(
    everything(),
    names_to = c("component"),
    values_to = c("count")
  )

# get the percent of projects with each component
components_long <- 
  components_long %>% 
  mutate(percent = ((count/369)*100),
         percent = round(percent,1))


#Clean up component titles
components_long$component[components_long$component=="internal_financial_audit"] = "Internal Financial Audit"
components_long$component[components_long$component=="external_financial_audit"] = "External Financial Audit"
components_long$component[components_long$component=="performance_audit"] = "Performance Audit"
components_long$component[components_long$component=="technical_audit"] = "Technical Audit"
components_long$component[components_long$component=="procurement"] = "Procurement"
components_long$component[components_long$component=="internal_oversight"] = "Internal Oversight"
components_long$component[components_long$component=="other_govt_oversight"] = "Other Government Oversight"
components_long$component[components_long$component=="third_party_oversight"] = "Third-Party Oversight"
components_long$component[components_long$component=="complaints_system"] = "Complaints System"
components_long$component[components_long$component=="community_monitoring"] = "Community Monitoring"
components_long$component[components_long$component=="responsibility"] = "Clear Implementer"
components_long$component[components_long$component=="timetable"] = "Timetable for Actions"
components_long$component[components_long$component=="info_disclosure"] = "Information Disclosure"
components_long$component[components_long$component=="extra_sanctions"] = "Sanctions/Remedies"
components_long$component[components_long$component=="early_warning_indicators"] = "Early Warning Indicators"

#Arrange by most common component first

components_long <- 
  components_long %>% 
  arrange(percent) %>% 
  mutate(percent_label = paste0(percent, "%"),
         component_factor = factor(component)) 
components_long

# Create lollipop graph,order everything, and save
lollipop = ggplot(components_long, aes(x=reorder(component_factor,count), y=count)) + 
  geom_point(size=3) + 
  geom_segment(aes(x=component_factor, 
                   xend=component_factor, 
                   y=0, 
                   yend=count)) + 
  geom_text(aes(label = percent_label), vjust = -1, size = 2.5) +
  scale_y_continuous(expand = c(0, 0), limits = c(0.0, 350), breaks = seq(0,350,100)) +
  labs(x = "GAAP Attributes",
       y= "GAAPs With Each Attribute") + 
  theme(axis.text.x = element_text(angle=65, vjust=0.98, hjust=1))
print(lollipop)
ggsave(lollipop, filename = "GAAPs_lollipop.pdf", width = 8, height = 6.5, dpi=600) 

# alternatively, plot wide
lollipop_wide = ggplot(components_long, aes(x=count, y=reorder(component_factor,count))) + 
  geom_point(size=3) + 
  geom_segment(aes(x=0, 
                   xend=count, 
                   y=component_factor, 
                   yend=component_factor)) + 
  scale_x_continuous(expand = c(0, 0), limits = c(0.0, 350), breaks = seq(0,350,100)) + 
  geom_text(aes(label = percent_label), vjust = 0.5, hjust=1, position = position_nudge(x = 30)) +
  labs(x = "Count of GAAPs With Each Attribute",
       y= "GAAP Attributes")  
print(lollipop_wide)
ggsave(lollipop_wide, filename = "GAAPs_lollipop_wide.pdf", width = 8, height = 6.5, dpi=600) 
ggsave(lollipop_wide, filename = "GAAPs_lollipop_wide.png", width = 8, height = 6.5, dpi=600) 
  
