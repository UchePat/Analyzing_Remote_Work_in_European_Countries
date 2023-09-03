# Analyzing Remote Work in European Countries

# 1 Data downloading
# As we always do, we are going to connect and download the desired data. 
# In this case, our data source is the Eurostat. It is 1.5 million rows of data. 

library(tidyverse)

download.file("https://ec.europa.eu/eurostat/api/dissemination/sdmx/2.1/data/LFSA_EHOMP/?format=SDMX-CSV&compressed=false", "data.csv")

df_european_data <- read_csv(file = 'estat_lfsa_ehomp_en.csv',
                             col_types = cols(sex = col_character())) # changes Sex column datatype to character

head(df_european_data)
str(df_european_data)
names(df_european_data)

unique(df_european_data$sex)


#---> As a first analytical step, we are going to check the TOP 5 countries with employees in any kind of Remote Work / Work From Home (WFH) mode.
df_european_data_filtered <-df_european_data %>% 
  filter(freq == "A" & unit == "PC" & wstatus == "EMP" & 
           sex == "T" & age == "Y20-64" & geo != "SE") %>% 
  select(geo, TIME_PERIOD, OBS_VALUE, frequenc) %>% 
  rename(remote_perc = OBS_VALUE, country = geo)
         

library(kableExtra)
# library(knitr)   # for kable()

df_european_data_filtered %>% 
  filter(frequenc == "NVR") %>% 
  mutate(remote_perc=if_else(frequenc == "NVR",   # if frequenc == "NVR"
                             round(1-remote_perc/100, 3)*100,  # use this as values
                             round(remote_perc/100, 3)*100)) %>%  # else use dis as values
  select(-frequenc) %>% 
  arrange(country, TIME_PERIOD) %>% 
  filter(TIME_PERIOD == "2020") %>% 
  slice_max(n=6, order_by=remote_perc) %>%  # displays top 6 rows by remote_perc column values
  kbl() %>%             # tabulates d data
  kable_styling(bootstrap_options = c("striped", "hover"))
  # kable()     # tabulates d data in HTML format


df_european_data_filtered_bar_plot <- df_european_data_filtered %>% 
  filter(frequenc == "NVR" & TIME_PERIOD =="2020") %>% 
  mutate(highlight = if_else(country == "EU27_2020","t","f"),
         remote_perc = if_else(frequenc == "NVR",
                               round(1-remote_perc/100,3),
                               round(remote_perc/100,3))) %>% 
  arrange(remote_perc) 


# Visualizing using Bar chart
df_european_data_filtered_bar_plot %>% 
  ggplot(aes(country, remote_perc, fill=highlight)) +
  geom_bar(stat="identity", position="dodge") +
  geom_text(aes(label=paste0(remote_perc*100,"%")), size=2) +  # adds values to top of each bar
  coord_flip() +
  scale_x_discrete(limits=df_european_data_filtered_bar_plot$country) +  # dis will sort the bars in d former x-axis(which is now y-axis) using the stated column
  scale_fill_manual(values=c( "t"="tomato", "f"="paleturquoise3" ),
                    guide="none") +    # changes d color of of d bars
  scale_y_continuous(labels=scales::percent) +   # turns d former y-axis(which is now x-axis) values into %s
  labs(title="Employee Percentage(%) with any Remote Work modality by Country",
       caption = "Source: Eurostat")


#---> And the TOP 10 with more relative increment?
df_european_data_filtered %>% 
  filter(frequenc == "NVR") %>% 
  mutate( remote_perc = if_else(frequenc == "NVR",
                                round(1-remote_perc/100,3)*100,
                                round(remote_perc/100,3)*100)) %>% 
  select(-frequenc) %>% 
  arrange(country, TIME_PERIOD) %>% 
  group_by(country) %>% 
  mutate(delta = (remote_perc - lag(remote_perc))/lag(remote_perc)*100) %>% 
  ungroup() %>% 
  filter(TIME_PERIOD=="2020") %>% 
  slice_max( n=10,order_by=delta) %>% 
  kbl() %>%             # tabulates d data
  kable_styling(bootstrap_options = c("striped", "hover"))


# 2. Remote worker profile in Europe
# (Usually vs Sometimes vs Never)
# A good way to summarize the country labour market profile regarding remote work is to make a Donut plot. 
# It's true that the human brain is not very good at comparing areas, but this plot will allow us to perceive the Remote Work adoption in each modality at a glance.
list_countries <- c("AT","BE","CH","CY","CZ",
                    "DE","DK","ES","EU15","EU27_2020","FI",
                    "FR","IS","IT","LU","MK",
                    "NL","NO","PL","PT","SK")

df_european_data_filtered %>%
         filter(TIME_PERIOD=="2020" & (country %in% list_countries )) %>% 
         group_by(country) %>% 
         mutate(ymax=cumsum(remote_perc),
                ymin=if_else(row_number() == 1, 0, lag(ymax)),
                labelPosition=(ymax+ymin)/2,
                label=paste0(remote_perc, " %")) %>%   # adds % to d stated column value
         rename(freq=frequenc) %>%
  ggplot(aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=freq)) +
  geom_rect() +       # creates a rectangle block chart(like a stacked Bar chart) with each unique value/class in freq column stacked on top of each other
  geom_text(x=1.5,
            aes(y=labelPosition, label=label, color=freq),
            size=2.2,
            check_overlap = T)+
  scale_fill_brewer(palette=3) + # changes color of d chart for d intending Donut chart
  scale_color_brewer(palette=3) + # labs colour
  facet_wrap(~country) +
  coord_polar(theta="y") +  # Creates Donut/Pie chart for each facet( ie the Donut chart is inside d Pie chart)
  theme_void() +  # Removes d theme so only d Pie chart is visible(not the Donut part of it)
  xlim(c(-1, 4)) +   # removes d Pie chart and displays d Donut chart
  labs(title="Remote Work composition by country",
       subtitle=" ",
       caption = "Source: Eurostat")


# Remote Work temporal evolution
# In order to bring more context, we are going to deep into the temporal evolution of each country. 
# We are tracking the employees in each country and year that have been working in some Remote mode. 
# We could see some clusters, with countries with steeper slope than others during the last year in the data reported.
df_european_data_filtered %>%
  filter(frequenc == "NVR" & (country %in% list_countries[list_countries != "CY"])) %>% 
  mutate( remote_perc = if_else( frequenc == "NVR",round(1-remote_perc/100,3),round(remote_perc/100,3)),) %>% 
  ggplot(aes(TIME_PERIOD, remote_perc, colour=remote_perc, group=country)) +
  geom_line() +
  facet_wrap(~country) +
  scale_colour_gradient(low = "yellow", high = "red", na.value = NA)+  # changes d gradient color of d chart
  scale_y_continuous(labels=scales::percent)  # changes d y-axis values to %s


# 3. Remote Work ~ Country GDP correlation
# As a final thought, the data is pointing out to us that Northern Europe countries are doing more Remote Work. 
# We are expecting that higher GDP countries are more prone to define a Work From Home model or hybrid ones, while the lower GDP ones have more difficulties or are not willing to enhance and embrace change.
# The industrial sector composition, kind and size of national companies, politic ideology and government seem such a high impact reasons and probably act as a causality of the heterogenity across the European States.
