library(dplyr)
library(ggplot2)
library(ggpubr)
library(readxl)
library(plotly)
library(gridExtra)
library(viridis)

#Read Data
world<-read_excel("https://github.com/weglinska/COVID/blob/main/Data_covid.xlsx")

world<- world[order(world$Data),]

world$Data<-as.character(world$Data)

#Assure that data on both infection and tests is supplied and that the number of tests is greater than that of infections

world <-dplyr::filter(world, !is.na(Tests_cumulated) & !is.na(Confirmed_cumulated) & Tests_cumulated>Confirmed_cumulated ) #for cumulated data
world <-dplyr::filter(world, !is.na(Tests)& !is.na(Confirmed) & Tests>Confirmed )

# Calculate local positivity
#Variables with _2 -> for individual month. Without - cumulated !!
world$LPR<-world$Confirmed_cumulated/world$Tests_cumulated
world$LPR2<-world$Confirmed/world$Tests

# Determine the time period
date_range<-unique(world$Data)

# Prepare empty vectors for global positivity, number of countries 
#for which data are available for each individual day, and
#global ratios needed for calculating the expected number of cases and tests
global_ratio<-c()
global_ratio2 <- c()
number_countries <- c()

r_cases <- c()
r_tests <- c()
r_cases2 <- c()
r_tests2 <- c()

for (k in date_range){ #for each date
  date_index <- which(world$Data==k) #get row indices
  number_countries[k] <- length(date_index) #get number of countries 
  global_ratio[k] <- sum(world$Confirmed_cumulated[date_index])/ sum(world$Tests_cumulated[date_index] ) #calculate global positivity
  r_cases[k] <-  sum(world$Confirmed_cumulated[date_index])/sum( world$Population[date_index] ) #calculate ratio for calculating the expected number of tests
  r_tests[k] <-  sum( world$Tests_cumulated[date_index] )/sum(world$Population[date_index]) #calculate ratio for calculating the expected number of cases
}


for (k in date_range){ #for each date
  date_index <- which(world$Data==k) #get row indices
  number_countries[k] <- length(date_index) #get number of countries 
  global_ratio2[k] <- sum(world$Confirmed[date_index])/ sum(world$Tests[date_index] ) #calculate global positivity
  r_cases2[k] <-  sum(world$Confirmed[date_index])/sum( world$Population[date_index] ) #calculate ratio for calculating the expected number of tests
  r_tests2[k] <-  sum( world$Tests[date_index] )/sum(world$Population[date_index]) #calculate ratio for calculating the expected number of cases
}

# Replicate global positivity with respect to the number of countries with a given date
global_positivity <- rep(global_ratio, number_countries)
global_positivity2 <- rep(global_ratio2, number_countries)

# Replicate ratios to calculated expected number of cases and tests with respect to the number of countries with a given date
r_cases <- rep (r_cases, number_countries )
r_tests <- rep (r_tests, number_countries )
r_cases2 <- rep (r_cases2, number_countries )
r_tests2 <- rep (r_tests2, number_countries )

# Assign global positivity as a new column of the data frame
world$GPR <- global_positivity
world$GPR2 <- global_positivity2


# Assign WCSIR and WSIR as a new column of the data frame
world$WCSIR <-world$LPR/world$GPR
world$WSIR <-world$LPR2/world$GPR2
# Assign global ratio for calculating the expected number of cases as a new column of the data frame
world$r_cases <- r_cases
world$r_cases2 <- r_cases2
# Assign global ratio for calculating the expected number of tests as a new column of the data frame
world$r_tests <- r_tests
world$r_tests2 <- r_tests2
# Assign  expected number of cases as a new column of the data frame
world$expected_cases <- world$r_cases*world$Population
world$expected_cases2 <- world$r_cases2*world$Population
# Assign  expected number of tests as a new column of the data frame
world$expected_tests <- world$r_tests*world$Population
world$expected_tests2 <- world$r_tests2*world$Population

# Assign CSIR and SIR as a new column of the data frame
world$CSIR <- world$Confirmed_cumulated/world$expected_cases
world$SIR <- world$Confirmed/world$expected_cases2
# Assign CSTR and STR as a new column of the data frame
world$CSTR <- world$Tests_cumulated/world$expected_tests
world$STR <- world$Tests/world$expected_tests2
# Create empty vectors for homogeneity of CSIR, SIR, WCSIR and WSIR
homogeneity_CSIR<-c()
homogeneity_WCSIR<-c()

homogeneity_SIR<-c()
homogeneity_WSIR<-c()

for (k in date_range){ #for every date
  date_index <- which(world$Data==k) #get row indices
  homogeneity_CSIR[k] <- sum(  (world$CSIR[date_index]-1)^2    ) #calculate homogeneity of CSIR
  homogeneity_WCSIR[k] <- sum(  (world$WCSIR[date_index]-1)^2   ) #calculate homogeneity of WCSIR
}

for (k in date_range){ #for every date
  date_index <- which(world$Data==k) #get row indices
  homogeneity_SIR[k] <- sum(  (world$SIR[date_index]-1)^2    ) #calculate homogeneity of CSIR
  homogeneity_WSIR[k] <- sum(  (world$WSIR[date_index]-1)^2   ) #calculate homogeneity of WCSIR
}

# Transform CSIR and  into a stack, assign column and date names
homogeneityCSIR <- stack(homogeneity_CSIR)
colnames(homogeneityCSIR)<- c("Homogeneity", "Date")

homogeneityCSIR$Date <- as.Date(date_range)

homogeneitySIR <- stack(homogeneity_SIR)
colnames(homogeneitySIR)<- c("Homogeneity", "Date")

homogeneitySIR$Date <- as.Date(date_range)


# Generate the plot of CSIR homogeneity
homogeneityCSIR_plot<-ggplot(homogeneityCSIR, aes(Date, Homogeneity, group=1))+ 
  geom_point() + geom_line(colour = "red")+xlab("Date")+ 
  ylab("Heterogeneity")+
  scale_x_date(labels = scales::date_format("%m-%Y"))+
  ggtitle("")+
  theme(axis.text.x=element_text(angle = -90, hjust = 0))

homogeneityCSIR_plot

homogeneitySIR_plot<-ggplot(homogeneitySIR, aes(Date, Homogeneity, group=1))+ 
  geom_point() + geom_line(colour = "red")+xlab("Date")+ 
  ylab("Heterogeneity")+
  scale_x_date(labels = scales::date_format("%m-%Y"))+
  ggtitle("")+
  theme(axis.text.x=element_text(angle = -90, hjust = 0))

homogeneitySIR_plot

# Calculate WCSIR homogeneity

homogeneityWCSIR <- stack(homogeneity_WCSIR)
colnames(homogeneityWCSIR)<- c("Homogeneity", "Date")
homogeneityWCSIR$Date <- as.Date(unique(world$Data))

homogeneityWSIR <- stack(homogeneity_WSIR)
colnames(homogeneityWSIR)<- c("Homogeneity", "Date")
homogeneityWSIR$Date <- as.Date(unique(world$Data))

#23. Generate the plot of WCSIR homogeneity
homogeneityWCSIR_plot<-ggplot(homogeneityWCSIR, aes(Date, Homogeneity, group=1))+ 
  geom_point() + geom_line(colour = "red")+xlab("Date")+ 
  ylab("Heterogeneity")+
  scale_x_date(labels = scales::date_format("%m-%Y"))+
  ggtitle("")+
  theme(axis.text.x=element_text(angle = -90, hjust = 0))
homogeneityWCSIR_plot

homogeneityWSIR_plot<-ggplot(homogeneityWSIR, aes(Date, Homogeneity, group=1))+ 
  geom_point() + geom_line(colour = "red")+xlab("Date")+ 
  ylab("Heterogeneity")+
  scale_x_date(labels = scales::date_format("%m-%Y"))+
  ggtitle("")+
  theme(axis.text.x=element_text(angle = -90, hjust = 0))
homogeneityWSIR_plot

# Generate the plot of CSTR

world$Data <- as.Date(world$Data)

CSTR_plot <- ggplot(data=world, aes(
  x=Data,
  y=CSTR,
  group = Kraj,
  colour = Kraj
)
)+
  geom_line(size=0.5) +
  geom_point( size=1, shape=21, fill="white")+
  scale_x_date(labels = scales::date_format("%m-%Y"))+
  theme(axis.text.x=element_text(angle = -90, hjust = 0), legend.position = "right")

CSTR_plot
ggplotly(CSTR_plot)

STR_plot <- ggplot(data=world, aes(
  x=Data,
  y=STR,
  group = Kraj,
  colour = Kraj
)
)+
  geom_line(size=0.5) +labs(x="Data",y="STR")+
  geom_point( size=1, shape=21, fill="white")+
  scale_x_date(labels = scales::date_format("%m-%Y"))+
  theme(axis.text.x=element_text(angle = -90, hjust = 0), legend.position = "right")

STR_plot
ggplotly(STR_plot)

CSTR_plot_inverse<- ggplot(data=world, aes(
  x=Data,
  y=1/CSTR,
  group = Kraj,
  colour = Kraj
)
)+
  geom_line(size=0.5) +
  geom_point( size=1, shape=21, fill="white")+
  scale_x_date(labels = scales::date_format("%m-%Y"))+
  theme(axis.text.x=element_text(angle = -90, hjust = 0), legend.position = "right")

CSTR_plot_inverse
ggplotly(CSTR_plot_inverse)

STR_plot_inverse<- ggplot(data=world, aes(
  x=Data,
  y=1/STR,
  group = Kraj,
  colour = Kraj
)
)+
  geom_line(size=0.5) +
  geom_point( size=1, shape=21, fill="white")+labs(x="Data",y="1/STR")+
  scale_x_date(labels = scales::date_format("%m-%Y"))+
  theme(axis.text.x=element_text(angle = -90, hjust = 0), legend.position = "right")

ggplotly(STR_plot_inverse)

# Generate the plot of CSIR
CSIR_plot <- ggplot(data=world, aes(
  x=Data,
  y=CSIR,
  group = Kraj,
  colour = Kraj
)
)+
  geom_line(size=0.5) +
  geom_point( size=1, shape=21, fill="white")+
  scale_x_date(labels = scales::date_format("%m-%Y"))+
  theme(axis.text.x=element_text(angle = -90, hjust = 0), legend.position = "right")

CSIR_plot
ggplotly(CSIR_plot)

SIR_plot <- ggplot(data=world, aes(
  x=Data,
  y=SIR,
  group = Kraj,
  colour = Kraj
)
)+
  geom_line(size=0.5) +
  geom_point( size=1, shape=21, fill="white")+labs(x="Data",y="SIR")+
  scale_x_date(labels = scales::date_format("%m-%Y"))+
  theme(axis.text.x=element_text(angle = -90, hjust = 0), legend.position = "right")

ggplotly(SIR_plot)


CSIR_plot_inverse<- ggplot(data=world, aes(
  x=Data,
  y=1/CSIR,
  group = Kraj,
  colour = Kraj
)
)+
  geom_line(size=0.5) +
  geom_point( size=1, shape=21, fill="white")+
  scale_x_date(labels = scales::date_format("%m-%Y"))+
  theme(axis.text.x=element_text(angle = -90, hjust = 0), legend.position = "right")

CSIR_plot_inverse
ggplotly(CSIR_plot_inverse)

SIR_plot_inverse<- ggplot(data=world, aes(
  x=Data,
  y=1/SIR,
  group = Kraj,
  colour = Kraj
)
)+
  geom_line(size=0.5) +
  geom_point( size=1, shape=21, fill="white")+labs(x="Date",y="1/SIR")+
  scale_x_date(labels = scales::date_format("%m-%Y"))+
  theme(axis.text.x=element_text(angle = -90, hjust = 0), legend.position = "right")

SIR_plot_inverse
ggplotly(SIR_plot_inverse)

# Generate the plot of LPR
LPR_plot <- ggplot(data=world, aes(
  x=Data,
  y=LPR,
  group = Kraj,
  colour = Kraj
)
)+
  geom_line(size=0.5) +
  geom_point( size=1, shape=21, fill="white")+
  geom_line( aes (x=Data, y=GPR), size=0.5)+
  scale_x_date(labels = scales::date_format("%m-%Y"))+
  theme(axis.text.x=element_text(angle = -90, hjust = 0), legend.position = "right")

ggplotly(LPR_plot)

LPR2_plot <- ggplot(data=world, aes(
  x=Data,
  y=LPR2,
  group = Kraj,
  colour = Kraj
)
)+
  geom_line(size=0.5) +
  geom_point( size=1, shape=21, fill="white")+labs(x="Date",y="LPR")+
  geom_line( aes (x=Data, y=GPR), size=0.5)+
  scale_x_date(labels = scales::date_format("%m-%Y"))+
  theme(axis.text.x=element_text(angle = -90, hjust = 0), legend.position = "right")

ggplotly(LPR2_plot)
LPR2_plot


LPR_plot_inverse<- ggplot(data=world, aes(
  x=Data,
  y=1/LPR,
  group = Kraj,
  colour = Kraj
)
)+
  geom_line(size=0.5) +
  geom_line(data=world, aes (x=Data, y=1/LPR), size=1.4)+
  geom_point( size=1, shape=21, fill="white")+
  scale_x_date(labels = scales::date_format("%m-%Y"))+
  theme(axis.text.x=element_text(angle = -90, hjust = 0), legend.position = "right")

ggplotly(LPR_plot_inverse)

LPR2_plot_inverse<- ggplot(data=world, aes(
  x=Data,
  y=1/LPR2,
  group = Kraj,
  colour = Kraj
)
)+
  geom_line(size=0.5) +
  geom_line(data=world, aes (x=Data, y=1/LPR), size=1.4)+
  geom_point( size=1, shape=21, fill="white")+
  scale_x_date(labels = scales::date_format("%m-%Y"))+
  theme(axis.text.x=element_text(angle = -90, hjust = 0), legend.position = "right")

ggplotly(LPR2_plot_inverse)


# Generate the plot of WCSIR
WCSIR_plot <- ggplot(data=world, aes(
  x=Data,
  y=WCSIR,
  group = Kraj,
  colour = Kraj
)
)+
  geom_line(size=0.5) +
  geom_point( size=1, shape=21, fill="white")+
  scale_x_date(labels = scales::date_format("%m-%Y"))+
  theme(axis.text.x=element_text(angle = -90, hjust = 0), legend.position = "right")

ggplotly(WCSIR_plot)

WSIR_plot <- ggplot(data=world, aes(
  x=Data,
  y=WSIR,
  group = Kraj,
  colour = Kraj
)
)+
  geom_line(size=0.5) +
  geom_point( size=1, shape=21, fill="white")+labs(x="Date",y="WSIR")+
  scale_x_date(labels = scales::date_format("%m-%Y"))+
  theme(axis.text.x=element_text(angle = -90, hjust = 0), legend.position = "right")

ggplotly(WSIR_plot)

WCSIR_plot_inverse<- ggplot(data=world, aes(
  x=Data,
  y=1/WCSIR,
  group = Kraj,
  colour = Kraj
)
)+
  geom_line(size=0.5) +
  geom_point( size=1, shape=21, fill="white")+
  scale_x_date(labels = scales::date_format("%m-%Y"))+
  theme(axis.text.x=element_text(angle = -90, hjust = 0), legend.position = "right")
ggplotly(WCSIR_plot_inverse)


GPR_plot<- ggplot(data=world, aes(
  x=Data,
  y=GPR,
  #group = Kraj,
  colour = Kraj
)
)+
  geom_point(size=0.5) +
  geom_point( size=1, shape=21, fill="white")+
  scale_x_date(labels = scales::date_format("%m-%Y"))+
  theme(axis.text.x=element_text(angle = -90, hjust = 0), legend.position = "right")
ggplotly(GPR_plot)

countries_example<-world%>%filter(Kraj=="USA"|Kraj=="Mexico"|Kraj=="Brazil"|Kraj=="Argentina"|Kraj=="Chile"|Kraj=="Colombia"|Kraj=="Australia"|Kraj=="Japan"
                        |Kraj=="India"|Kraj=="Poland"|Kraj=="Germany"|Kraj=="Italy"|Kraj=="Spain"|Kraj=="Norway"|Kraj=="Turkey"|Kraj=="South Africa"
                        |Kraj=="Nigeria"|Kraj=="Morocco"|Kraj=="Ethiopia"|Kraj=="Angola")

WCSIR2_plot <- ggplot(data=countries_example, aes(
  x=Data,
  y=CSIR,
  group = Kraj,
  colour = Kraj
)
)+
  geom_line(size=0.5) +
  geom_point( size=1, shape=21, fill="white")+labs(x="Date",y="CSIR")+
  scale_x_date(labels = scales::date_format("%m-%Y"))+
  theme(axis.text.x=element_text(angle = -90, hjust = 0), legend.position = "right")

ggplotly(WCSIR2_plot)

# Function to generate the plot
generate_plot <- function(data, title) {
  ggplot(data, aes(x = LPR, y = WCSIR, colour = Data)) +
    geom_point() +
    geom_path() +
    labs(title = title, x = "LPR", y = "WCSIR") +
    theme_minimal() +
    guides(color = guide_legend(title = "Date"))
}

# List of country names
country_names <- c("Italy", "Nigeria", "Colombia", "USA", "India", "Australia")

# Generate and display the plots
plots <- lapply(country_names, function(country) {
  filtered_data <- world %>% filter(Kraj == country)
  generate_plot(filtered_data, country)
})

# Combine the plots
combined_plot <- do.call(gridExtra::grid.arrange, c(plots, nrow = 2, bottom = "common"))

# Display the combined plot
print(combined_plot)


world2 <- map_data("world")

# Function to generate the plot
generate_plot <- function(data, date, title) {
  plot_data <- world2 %>%
    merge(data, by.x = "region", by.y = "Kraj", all.x = TRUE) %>%
    arrange(group, order)
  
  ggplot(plot_data, aes(x = long, y = lat, group = group, fill = WCSIR, color = WCSIR)) +
    geom_polygon(color = "white", size = 0.2) +
    scale_fill_viridis("", na.value = "gray90") +
    theme_minimal() +
    theme(
      axis.text = element_blank(),
      axis.title = element_blank(),
      panel.grid = element_blank()
    ) +
    scale_fill_gradient(low = "#ADD8E6", high = "#00008B", limits = c(0, 70)) +
    ggtitle(title)
}

# List of dates and corresponding titles
dates <- c("2021-02-01", "2021-05-01", "2021-08-01", "2021-12-01")
titles <- c("February 2021", "May 2021", "August 2021", "December 2021")

# Generate and display the plots
plots <- lapply(dates, function(date) {
  filtered_data <- world %>% filter(Data == date)
  generate_plot(filtered_data, date, titles[dates == date])
})

# Display the plots
for (plot in plots) {
  print(plot)
}