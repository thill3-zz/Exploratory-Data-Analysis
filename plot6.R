a <- Sys.time(); NEI <- readRDS("summarySCC_PM25.rds"); b <- Sys.time(); b-a
#18-19 seconds
c <- Sys.time(); SCC <- readRDS("Source_Classification_Code.rds"); d <- Sys.time(); d-c
#<.2 seconds
names(NEI)
dim(NEI) #6497651x6
#[1] "fips"      "SCC"       "Pollutant" "Emissions" "type"      "year"
names(SCC)
dim(SCC) #11717x15
# [1] "SCC"                 "Data.Category"       "Short.Name"          "EI.Sector"          
# [5] "Option.Group"        "Option.Set"          "SCC.Level.One"       "SCC.Level.Two"      
# [9] "SCC.Level.Three"     "SCC.Level.Four"      "Map.To"              "Last.Inventory.Year"
# [13] "Created_Date"        "Revised_Date"        "Usage.Notes" 

# 6) Compare emissions from motor vehicle sources in Baltimore City with 
# emissions from motor vehicle sources in Los Angeles County, California 
# (fips == "06037"). Which city 
# has seen greater changes over time in motor vehicle emissions?
isVeh <- function(input) {
        (grepl("[Vv]eh", input))
}
library(dplyr)
SCCVehCodes1 <- SCC$SCC[isVeh(SCC$Short.Name)]
NEIBCMLACVehByYear <- NEI %>% 
        filter(SCC %in% SCCVehCodes1, fips == "24510" | fips == "06037") %>% 
        group_by(fips, year) %>% 
        summarise(sum(Emissions))
NEIBCMLACVehByYear1 <- as.data.frame(NEIBCMLACVehByYear)
#png(filename = "plot6.png",
#    width = 480,
#    height = 480)
par(mar = c(5,5,2,2))
library(ggplot2)
fips_names <- c(
        '06037' = "Los Angeles County, CA",
        '24510' = "Baltimore City, MD"
)
#Credit to https://stackoverflow.com/questions/3472980/ggplot-how-to-change-facet-labels
#for the above line of code
with(NEIBCMLACVehByYear1,
     qplot(x = year
           , y = NEIBCMLACVehByYear1[,3]
           , data = NEIBCMLACVehByYear1
           , facets = .~fips
           , ylab = "Sum of PM2.5 Emissions by vehicles"
     )
     + theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)
             , plot.title = element_text(hjust = 0.5)) 
     + geom_smooth(method = "lm", se = FALSE)
     + facet_grid(.~fips, labeller = as_labeller(fips_names))
     #Credit to https://stackoverflow.com/questions/3472980/ggplot-how-to-change-facet-labels
     #for the facet_grid code
)
#dev.off()

#LA's emissions have gone up, and Baltimore City's have gone down. LA's appear
#to have changed more.

#This might be more aesthetically pleasing
# ggplot(bothNEI, aes(x=factor(year), y=Emissions, fill=city)) 
#         + geom_bar(aes(fill=year),stat="identity") 
#         + facet_grid(scales="free", space="free", .~city) 
#         + labs(x="year", y=expression("Total PM"[2.5]*" Emission (Kilo-Tons)")) 
#         + labs(title=expression("PM"[2.5]*" Motor Vehicle Source Emissions in Baltimore & LA, 1999-2008"))

#Two line graphs on one grid with legend
# qplot(Year, Emissions, data = Baltimore_vs_LA_car_year, color = City, geom = "line") 
# + ggtitle("Emissions in Baltimore City and LA County by Motor Vehicle ") 
# + ylab("Total Emissions (tons)") 
# + xlab("Year") 