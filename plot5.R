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

# 5) How have emissions from motor vehicle sources changed from 1999-2008 in 
# Baltimore City?
isVeh <- function(input) {
        (grepl("[Vv]eh", input))
}
library(dplyr)
SCCVehCodes <- SCC$SCC[isVeh(SCC$Short.Name)]
NEIBCMVehByYear <- NEI %>% 
        filter(SCC %in% SCCVehCodes, fips == "24510") %>% 
        group_by(year) %>% 
        summarise(sum(Emissions))
NEIBCMVehByYear1 <- as.data.frame(NEIBCMVehByYear)
png(filename = "plot5.png",
    width = 480,
    height = 480)
par(mar = c(5,5,2,2))
library(ggplot2)
with(NEIBCMVehByYear1,
     qplot(year
           , NEIBCMVehByYear1[,2]
           , data = NEIBCMVehByYear1
           , ylab = "Sum of PM2.5 Emissions by vehicles\n In Baltimore City Maryland"
     )
     + theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)
             , plot.title = element_text(hjust = 0.5)) 
     + geom_smooth(method = "lm", se = FALSE)
)
dev.off()

#That would indicate that emissions from vehicles have decreased overall

#connected lines
# chart <- ggplot(baltimore_car_year,aes(year, Emissions)) 
# chart 
# + geom_line() 
# + xlab("year") 
# + ylab("Total Emissions") 
# + ggtitle("Total Baltimore Emissions by Motor Vehicle" )