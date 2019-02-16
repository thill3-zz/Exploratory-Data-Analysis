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

# 4) Across the United States, how have emissions from coal combustion-related 
# sources changed from 1999-2008?
isCoal <- function(input) {
        (grepl("[Cc]oal", input))
}
library(dplyr)
SCCCoalCodes <- SCC$SCC[isCoal(SCC$Short.Name)]
NEICoalByYear <- NEI %>% 
        filter(SCC %in% SCCCoalCodes) %>% 
        group_by(year) %>% 
        summarise(sum(Emissions))
NEICoalByYear1 <- as.data.frame(NEICoalByYear)
png(filename = "plot4.png",
    width = 480,
    height = 480)
par(mar = c(5,5,2,2))
library(ggplot2)
with(NEICoalByYear1,
     qplot(year
           , NEICoalByYear1[,2]
           , data = NEICoalByYear1
           , ylab = "Sum of PM2.5 Emissions by coal"
     )
     + theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)
             , plot.title = element_text(hjust = 0.5)) 
     + geom_smooth(method = "lm", se = FALSE)
)
dev.off()
#We see that the trend is downward, and coal emissions have decreased 
#significantly