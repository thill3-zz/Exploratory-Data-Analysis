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

# 3) Of the four types of sources indicated by the type 
# (point, nonpoint, onroad, nonroad) variable, which of these four sources have 
# seen decreases in emissions from 1999-2008 for Baltimore City? 
# Which have seen increases in emissions from 1999-2008? Use the ggplot2 
# plotting system to make a plot answer this question.
library(dplyr)
NEIT <- tibble(NEI)
sumBCMByYearType <- NEI %>% 
        filter(fips == "24510") %>% 
        group_by(year, type) %>% 
        summarise(sum(Emissions))
sumBCMByYearType1 <- as.data.frame(sumBCMByYearType)
library(ggplot2)
png(filename = "plot3.png",
    width = 480,
    height = 480)
with(sumBCMByYearType1
     , qplot(year
             , sumBCMByYearType1[,3]
             , data = sumBCMByYearType1
             , facets = .~type
             , ylab = "Total Emissions"
     )
     + theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)
             , plot.title = element_text(hjust = 0.5)) 
     + geom_smooth(method = "lm", se = FALSE)
     + ggtitle("Baltimore City Maryland - sum of Pm2.5 by Year and Type")
)
dev.off()
#the axis label rotation code can be found in a lot of places on the internet
#Looks like all the sources except "POINT" have decreased over time

#try this for better aesthetics - 2x2 grid
# ggplot(NEI3.2,aes(year,Emissions))
# + geom_bar(stat="identity")
# + facet_wrap(~type)
# + ggtitle("Total PM2.5 Emissions in Baltimore by Type from '99 to '08")

#or a 1x4 grid with colored bars
# ggplot(baltimoreNEI,aes(factor(year),Emissions,fill=type)) 
# + geom_bar(stat="identity") 
# + theme_bw() 
# + guides(fill=FALSE)
# + facet_grid(.~type,scales = "free",space="free") 
# + labs(x="year", y=expression("Total PM"[2.5]*" Emission (Tons)")) 
# + labs(title=expression("PM"[2.5]*" Emissions, Baltimore City 1999-2008 by Source Type"))

#Or four line graphs on one grid
# chart <- ggplot(balt_emission, aes(year, Emissions, color = type)) 
# chart 
# + geom_line() 
# + xlab("year") 
# + ylab("Total Emissions") 
# +ggtitle("Total Baltimore Emissions" )