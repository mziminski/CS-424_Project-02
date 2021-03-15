# libraries to include
install.packages("xlsx")                                         
library("xlsx")
library(reshape2)
# Prevent numbers from going to Scientific Notation
options(scipen = 999)

# Interested in rows: TYPE OF PRODUCER,ENERGY SOURCE equal to Total Electric Power Industry,Total
# [You] should convert the STATE, TYPE OF PRODUCER, and ENERGY SOURCE to categorical values
setwd("/Users/mziminski/Developer/School Projects/cs424/CS-424_Project-02/Data")
file = "egrid2018_data_v2.xlsx"

## download Excel spreadsheet into RStudio, then convert to dataframe, and start at 2nd row 
# skip first row (col name descriptions)
dfdata <- xlsx::read.xlsx(file, sheetName="PLNT18", as.data.frame=TRUE)
# get column names
# colnames(dfdata)

# Calculate the percentage of the specific energy source of the total net gen
dfdata["PERCENT_COAL_GEN"] <- round(dfdata["COAL_GEN"] / dfdata["NET_GEN"], digits=3) * 100
dfdata["PERCENT_OIL_GEN"] <- round(dfdata["OIL_GEN"] / dfdata["NET_GEN"], digits=3) * 100
dfdata["PERCENT_GAS_GEN"] <- round(dfdata["GAS_GEN"] / dfdata["NET_GEN"], digits=3) * 100
dfdata["PERCENT_NUCLEAR_GEN"] <- round(dfdata["NUCLEAR_GEN"] / dfdata["NET_GEN"], digits=3) * 100
dfdata["PERCENT_HYDRO_GEN"] <- round(dfdata["HYDRO_GEN"] / dfdata["NET_GEN"], digits=3) * 100
dfdata["PERCENT_BIOMASS_GEN"] <- round(dfdata["BIOMASS_GEN"] / dfdata["NET_GEN"], digits=3) * 100
dfdata["PERCENT_WIND_GEN"] <- round(dfdata["WIND_GEN"] / dfdata["NET_GEN"], digits=3) * 100
dfdata["PERCENT_SOLAR_GEN"] <- round(dfdata["SOLAR_GEN"] / dfdata["NET_GEN"], digits=3) * 100
dfdata["PERCENT_GEOTHERMAL_GEN"] <- round(dfdata["GEOTHERMAL_GEN"] / dfdata["NET_GEN"], digits=3) * 100
dfdata["PERCENT_OTHER_GEN"] <- round(dfdata["OTHER_GEN"] / dfdata["NET_GEN"], digits=3) * 100


# Calculate the percentage of nonrenewable energy of the total net gen
dfdata["PERCENT_NONRENEWABLE_GEN"] <- round(dfdata["NET_NONRENEWABLE_GEN"] / dfdata["NET_GEN"], digits=3) * 100
# Calculate the percentage of renewable energy of the total net gen
dfdata["PERCENT_RENEWABLE_GEN"] <- round(dfdata["NET_RENEWABLE_GEN"] / dfdata["NET_GEN"], digits=3) * 100

# from https://stackoverflow.com/questions/18142117/how-to-replace-nan-value-with-zero-in-a-huge-data-frame
is.nan.data.frame <- function(x)
do.call(cbind, lapply(x, is.nan))
dfdata[is.nan(dfdata)] <- 0.0

# Export df as XLSX
write.xlsx(dfdata, "egrid2018_data_v2.xlsx", sheetName="PLNT18", row.names=FALSE, showNA=FALSE)


