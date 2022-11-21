#install RSQLite
install.packages("RSQLite");
library("RSQLite");

#connect to RSQLite
conn <- dbConnect(RSQLite::SQLite(),"Querying_DatabaseDB.sqlite")
conn

#CROP_DATA
df1 <- dbExecute(conn,
                "CREATE TABLE CROP_DATA (
                  CD_ID INTEGER NOT NULL,
                  YEAR DATE NOT NULL,
                  CROP_TYPE VARCHAR(20) NOT NULL,
                  GEO VARCHAR(20) NOT NULL,
                  SEEDED_AREA INTEGER NOT NULL,
                  HARVESTED_AREA INTEGER NOT NULL,
                  PRODUCTION INTEGER NOT NULL,
                  AVG_YIELD INTEGER NOT NULL,
                  PRIMARY KEY (CD_ID)
                )",
                errors=FALSE)
if (df1 == -1){
  cat ("An error has occurred. \n")
  msg <- odbcGetErrMsg(conn)
  print(msg)
} else {
  cat ("Table was created successfully.\n")
}

#FARM_PRICES
df2 <- dbExecute(conn,
                "CREATE TABLE FARM_PRICES (
                CD_ID INTEGER NOT NULL,
                DATE DATE NOT NULL,
                CROP_TYPE VARCHAR(20) NOT NULL,
                GEO VARCHAR(20) NOT NULL,
                PRICE_PRERMT FLOAT,
                PRIMARY KEY (CD_ID)
                )",
                errors=FALSE)

if (df2 == -1){
  cat ("An error has occurred. \n")
  msg <- odbcGetErrMsg(conn)
  print(msg)
} else {
  cat ("Table was created successfully.\n")
}

#DAILY_FX
df3 <- dbExecute(conn,
                "CREATE TABLE DAILY_FX (
                DFX_ID INTEGER NOT NULL,
                DATE DATE NOT NULL,
                FXUSDCAD FLOAT,
                PRIMARY KEY (DFX_ID)
                )",
                errors=FALSE)

if (df3 == -1){
  cat ("An error has occurred. \n")
  msg <- odbcGetErrMsg(conn)
  print(msg)
} else {
  cat ("Table was created successfully.\n")
}

#MONTHLY_FX
df4 <- dbExecute(conn,
                "CREATE TABLE MONTHLY_FX (
                DFX_ID INTEGER NOT NULL,
                DATE DATE NOT NULL,
                FXUSDCAD FLOAT,
                PRIMARY KEY (DFX_ID)
                )",
                errors=FALSE)

if (df4 == -1){
  cat ("An error has occurred. \n")
  msg <- odbcGetErrMsg(conn)
  print(msg)
} else {
  cat ("Table was created successfully.\n")
}

#load data
crop_data_df <- read.csv('https://cf-courses-data.s3.us.cloud-object-storage.appdomain.cloud/IBM-RP0203EN-SkillsNetwork/labs/Final%20Project/Annual_Crop_Data.csv')

farm_prices_df <- read.csv('https://cf-courses-data.s3.us.cloud-object-storage.appdomain.cloud/IBM-RP0203EN-SkillsNetwork/labs/Final%20Project/Monthly_Farm_Prices.csv')

daily_fx_df <- read.csv('https://cf-courses-data.s3.us.cloud-object-storage.appdomain.cloud/IBM-RP0203EN-SkillsNetwork/labs/Final%20Project/Daily_FX.csv')

monthly_fx_df <- read.csv('https://cf-courses-data.s3.us.cloud-object-storage.appdomain.cloud/IBM-RP0203EN-SkillsNetwork/labs/Final%20Project/Monthly_FX.csv')

head(crop_data_df)
head(farm_prices_df)
head(daily_fx_df)
head(monthly_fx_df)

dbWriteTable(conn, "CROP_DATA", crop_data_df, overwrite=TRUE, header=TRUE)
dbWriteTable(conn, "FARM_PRICES", farm_prices_df,overwrite=TRUE, header=TRUE)
dbWriteTable(conn, "DAILY_FX", daily_fx_df, overwrite=TRUE, header=TRUE)
dbWriteTable(conn, "MONTHLY_FX", monthly_fx_df, overwrite=TRUE, header=TRUE)

#count records from FARM_PRICES

dbGetQuery(conn, "SELECT COUNT (*) FROM FARM_PRICES")

#list which geographies are included in the farm prices data set

dbGetQuery(conn, "SELECT DISTINCT (GEO) FROM FARM_PRICES")

#how many hectares of Rye were harvested in Canada in 1968?

dbGetQuery(conn, "SELECT SUM (HARVESTED_AREA) FROM CROP_DATA
           WHERE CROP_TYPE='Rye' AND STRFTIME('%Y',YEAR)='1968'
           AND GEO = 'Canada'")

#query and display the first six rows of the farm prices table for Rye

dbGetQuery(conn, "SELECT * FROM FARM_PRICES WHERE CROP_TYPE = 'Rye' LIMIT 6")

#which provinces grew Barley?

dbGetQuery(conn, "SELECT DISTINCT(GEO) FROM FARM_PRICES WHERE CROP_TYPE = 'Barley'")

#find the first and last dates for the farm prices data

dbGetQuery(conn, "SELECT MIN(DATE), MAX(DATE) FROM FARM_PRICES")

#which crops have ever reached a farm price greater than or equal to $350 per metric tonne?

dbGetQuery(conn, "SELECT DISTINCT(CROP_TYPE) FROM FARM_PRICES
           WHERE PRICE_PRERMT <= 350")

#rank the crop types harvested in Saskatchewan in the year 2000 by their yield

dbGetQuery(conn, "SELECT YEAR, CROP_TYPE, GEO, HARVESTED_AREA, AVG_YIELD FROM CROP_DATA
           WHERE STRFTIME('%Y',YEAR)='2000' AND GEO = 'Saskatchewan'
           ORDER BY AVG_YIELD DESC")

#rank the crops and geographies by their average yield since 2000 to show which crop and province have had the highest average yield

dbGetQuery(conn, "SELECT CROP_TYPE, GEO, AVG(AVG_YIELD) AS AVG_YD FROM CROP_DATA
           WHERE STRFTIME('%Y',YEAR)>='2000'
           GROUP BY CROP_TYPE, GEO
           ORDER BY AVG(AVG_YIELD) DESC")

#use a subquery to determine how much wheat was harvested in Canada in the most recent year of the data

dbGetQuery(conn, "SELECT CROP_TYPE, GEO, YEAR, HARVESTED_AREA FROM CROP_DATA 
           WHERE YEAR = (SELECT MAX(YEAR) FROM CROP_DATA) AND 
           GEO = 'Canada' AND CROP_TYPE = 'Wheat'")

#use an implicit inner join to get the monthly price per metric tonne of Canola grown in Saskatchewan in both Canadian and USD

dbGetQuery(conn, "SELECT M.DATE, F.CROP_TYPE, F.GEO, F.PRICE_PRERMT AS PRICE_PER_MT, (F.PRICE_PRERMT / M.FXUSDCAD) AS PRICE_PER_MT_USD
           FROM FARM_PRICES F, MONTHLY_FX M
           WHERE F.DATE = M.DATE
           AND F.CROP_TYPE = 'Canola' AND F.GEO = 'Saskatchewan'
           ORDER BY M.DATE DESC
           LIMIT 6")

