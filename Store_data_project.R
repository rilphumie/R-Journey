Store_data <- read_excel("Store data (1).xlsx")
Store_data
storedata = as.data.frame(read_xlsx("Store data (1).xlsx"))
# 2. The line  of code above reads the store_data as a dataframe instead of a tibble
storedata
unique(storedata$FORMAT)
#3A There are 3 major store format which are Convenience, Chemist and Destination.The syntax UNIQUE displays the unique and distinct variable.
unique(storedata$Region)
#B There are 3 number of regions which are Region 1, 2 and 3. I also used the unique syntax to derive this
unique(storedata$YEAR)
#C The year of sale is 2007 alone. The sales record just a Year.
store_19 <- storedata[storedata$STORE_ID == "19",]
# Dataframe for store19 
store_19
#4 This displays records for store 19 alone before calculating the sum of all tax-included sales
sum(store_19$SALES_TISP)
# The sum of tax-included sales for store 19 is 8557.75 for all time.
Store_Copy1 <- read_excel("Store data (1).xlsx")
Store_Copy <- as.data.frame(Store_Copy)
#5 Creating a new copy of the storedata dataframe.
View(Store_Copy)
#6 viewing and visually inspecting the new duplicated data
#7 finding the number of stores that do not have 12 months of sales information.
#a first remove all duplicate values, and create a new table. 
sum(!duplicated(storedata[,c("STORE_ID", "MONTH")]))
#b creating new dataframe called storeid, using Store_id and Month as arguements
storeid <- table(storedata$STORE_ID,storedata$MONTH)
storeid
#c The code below converts the table created in #b back to dataframe.
countrow <- as.data.frame.matrix(storeid)
#d counting all rows in this dataframe and appending a column called Total
countrow$Total <- rowSums(countrow)
countrow
#e subsetting the dataframe to find stores with column less than 12 and getting a summary of the dataframe
summary(countrow)
row.names(countrow[countrow$Total < 12,])

#sum(countrow < 12);
#View(storeid[countrow < 12,])
View(countrow[countrow$Total < 12,])


# SECTION 2 
Store_Copy
Store_Copy$avg_price <- Store_Copy$SALES_TISP/Store_Copy$SALES_UNITS 
# Code above adds a new column avg_price to the dataframe. This is derived by dividing SALES_TISP by the SALES_UNITS
Store_Copy
Store_Copy$Space_Yield <- Store_Copy$SALES_TISP/Store_Copy$SPACE
# Code above adds a new column Space_Yield to the dataframe 
Store_Copy
Store_Copy$NDSA_Yield <- Store_Copy$SALES_TISP/Store_Copy$NDSA 
# Code above adds a new column NDSA_Yield to the dataframe. The column is derived from dividing SALES_TISP by NDSA 
Store_Copy
Store_Copy$LOG_TISP <- log10(Store_Copy$SALES_TISP)
# Code above adds a new column LOG_TISP to the dataframe. The column shows the log10 of SALES_TISP
Store_Copy
Store_Copy$LOG_TESP <- log10(Store_Copy$SALES_TESP)
# Code above adds a new column LOG_TESP to the dataframe. The column shows the log10 of SALES_TESP
Store_Copy$LOG_Space <- log10(Store_Copy$SPACE)
# Code above adds a new column LOG_SPACE to the dataframe. The column shows the log10 of SPACE
Store_Copy$LOG_NDSA <- log10(Store_Copy$NDSA)
# Code above adds a new column LOG_NDSA to the dataframe. The column shows the log10 of NDSA
Store_Copy$LOG_Price <- log10(Store_Copy$avg_price)
# Code above adds a new column LOG_Price to the dataframe. The column shows the log10 of avg_Price
Store_Copy$LOG_Space_Yield <- log10(Store_Copy$Space_Yield)
# Code above adds a new column LOG_Space_Yield to the dataframe. The column shows the log10 of Space_Yield
Store_Copy$LOG_NDSA_Yield <- log10(Store_Copy$NDSA_Yield)
# Code above adds a new column LOG_NDSA_Yield to the dataframe. The column shows the log10 of NDSA_Yield
Store_Copy
Store_Copy$Date <- as.Date(with(Store_Copy, paste(YEAR, MONTH, 1, sep = "-")),"%Y-%m-%d")
# A new Date column has been added to the table with the code above.
Store_Copy

#2 creating a column RegionsText and passing in South, Pacific and Mid-Atlantic as arguements.
Store_Copy$RegionsText <- if (Store_Copy$Region = 1) {
  "South"
}else if (Store_Copy$Region = 2) {
  "Pacific"
}else "Mid_Atlantic"

Store_Copy$RegionsText = NULL;

Store_Copy$RegionsText = "";
Store_Copy$RegionsText[Store_Copy$Region == 1] = "South";
Store_Copy$RegionsText[Store_Copy$Region == 2] = "Pacific";
Store_Copy$RegionsText[Store_Copy$Region == 3] = "Mid_Atlantic";
table(Store_Copy$Region, Store_Copy$RegionsText)

#3 CONVENIENCE (sales_tisp)
salestispconvenience <- Store_Copy[Store_Copy$FORMAT == "Convenience",]
salestispconvenience
# creating a seperate table for convenience Format and getting the SALES_TISP and NDSA
sum_convenience <- sum(salestispconvenience$SALES_TISP)
sum_convenience
# Getting the sum or total of SALES_TISP. The value of the sum is 90090.74
len_convenience <- length(salestispconvenience$SALES_TISP)
len_convenience
# Deriving the length or count of sales_tisp to get the average, which is 54
ave_convenience <- sum_convenience/len_convenience
ave_convenience
# Dividing the sum of sales_tisp by its count gives the average for salestisp. The value is 1668.347

# A seperate dataframe for Convenience format and SALES_TISP
maxquant <- Store_Copy[Store_Copy$FORMAT == "Convenience","SALES_TISP"]
# Displays the maximum of convenience sales_tisp which is 4244.46
max(maxquant)
# Displays the minimum of convenience sales_tisp which is 195.48
min(maxquant)
# Cutoff value for highest 1% is 4090.071
high <- quantile(maxquant, 0.99)
high
# Cutoff value for lowest 1% is 232.5
low <- quantile(maxquant, 0.01)
low 
# Number of values in the highest 1%
length(low)


# CONVENIENCE (NDSA)
sum_convenience_NDSA <- sum(salestispconvenience$NDSA)
sum_convenience_NDSA
# Getting the sum or total of NDSA. The value of the sum is 20002
len_convenience_NDSA <- length(salestispconvenience$NDSA)
len_convenience_NDSA
# Deriving the length or count of NDSA to get the average, which is 54
ave_convenience_NDSA <- sum_convenience_NDSA/len_convenience_NDSA
ave_convenience_NDSA
# Dividing the sum of NDSA by its count gives the average for NDSA. The value is 370.4074

# A seperate dataframe for Convenience format and NDSA
quantNDSA <- Store_Copy[Store_Copy$FORMAT == "Convenience","NDSA"]
# Displays the maximum of convenience NDSA which is 777
max(quantNDSA)
# Displays the minimum of convenience NDSA which is 215
min(quantNDSA)
# Cutoff value for highest 1% is 777
highNDSA <- quantile(quantNDSA, 0.99)
highNDSA
length(highNDSA)
# Cutoff value for lowest 1% is 215
lowNDSA <- quantile(quantNDSA, 0.01)
lowNDSA 
# Number of values in the highest 1%
length(low)



# CHEMIST(sales_tisp)
salestispchemist <- Store_Copy[Store_Copy$FORMAT == "Chemist",]
salestispchemist
# creating a seperate table for chemist Format and getting the SALES_TISP and NDSA
sum_chemist <- sum(salestispchemist$SALES_TISP)
sum_chemist
# Getting the sum or total of SALES_TISP. The value of the sum is 74083.14
len_chemist <- length(salestispchemist$SALES_TISP)
len_chemist
# Deriving the length or count of sales_tisp to get the average, which is 84
ave_chemist <- sum_chemist/len_chemist
ave_chemist
# Dividing the sum of sales_tisp by its count gives the average for salestisp. The value is 881.9421

# A seperate dataframe for Chemist format and SALES_TISP
quantsalestisp_chem <- Store_Copy[Store_Copy$FORMAT == "Chemist","SALES_TISP"]
quantsalestisp_chem
# Displays the maximum of chemist sales_tisp which is 3681.9
max(quantsalestisp_chem)
# Displays the minimum of chemist sales_tisp which is 87.89
min(quantsalestisp_chem)
# Cutoff value for highest 1% is 3087.803
highchem_ST <- quantile(quantsalestisp_chem, 0.99)
highchem_ST
length(highchem_ST)
# Cutoff value for lowest 1% is 142.7281
lowchem_ST <- quantile(quantsalestisp_chem, 0.01)
lowchem_ST 
# Number of values in the highest 1%
length(lowchem_ST )


# CHEMIST(NDSA)
sum_chemist_NDSA <- sum(salestispchemist$NDSA)
sum_chemist_NDSA
# Getting the sum or total of NDSA. The value of the sum is 20652
len_chemist_NDSA <- length(salestispchemist$NDSA)
len_chemist_NDSA
# Deriving the length or count of NDSA to get the average, which is 84
ave_chemist_NDSA <- sum_chemist_NDSA/len_chemist_NDSA
ave_chemist_NDSA
# Dividing the sum of NDSA by its count gives the average for NDSA. The value is 245.8571

# A seperate dataframe for Chemist format and NDSA
quantNDSA_chem <- Store_Copy[Store_Copy$FORMAT == "Chemist","NDSA"]
quantNDSA_chem
# Displays the maximum of chemist NDSA which is 294
max(quantNDSA_chem)
# Displays the minimum of chemist NDSA which is 199
min(quantNDSA_chem)
# Cutoff value for highest 1% is 294
highchem_NDSA <- quantile(quantNDSA_chem, 0.99)
highchem_NDSA
length(highchem_NDSA)
# Cutoff value for lowest 1% is 199
lowchem_NDSA <- quantile(quantNDSA_chem, 0.01)
lowchem_NDSA 
# Number of values in the highest 1%
length(lowchem_NDSA )

# DESTINATION(sales_tisp)
salestispdestination <- Store_Copy[Store_Copy$FORMAT == "Destination",]
salestispdestination
# creating a seperate table for destination Format and getting the SALES_TISP and NDSA
sum_destination <- sum(salestispdestination$SALES_TISP)
sum_destination
# Getting the sum or total of SALES_TISP. The value of the sum is 1515225
len_destination <- length(salestispdestination$SALES_TISP)
len_destination
# Deriving the length or count of sales_tisp to get the average, which is 1062
ave_destination <- sum_destination/len_destination
ave_destination
# Dividing the sum of sales_tisp by its count gives the average for salestisp. The value is 1426.766


# A seperate dataframe for Destination format and SALES_TISP
quantsalestisp_dest <- Store_Copy[Store_Copy$FORMAT == "Destination","SALES_TISP"]
quantsalestisp_dest
# Displays the maximum of Destination sales_tisp which is 4594.17
max(quantsalestisp_dest)
# Displays the minimum of Destination sales_tisp which is 43.28
min(quantsalestisp_dest)
# Cutoff value for highest 1% is 3967.732
highdest_ST <- quantile(quantsalestisp_dest, 0.99)
highdest_ST
length(highdest_ST)
# Cutoff value for lowest 1% is 127.5327
lowdest_ST <- quantile(quantsalestisp_dest, 0.01)
lowdest_ST 
# Number of values in the highest 1%
length(lowdest_ST )

#DESTINATION(NDSA)
sum_destination_NDSA <- sum(salestispdestination$NDSA)
sum_destination_NDSA
# Getting the sum or total of NDSA. The value of the sum is 740559
len_dest_NDSA <- length(salestispdestination$NDSA)
len_dest_NDSA
# Deriving the length or count of NDSA to get the average, which is 1062
ave_dest_NDSA <- sum_destination_NDSA/len_dest_NDSA
ave_dest_NDSA
# Dividing the sum of NDSA by its count gives the average for NDSA. The value is 697.3249

# A seperate dataframe for Chemist format and NDSA
quantNDSA_dest <- Store_Copy[Store_Copy$FORMAT == "Destination","NDSA"]
quantNDSA_dest
# Displays the maximum of Destination NDSA which is 2652
max(quantNDSA_dest)
# Displays the minimum of Destination NDSA which is 199
min(quantNDSA_dest)
# Cutoff value for highest 1% is 2147
highdest_NDSA <- quantile(quantNDSA_dest, 0.99)
highdest_NDSA
length(highdest_NDSA)
# Cutoff value for lowest 1% is 199
lowdest_NDSA <- quantile(quantNDSA_dest, 0.01)
lowdest_NDSA 
# Number of values in the highest 1%
length(lowdest_NDSA )


#4
install.packages("dplyr")
library(dplyr)