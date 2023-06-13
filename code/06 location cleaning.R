# setup #####################
library(tidyverse)
library(dplyr)
library(readxl)
library(readr)
library(writexl)


# load data ##################
df <- read_xlsx("data/Cotton heatress Longitde and Latitude.xlsx")
write.csv(df, file = "data/Cotton heatress Longitde and Latitude.csv", row.names = FALSE)


df_c <- read_csv("data/Cotton heatress Longitde and Latitude.csv")
df_q <- read_csv("output/cleaned/cleaned.csv")

loc_q <- df_q %>%
  dplyr::select(loc, loccode) %>% 
  arrange(loccode) %>% 
  distinct()

loc_c <- df_c %>% 
  dplyr::select(loc, loccode) %>%
  arrange(loccode) %>%
  distinct() 


# df_c
if ("loc" %in% colnames(df_c)) {
  # Phoenix, AZ
  df_c$loc[df_c$loc == "Phenix, AZ"] <- "Phoenix, AZ"
  # El Paso, TX(irri)
   df_c$loc[df_c$loc == "EL Paso, TX(irri)"] <- "El Paso, TX(irri)"
   df_c$loc[df_c$loc == "EL Paso, TX(PIMA)"] <- "El Paso, TX(irri)"
  # Pecos, TX(irri)
   df_c$loc[df_c$loc == "Pecos, TX (IRRI)"] <- "Pecos, TX(irri)"
   df_c$loc[df_c$loc == "Pecos"] <- "Pecos, TX(irri)"
  # Lubboc, TX(irri)
   df_c$loc[df_c$loc == "lubbock, TX(irri"] <- "Lubbock, TX(irri)"
   df_c$loc[df_c$loc == "LUB"] <- "Lubbock, TX(irri)"
   df_c$loc[df_c$loc == "Lubbock"] <- "Lubbock, TX"
   df_c$loc[df_c$loc == "lubbock"] <- "Lubbock, TX"
   df_c$loc[df_c$loc == "Lubbock (irri)"] <- "Lubbock, TX(irri)"
   df_c$loc[df_c$loc == "Lubbock, TX(IRR)"] <- "Lubbock, TX(irri)"
   df_c$loc[df_c$loc == "lubbock"] <- "Lubbock, TX"
  # Chillicothe, TX(irri)   Chillicothe, TX(iiri)
   df_c$loc[df_c$loc == "Chillicoth, TX(irri)"] <- "Chillicothe, TX(irri)"
   df_c$loc[df_c$loc == "Chillicothe, TX(iiri)"] <- "Chillicothe, TX(irri)"
   df_c$loc[df_c$loc == "Chillicothe, TX(IRR)"] <- "Chillicothe, TX(irri)"
   df_c$loc[df_c$loc == "hillicothe, TX"] <- "Chillicothe, TX(irri)"
  #  Altus, OK(irri)
   df_c$loc[df_c$loc == "Altus, TX(irri)"] <- "Altus, OK(irri)"   
   df_c$loc[df_c$loc == "Altus"] <- "Altus, OK"
   # College Station, TX
   df_c$loc[df_c$loc == "College Station"] <- "College Station, TX" 
   df_c$loc[df_c$loc == "College station"] <- "College Station, TX" 
   # Weslaco, TX 
   df_c$loc[df_c$loc == "Weslaco"] <- "Weslaco, TX"  
   df_c$loc[df_c$loc == "Weslago, TX"] <- "Weslaco, TX"
   df_c$loc[df_c$loc == "Weslago"] <- "Weslaco, TX"
   # Bossier City, LA
   df_c$loc[df_c$loc == "Bosserier City, LA"] <- "Bossier City, LA"
   df_c$loc[df_c$loc == "Bossier City, TX"] <- "Bossier City, LA"
   df_c$loc[df_c$loc == "Bossier City"] <- "Bossier City, LA"
   # Saint Joseph, LA
   df_c$loc[df_c$loc == "Saint Joseph, TX"] <- "Saint Joseph, LA"
   df_c$loc[df_c$loc == "San Joseph, LA"] <- "Saint Joseph, LA"
   df_c$loc[df_c$loc == "Saint Joseph"] <- "Saint Joseph, LA"
   # Stoneville, MS
   df_c$loc[df_c$loc == "stoneville, MS"] <- "Stoneville, MS"
   df_c$loc[df_c$loc == "Stonvelle, MS"] <- "Stoneville, MS"
   df_c$loc[df_c$loc == "Stoneville"] <- "Stoneville, MS" 
   df_c$loc[df_c$loc == "Stoneville,MS"] <- "Stoneville, MS" 
   # Clarkdale, AZ
   df_c$loc[df_c$loc == "Clarkedale, AR"] <- "Clarkdale, AZ"
   df_c$loc[df_c$loc == "Clarkdale, AR"] <- "Clarkdale, AZ"
   # Auburn, AL
   df_c$loc[df_c$loc == "Alburn, AL"] <- "Auburn, AL"
   df_c$loc[df_c$loc== "Aubur, AL"] <- "Auburn, AL"
   df_c$loc[df_c$loc == "Auburn"] <- "Auburn, AL"
   # Tifton, GA
   df_c$loc[df_c$loc == "Tipton, GA"] <- "Tifton, GA"
   # Florence, SC
   df_c$loc[df_c$loc == "Forence, SC"] <- "Florence, SC"
   df_c$loc[df_c$loc == "Florence"] <- "Florence, SC"
   # Rocky Mount, NC
   df_c$loc[df_c$loc == "Ricky Mount, NC"] <- "Rocky Mount, NC"
   df_c$loc[df_c$loc == "Rocky mount"] <- "Rocky Mount, NC"
   # Chickasha, OK
   df_c$loc[df_c$loc == "Chickasa, OK(dry)"] <- "Chickasha, OK(dry)"
   df_c$loc[df_c$loc == "Chickasha, TX(dry)"] <- "Chickasha, OK(dry)"
   df_c$loc[df_c$loc == "Chickasa, OK"] <- "Chickasha, OK"
   df_c$loc[df_c$loc == "Chickasha (dry)"] <- "Chickasha, OK(dry)"
   df_c$loc[df_c$loc == "Chickasha OK(dry)"] <- "Chickasha, OK(dry)"
   df_c$loc[df_c$loc == "Chickasha(dry)"] <- "Chickasha, OK(dry)"
   df_c$loc[df_c$loc == "Chickasha (irri)"] <- "Chickasha, OK(irri)"
   df_c$loc[df_c$loc == "Chickasha(irri)"] <- "Chickasha, OK(irri)"
   df_c$loc[df_c$loc == "Chickasha, TX(irri)"] <- "Chickasha, OK(irri)"
   df_c$loc[df_c$loc == "Chickasa, OK(irri)"] <- "Chickasha, OK(irri)"
   # Artesia, NM(irri)
   df_c$loc[df_c$loc == "Artesia, Nm(irri)"] <- "Artesia, NM(irri)"
   # Maricopa, AZ
   df_c$loc[df_c$loc == "Maricopa"] <- "Maricopa, AZ"
   df_c$loc[df_c$loc == "Maricopa, AX"] <- "Maricopa, AZ"
   # Starkville, MS
   df_c$loc[df_c$loc == "Starkville"] <- "Starkville, MS"
   df_c$loc[df_c$loc == "starkville"] <- "Starkville, MS"
  # Beeville, TX
   df_c$loc[df_c$loc == "Beeville"] <- "Beeville, TX"
   df_c$loc[df_c$loc == "Beeville, OK"] <- "Beeville, TX"
   # Portageville, MO
   df_c$loc[df_c$loc == "Portageville, MS"] <- "Portageville, MO"
   df_c$loc[df_c$loc == "Portegeville, MO"] <- "Portageville, MO"
   df_c$loc[df_c$loc == "Portageville"] <- "Portageville, MO"
   # La Mesa, NM
   df_c$loc[df_c$loc == "Lamesa, NM"] <- "La Mesa, NM"
   # Belle Mina, AL
   df_c$loc[df_c$loc== "Bell Mina, AL"] <- "Belle Mina, AL"
   df_c$loc[df_c$loc == "Belle Mina"] <- "Belle Mina, AL"
   # Rohwer, AR
   df_c$loc[df_c$loc == "Rohwer, AZ"] <- "Rohwer, AR"
   # Five Points, CA
   df_c$loc[df_c$loc== "Five Points"] <- "Five Points, CA"
   # Lemoore, CA
   df_c$loc[df_c$loc== "Lemore, CA"] <- "Lemoore, CA"
   # W Side Field Station, CA
   df_c$loc[df_c$loc == "W field station, CA"] <- "W Side Field Station, CA"
   df_c$loc[df_c$loc == "w side field station, CA"] <- "W Side Field Station, CA"
   df_c$loc[df_c$loc == "W side field station, CA"] <- "W Side Field Station, CA"
   df_c$loc[df_c$loc == "W. Side Field Station, CA"] <- "W Side Field Station, CA"
   # Griffin, GA
   df_c$loc[df_c$loc == "Griffin"] <- "Griffin, GA"
   #  Las Cruces, NM
   df_c$loc[df_c$loc == "Las Cruces"] <- "Las Cruces, NM"
   df_c$loc[df_c$loc == "Las cruces, NM"] <- "Las Cruces, NM"
   # Lamesa, TX
   df_c$loc[df_c$loc == "Lamesa"] <- "Lamesa, TX"
   # Suffolk, VA
   df_c$loc[df_c$loc == "Suffolk, CA"] <- "Suffolk, VA"
   df_c$loc[df_c$loc == "Suffolk, VA ??"] <- "Suffolk, VA"
   # Ames Plantation, TN
   df_c$loc[df_c$loc == "Ames Plantatino, TN"] <- "Ames Plantation, TN"
   # Yuma, AZ
   df_c$loc[df_c$loc == "Yuma, AZ??"] <- "Yuma, AZ??"
   # Dallas, TX
   df_c$loc[df_c$loc == "Dallas"] <- "Dallas, TX"
   # Thrall, TX
   df_c$loc[df_c$loc == "Thrall"] <- "Thrall, TX"
   # Keiser, AR
   df_c$loc[df_c$loc == "Kerser, AR"] <- "Keiser, AR"
   df_c$loc[df_c$loc == "Keiser"] <- "Keiser, AR"
   # Tipton, OK
   df_c$loc[df_c$loc == "tiptop, OK"] <- "Tipton, OK"
   df_c$loc[df_c$loc == "Tipton"] <- "Tipton, OK"
   # Commerce, TX
   df_c$loc[df_c$loc== "Commerce"] <- "Commerce, TX"
   # Corpus Christi, TX
   df_c$loc[df_c$loc == "C Christi"] <- "Corpus Christi, TX"
   df_c$loc[df_c$loc == "C Christi-D"] <- "Corpus Christi, TX"
   # Maricopa, AZ
   df_c$loc[df_c$loc == "maricopa"] <- "Maricopa, AZ"
   # Knoxville, TN
   df_c$loc[df_c$loc == "knoxville ???"] <- "Knoxville, TN"
} 
  
df %>% 
  distinct(loc) %>%
  arrange(loc) %>% 
  mutate(irrigation = case_when( 
    grepl("irri|irr", loc) ~ "irrigated",
    
    
    T ~ NA
    ))

  mutate(loc_fixed = case_when(
    grepl("Altus|ALTUS", loc) ~ "Altus, OK",
    grepl("Artesisa", loc) ~ "Artesia, NA",
    
    
    
    T ~ loc 
    
  ))
# Define the target columns and the reference column
target_columns <- c("loc", "Latitude", "Longitude")  # Specify the columns where you expect NA values
reference_column <- "loccode"  # Specify the column where you expect non-NA values

rows_with_na <- df_c[rowSums(is.na(df[, target_columns])) > 0 & !is.na(df_c[, reference_column]), ]

print(rows_with_na)




