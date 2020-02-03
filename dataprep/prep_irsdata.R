# Building a Shiny App of the Nonprofit and Foundation Landscape
# Michele Claibourn
# CommPAS Lab Project
# Created: November 20, 2019
# Prepare Data

#........................................
# 1. Setup, libraries, read in data ----
library(tidyverse)
library(sf)
library(tigris)
library(googledrive)
library(googlesheets4)


# Acquire data
# # irs <- read_csv("bmf.bm1812-1.csv") # 2018 data
# url <- "https://nccs-data.urban.org/data/bmf/2019/bmf.bm1908.csv"
# irs <- read_csv(url)

# after reading in initially (e.g., for later updates)
load("dev/initial.Rdata")


# read corrections from google sheets (based on manual data quality checks)
# location errors
npmap_gs <- drive_get("Outliers on Map")
npmap <- read_sheet(npmap_gs, sheet = "Sheet1")

npmap_fips <- npmap %>% # correct fips code
  filter(!is.na(newfips)) %>% 
  select(EIN, newfips) %>% 
  mutate(EIN = as.character(EIN))

npmap_addr <- npmap %>% # correct address
  filter(!is.na(newaddress)) %>% 
  select(EIN, newaddress) %>% 
  mutate(EIN = as.character(EIN)) %>% 
  separate(newaddress, into = c("newaddr", "newcity", "newzip"), sep = ",") %>% 
  mutate(newzip = str_extract(newzip, "[0-9]+"))

# ngo type errors
nporg_gs <- drive_get("Outliers by Organization Type")
nporg <- read_sheet(nporg_gs, sheet = "correct")

nporg <- nporg %>%  # correct FNDNCD 
  rename(newcode = "New FNDNCD Code") %>% 
  select(EIN, newcode) %>% 
  mutate(EIN = as.character(EIN))


# read in local fips
region <- read_csv("dev/county_codes.csv")
region <- region %>% mutate(fips = paste0("51", code)) 


#........................................
# 2. Data cleaning ----

# limit to VA
va <- irs %>% filter(STATE == "VA")

# correct orgs with wrong fips codes
va <- va %>% 
  left_join(npmap_fips, by = "EIN") %>% 
  mutate(FIPS = if_else(!is.na(newfips), newfips, FIPS))

# correct orgs with wrong address
va <- va %>% 
  left_join(npmap_addr, by = "EIN") %>% 
  mutate(ADDRESS = if_else(!is.na(newaddr), newaddr, ADDRESS),
         CITY = if_else(!is.na(newcity), newcity, CITY),
         ZIP5 = if_else(!is.na(newzip), newzip, ZIP5))

# correct orgs incorrectly categorized as FNDNCD 15 or 4 or incorrectly not categorized as FNDNCD 15 or 4
va <- va %>% 
  left_join(nporg, by = "EIN") %>% 
  mutate(FNDNCD = if_else(!is.na(newcode), newcode, FNDNCD))
  
# reduce to local nonprofits
# Albemarle, Augusta, Buckingham, Fluvanna, Greene, Louisa, Nelson, Orange, Rockingham
local <- va %>% filter(FIPS %in% region$fips) 

# next time, check for CHOATE (ein = "010704666") and WINTERWOODS (ein = "452259789")
local_ngo <- local_ngo %>% filter(ein != "010704666")
local_ngo_geo <- local_ngo_geo %>% filter(ein != "010704666")
local_fnd <- local_fnd %>% filter(ein != "452259789")
local_fnd_geo <- local_fnd_geo %>% filter(ein != "452259789")


#........................................
# 3. Recode, add value labels, etc. ----

local <- local %>% 
  select(NAME, SEC_NAME, EIN, ASSETS, INCOME, TAXPER, ADDRESS, CITY, STATE, ZIP5, FIPS,
         CASSETS, CTOTREV, CTAXPER, FILER, ZFILER, FNDNCD, LEVEL1, LEVEL2, NTMAJ5, 
         NTMAJ10, NTMAJ12, LEVEL3, LEVEL4)  
names(local) <- str_to_lower(names(local))

# Make factors
var <- c("fndncd", "level1", "level2", "level3", "level4", "ntmaj5", "ntmaj10", "ntmaj12")
local <- local %>% 
  mutate_at(var, factor)

# Recode into labels
local <- local %>% 
  mutate(fndncd = fct_recode(fndncd,
                             "Nonprofit" = "15", "Foundation" = "4", "Church" = "10",
                             "Operating Foundation" = "3", "School" = "11", "Hospital, Research"= "12",
                             "College Benefit" = "13", "Mutual Benefit" = "16", "Support Org" = "17",
                             "Support Org Type I" = "21", "Support Org Type II" = "22", "Not 501(c)(3)" = "0"),
         level1 = fct_recode(level1,
                             "Public Charity" = "PC", "Private Foundation" = "PF", "Other" = "O"),
         level2 = fct_recode(level2, 
                             "Operating" = "O", "Supporting" = "S", "Mutual Benefit" = "M"),
         level3 = fct_recode(level3,
                             "Arts, Culture" = "AR", "Education" = "ED", "Environment, Animals" = "EN",
                             "Health" = "HE", "Human Services" = "HS", "International Affairs" = "IN",
                             "Mutual Benefit" = "MO", "Public Benefit" = "PB", "Religion-Related" = "RE", 
                             "Unknown" = "UN", "Single Org" = "ZA", "Fundraising Major Group" = "ZB", 
                             "Private Grantmaking" = "ZC", "Public Foundation" = "ZD", "General Fundraising" = "ZE", 
                             "Other Public Benefit" = "ZF"),
         level4 = fct_recode(level4, 
                             "Arts, Culture" = "A", "Education" = "B", "Environment" = "C", 
                             "Animal-Related" = "D", "Health" = "E", "Mental Health" = "F", 
                             "Diseases/Medical Discipline" = "G", "Medical Research" = "H", 
                             "Crime/Legal" = "I", "Employment" = "J", "Food, Nutrition" = "K",
                             "Housing" = "L", "Public Safety" = "M", "Recreation, Sports" ="N", 
                             "Youth Development" = "O", "Other Human Services" = "P", 
                             "International Affairs" = "Q", "Civil Rights/Social Action" = "R",
                             "Community Improvement" = "S", "Philanthropy, Voluntarism" = "T", 
                             "Research Institutes-Science/Tech" = "U", "Research Institutes-Social Science" = "V", 
                             "Public Benefit, Multipurpose" = "W", "Religion-Related" = "X", 
                             "Mutual, Member Benefit" = "Y", "Unknown" = "Z"),
         ntmaj5 = fct_recode(ntmaj5, 
                             "Arts, Culture" = "AR", "Education" = "ED", "Health" = "HE", 
                             "Human Services" = "HU", "Other" = "OT"),
         ntmaj10 = fct_recode(ntmaj10, 
                              "Arts, Culture" = "AR", "Education" = "ED", "Environment" = "EN", 
                              "Health" = "HE", "Human Services" = "HU", "International" = "IN", 
                              "Mutual Benefit" = "MU", "Public Benefit" = "PU", "Religion-Related" = "RE", 
                              "Unknown" = "UN"),
         ntmaj12 = fct_recode(ntmaj12, 
                             "Arts, Culture" = "AR", "Higher Education" = "BH", "Education" = "ED", 
                             "Hospitals" = "EH", "Environment" ="EN", "Health" = "HE", 
                             "Human Services" = "HU", "International" = "IN", "Mutual Benefit" = "MU",
                             "Public Benefit" = "PU", "Religion-Related" = "RE", "Unknown" = "UN"),
         locality = as.factor(fips),
         locality = fct_recode(locality,
                               Albemarle = "51003", Augusta = "51015",
                               Buckingham = "51029", Fluvanna = "51065",
                               Greene = "51079", Louisa = "51109",
                               Madison = "51113", Nelson = "51125",
                               Orange = "51137", Charlottesville = "51540",
                               Staunton = "51790", Waynesboro = "51820"))

local <- local %>% 
  rename(code = "fndncd", major_sector = "ntmaj5", major_group = "ntmaj10", major_group2 = "ntmaj12",
         type = "level1", type2 = "level2", major_category = "level3", major_category2 = "level4") 


#........................................
# 4. Generate key data frames ----

local_ngo <- local %>% 
  filter(code == "Nonprofit")

local_fnd <- local %>% 
  filter(code == "Foundation")


#........................................
# 5. Add lat/lon to orgs ----

# Try google api for missing addresses
library(ggmap)
register_google(key = "") # add own key here

# nonprofits
local_ngo_geo <- local_ngo %>% # create full address
  mutate(address_full = paste(address, city, state, zip5, sep = ", "))

local_ngo_geo <- local_ngo_geo %>% mutate_geocode(address_full) # call google api
# https://gis.stackexchange.com/questions/101507/in-what-coordinate-system-and-unit-measures-the-coordinates-returned-by-the-goog
# says uses WGS84, Web Mercator (Auxiliary Sphere) - EPSG 3857

local_ngo_geo <- local_ngo_geo %>% 
  mutate(pobox = if_else(str_detect(address, "PO BOX"), 1, 0))
sum(!is.na(local_ngo_geo$lat)) # 1 missing, 840 present
local_ngo_geo %>% filter(is.na(lat)) %>% select(address_full) # fix this?

# foundations
local_fnd_geo <- local_fnd %>% # create full address
  mutate(address_full = paste(address, city, state, zip5, sep = ", "),
         pobox = if_else(str_detect(address, "PO BOX"), 1, 0))

local_fnd_geo <- local_fnd_geo %>% mutate_geocode(address_full) # call google api
sum(is.na(local_fnd_geo$lat)) # 0 missing

# mis-coded, fix for now
tmp <- geocode("2043 BROWNSVILLE ROAD, CHARLOTTESVILLE, VA, 22901")
local_ngo_geo <- local_ngo_geo %>%
  mutate(lat = if_else(ein == 271100791, tmp$lat, lat),
         lon = if_else(ein == 271100791, tmp$lon, lon))


#........................................
# 6. Convert to sf object ----

local_ngo_geo <- local_ngo_geo %>% 
  filter(!is.na(lat)) # sf object does not accept missing lat/lon

local_ngo_geo <- as.data.frame(local_ngo_geo) %>% 
  st_as_sf(coords=c("lon", "lat"), crs=3857, remove = FALSE)
st_crs(local_ngo_geo) # check
local_ngo_geo <- st_transform(local_ngo_geo, 4326)

local_fnd_geo <- local_fnd_geo %>% 
  filter(!is.na(lat)) # sf object does not accept missing lat/lon

local_fnd_geo <- as.data.frame(local_fnd_geo) %>% 
  st_as_sf(coords=c("lon", "lat"), crs=3857, remove = FALSE)
st_crs(local_fnd_geo) # check
local_fnd_geo <- st_transform(local_fnd_geo, 4326)


#........................................
# 7. Add spatial features ----
# download census tract polygons (from tigris)
tracts <- tracts(state = 'VA', county = region$code) 
tracts_sf <- st_as_sf(tracts) # convert to sf object
st_crs(tracts_sf) # check
tracts_sf <- st_transform(tracts_sf, 4326)

# get locality polygons
counties <- counties(state = 'VA') # from tigris
counties <- counties %>% subset(COUNTYFP %in% region$code)
counties_sf <- st_as_sf(counties)
st_crs(counties_sf)
counties_sf <- st_transform(counties_sf, 4326)

# overlay visually (another check)
plot(counties_sf$geometry)
plot(tracts_sf$geometry)
plot(local_ngo_geo$geometry)


#........................................
# 8. Clean up and save ----
save(irs, vgin, file = "dev/initial.Rdata") # to call for revisions
# load("initial.Rdata")

# clean up for app
rm(irs, va, vgin, region, localvgin, counties, tracts, var, 
   npmap, npmap_addr, npmap_fips, npmap_gs, nporg, nporg_gs, tmp)

save.image("dev/www/nonprofit_data.Rdata")
# load("dev/www/nonprofit_data.Rdata")
