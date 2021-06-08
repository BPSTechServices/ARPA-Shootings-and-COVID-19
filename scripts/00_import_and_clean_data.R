source("./scripts/global.R")

acs19 <- load_variables(2019, "acs5", cache = TRUE)


###### Step 1. Load resource files and initial datasets #####
##### 1.1 PPB service area #####
ppb_districts <- geojsonsf::geojson_sf("https://opendata.arcgis.com/datasets/f8508520c1c74d199ae83db0fad67d94_254.geojson")

ppb_juris <- ppb_districts %>% st_make_valid() %>% summarize() %>% st_transform(2913)

##### 1.2 Tract data (shapes and Urban Institute dataset) #####
msa_tracts <- rbind(
  tigris::tracts(state = "OR", cb = FALSE), 
  tigris::tracts(state = "WA", cb = FALSE)) %>%
  filter(substr(GEOID, 1,5) %in% c('41005', '41009', '41051', '41067', '41071', '53011', '53059')) %>%
  st_transform(2913)

## https://datacatalog.urban.org/dataset/rental-assistance-priority-index
ui_rapi <- rio::import("./data/housing_index_state_adj.csv") %>%
  mutate(GEOID = as.character(GEOID)) %>%
  filter(GEOID %in% msa_tracts$GEOID) %>%
  left_join(., select(msa_tracts, GEOID), by = "GEOID") %>%
  st_as_sf() %>% st_transform(2913) %>%
  st_filter(., st_buffer(ppb_juris, -1000)) %>% ## Negative 1000 ft buffer 
  filter(!(GEOID %in% c("41005020800", "41005022208"))) ## Exclude two tracts that are mostly outside Portland

## Test maps
# mapview(ui_rapi, zcol = "total_index_quantile")
# mapview(ui_rapi, zcol = "perc_low_income_jobs_lost") + shootings %>% mapview()

## Download population data
population_tract <- get_acs(geography = "tract",
                  state = "OR", county = "Multnomah",
                  table = "B01001", 
                  year = 2019) %>%
  mutate(varnum = as.numeric(substr(variable, 8,10)))

## Total population
tot_pop <- population_tract %>% filter(varnum == 1) %>% 
  select(GEOID, tot_pop = estimate)

## Males age 15 - 24
pop_m15t24 <- population_tract %>% filter(varnum >= 6 & varnum <= 10) %>% 
  group_by(GEOID) %>% summarize(pop_m15t24 = sum(estimate)) %>%
  select(GEOID, pop_m15t24) 

## Males age 25 - 54
pop_m25t54 <- population_tract %>% filter(varnum >= 11 & varnum <= 16) %>% 
  group_by(GEOID) %>% summarize(pop_m25t54 = sum(estimate)) %>%
  select(GEOID, pop_m25t54) 

## Combine results
tot_pop <- tot_pop %>%
  left_join(., pop_m15t24, by = "GEOID") %>%
  left_join(., pop_m25t54, by = "GEOID") %>%
  mutate(pct_m15t24 = pop_m15t24 / tot_pop,
         pct_m25t54 = pop_m25t54 / tot_pop,
         pct_m15t54 = (pop_m15t24 + pop_m25t54) / tot_pop)

##### 1.3 ZIP code shapes #####
pdx_msa <- tigris::core_based_statistical_areas(cb = TRUE) %>% filter(GEOID == "38900") %>% st_transform(2913)

## ZIP code to Zip Code Tabulation Area (ZCTA) crosswalk
zip2zcta <- rio::import("https://udsmapper.org/wp-content/uploads/2020/09/Zip_to_zcta_crosswalk_2020.xlsx")

## Grab ZCTAs for region
msa_zips <- rbind(
  tigris::zctas(state = "OR", cb = TRUE),
  tigris::zctas(state = "WA", cb = TRUE)
) %>%
  select(ZCTA = GEOID10) %>%
  st_transform(2913) %>%
  st_filter(., pdx_msa) 

# mapview(msa_zips, label = "ZCTA")

pdx_place <- tigris::places(state = "OR", cb = T) %>% st_transform(2913) %>% filter(GEOID == "4159000")

## ZIP codes within Portland
pdx_zips <- msa_zips %>%
  st_filter(., st_buffer(pdx_place, -5000)) # Negative 5000 ft buffer 

# mapview(pdx_zips, label = "ZCTA")

##### 1.4 COVID-19 case counts #####
## Data sources:
# https://public.tableau.com/app/profile/oregon.health.authority.covid.19/viz/OregonCOVID-19CasesbyZIPCode-SummaryTable/CasesbyZIPCodeSummaryTable
# https://clark.wa.gov/public-health/covid-19-data

covid_cases <- rio::import("./data/covid_cases_zip.csv") %>%
  janitor::clean_names() %>%
  mutate_at(.vars = vars(cases, rate_per_100_000), .funs = as.numeric) %>%
  left_join(., zip2zcta, by = c("zip" = "ZIP_CODE")) %>%
  select(zip, ZCTA, cases, covid_rate = rate_per_100_000)

##### 1.5 Homeless camp reportings #####
camp_reportings_weekly <- geojsonsf::geojson_sf("https://www.portlandmaps.com/arcgis/rest/services/Public/Campsite_Reporting/MapServer/1/query?where=1%3D1&text=&objectIds=&time=&geometry=&geometryType=esriGeometryEnvelope&inSR=&spatialRel=esriSpatialRelIntersects&distance=&units=esriSRUnit_Foot&relationParam=&outFields=&returnGeometry=true&returnTrueCurves=false&maxAllowableOffset=&geometryPrecision=&outSR=&havingClause=&returnIdsOnly=false&returnCountOnly=false&orderByFields=&groupByFieldsForStatistics=&outStatistics=&returnZ=false&returnM=false&gdbVersion=&historicMoment=&returnDistinctValues=false&resultOffset=&resultRecordCount=&returnExtentOnly=false&datumTransformation=&parameterValues=&rangeValues=&quantizationParameters=&featureEncoding=esriDefault&f=geojson") %>%
  st_transform(2913) 

mapview(camp_reportings_weekly)

camps_reported_by_tract <- camp_reportings_weekly %>%
  st_join(., select(msa_tracts, GEOID)) %>%
  st_drop_geometry() %>%
  group_by(GEOID) %>%
  summarize(camps_reported = n())

##### 1.6 Shooting and crime data #####
shootings_raw <- rio::import("./data/Shootings.csv") %>%
  janitor::clean_names()

set.seed(100)

shootings <- shootings_raw %>%
  filter(open_data_latitude != 0) %>% # Filter out those without geolocations
  mutate_at(.vars = vars(open_data_latitude:y), .funs = as.numeric) %>%
  mutate(occurence_date = lubridate::mdy(occurence_date)) %>%
  st_as_sf(coords = c("open_data_longitude", "open_data_latitude"), crs = 4269) %>%
  st_transform(2913)
  

shootings_by_tract <- shootings %>%
  st_join(., select(ui_rapi, GEOID)) %>%
  filter(occurence_date >= lubridate::mdy("05/20/2020")) %>%
  st_drop_geometry() %>%
  group_by(GEOID) %>%
  summarize(total_shootings = n())

shootings_by_tract_jitter <- shootings %>%
  st_jitter(., amount = 500) %>%
  st_join(., select(ui_rapi, GEOID)) %>%
  filter(occurence_date >= lubridate::mdy("05/20/2020")) %>%
  st_drop_geometry() %>%
  group_by(GEOID) %>%
  summarize(total_shootings_jitter = n())

cases.sf <- pdx_zips %>%
  left_join(., covid_cases) %>%
  arrange(ZCTA, desc(covid_rate)) %>%
  distinct(ZCTA, .keep_all = TRUE) %>% 
  ## Manually change 97208 COVID case rate to the encompassing ZIP code case rate
  mutate(covid_rate = ifelse(ZCTA == 97208, covid_rate[ZCTA == 97209], covid_rate)) %>% 
  st_transform(2913)

# cases.sf  %>% mapview(., label = "ZCTA")

##### Step 2: Combine working data #####

working_data <- ui_rapi %>%
  st_transform(2913) %>%
  filter(GEOID != "41051980000") %>% # Remove Swan Island
  areal::aw_interpolate(., tid = GEOID, 
                        source = cases.sf, 
                        sid = "ZCTA", 
                        weight = "sum", 
                        output = "sf", 
                        intensive = "covid_rate") %>%
  left_join(., shootings_by_tract, by = "GEOID") %>%
  left_join(., shootings_by_tract_jitter, by = "GEOID") %>%
  left_join(., camps_reported_by_tract, by = "GEOID") %>%
  left_join(., tot_pop, by = "GEOID") %>%
  mutate(camps_reported = ifelse(is.na(camps_reported), 0, camps_reported),
         shooting_rate = ifelse(is.na(total_shootings) | total_shootings == 0, 0, total_shootings / tot_pop * 100000),
         shooting_rate_jitter = ifelse(is.na(total_shootings_jitter) | total_shootings_jitter == 0, 0, total_shootings_jitter / tot_pop * 100000))

saveRDS(working_data, "./data/working_data.rds")

