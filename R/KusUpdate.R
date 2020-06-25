# Scheduled Task: Update data in Kus and re-deploy app.
# Can only use relative file paths if you establish your working directory first.
#======================================================
# Loadout ----
  library(cdmsR)
  library(cuyem)
  library(tidyverse)
  library(rsconnect)
  library(lubridate)
  library(rmarkdown)
  library(kableExtra)

# Set Working Directory
setwd("C:\\Users\\tylers\\Documents\\R\\kus")

# source scripts
  source("./R/cdms_api_keys.R")
  source("./R/summariseSGS.R")
  source("./R/summariseRST.R")
  source("./R/summariseAGE.R")
  source("./R/sum_FCHN_redds.R")

# source("C:\\Users\\tylers\\Documents\\R\\kus\\R\\cdms_api_keys.R")
# source("C:\\Users\\tylers\\Documents\\R\\kus\\R\\summariseSGS.R")
# source("C:\\Users\\tylers\\Documents\\R\\kus\\R\\summariseRST.R")
# source("C:\\Users\\tylers\\Documents\\R\\kus\\R\\summariseAGE.R")
# source("C:\\Users\\tylers\\Documents\\R\\kus\\R\\sum_FCHN_redds.R")


# CDMS Login ----
  keys <- cdmsKeys()
    cdms_host <- keys[1]
    username <- keys[2]
    api_key <- keys[3]
  cdmsLogin(username, api_key, cdms_host)

# Datasets List ----
  # will need to be modified as new datasets are added.
  datasets <- getDatastores(cdms_host = cdms_host) %>%
    mutate(TablePrefix = gsub("_", '', TablePrefix)) %>%
    select(DatastoreId = Id, DatastoreName = Name, Prefix = TablePrefix) %>%
    filter(!DatastoreId %in% c(81:84, 87:91, 93, 98:99)) %>%
    arrange(DatastoreName)
  save(datasets, file = './data/datasets.rda')
  # save(datasets, file = 'C:\\Users\\tylers\\Documents\\R\\kus\\data\\datasets.rda')
  
# Update Raw Data ----

  # Redd
  SGSRedd <- getDatasetView(78, cdms_host)

  SGSRedd <- clean_reddData(SGSRedd) %>%
      mutate(SpeciesRun = paste(Run, Species))
  save(SGSRedd, file = "./data/datasets.rda")
  # save(SGSRedd, file = "C:\\Users\\tylers\\Documents\\R\\kus\\data\\datasets.rda")
  
  # Carcass
  SGSCarcass <- getDatasetView(79, cdms_host)

  SGSCarcass <- clean_carcassData(SGSCarcass) %>%
      mutate(SpeciesRun = paste(Run, Species))
  save(SGSCarcass, file = './data/SGSCarcass.rda')
  # save(SGSCarcass, file = 'C:\\Users\\tylers\\Documents\\R\\kus\\data\\SGSCarcass.rda')
  
  # Age
  NPTAge <- getDatasetView(80, cdms_host) %>%
    mutate(SpeciesRun = paste(Run, Species))
  save(NPTAge, file = './data/NPTAge.rda')
  # save(NPTAge, file = 'C:\\Users\\tylers\\Documents\\R\\kus\\data\\NPTAge.rda')
  
  # Lamprey
  LampreyData <- getDatasetView(97, cdms_host) %>%
    mutate(SpeciesRun = Species)
  save(LampreyData, file = './data/LampreyData.rda')
  # save(LampreyData, file = 'C:\\Users\\tylers\\Documents\\R\\kus\\data\\LampreyData.rda')
  
  # Sturgeon
  NPTSturgeon <- getDatasetView(92, cdms_host) %>%
    mutate(SpeciesRun = Species)
  save(NPTSturgeon, file = './data/NPTSturgeon.rda')
  # save(NPTSturgeon, file = 'C:\\Users\\tylers\\Documents\\R\\kus\\data\\NPTSturgeon.rda')
  
  # RST Abundance
  NPTRST <- getDatasetView(datastoreID = 85, cdms_host) %>%
    mutate(SpeciesRun = paste(Run, Species)) %>%
    rename(Ab_SE = StdError, Ab_L95 = Lower95, Ab_U95 = Upper95)
  save(NPTRST, file = './data/NPTRST.rda')
  # save(NPTRST, file = 'C:\\Users\\tylers\\Documents\\R\\kus\\data\\NPTRST.rda')
  
  # RST Survival
  NPTJuvSurvival <- getDatasetView(datastoreID = 86, cdms_host) %>%
    mutate(SpeciesRun = paste(Run, Species)) %>%
    rename(Surv_SE = StdError, Surv_L95 = Lower95, Surv_U95 = Upper95)
  save(NPTJuvSurvival, file = './data/NPTJuvSurvival.rda')
  # save(NPTJuvSurvival, file = 'C:\\Users\\tylers\\Documents\\R\\kus\\data\\NPTJuvSurvival.rda')
  
  # FINS - raw and cleaned data  (300k+ records)
  AdultWeirData <- getDatasetView(99, cdms_host) %>%
    mutate(SpeciesRun = paste(Run, Species),
           SpeciesRun = gsub('NA ', '', SpeciesRun),
           year = year(`Trapped Date`)) %>%
    mutate_if(is.numeric, as.numeric) # having a DT error (this should fix...)
  save(AdultWeirData, file = './data/AdultWeirData.rda')
  # save(AdultWeirData, file = 'C:\\Users\\tylers\\Documents\\R\\kus\\data\\AdultWeirData.rda')
  
  AdultWeirData_clean <- clean_weirData(AdultWeirData) %>%
    mutate(monthday = format.Date(trapped_date, '%m/%d')) %>% # monthday for plotly
    rename(SpeciesRun = speciesrun) %>%
    mutate_if(is.numeric, as.numeric) # DT error. is this fixing it? I don't know.
  save(AdultWeirData_clean, file = './data/AdultWeirData_clean.rda')
  # save(AdultWeirData_clean, file = 'C:\\Users\\tylers\\Documents\\R\\kus\\data\\AdultWeirData_clean.rda')
  
# Updates for Summary Data tabs ----

  # SGS Summary
  SGSsummary <- summariseSGS(SGSRedd, SGSCarcass)
  save(SGSsummary, file = './data/SGSsummary.rda')
  # save(SGSsummary, file = 'C:\\Users\\tylers\\Documents\\R\\kus\\data\\SGSsummary.rda')
  
  # Weir Collections
  weir_list <<- AdultWeirData_clean %>%
    group_by(facility, trap, SpeciesRun) %>%
    filter(str_detect(facility, 'NPT'),
           SpeciesRun %in% c('Fall Chinook', 'Spring Chinook', 'Summer Chinook', 'Summer Steelhead')) %>%
    distinct(facility)

  NPTweir <- AdultWeirData_clean %>%
    filter(trap %in% unique(weir_list$trap),
           SpeciesRun %in% c('Fall Chinook', 'Spring Chinook', 'Summer Chinook', 'Summer Steelhead'),
           trap_year == year(Sys.Date()))
  save(NPTweir, file = './data/NPTweir.rda')
  # save(NPTweir, file = 'C:\\Users\\tylers\\Documents\\R\\kus\\data\\NPTweir.rda')
  

  daily_weir <- cnt_groups(NPTweir, trap, SpeciesRun, monthday, origin) %>%
    mutate(origin = paste(year(Sys.Date()), ' ', origin, sep=''))

  historic_weir <- cnt_groups(AdultWeirData_clean %>%
                                filter(trap %in% unique(weir_list$trap),
                                       trap_year != year(Sys.Date())),   # YEAR FILTER
                              origin, trap, SpeciesRun, trapped_date, monthday, origin) %>%
    group_by(trap, monthday, SpeciesRun, origin) %>%
    summarize(n = mean(n)) %>%
    mutate(origin = paste('2012-', year(Sys.Date())-1, ' ', origin, ' (AVG)', sep = ''))

  p_weir_df <- bind_rows(daily_weir, historic_weir) # for plotly
  save(p_weir_df, file = './data/p_weir_df.rda')
  # save(p_weir_df, file = 'C:\\Users\\tylers\\Documents\\R\\kus\\data\\p_weir_df.rda')
  
  # Juvenile Summary
  JUVsummary <- summariseRST(NPTRST, NPTJuvSurvival)
  save(JUVsummary, file = './data/JUVsummary.rda')
  # save(JUVsummary, file = 'C:\\Users\\tylers\\Documents\\R\\kus\\data\\JUVsummary.rda')
  
  # Age Summary
  AGEsummary <- summariseAGE(NPTAge)
  save(AGEsummary, file = './data/AGEsummary.rda')
  # save(AGEsummary, file = 'C:\\Users\\tylers\\Documents\\R\\kus\\data\\AGEsummary.rda')
  
  # Fall Chinook Summary
  FCHNsummary <- sum_FCHN_redds(SGSRedd)
  save(FCHNsummary, file = './data/FCHNsummary.rda')
  # save(FCHNsummary, file = 'C:\\Users\\tylers\\Documents\\R\\kus\\data\\FCHNsummary.rda')
  
  # CUSTOM QUERIES:
  RSTcq <- JUVsummary[[2]]
  save(RSTcq, file = './data/RSTcq.rda')
  # save(RSTcq, file = 'C:\\Users\\tylers\\Documents\\R\\kus\\data\\RSTcq.rda')
  
# Update (Knit) R Markdown PDF Reports ----
  
  # RENDER IS NOT CURRENTLY PRODUCING THE DESIRED OUTPUT. MUST MANUALLY KNIT.  These are fine until you fix.
  
  # rmarkdown::render(input = 'JUV_Status_Report.Rmd', output_dir = "C:/Users/tylers/Documents/R/kus/pdf/",
                    # output_format = "pdf_document") #, runtime = 'static')
  # 
  # rmarkdown::render(input = 'test.Rmd', output_format = "pdf_document") #, runtime = 'static')
  # 
  # rmarkdown::render(input = 'SGS_Status_Report.Rmd', output_dir = "C:/Users/tylers/Documents/R/kus/pdf/",
  #                   output_format = "pdf_document")
  
# Deploy Kus ----
  deploy_time <- Sys.time()-21600  # adjust so time displays in Mountain Time.
  # This will appear in UTC when passed to shinyapps.io (-6hr(MT) = 21600 seconds, -7(PT)=25200)
  save(deploy_time, file = "./data/deploy_time.rda")
  # save(deploy_time, file = "C:\\Users\\tylers\\Documents\\R\\kus\\data\\deploy_time.rda")
  

rsconnect::deployApp(appName = 'TestKus', appDir = "C:/Users/tylers/Documents/R/kus", launch.browser = FALSE, forceUpdate = T)


  
# Successful Deploy: Application successfully deployed to https://tstright.shinyapps.io/TestKus/
