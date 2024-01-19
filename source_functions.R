#If packages not on machine, install them.
if (!require(lubridate)) install.packages('lubridate')
if (!require(tidyverse)) install.packages('tidyverse')
if (!require(devtools)) install.packages('devtools')
if (!require(PooledInfRate)) devtools::install_github("https://github.com/CDCgov/PooledInfRate",
                                                      build_vignettes = TRUE)#
if (!require(stringr)) install.packages('stringr')
if (!require(epitools)) install.packages('epitools')
if (!require(plotly)) install.packages('plotly')
if (!require(DT)) install.packages('DT')
if (!require(httr)) install.packages('httr')
if (!require(readxl)) install.packages('readxl')
if (!require(jsonlite)) install.packages('jsonlite')
if (!require(knitr)) install.packages('knitr')
if (!require(kableExtra)) devtools::install_github("kupietz/kableExtra")


#Load relevant libraries
library(lubridate)
library(tidyverse)
library(devtools)
library(PooledInfRate)
library(stringr)
library(epitools)
library(plotly)
library(DT)
library(httr)
library(readxl)
library(jsonlite)
library(knitr)
library(kableExtra)




#This function returns the associated access token.
getToken = function(){
  

  #Prompt credentials
  username = rstudioapi::askForPassword("Gateway username")
  password = rstudioapi::askForPassword("Gateway password")
  
  
  #HTTP 
  headers = c(
    'Content-Type' = 'application/json'
  )
  
  body = paste('{
      "username":', '\"',username,'\",',
               '"password":' ,'\"',password,'\"',
               '}', sep="");
  
  
  response <- POST( url = "https:/api.vectorsurv.org/login", body = body, add_headers(headers))
  
  response_content <- content(response, 'parsed') 
 
  token <- response_content$token
  

 
 
  
  return(token)
}

#Returns collections data for specified years. Range: [Start_year, End_year]
#Calls getToken()




getArthroCollections<- function(token, start_year, end_year){
  url <- "https://api.vectorsurv.org/v1/arthropod/collection"

  

  headers <- c(
    Authorization = paste("Bearer", token),
    "Content-Type" = "application/json"
  )
  
  #Gathers data matching parameters  
  collections=data.frame()
  i = 1
  while(i>0){
    params <- list(
      `populate[]` = "arthropods",
      `populate[]` = "agency",
      `populate[]` = "trap",
      pageSize = "1000",
       page = as.character(i),
      `query[surv_year][$between][0]` = start_year,
      `query[surv_year][$between][1]` = end_year
     # `query[agency][$in][0]` = agency_id
    
     
    )
    
  
    # Append the query string to the URL
    url_with_params <- modify_url(url, query = params)
    
    tryCatch({
      response <- GET(url_with_params, add_headers(headers))
      content <- content(response, as = "text")
     
      df_content = fromJSON(content, flatten = T)
      
      #Breaks loop when df_content returns no more data 
      if(length(df_content$rows)<=0){break}
      
      collections =  rbind(collections, df_content$rows)
      
    }, error = function(e) {
      print(e)
    })
    
    i=i+1
  }
 
  #Prevents conflicting data types within $arthropods list
   collections$arthropods=lapply(collections$arthropods, as.data.frame)
  
 collections =
    collections%>%
    unnest(arthropods, keep_empty = T,names_sep ="_" )

  colnames(collections) =  str_replace(colnames(collections), "arthropods_","")%>%
    str_replace_all(pattern = "\\.",replacement = "_")
  
  colnames(collections)[1]="collection_id"
  
  return(collections)
  
}

#Returns pools data for specified years. Range: [Start_year, End_year]
#Calls getToken()
getPools<- function(token, start_year, end_year){
  url <- "https://api.vectorsurv.org/v1/arthropod/pool"

  headers <- c(
    Authorization = paste("Bearer", token),
    "Content-Type" = "application/json"
  )
  
  #
  pools = data.frame()
  i=1
  while(i>0){
    params <- list(
      `populate[]` = "agency",
      `populate[]` = "test",
      `populate[]` = "status",
      `populate[]` = "trap",
      `populate[]` = "species",
      pageSize = "1000",
      page= as.character(i),
      `query[surv_year][$between][0]` = start_year,
      `query[surv_year][$between][1]` = end_year
     # `query[agency]` = agency_id
      
      
    )
    
    # Create the query string with URL parameters
    query_string <- paste0(
      "&", paste(names(params), "=", unlist(params), collapse = "&",sep="")
    )
    
    # Append the query string to the URL
    url_with_params <- modify_url(url, query = params)
    #print(url_with_params)
    tryCatch({
      response <- GET(url_with_params, add_headers(headers))
      content <- content(response, as = "text")
      df_content = fromJSON(content, flatten = T)
      
      if(length(df_content$rows)<=0){break}
      
      pools =  rbind(pools, df_content$rows)
      
    }, error = function(e) {
      print(e)
    })
    
    i=i+1
  }
  #Prevents conflicting data types within $test list
  pools$test=lapply(pools$test, as.data.frame)
  
  pools = pools%>%
    unnest(test, keep_empty = T, names_sep = "_")
  colnames(pools) =  str_replace(colnames(pools), "test_","")%>%
    str_replace_all(pattern = "\\.",replacement = "_")
  colnames(pools)[1]="pool_id"
  colnames(pools)[18]="pool_comments"
  colnames(pools)[16]="test_comments"
  
  return(pools)
  
}


##Required: collections, interval
#Collections data should be retrieved from getArthroCollections(...)
#interval to calculate abundance on, accepts "Week", "Biweek", "Month" where
#both Week and Biweek are epiweek and disease biweek.

##Optional: species_list, trap_list
#species_list, trap_list filter the data according to abbreviated scientific name and trap acronym  
#If species_list, trap_list are left as NULL, the default assumes "All Options Selected"
#
getAbundance <- function(collections,interval, species_list = NULL, trap_list = NULL, species_seperate=FALSE){
 # collections$collection_date=as.Date(collections[['collection_date']])
  
  collections$EPIYEAR = epiyear(collections$collection_date)
  
  if(!interval%in%c("Week","Biweek","Month")){
    return("Incorrect interval input. Interval accepts inputs of 'Week','Biweek'or 'Month'")
  }
  collections$INTERVAL = switch(interval, 
                                 "Week"= as.numeric(epiweek(collections$collection_date)),
                                 "Biweek"= as.numeric(ceiling(epiweek(collections$collection_date)/2)),
                                 "Month"= as.numeric(month(collections$collection_date)))
  
  if(is.null(species_list)){
    species_list = unique(collections$species_display_name)
  }
  if(is.null(trap_list)){
    trap_list = unique(collections$trap_acronym)
  }
  
  if(species_seperate==FALSE){
  #We want to filter for females in the case of counts so here we care about species and trap type 
  collections %>% group_by(EPIYEAR, INTERVAL) %>% 
    filter(species_display_name %in% species_list,
           trap_acronym %in% trap_list,
           sex_type == "female", 
           trap_nights!=0,
           num_trap!=0,
           trap_problem_bit==FALSE)%>%
    summarise(Count = sum(num_count, na.rm=T)) -> cts
  
  #Trap Nights should not be based off of any specific sex type or species unless because we are looking at ALL possible collection opportunities present at the setting. Filter for distinct collections
  collections%>%
    filter(trap_acronym %in% trap_list,
           trap_nights!=0,
           num_trap!=0,
           trap_problem_bit==FALSE)%>%
    distinct_at(vars(collection_id), .keep_all = TRUE)%>%
    group_by(EPIYEAR,INTERVAL)%>%
    summarise(Trap_Events=sum(trap_nights*num_trap))->tns
  }
  
  
  if(species_seperate==TRUE){
    #We want to filter for females in the case of counts so here we care about species and trap type 
    collections %>% group_by(EPIYEAR, INTERVAL, species_display_name) %>% 
      filter(species_display_name %in% species_list,
             trap_acronym %in% trap_list,
             sex_type == "female", 
             trap_nights!=0,
             num_trap!=0,
             trap_problem_bit==FALSE)%>%
      summarise(Count = sum(num_count, na.rm=T)) -> cts
    
    #Trap Nights should not be based off of any specific trap type or species unless because we are looking at ALL possible collection opportunities present at the setting. Filter for distinct collections
    collections%>%
      filter(trap_acronym %in% trap_list,
             trap_nights!=0,
             num_trap!=0,
             trap_problem_bit==FALSE)%>%
      group_by(EPIYEAR,INTERVAL)%>%
      distinct_at(vars(collection_id), .keep_all = TRUE)%>%
      summarise(Trap_Events=sum(trap_nights*num_trap))->tns
  }
  
  AB <- merge(cts, tns, by = c("EPIYEAR","INTERVAL"))
  
  AB$Abundance = round(AB$Count/AB$Trap_Events, 2)
  AB = AB %>% arrange(desc(EPIYEAR), (INTERVAL))
  colnames(AB)[2] = interval
  parameters=paste("Species: ", species_list, "Trap: ", trap_list)
  #AB$Filters=parameters
  return(AB)
  
}



##Required: collections, interval,target_year
#'collections' data should be retrieved from getArthroCollections(...)

#interval to calculate abundance on, accepts "Week", "Biweek", "Month" where both Week and Biweek are epiweek and disease biweek.
#target_year: the year to calculate the abundance anomaly analysis on. The function will check if 5 past years of data are present in provided data 

##Optional: species_list, trap_list
#species_list, trap_list filter the data according to abbreviated scientific name (Cx pipiens etc) and trap acronym  
#If species_list, trap_list are left as NULL, the default assumes "All Options Selected"
#Delta change refers to the percent change between the target year and interval abundance and the five year average abundance for that interval
getAbundanceAnomaly<- function(collections, interval, target_year,
                               species_list = NULL, trap_list = NULL, species_seperate){
  
  #check that at least 5 years of data is present from target year. E.g.
  present_years = unique(collections$surv_year)
  
  target_range = seq(target_year-5,target_year, by=1)
  
  if(FALSE%in%(target_range %in% present_years)){return("Collections Data provided has insufficent years for a five year average, please ensure collections data contains data five years previous the target year.")}
  ab_data = getAbundance(collections,interval, species_list, trap_list,species_seperate)
  colnames(ab_data)[2] ="INTERVAL"
  
  if(species_seperate==T){
    ab_data %>% 
      group_by(INTERVAL, species_display_name) %>% 
      filter(EPIYEAR != target_year )%>%
      summarise(Five_Year_Avg =  mean(Abundance)) -> yr_int_average
    ab_data_yr = ab_data %>% filter(EPIYEAR == target_year)
    
    ab_av <- merge(ab_data_yr, yr_int_average, by=c("INTERVAL","species_display_name"))
  }
  else{
  ab_data %>% 
    group_by(INTERVAL) %>% 
    filter(EPIYEAR != target_year )%>%
    summarise(Five_Year_Avg =  mean(Abundance)) -> yr_int_average
    
    ab_data_yr = ab_data %>% filter(EPIYEAR == target_year)
    
    ab_av <- merge(ab_data_yr, yr_int_average, by="INTERVAL")
  }

  
  ab_av$Delta = round((ab_av$Abundance - ab_av$Five_Year_Avg)/ab_av$Five_Year_Avg,4)*100
  ab_av = ab_av %>% 
    arrange("INTERVAL")
  
  colnames(ab_av)[1] = interval

  
  return(ab_av)
}

#Requires pools data from getPools
#interval to calculate abundance on, accepts "Week", "Biweek", "Month" where both Week and Biweek are epiweek and disease biweek.
#target_year: the year to calculate the infection rate on. The function will check if 5 past years of data are present in provided data 
#target_disease: accepts disease acronyms: "WNV", "SLEV",etc unique(pools$trap_acronym) will give a list of all options
#pt_estimate: "mle","bc-mle", "mir"
##Optional: species_list, trap_list
#species_list, trap_list filter the data according to abbreviated scientific name (Cx pipiens etc) and trap acronym (NJLT, CO2, GRVD...) 
#If species_list, trap_list are left as NULL, the default assumes "All Options Selected"

getInfectionRate = function(pools,interval, target_year,target_disease,pt_estimate, species_list = NULL, trap_list = NULL){
  
  pools$collection_date=as.Date(pools$collection_date)

  pools$EPIYEAR = epiyear(pools$collection_date) 
  pools$INTERVAL = switch(interval, 
                                "Week"= as.numeric(epiweek(pools$collection_date)),
                                "Biweek"= as.numeric(ceiling(epiweek(pools$collection_date)/2)),
                                "Month"= as.numeric(month(pools$collection_date)))
  
  if(is.null(species_list)){
    species_list = unique(pools$species_display_name)
  }
  if(is.null(trap_list)){
    trap_list = unique(pools$trap_acronym)
  }
  pools$status_name = ifelse(pools$status_name == "Confirmed", 1, 0)
  
  
  
  #Function which filters data accordingly, the the example data there is information for ever disease 
  #Inter iszzz
  check = function(target_year, inter, target_disease,pt_estimate,species_list,trap_list){
    data = pools %>% filter(EPIYEAR == target_year & 
                              INTERVAL == inter &
                              species_display_name %in% species_list &
                              trap_acronym%in% trap_list &
                              target_acronym == target_disease)
    #Calculates the infection rate point estimate. See `?pIR` for more information on the function and ways to get confidence intervals
    
    if(dim(data)[1]>0){
      ir = (pIR(data$status_name, data$num_count, pt.method = pt_estimate)[,1])* 1000
      ir_l= pIR(data$status_name, data$num_count, pt.method = pt_estimate)[,3]* 1000
      ir_u= pIR(data$status_name, data$num_count, pt.method = pt_estimate)[,2]* 1000
      

      # mle = (pIR(data$status_name, data$num_count, pt.method = "mle")[,1])* 1000
      #cmle = (pIR(data$status_name, data$num_count, pt.method = "bc-mle")[,1])* 1000
      return(c(target_year, inter, target_disease ,ir,ir_u,ir_l))
    }
    
  }
  
  #returns the three point estimate Infection Rate values for west nile in week 39 of 2022 for agency MIDD 
  
  
  #Builds a data frame of the point estimates for IR for all elements in data.
  IR = c()
  #for (year in sort(unique(pools$EPIYEAR)))
  for (inter in sort(unique(pools$INTERVAL)))
    #for(target in sort(unique(pools$target_acronym)))
    IR = rbind(IR,(check(target_year, inter, target_disease,pt_estimate,species_list,trap_list)))
  
  
  colnames(IR) = c("Year", interval, "Disease", "Point_Estimate","Lower_CI", "Upper_CI")
  IR = as.data.frame(IR)
  IR[c(2,4:6)] = sapply(IR[c(2,4:6)], as.numeric)
  
  return(IR)
}


#Produces a frequency table for positive and negative pools counts by year
getPoolsComparisionTable = function(pools, interval, target_disease, species_seperate=F){
  
  pools$EPIYEAR = epiyear(pools$collection_date)
  
  if(!interval%in%c("Week","Biweek","Month")){
    return("Incorrect interval input. Interval accepts inputs of 'Week','Biweek'or 'Month'")
  }
  pools$INTERVAL = switch(interval,
                          "Week"= as.numeric(epiweek(pools$collection_date)),
                          "Biweek"= as.numeric(ceiling(epiweek(pools$collection_date)/2)),
                          "Month"= as.numeric(month(pools$collection_date)))
  pools_status = pools %>%
    filter(target_acronym==target_disease)%>%
    group_by(EPIYEAR, INTERVAL) %>%
    count(status_name)%>%
    pivot_wider(id_cols = c(EPIYEAR,INTERVAL),
                names_from = "status_name",
                values_from = "n",values_fill = 0)%>%
    mutate(Total = sum(Confirmed,Negative, na.rm=T))%>%
    mutate(`Percent Positive` = round((Confirmed/Total)*100,2))
  
  if(species_seperate == T){
    pools_status = pools %>% filter(target_acronym==target_disease)%>%
      group_by(EPIYEAR, species_display_name, INTERVAL) %>%
      count(status_name)%>%
      pivot_wider(id_cols = c(EPIYEAR,INTERVAL, species_display_name),
                  names_from = "status_name",
                  values_from = "n",values_fill = 0)%>%
      mutate(Total = sum(Confirmed,Negative, na.rm=T))%>%
      mutate(`Percent Positive` = round((Confirmed/Total)*100, 2))
    
  }
  
  colnames(pools_status)[1:2] = c("Year",interval)
  
  return(pools_status)
}




#Produces a table of the types of traps set for each year present in the current data
getTrapTypeTally = function(collections){
  
  tt_tb= table(collections$trap_acronym,collections$surv_year)

  
  return(tt_tb)
  
}


##Takes five year Abundance Anomaly output and returns a processed form which can be handled by ggplot

ProcessAbunAnom = function(AbAnomOutput){
  
  colnames(AbAnomOutput)[grep("Abundance",colnames(AbAnomOutput), value=F)]=paste(AbAnomOutput$EPIYEAR,"Abundance", sep="_")
  
  ab_name = grep("Abundance",colnames(AbAnomOutput), value=T)
  
  AbAnomOutput_L = AbAnomOutput %>% 
    pivot_longer(cols=c(ab_name,
                        "Five_Year_Avg",
                        "Delta"), 
                 values_to = "Abundance_Calculation",
                 names_to = "Abundance_Type")
  
  
  
  
  return(AbAnomOutput_L)
}


plotInfectionRate = function(InfRtOutput){
  interval_name = colnames(InfRtOutput)[2]
  colnames(InfRtOutput)[2]="INTERVAL"
  InfRtOutput %>%
    ggplot(aes(x = INTERVAL, y = Point_Estimate))+
    geom_point( color="navyblue")+
    geom_path( fill="navyblue")+
    geom_line(aes(INTERVAL, Lower_CI), color = "steelblue", size = 0.1) + 
    geom_line(aes(INTERVAL, Upper_CI), color = "steelblue", size = 0.1) + 
    geom_ribbon(aes(ymin=Lower_CI, ymax=Upper_CI), alpha=0.2, fill = "steelblue2", color=NA) +
    labs(x=interval_name, y="Point Estimate (MLE and 95% CI)")+
    ggtitle("WNV Infection Rate")+
    coord_cartesian(xlim =c(39,42), ylim =c(0,15)) +
    scale_x_continuous(breaks = seq(39,42, by=1))->IR_plot
  
  return(IR_plot)
  
}

getVectorIndex  = function(collections, pools, interval,
                           target_year,
                           target_disease,pt_estimate,
                           species_list=NULL, 
                           
                           trap_list =  NULL){
  
  if(!identical(sort(unique(pools$surv_year)), sort(unique(collections$surv_year)))){
    return("Years in Pools and Collections data do not match. Years much match for function to operate.")
  }
  
  IR = getInfectionRate(pools, interval, target_year, target_disease, pt_estimate, species_list, trap_list) 
  AB = getAbundance(collections,interval,
                    species_list, 
                    trap_list, 
                    species_seperate=FALSE)%>%
    filter(EPIYEAR==target_year)
  
  VI = merge(AB,IR, by = interval)
  VI$VectorIndex = VI$Abundance*VI$Point_Estimate
  
  return(VI)
  
}



