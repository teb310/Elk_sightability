# Name_fixer converts misspelled or abbreviated EPU names to standard names
name_fixer <- function(x){
  output <- case_when(
    grepl("Rainy", x, ignore.case = TRUE) ~ "Rainy-Gray",
    grepl("Narrow", x, ignore.case = TRUE) ~ "Tzoonie-Narrows",
    grepl("Desert", x, ignore.case = TRUE) ~ "Deserted-Stakawus",
    grepl("Cheh", x, ignore.case = TRUE) ~ "Chehalis",
    grepl("Sech", x, ignore.case = TRUE) ~ "Sechelt Peninsula",
    grepl("Homa", x, ignore.case = TRUE) ~ "Homathko",
    grepl("Lois", x, ignore.case = TRUE) ~ "Lois",
    grepl("Eldr", x, ignore.case = TRUE) ~ "Eldred",
    grepl("Hasl", x, ignore.case = TRUE) ~ "Haslam",
    grepl("Dani", x, ignore.case = TRUE) ~ "Powell-Daniels",
    grepl("Quat", x, ignore.case = TRUE) ~ "Quatam",
    grepl("Lill", x, ignore.case = TRUE) ~ "Lower Lillooet",
    grepl("Van", x, ignore.case = TRUE) ~ "Vancouver",
    grepl("Squam", x, ignore.case = TRUE) ~ "Squamish",
    grepl("Indian", x, ignore.case = TRUE) ~ "Indian",
    grepl("Stave", x, ignore.case = TRUE) ~ "Stave",
    grepl("Theo", x, ignore.case = TRUE) ~ "Theodosia",
    grepl("Mcnab", x, ignore.case = TRUE) ~ "McNab",
    grepl("Bear", x, ignore.case = TRUE) ~ "Bear",
    TRUE ~ x
  )
  return(output)
}

# Standard_survey standardizes survey types
standard_survey <- function(x){
  output <- case_when(
    grepl("incidental", x, ignore.case = TRUE) ~ "Incidental",
    grepl("telemetry", x, ignore.case = TRUE) ~ "Telemetry",
    grepl("transect", x, ignore.case = TRUE) ~ "Inventory",
    grepl("inventory", x, ignore.case = TRUE) ~ "Inventory",
    grepl("capture", x, ignore.case = TRUE) ~ "Capture",
    TRUE ~ "Other")
  return(output)
}

# compile_sheets binds all sheets in a file (filepath) that follow a certain naming patter (type)
# E.g. to bind survey data sheets from all years, type should be "Data"
compile_sheets <- function(filepath,type){
  sheets <- excel_sheets(filepath)
  sheets <- subset(sheets, str_detect(sheets, paste0(type)) ==T)
  output <- data.frame(matrix(ncol = 0, nrow = 0))
  for(i in 1:length(sheets))
  {
    output <- bind_rows(output, read_excel(filepath, sheet = paste0(sheets[i])))
  }
  return(output)
}

oper.datify <- function(df){
  transmute(.data = df,
            x = voc,
            ym1 = total-1,
            h = as.double(stratum),
            q = 1,
            z = 1,
            yr = year.ID,
            subunits = as.double(stratum),
            survey.type = NULL)
}

augment <- function(df){
  aug <- df %>%
    # need to determine max annual m of each h
  group_by(yr, h) %>%
    summarize(m = n()) %>%
    ungroup() %>%
    group_by(h) %>%
    reframe(yr = yr,
            m = m,
            m.max = max(m)) %>%
    ungroup() %>%
    mutate(b = 2*m.max,
           aug = b-m)
  # create augmented dataframe
  oper.dat.aug <- aug[rep(1:nrow(aug), aug$aug),] %>%
    mutate(x = NA, ym1 = NA, h = h, q = NA, z = 0, yr = yr, subunits = h, .keep="none") %>%
    ungroup()
  # combine dataframes
  output <- rbind(df, oper.dat.aug) %>%
    arrange(yr, h, q)
  return(output)
}

plot.datify <- function(df){
  df %>%
    select(yr, h) %>%
    distinct() %>%
    mutate(h.plots = h, 
           yr.plots = yr) %>%
    select(h.plots, yr.plots) %>%
    arrange(yr.plots, h.plots)
}

scalar.datify <- function(operdat, plotdat){
  output <- as.data.frame(matrix(NA, 1, (nrow(plotdat))))
  i <- 1
  for(i in 1:nrow(plotdat)){
    output[,i] <- as.double(nrow(operdat %>% filter(yr == plotdat$yr.plots[i], h == plotdat$h.plots[i])))
    colnames(output)[i] <- paste("h", plotdat$h.plots[i], "y", plotdat$yr.plots[i], sep = "")
  }
  
  output <- output %>%
    mutate(R = as.double(nrow(sight.dat)),
           Ngroups = as.double(nrow(operdat)),
           Nsubunits.yr = as.double(nrow(plotdat)))
  return(output)
}
scalar.sumsify <- function(plotdat, scalardat){
  # Create scalar.sums to ease modelling
  # tells us how many rows belong to each year/stratum combo
  output <- matrix(NA, nrow(plotdat), 2)
  for (i in 1:nrow(plotdat)){
    t <- i-1
    output[i, 1] <- sum(scalardat[,0:t], 1)
    output[i, 2] <- sum(scalardat[,0:i])
  }
  return(output)
}

rjags_to_table <- function(jagsoutput){
  jags.summary <- as.data.frame(jagsoutput$BUGSoutput$summary)
  
  tau.jags <- matrix(NA,(nrow(jags.summary)-3),9)
  tau.jags <- as.data.frame(tau.jags)
  tau.jags[,1] <- as.numeric(str_extract(colnames(scalar.dat)[1:length(jagsoutput$BUGSoutput$median$tau.hat)], "(?<=h)[:digit:]{1,2}"))
  tau.jags[,2] <- as.numeric(str_extract(colnames(scalar.dat)[1:length(jagsoutput$BUGSoutput$median$tau.hat)], "(?<=y)[:digit:]{1,2}"))
  tau.jags[,3] <- round(jags.summary$`50%`[4:nrow(jags.summary)])
  tau.jags[,4] <- round(jags.summary$`2.5%`[4:nrow(jags.summary)])
  tau.jags[,5] <- round(jags.summary$`97.5%`[4:nrow(jags.summary)])
  tau.jags[,6] <- round(jags.summary$`25%`[4:nrow(jags.summary)])
  tau.jags[,7] <- round(jags.summary$`75%`[4:nrow(jags.summary)])
  tau.jags[,8] <- round(jags.summary$Rhat[4:nrow(jags.summary)], 3)
  tau.jags[,9] <- round(jags.summary$sd[4:nrow(jags.summary)]/jags.summary$mean[4:nrow(jags.summary)], 3)
  
  rm(jagsoutput)
  
  colnames(tau.jags) <- c("ID", "year.ID", "Model","JAGS_lcl_95", "JAGS_ucl_95", "JAGS_lcl_50", "JAGS_ucl_50", "Rhat", "cv") 
  output <- left_join(tau.jags, year.ID, by="year.ID") %>%
    left_join(EPU.list, by="ID") %>%
    select(-year.ID, -ID)
  
  return(output)
}
