# Elk Sightability Analysis

runModel <- function(file_path) {

# 1 Load and clean ####

## 1.1 LOAD DATA ####

file <- paste0(file_path())

setwd(input_wd)

# Extract observations from all years
# If you didn't name your survey data sheets with "Data", replace below
obs.all <- compile_sheets(file, "Data")

# fix EPU names & survey types
obs.all$EPU <- name_fixer(obs.all$EPU)
obs.all$survey.type <- standard_survey(obs.all$survey.type) 

# Bring in summary data from each year
# This acts as a list of all the EPUs that were properly surveyed that year
eff <- compile_sheets(file, "Summary") %>%
  filter(!is.na(min_count))

eff$EPU <- name_fixer(eff$EPU)

# Save EPU names from reliable source
setwd(wd)
EPU.areas <- read_csv("www/EPU_areas.csv")
EPU.list <- as.character(EPU.areas$EPU)

## 1.4 SIGHTABILITY DATA ####

# we will have one sightability dataframe for all sexes/ages, which means we're assuming
# equal sightability of bulls, cows, and calves -> may not be true.

sight <- obs.all %>%
  # we'll create a field to tell us whether each year counts toward sightability data
  group_by(year) %>%
  mutate(sight = if_else(all(c("Inventory", "Telemetry") %in% unique(survey.type)), TRUE, FALSE)) %>%
  # we only want inventory and telemetry obs to keep things simple
  filter(survey.type == "Telemetry" | survey.type == "Inventory",
         # only keep years where sightability trials were done
         sight == TRUE,
         # we only want observations with collars
         collars>0
  )

# duplicate observations with >1 collars
sight.dup <- as.data.frame(matrix(NA, 0, ncol(sight)))
colnames(sight.dup) <- colnames(sight)

for(i in 1:nrow(sight))
{
  if(sight$collars[i]>1){
    sight.dup <- rbind(sight.dup, rep(sight[i,], sight$collars[i]-1))
    sight.dup$date <- as.Date(sight.dup$date)
  }
}
sight <- bind_rows(sight, sight.dup)

## 1.6 OBSERVATIONAL DATASET ####

# make sure we're only keeping data from EPUs with summary data that year
obs <- inner_join(obs.all, eff %>% select(year, EPU), by=c("EPU","year")) %>%
  mutate(
    subunit = EPU,
    # make sure there are no NAs in observation counts
    mutate(across(c(cow, calf, spike, bull, UC, total), ~if_else(is.na(.), 0, .))),
    voc = voc,
    .keep="unused") %>%
  # only keep inventory & capture surveys
  filter(survey.type=="Inventory" | survey.type=="Capture" | survey.type=="Telemetry")

# make sure all totals = sum of cows, calves, etc

obs <- obs %>%
  mutate(UC = if_else(
    total > (cow+calf+spike+bull+UC), as.numeric(total-(cow+calf+spike+bull)),
    as.numeric(UC)),
    total = as.numeric(cow+calf+spike+bull+UC))

## 1.6 EFFORT ####

# Amend EPU.list to only include surveyed EPUs, then assign ID numbers
EPU.list <- data.frame(EPU = unique(obs$subunit)) %>%
  mutate(ID = seq(1,length(EPU),1))

eff <- inner_join(eff, EPU.list, by="EPU")

# Add ID to obs

obs <- inner_join(obs, eff %>% select(year, EPU, ID), by=c(c("subunit"="EPU"),"year")) %>%
  mutate(
    stratum = ID,
    .keep="unused")

#including telemetry obs helps some EPUs but hurts others -> depends on effect on average group size

group <- obs %>%
  group_by(subunit) %>%
  summarize(avg_group = mean(total))

# if keeping telems brings avg groupsize closer to EPU's total average, then keep them
telem.stats <- obs %>%
  ungroup() %>%
  group_by(year, subunit, survey.type) %>%
  summarize(n = n(),
            count = sum(total)) %>%
  ungroup() %>%
  pivot_wider(names_from = survey.type, 
              values_from = c(n, count)) %>%
  mutate(n_telem = replace_na(n_Telemetry, 0),
         n_nontelem = replace_na(n_Inventory, 0)+replace_na(n_Capture,0),
         count_telem = replace_na(count_Telemetry, 0),
         count_nontelem = replace_na(count_Inventory, 0) + replace_na(count_Capture, 0),
         .keep="unused") %>%
  filter(count_telem>0) %>%
  mutate(avg_group_nontelem = (count_nontelem/n_nontelem),
         avg_group_telem = (count_telem+count_nontelem)/(n_telem+n_nontelem)) %>%
  left_join(group, by="subunit") %>%
  mutate(keep_telem= if_else(abs(avg_group_telem-avg_group)>=abs(avg_group_nontelem-avg_group), F, T))

# add telem direction to obs
obs <- left_join(obs, telem.stats %>% select(year, subunit, keep_telem), by=c("year", "subunit")) %>%
  filter(!(survey.type=="Telemetry"& keep_telem==F))

# 2 Prepare data ####

## 2.1 SIGHT DAT ####

# s = habitat indicator ()
# x = visual obstrcution measurements associated with the test trial data used to develop the sightability model
# a = activity indicator (0 if bedded, 1 if standing/moving)
# z = detection indicator (1 if the group was observed, 0 otherwise)
# t = group size

sight.dat <- sight %>%
  mutate(
    observed = as.integer(if_else(survey.type=="Inventory", 1, 0)),
    grpsize = as.integer(total)
  )  %>%
  # standardize habitat
  mutate(
    # 1 - rock / other (gravel, landfill, road, slide, other)
    # 2 - meadow / riparian (field, meadow, riparian, wetland, river)
    # 3 - cutblock / powerline (block, powerline, NSR, FTG)
    # 4 - mature forest (mature, old)
    habitat = case_when(
      grepl("mature|old|conifer", habitat, ignore.case = TRUE) ~ 4,
      grepl("block|powerline|nsr|ftg", habitat, ignore.case = TRUE) ~ 3,
      grepl("field|meadow|riparian|wetland|river", habitat, ignore.case = TRUE) ~ 2,
      grepl("gravel|landfill|road|wtp|other|slide", habitat, ignore.case = TRUE) ~ 1
    ),
    # standardize activity
    activity = case_when(
      grepl("standing|moving|run", activity, ignore.case = TRUE) ~ 1,
      grepl("bed", activity, ignore.case = TRUE) ~ 0),
    a = as.double(activity),
    s = as.double(habitat),
    t = as.double(grpsize),
    x.tilde = as.double(voc),
    z.tilde = as.double(observed)) %>%
  select(a, s, t, x.tilde, z.tilde)

glimpse(sight.dat) # check - looks the same as Fieberg's sight_dat csv

### 2.1.1 test correlations ####
# sight.dat %>% group_by(z.tilde) %>% summarize(mean = mean(x.tilde))
# 
# x.z <- cor.test(sight.dat$z.tilde, sight.dat$x.tilde, method="pearson")
# a.z <- cor.test(sight.dat$z.tilde[!is.na(sight.dat$a)], sight.dat$a[!is.na(sight.dat$a)], method="pearson")
# s.z <- cor.test(sight.dat$z.tilde[!is.na(sight.dat$s)], sight.dat$s[!is.na(sight.dat$s)], method="pearson")
# t.z <- cor.test(sight.dat$z.tilde[!is.na(sight.dat$t)], sight.dat$t[!is.na(sight.dat$t)], method="pearson")
# 
# Correlation <- as.data.frame(matrix(NA, 4, 3))
# Correlation[1,] <- c("VOC", x.z$estimate, x.z$p.value)
# Correlation[2,] <- c("Activity", a.z$estimate, a.z$p.value)
# Correlation[3,] <- c("Habitat", s.z$estimate, s.z$p.value)
# Correlation[4,] <- c("Group size", t.z$estimate, t.z$p.value)
# colnames(Correlation) <- c("Variable", "Correlation", "p")
# 
# write.csv(Correlation, "C:/Users/TBRUSH/R/SightabilityModels/output/Correlation.csv", row.names = FALSE)

### 2.1.2 finish sight.dat ####
# voc is the only factor significantly correlated with sightability -> select only voc
sight.dat <- sight.dat %>% select(x.tilde, z.tilde)

## 2.2 OPER DAT ####

### 2.2.1 Non-augmented data ####

# Get year ID
year.ID <- as.data.frame(matrix(NA, length(unique(obs$year)), 2))
colnames(year.ID) <- c("year", "year.ID")

year.ID[,1] <- unique(obs$year) %>% sort()
year.ID[,2] <- seq(1,length(unique(obs$year)))

# join to oper.dat
oper.dat <- left_join(obs, year.ID, by="year")

# get non-augmented data organized
oper.dat <- oper.dat %>%
  # model won't accept voc = 0 or 1, fix below
  mutate(voc = if_else(voc==1, 0.99,
                       if_else(voc==0, 0.01, voc)))

# try adding avg voc where its missing
voc.stats <- oper.dat %>%
  group_by(subunit) %>%
  summarize(avg = as.double(mean(voc, na.rm=T)),
            med = median(voc, na.rm=T))

oper.dat <- left_join(oper.dat, voc.stats, by="subunit") %>%
  mutate(voc = if_else(is.na(voc), avg, voc)) %>%
  mutate(voc = if_else(voc=="NaN", NA, voc))

# create an oper.dat summary dataframe to make sure we don't lose any stratums in the following filtering
blank_rows <- oper.dat %>%
  group_by(year.ID, stratum) %>%
  summarize(x = NA,
            ym1 = 0,
            q = 1,
            z = 0) %>%
  rename(yr = year.ID, h = stratum) %>%
  mutate(subunits = h) %>%
  select(x, ym1, h, q, z, yr, subunits)

# create cow, calf, and bull dataframes
oper.dat.cow <- oper.dat %>%
  filter(cow>0) %>%
  mutate(total = cow) %>%
  oper.datify() %>%
  rbind(blank_rows)

oper.dat.calf <- oper.dat %>%
  filter(calf>0) %>%
  mutate(total = calf) %>%
  oper.datify() %>%
  rbind(blank_rows)

oper.dat.bull <- oper.dat %>%
  filter(bull>0) %>%
  mutate(total = bull) %>%
  oper.datify() %>%
  rbind(blank_rows)

oper.dat <- oper.dat %>%
  oper.datify()

### 2.2.2 Augmented data ####

oper.dat <- oper.dat %>%
  augment()

oper.dat.cow <- oper.dat.cow %>%
  augment()

oper.dat.calf <- oper.dat.calf %>%
  augment()

oper.dat.bull <- oper.dat.bull %>%
  augment()

## 2.3 PLOT DAT ####

plot.dat <- oper.dat %>%
  plot.datify()

plot.dat.cow <- oper.dat.cow %>%
  plot.datify()

plot.dat.calf <- oper.dat.calf %>%
  plot.datify()

plot.dat.bull <- oper.dat.bull %>%
  plot.datify()

## 2.4 SCALAR DAT ####
scalar.dat <- scalar.datify(oper.dat, plot.dat)

scalar.dat.cow <- scalar.datify(oper.dat.cow, plot.dat.cow)

scalar.dat.calf <- scalar.datify(oper.dat.calf, plot.dat.calf)

scalar.dat.bull <- scalar.datify(oper.dat.bull, plot.dat.bull)

# Create scalar.sums to ease modelling
# tells us how many rows belong to each year/stratum combo
scalar.sums <- scalar.sumsify(plot.dat, scalar.dat)

scalar.sums.cow <- scalar.sumsify(plot.dat.cow, scalar.dat.cow)

scalar.sums.calf <- scalar.sumsify(plot.dat.calf, scalar.dat.calf)

scalar.sums.bull <- scalar.sumsify(plot.dat.bull, scalar.dat.bull)

## 2.5 SAVE INPUTS ####

setwd(input_wd)

# JAGS inputs
jags_input_names <- c("sight.dat", "oper.dat", "plot.dat", "scalar.dat", "eff", "scalar.sums")
jags_input <- ls(pattern = paste0("^", paste(jags_input_names, collapse = "|")))
save(list = jags_input, file = paste0("jags_input_", format(Sys.time(), "%Y%b%d_%H%M"), ".rdata"))
# other inputs
other_inputs <- c("compile_sheets", "name_fixer", "rjags_to_table", "input_wd","output_wd","file","EPU.list","year.ID", "eff")
save(list= other_inputs, file=paste0("other_inputs_", format(Sys.time(), "%Y%b%d_%H%M"), ".rdata"))

files_to_keep <- c(jags_input, other_inputs, "other_inputs")

rm(list = setdiff(ls(), files_to_keep))

# 3 Bayesian Analysis ####

setwd(wd)

## 3.1 SET PARAMETERS ####

# specify initial values
inits <-  function() list(bo=runif(1), bvoc=runif(1))

# Parameters monitored
params <- c("bo", "bvoc", "tau.hat")

# MCMC settings
ni <- 40000 # build to 40000
nt <- 2     # 50% thinning rate (discard every 2nd iteration)
nb <- 20000
nc <- 3

## 3.2 RUN THE MODEL ####
# All data
bundle.dat <- list(x.tilde=sight.dat$x.tilde, z.tilde=sight.dat$z.tilde, #sight.dat
                   x=oper.dat$x, ym1=oper.dat$ym1, h=oper.dat$h, q=oper.dat$q, z=oper.dat$z, yr=oper.dat$yr, subunits=oper.dat$subunits, # oper.dat
                   h.plots=plot.dat$h.plots, yr.plots=plot.dat$yr.plots, # plot_dat
                   R=scalar.dat$R, Ngroups=scalar.dat$Ngroups, Nsubunits.yr=scalar.dat$Nsubunits.yr, scalars=scalar.sums, #scalar.dat
                   years=length(unique(plot.dat$yr.plots)), stratums=length(unique(plot.dat$h.plots)))

jags_output <- jags(bundle.dat, inits, params, "www/beta_binom_model_elk2022.txt", nc, ni, nb, nt)

# Cows
bundle.dat.cow <- list(x.tilde=sight.dat$x.tilde, z.tilde=sight.dat$z.tilde, #sight.dat
                       x=oper.dat.cow$x, ym1=oper.dat.cow$ym1, h=oper.dat.cow$h, q=oper.dat.cow$q, z=oper.dat.cow$z, yr=oper.dat.cow$yr, subunits=oper.dat.cow$subunits, # oper.dat.cow
                       h.plots=plot.dat.cow$h.plots, yr.plots=plot.dat.cow$yr.plots, # plot.dat.cow
                       R=scalar.dat.cow$R, Ngroups=scalar.dat.cow$Ngroups, Nsubunits.yr=scalar.dat.cow$Nsubunits.yr, scalars=scalar.sums.cow, #scalar.dat.cow
                       years=length(unique(plot.dat.cow$yr.plots)), stratums=length(unique(plot.dat.cow$h.plots)))

jags_output_cow <- jags(bundle.dat.cow, inits, params, "www/beta_binom_model_elk2022.txt", nc, ni, nb, nt)

# Calves
bundle.dat.calf <- list(x.tilde=sight.dat$x.tilde, z.tilde=sight.dat$z.tilde, #sight.dat
                        x=oper.dat.calf$x, ym1=oper.dat.calf$ym1, h=oper.dat.calf$h, q=oper.dat.calf$q, z=oper.dat.calf$z, yr=oper.dat.calf$yr, subunits=oper.dat.calf$subunits, # oper.dat.calf
                        h.plots=plot.dat.calf$h.plots, yr.plots=plot.dat.calf$yr.plots, # plot.dat.calf
                        R=scalar.dat.calf$R, Ngroups=scalar.dat.calf$Ngroups, Nsubunits.yr=scalar.dat.calf$Nsubunits.yr, scalars=scalar.sums.calf, #scalar.dat.calf
                        years=length(unique(plot.dat.calf$yr.plots)), stratums=length(unique(plot.dat.calf$h.plots)))

jags_output_calf <- jags(bundle.dat.calf, inits, params, "www/beta_binom_model_elk2022.txt", nc, ni, nb, nt)

# Bulls
bundle.dat.bull <- list(x.tilde=sight.dat$x.tilde, z.tilde=sight.dat$z.tilde, #sight.dat
                        x=oper.dat.bull$x, ym1=oper.dat.bull$ym1, h=oper.dat.bull$h, q=oper.dat.bull$q, z=oper.dat.bull$z, yr=oper.dat.bull$yr, subunits=oper.dat.bull$subunits, # oper.dat.bull
                        h.plots=plot.dat.bull$h.plots, yr.plots=plot.dat.bull$yr.plots, # plot.dat.bull
                        R=scalar.dat.bull$R, Ngroups=scalar.dat.bull$Ngroups, Nsubunits.yr=scalar.dat.bull$Nsubunits.yr, scalars=scalar.sums.bull, #scalar.dat.bull
                        years=length(unique(plot.dat.bull$yr.plots)), stratums=length(unique(plot.dat.bull$h.plots)))

jags_output_bull <- jags(bundle.dat.bull, inits, params, "www/beta_binom_model_elk2022.txt", nc, ni, nb, nt)

## 3.4 SAVE OUTPUTS ####

setwd(output_wd)

jags_output_names <- c("jags_output", "scalar.dat")
jags_outputs <- ls(pattern = paste0("^", paste(jags_output_names, collapse = "|")))
save(list = jags_outputs, file=paste0("jags_output_", format(Sys.time(), "%Y%b%d_%H%M"), ".rdata"))

files_to_keep <- c(jags_outputs, other_inputs)
rm(list = setdiff(ls(), files_to_keep))

# 4 Plot & Report ####

## 4.1 LOAD DATA ####
setwd(input_wd)

# Get summary data (i.e. standard estimates) from your excel file
standard <- compile_sheets(file, "Summary") %>%
  rename(Standard = estimate)
standard$EPU <- name_fixer(standard$EPU)

## 4.3 CLEAN OUTPUTS ####

### 4.3.1 Bayesian ####

jags_table <- rjags_to_table(jags_output)

jags_table_cow <- rjags_to_table(jags_output_cow)

jags_table_calf <- rjags_to_table(jags_output_calf)

jags_table_bull <- rjags_to_table(jags_output_bull)

### 4.3.2 Standard ####

# Need to extract most recent target numbers
target <- standard %>%
  group_by(EPU) %>%
  filter(year == max(year)) %>%
  select(EPU, target)

standard <- standard %>%
  select(-target) %>%
  left_join(target, by="EPU")

### 4.3.3 Combine tables ####

# create a dataframe that combines the important elements of all dataframes
results.all <- left_join(jags_table %>% select(year, EPU, Model, JAGS_lcl_95, JAGS_ucl_95, JAGS_lcl_50, JAGS_ucl_50),
                         standard, 
                         by=c("EPU", "year")) %>%
  left_join(jags_table_cow %>% select(Cow=Model, EPU, year), by=c("EPU", "year")) %>%
  left_join(jags_table_calf %>% select(Calf=Model, EPU, year), by=c("EPU", "year")) %>%
  left_join(jags_table_bull %>% select(Bull=Model, EPU, year), by=c("EPU", "year"))

# calculate calf:100 cows and bull:100 cows ratios
results.all <- results.all %>%
  mutate("calf_cow" = Calf*100/Cow,
         "bull_cow" = Bull*100/Cow) %>%
  select(-Cow, -Calf, -Bull)

# uncomment any commented sections below if you're including mHT estimates
results.long <- pivot_longer(results.all,
                             c(Model,
                               Standard),
                             names_to = "method",
                             values_to = "estimate") %>%
  mutate(lcl_50 =
           if_else(method == "Model", JAGS_lcl_50, as.double(NA))) %>%
  
  mutate(ucl_50 =
           if_else(method == "Model", JAGS_ucl_50, as.double(NA))) %>%
  mutate(lcl_95 =
           if_else(method == "Model", JAGS_lcl_95, as.double(NA))) %>%
  mutate(ucl_95 =
           if_else(method == "Model", JAGS_ucl_95, as.double(NA))) %>%
  select(-JAGS_lcl_50,-JAGS_ucl_50, -JAGS_lcl_95,-JAGS_ucl_95)

results_filename <- paste0("Results_", format(Sys.time(), "%Y%b%d_%H%M"), ".csv")

setwd(output_wd)
write.csv(results.long, results_filename, row.names = F)

# 
# ## 4.5 EXTRAS ####
# 
# ### 4.5.1 Agreement ####
# 
# agree.BS = agree_test(x = results.all$JAGS,
#                       y = results.all$Standard, 
#                       delta = 1)
# print(agree.BS) # 65%
# agree.BS_plot = plot(agree.BS) +
#   scale_y_continuous(breaks = c(seq(-300, 400, by = 100)), limits = c(-300, 400))
# agree.BS_plot
# 
# ggsave("Method_agreement.jpeg", width = 10, height = 7, units="in")
# 
# # Agreement Table
# Agreement <- as.data.frame(matrix(NA, 3, 2))
# 
# Agreement[1,] <- c("Bayesian vs. Standard", agree.BS$ccc.xy[1])
# 
# colnames(Agreement) <- c("Test", "CCC")
# 
# write.csv(Agreement,"Agreement.csv", row.names = FALSE)

}

output <- runModel(file_path)

