setwd(output_wd)

results <- read_csv("Results_allyears_new.csv")

model_results <- results %>%
  filter(method=="Model") %>%
  mutate(group = "total",
         calf_cow = if_else(is.na(calf_cow), 0, calf_cow)) %>%
  select(-method)
# get class-level estimates
antlered <- model_results %>%
  mutate(group = "antlered",
         bull = round(bull_cow/(bull_cow+100)*estimate, digits=0),
         antlered_estimate = round(bull/(percent_branched/100), digits=0),
         p.antlered = antlered_estimate/estimate,
         antlered_lcl_50 = round(p.antlered*lcl_50, digits=0),
         antlered_ucl_50 = round(p.antlered*ucl_50, digits=0),
         antlered_lcl_95 = round(p.antlered*lcl_95, digits=0),
         antlered_ucl_95 = round(p.antlered*ucl_95, digits=0),
         antlerless_estimate = estimate-antlered_estimate,
         antlerless_lcl_50 = lcl_50-antlered_lcl_50,
         antlerless_ucl_50 = ucl_50-antlered_ucl_50,
         antlerless_lcl_95 = lcl_95-antlered_lcl_95,
         antlerless_ucl_95 = ucl_95-antlered_ucl_95)

antlerless <- antlered %>%
  mutate(group = "antlerless") %>%
  select(year, EPU, group, starts_with("antlerless")) %>%
  rename_with(.fn = ~gsub("antlerless_", "", .x),
              .cols = starts_with("antlerless"))

antlered <- antlered %>%
  select(year, EPU, group, starts_with("antlered")) %>%
  rename_with(.fn = ~gsub("antlered_", "", .x),
              .cols = starts_with("antlered"))

estimates <- model_results %>%
  mutate(group = "total") %>%
  select(year, EPU, group, estimate:ucl_95) %>%
  rbind(antlered, antlerless)

estimates <- full_join(estimates, model_results %>% select(year, EPU, group, target, calf_cow:cv), by=c("year", "EPU", "group"))

# make or import a harvest objective table
objectives <- as.data.frame(matrix(c("I", "I", "M", "I", "M", "D", "M", "D", "D"), nrow = 3, ncol = 3, dimnames = list(c(1,2,3), c(1,2,3))))

objectives$estimate.o <- as.numeric(rownames(objectives))

objectives <- pivot_longer(data = objectives, cols = 1:3, names_to = "calf_cow.o", values_to = "objective") %>%
  mutate(calf_cow.o = as.numeric(calf_cow.o))

# make or import your harvest rates
harvest_rates <- as.data.frame(matrix(c(5, 7.5, 10, 1, 4, 6, 3, 6, 15), 
                                      nrow=3, ncol=3, 
                                      dimnames = list(NULL, c("antlered", "antlerless", "archery"))))
harvest_rates$objective <- c("I", "M", "D")
harvest_rates <- pivot_longer(harvest_rates, antlered:archery, names_to = "group", values_to = "rate")

# make a harvest dataframe
harvest <- estimates %>%
  mutate(calf_cow.o = if_else(calf_cow<25, 1,
                              if_else(calf_cow >=25 & calf_cow <=35, 2,
                                      if_else(calf_cow>35, 3, NA))),
# can change what estimate we use here - make it depend on CV or sightability?
         estimate.o = if_else((estimate/target) <0.75, 1,
                              if_else((estimate/target)>=0.75 & (estimate/target)<=1, 2,
                                      if_else((estimate/target)>1, 3, NA)))) %>%
  # join the objectives table
  left_join(objectives, by=c("calf_cow.o", "estimate.o"))

# create harvest recommendations
harvest_allowed <- harvest %>%
  filter(group=="total") %>%
  mutate(allow = if_else(estimate > 50 &
                           bull_cow > 20 &
                           percent_branched > 30,
                         TRUE,
                         FALSE,
                         missing = F)) %>%
  select(year, EPU, allow, objective, cv) %>%
  right_join(harvest %>% select(-objective, -cv), by=c("year", "EPU")) %>%
  select(-(target:estimate.o))

# join the harvest rates table
harvest_recs <- harvest_allowed %>%
  filter(allow==T) %>%
  mutate(group = if_else(group=="total", "archery", group)) %>%
  left_join(harvest_rates, by=c("objective", "group")) %>%
  mutate(MAH = rate/100*estimate)
