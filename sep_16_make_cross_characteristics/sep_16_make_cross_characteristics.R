## This file merge the current 4waymerged_acc_with_ve_total.csv to Ben's data

panel_data <- readstata13::read.dta13('sep_16_make_cross_characteristics/4waymerged_PANEL.dta')
ve_total_data <- read.csv('fars/4waymerged_acc_with_ve_total.csv')

## remove unnecessay variables
ve_total_data <- ve_total_data[,-c(5:114)] 
index_panel_keep <- c(1:3,5:77,140:150, 165:length(panel_data))
names(panel_data)[index_panel_keep]

panel_data <- panel_data[, index_panel_keep]
# remove duplicates
panel_data <- unique(panel_data)
nrow(unique(panel_data[,1:3]))

test <- panel_data[panel_data$brandname=='acura' &
                     panel_data$modelname=='ilx' &
                     panel_data$modelyear=='2013',]
temp <- apply(test, 2, function(x) all(x==x[1]))
subset(temp, !temp)

# discover that 'driverdeathper10000UnitYears' and 'AgeTrunc' has different value in a same model
# remove those variables
panel_data <- panel_data[,-c(which(names(panel_data)=='driverdeathper10000UnitYears'),
                             which(names(panel_data)=='AgeTrunc'))]
panel_data <- unique(panel_data)
nrow(panel_data) == nrow(unique(panel_data[,1:3]))

# merge panel_data with ve_total_data
nrow(ve_total_data) #106805
merged_table <- merge(ve_total_data, panel_data, by.x=c('BrandName_N','ModelName_N','ModelYear'),
                      by.y=c('brandname','modelname','modelyear'), all=TRUE) #107269

nrow(merged_table[is.na(merged_table$NewReportingDateQuarterly),]) #464
missing_in_ve <- merged_table[is.na(merged_table$NewReportingDateQuarterly),]

merged_table <- merge(ve_total_data, panel_data, by.x=c('BrandName_N','ModelName_N','ModelYear'),
                      by.y=c('brandname','modelname','modelyear'), all.x = TRUE) #106805

accident_1 <- merged_table[merged_table$ve_total == 1,] #nrow = 45451
accident_2 <- merged_table[merged_table$ve_total == 2,] #nrow = 61354

accident_1$ID <- NULL
accident_2$ID <- NULL

accident_1 <- unique(accident_1)
accident_2 <- unique(accident_2)

write.csv(accident_1, 'sep_16_make_cross_characteristics/1_vehicle_accidents.csv', row.names = F)
write.csv(accident_2, 'sep_16_make_cross_characteristics/2_vehicle_accidents.csv', row.names = F)

# remove the rows with only 1 car recorded
accident_1 <- read.csv('sep_16_make_cross_characteristics/1_vehicle_accidents.csv')
accident_2 <- read.csv('sep_16_make_cross_characteristics/2_vehicle_accidents.csv')

accident_2$ID <- paste(accident_2$st_case, accident_2$year)
n_occur <- data.frame(table(accident_2$ID))
# '220281 2010' has 4 accidents

n_occur <- n_occur[n_occur$Freq > 1,]
index_has_two_acc <- accident_2$ID %in% n_occur$Var1
accident_2_delete_incomplete <- accident_2[index_has_two_acc,]
accident_2_delete_incomplete$ID <- NULL

write.csv(accident_2_delete_incomplete, 'sep_16_make_cross_characteristics/2_vehicle_accidents_delete_single_record.csv', row.names = F)


# deal with '220281 2010' 
#PROBLEM SOLVED ID has different value in ve
#temp <- accident_2[accident_2$ID=='220281 2010',]

#temp1 <- apply(temp[c(1,2),], 2, function(x) all(x==x[1]))
#subset(temp1, !temp1)

#temp2 <- apply(temp[c(3,4),], 2, function(x) all(x==x[1]))
#subset(temp2, !temp2)

#### SEP 18, drop the variables were not dropped


#### Create a set of names of variables to keep

variables_selected = 'BrandName_N	ModelName_N	ModelYear NewReportingDateQuarterly year st_case reg_stat a_vroll a_drdis	a_drdro	a_spveh	dr_drink 
prev_acc	prev_sus	prev_dwi	prev_spd	prev_oth Weather.Cond	Light.Cond driver.death 
driver.sex_male	driver.sex_female	driver.sex_missing	driver.race_missing	driver.race_hisp	driver.race_white	driver.race_black	driver.race_amerind	driver.race_asian	driver.race_pacisl	driver.race_other
driver.age_less_16	driver.age_16_20	driver.age_21_24	driver.age_25_34	driver.age_35_44	driver.age_45_64	driver.age_65_above	driver.age_missing
air_bag_not_depl rest_not_used Impact.Desc AnyIIHSBadge med_epa_combined
med_lengthins med_widthins med_heighthins mo_apillarrearwardmovementcm sd_bpillarto
rf_strengthtoweightratio sd_maximumviscouscriterionms mo_chestmaximumcompressionmm
mo_headheadhic15 type new_drivetype RatingsMissing_SD_By_Year_FE	RatingsMissing_SO_By_Year_FE	RatingsMissing_MO_By_Year_FE	RatingsMissing_RF_By_Year_FE
WeightLT__2_5K	WeightBTW__2_5K_3K	WeightBTW__3K_3_5K	WeightBTW__3_5K_4K	WeightBTW__4K_4_5K	WeightBTW__4_5K_5K	WeightGT__5K
'

variables_selected = strsplit(variables_selected, '[[:space:]]+')[[1]]
#### select the variables
accident_2_delete_incomplete <- read.csv('sep_16_make_cross_characteristics/2_vehicle_accidents_delete_single_record.csv')
accident_selected <- accident_2_delete_incomplete[,variables_selected]

#### Modify age and sex and race variables
### Sex
temp_function <- function(x){
  x <- x[21:23]
  if(x[1] == 1){
    return('male')
  }
  if(x[2] == 1){
    return('female')
  }
  if(x[3] == 1){
    return('missing')
  }
  stop('incorrect row')
}
accident_selected$sex <- apply(accident_selected, 1, temp_function)

### Race
temp_function <- function(x){
  x <- x[24:31]
  index <- x==1
  index <- as.logical(index)
  if (any(index)==F | length(subset(index, index)) > 1){
    stop('incorrect row')
  }
  races <- c('missing','hisp','white','black','amerind','asian','pacisl',
             'other')
  return(races[index])
}
accident_selected$race <- apply(accident_selected, 1, temp_function)

### Age
temp_function <- function(x){
  thisrow <- x
  x <- as.numeric(x[32:39])
  #Matrix calculation to change to different groups
  trans_matrix <- matrix(c(1,1,0,0,0,0,0,0,
                          0,0,1,1,0,0,0,0,
                          0,0,0,0,1,0,0,0,
                          0,0,0,0,0,1,0,0,
                          0,0,0,0,0,0,1,0,
                          0,0,0,0,0,0,0,1),
                         byrow=T, ncol=8)
  x <- c(trans_matrix %*% x)
  if (length(subset(x, x==1))!= 1){
    print(x)
    print(thisrow)
    stop('incorrect row')
  }
  age_groups <- c('<20','21-34','35-44','45-64','65+','missing')
  return(age_groups[x==1])
}
accident_selected$age <- apply(accident_selected, 1, temp_function)
rm(temp_function)

accident_selected <- accident_selected[,-c(21:23,24:31, 32:39)]

#### Create the variables of another car

accident_selected$NewReportingDateQuarterly <- NULL
accident_selected <- unique(accident_selected)

# create variable sets
val_list <- names(accident_selected)

indentity_val <- c(1:5)
car_char <- c(23:46)
driver_char <- c(8:16,47:49,6)
acc_info <- c(17,18,21, 20,7,22)
target <- 19


# create the_other_car variables
accident_selected$ID <- paste(accident_selected$year, accident_selected$st_case)
accident_selected$unique_id <- 1:nrow(accident_selected)
  
temp_function <- function(x){
  print(x[51])
  another_car_list <- accident_selected[accident_selected$ID == x[50] & accident_selected$unique_id != trimws(x[51]),]
  if (nrow(another_car_list) != 1){
    print(another_car_list)
    print(x)
    stop('error')
  }
  another_car_list <- as.character(another_car_list)
  return(c(x,another_car_list[c(car_char,driver_char)]))
}

rows <- apply(accident_selected, 1, temp_function)
new_cols <- paste('other_',val_list[c(car_char, driver_char)])
new_cols <- gsub('[[:space:]]', '', new_cols)
rows <- t(rows)
new_data_frame <- as.data.frame(rows, col.names=c(names(accident_selected), new_cols))
colnames(new_data_frame) <- c(names(accident_selected), new_cols)

write.csv(new_data_frame, 'sep_16_make_cross_characteristics/acc_with_other_characteristics_sep18.csv')  




  
