## This file merge the current 4waymerged_acc_with_ve_total.csv to Ben's data

panel_data <- readstata13::read.dta13('sep_16_make_cross_characteristics/4waymerged_PANEL.dta')
ve_total_data <- read.csv('fars/4waymerged_acc_with_ve_total.csv')

## remove unnecessay variables
ve_total_data <- ve_total_data[,-c(16:114)]
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

write.csv(accident_1, 'sep_16_make_cross_characteristics/1_vehicle_accidents.csv', row.names = F)
write.csv(accident_2, 'sep_16_make_cross_characteristics/2_vehicle_accidents.csv', row.names = F)

n_accidents <- apply(accident_2, 1, function(x) nrow(accident_2[accident_2$BrandName_N == x[1] &
                                                                  accident_2$ModelName_N == x[2] &
                                                                  accident_2$ModelYear == x[3], ]))







