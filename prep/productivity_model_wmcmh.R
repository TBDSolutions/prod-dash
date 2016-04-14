##########################
### PRODUCTIVITY MODEL ###
##########################

# READ DATA

  # Install packages
    library(readxl)
    library(dplyr)
    library(RCurl)
    
  # Read service data
    source("prep/function_combineServices.R")
    
    services <- combineServices(directory = "data/raw/Services")
    
  # Read map of codes 
    codemap <- read_excel("data/raw/ServiceCodeToCPTHCPCmap.xlsx")
  
  # Read employee data
    employee1 <- read_excel("data/raw/Employee/StaffJobTitlesWageRates.xlsx")
    employee2 <- read_excel("data/raw/Employee/StaffJobTitlesWageRates_09-25-15.xlsx")
    employee3 <- read_excel("data/raw/Employee/StaffJobTitlesWageRates_11-17-15.xlsx")
    employee4 <- read_excel("data/raw/Employee/StaffJobTitlesWageRates_01-7-16.xlsx")
    employee5 <- read_excel("data/raw/Employee/StaffJobTitlesWageRates_02-23-16.xlsx")
    employee6 <- read_excel("data/raw/Employee/StaffJobTitlesWageRates_04-13-16.xlsx")
    
  # Clean differences
    names(employee2)[7] <- "ProviderNum"   # Rename unmatching col name
    names(employee3)[7] <- "ProviderNum"   # Rename unmatching col name
    names(employee4)[7] <- "ProviderNum"   # Rename unmatching col name
    names(employee5)[7] <- "ProviderNum"   # Rename unmatching col name
    names(employee6)[7] <- "ProviderNum"   # Rename unmatching col name
    
    employee3 <- employee3 %>% select(1:14)
    
    source("prep/function_combineEmployee.R")
    
    employee <- combineEmployee(employee1, employee2)
    employee <- combineEmployee(employee, employee3)
    employee <- combineEmployee(employee, employee4)
    employee <- combineEmployee(employee, employee5)
  
    rm(employee1);rm(employee2);rm(employee3);rm(employee4);rm(employee5)
    
  # Read payroll files showing # hours for which employees were paid
    
    source("prep/function_combinePaidHrs.R")
    paidhrs <- combinePaidHrs2(directory = "data/raw/PaidHrs")
    
    paidhrs <- 
    paidhrs %>% 
      select(employee_number:effective_date,pay_type,hours:wages,Division:gl_program) 
    
  # Get standard cost data from CMS PPS
    # https://www.cms.gov/Medicare/Medicare-Fee-for-Service-Payment/PhysicianFeeSched/PFS-Relative-Value-Files-Items/RVU16A.html?DLPage=1&DLEntries=10&DLSort=0&DLSortDir=descending
    rvu <- read.csv("data/raw/RVU/2016/PPRRVU16_V1110_V2.csv", skip = 9)
    gcpi <- read.csv("data/raw/RVU/2016/CY2016_GPCIs.csv")

  # Get standard cost data from 404 report
    open404 <- getURL("https://raw.githubusercontent.com/j-hagedorn/open404/master/data/clean/Master.csv",
                   ssl.verifypeer = 0L, followlocation = 1L)
    writeLines(open404,'temp.csv')
    open404 <- read.csv('temp.csv')

# CLEAN DATA

  library(dplyr)
  library(magrittr)
  
  # CODEMAP
    names(codemap)[1] <- "ServiceCode"   # Rename column 
    names(codemap)[2] <- "Code"   # Rename column
    names(codemap)[3] <- "ServiceDesc"   # Rename column
    names(codemap)[4] <- "Increment"   # Rename column 
    # Delete blank rows at end
      codemap <- codemap[-c(115:118), ] # Delete rows with ONE:MANY mapping of INTERNAL:HCPCS
    # Add rows for codes where multiple codes are listed in a single row
      codemap[nrow(codemap)+1,] <- c("309", "H2011", 
                                     "Crisis Intervention","15 min round down")
      codemap[nrow(codemap)+1,] <- c("310", "H2011", 
                                     "Crisis Intervention","15 min round down")
      codemap[nrow(codemap)+1,] <- c("206", "H2015", "Community Living Supports",
                                     "15 min round down")
      codemap[nrow(codemap)+1,] <- c("207", "H2015", "Community Living Supports",
                                     "15 min round down")
      codemap[nrow(codemap)+1,] <- c("700", "Indirect", 
                                     "Indirect Service","encounter")
      codemap <- codemap %>% arrange(ServiceCode)
    # Delete the rows we replaced
      codemap <- codemap[-c(18,59), ] # Delete rows with ONE:MANY mapping of INTERNAL:HCPCS
    # Split codes and modifiers
      codemod <- as.data.frame(stringr::str_split_fixed(codemap$Code, " ", 2))
      codemap <- cbind(codemap, codemod)
      remove(codemod)

    codemap <- codemap %>% rename(HCPCS = V1, MOD = V2) %>%
                           mutate(ServiceCode = as.factor(ServiceCode),
                                  Code = as.factor(Code),
                                  ServiceDesc = as.factor(ServiceDesc)) %>%
                           select(-Code) 

    # Now map increments to numeric values (so Served Hours / Increment = Units)...
      codemap$convert <-
      car::recode(codemap$Increment, 
                  "'15 min round down' = .25;
                  '15 min round UP' = .25;
                  'day' = 24;
                  'encounter' = NA;
                  'injection - N/A' = NA;
                  'item' = NA;
                  'mile' = NA;
                  'month' = NA;
                  'Per Hour' = 1")
    

  # EMPLOYEE
    names(employee)[1] <- "lastname" 
    names(employee)[2] <- "firstname" 
    names(employee)[3] <- "title" 
    names(employee)[4] <- "payclass" 
    names(employee)[5] <- "unitrate" 
    names(employee)[6] <- "year_pay" 
    names(employee)[8] <- "stafftype" 
    names(employee)[9] <- "gl_id"
    names(employee)[10] <- "gl_desc"
    names(employee)[11] <- "hrs_pay_pd" 
    names(employee)[13] <- "cc_id" 
    names(employee)[14] <- "cc_desc" 
    
    # Strip trailing whitespace from names  
      employee$lastname <- sub("\\s+$", "", employee$lastname)
      employee$firstname <- sub("\\s+$", "", employee$firstname)
      employee$title <- as.factor(sub("\\s+$", "", employee$title))
      employee$gl_id <- as.factor(sub("\\s+$", "", employee$gl_id))
      employee$gl_desc <- as.factor(sub("\\s+$", "", employee$gl_desc))
      employee$cc_desc <- as.factor(sub("\\s+$", "", employee$cc_desc))

      employee$title <- car::recode(employee$title, 
                                       "'Aide'= 'CSA';
                                        'Consumer Aide' = 'CSA'")
      
    employee <- 
    employee %>% 
      distinct() %>% # remove multiples from loading multi-month files
      select(lastname, firstname, title, payclass, 
             unitrate, year_pay, ProviderNum,
             stafftype, hrs_pay_pd, 
             gl_id, gl_desc,
             cc_id, cc_desc) %>%
      mutate(staffname = as.factor(paste(lastname,firstname,sep = ", ")),
             title = as.factor(title),
             payclass = as.factor(payclass),
             ProviderNum = as.factor(ProviderNum),
             stafftype = as.factor(stafftype),
             cc_id = as.factor(cc_id)) %>%
      select(-lastname, -firstname)

    # Assign clinical percentage values based on Job Type
      employee$clin_pct <- car::recode(employee$title, 
                                            "'Administrator 105' = 0;
                                            'Applied Behavioral Psych' = 0.70;
                                            'C&EPS'= 0.78;
                                            'Care Manager'= 0.42;
                                            'CSA'= 0.70;
                                            'Deputy Director 109' = 0;
                                            'Deputy Director 111' = 0;
                                            'Director 116'= 0.50;
                                            'Intern'= NA;
                                            'Medical Assistant'= 0;
                                            'MHC'= 0.77;
                                            'Psychiatrist 116'= 0.88;
                                            'Service Entry Specialist'= 0.30;
                                            'Staff Nurse (RN)'= 0.42;
                                            'SUD'= 0.88;
                                            'Supervisor 101'= 0;
                                            'Supervisor 102'= 0.50;
                                            'Supervisor 104'= 0.50;
                                            'Supports Coordinator'= 0.42;
                                            'Team Leader 104' = 0;
                                            'Team Leader 106' = 0;
                                            else= 0.625")
      
      # Make it numeric
      employee$clin_pct <- as.numeric(levels(employee$clin_pct))[employee$clin_pct]
      
      # Manage exceptions
      employee <-
      employee %>%
        mutate(clin_pct = ifelse(title == "MHC" & cc_desc == "Service Entry",
                                 yes =  0.50, no = clin_pct),
               clin_pct = ifelse(title == "Staff Nurse (RN)" 
                                 & cc_desc == "Health Services"
                                 & hrs_pay_pd >= 80, #i.e. full time
                                 yes =  0.84, no = clin_pct),
               clin_pct = ifelse(title == "Staff Nurse (RN)" 
                                 & cc_desc == "Health Services"
                                 & hrs_pay_pd < 80, #i.e. part time
                                 yes =  0.30, no = clin_pct),
               clin_pct = ifelse(title == "Staff Nurse (RN)" 
                                 & cc_desc != "Health Services"
                                 & hrs_pay_pd < 80, #i.e. part time
                                 yes =  0.40, no = clin_pct)
               )
      
      # Shorten names of cc_desc for display
      employee$cc_desc <- car::recode(employee$cc_desc,      
                                      "'CES/CSS/PWC/DU' = 'CES';
                                      'Children\\'s Outreach' = 'Child Outreach';
                                      'CSM-MI Adult' = 'Case Mgmt MIA';
                                      'CSM - DD' = 'Case Mgmt DD';
                                      'SUD/Mild to Moderate' = 'SUD and MildMod'")                            
      
     

    # Apply fringe % based on GL Program Code
        employee$fringe <- car::recode(employee$gl_id, 
                                             "'BA'= 0.45;
                                             'CERTMA'= 0.40;
                                             'CONT'= 0.08;
                                             'DR'= 0.26;
                                             'NONDEGREE'= 0.41;
                                             'PFS'= 0.08;
                                             'SUBTEM'= 0.08;
                                             else= 0")
        employee$fringe <- as.numeric(levels(employee$fringe))[employee$fringe]
     
    # Calculate FTEs and apply overhead per Cost Center 
      employee$ovr_per_fte <- car::recode(as.character(employee$cc_id), 
                                           "'301'= 64856;
                                           '411'= 113261;
                                           '451'= 68345;
                                           '501'= 46964;
                                           '521'= 42176;
                                           '551'= 48054;
                                           '553'= 44429;
                                           '622'= 56970;
                                           '653'= 51387;
                                           else= 0")
      
      employee <- 
      employee %>% 
        mutate(fte = hrs_pay_pd/80, # 80=40*2 wks per pay pd
               ovr = round(fte * ovr_per_fte, digits = 2)) 
           
  # SERVICES

    services <-
    services %>%
      mutate(ProviderNum = as.factor(ProviderNum),
             ServiceCode = as.factor(ServiceCode),
             ServeHours = round(difftime(EndDate, BeginDate, units = "hours"),
                                digits = 2),
             Date = as.Date(EndDate)) %>%
      select(ProviderNum, ServiceCode, 
             Units, NumInGroup, Date, ServeHours) 

  # PAID HOURS

    # Create Month and Year variables
    paidhrs$Month <- lubridate::month(paidhrs$effective_date)
    paidhrs$Year <- lubridate::year(paidhrs$effective_date) 
    # Modify dates for conversion to XTS
    paidhrs$Week_Dt <- lubridate::floor_date(paidhrs$effective_date, unit = "week")
    paidhrs$Month_Dt <- lubridate::floor_date(paidhrs$effective_date, unit = "month")
    
    # Remove leading zeroes from employee number to match with Employee and Service
    paidhrs$employee_number <- gsub("(^|[^0-9])0+", "\\1", 
                                    paidhrs$employee_number, 
                                    perl = TRUE)
    
    # Make monthly table
    paidhrs_month <-
    paidhrs %>% 
      filter(pay_type != "PTO Sell") %>% # Remove PTO Sell from paid hrs calc
      rename(ProviderNum = employee_number) %>%
      group_by(ProviderNum, Month_Dt) %>%
      summarise(hours = sum(hours),
                wages = sum(wages)) %>%
      mutate(ID = paste(ProviderNum, Month_Dt, sep = "-")) %>% # Make ID to join to monthly services
      ungroup() %>%
      select(-ProviderNum, -Month_Dt)
    
    # Make weekly table
    paidhrs_week <-
      paidhrs %>% 
      filter(pay_type != "PTO Sell") %>% # Remove PTO Sell from paid hrs calc
      rename(ProviderNum = employee_number) %>%
      group_by(ProviderNum, Week_Dt) %>%
      summarise(hours = sum(hours),
                wages = sum(wages)) %>%
      mutate(ID = paste(ProviderNum, Week_Dt, sep = "-")) %>% # Make ID to join to monthly services
      ungroup() %>%
      select(-ProviderNum, -Week_Dt)
  
  # RVU
    # The geographic practice cost index (GPCIs) are applied in the 
    # calculation of a fee schedule payment amount by multiplying the 
    # RVU for each component times the GPCI for that component.
    
    # Facility practice expense RVUs will be used for services performed 
    # in inpatient or outpatient hospital settings, emergency rooms, skilled 
    # nursing facilities, or ambulatory surgical centers (ASCs). The 
    # non-facility practice expense relative value units will be used for 
    # services furnished in all other settings

    # Non-Facility Pricing Amount = 
      # [(Work RVU * Work GPCI) + (Non-Facility PE RVU * PE GPCI) + 
      # (MP RVU * MP GPCI)] * Conversion Factor (CF)

    # Filter GCPI for geo region and clean names
      gcpi <-
      gcpi %>%
        filter(Locality.name == "Rest of Michigan") %>%
        rename(work_gpci = Work......GPCI,
               pe_gpci = PE......GPCI,
               mp_gpci = MP.....GPCI) %>% droplevels

    # Add GCPI values to RVU table
      rvu$work_gpci <- gcpi$work_gpci
      rvu$pe_gpci <- gcpi$pe_gpci
      rvu$mp_gpci <- gcpi$mp_gpci
      rm(gcpi)
      
    # Rename RVU table variables
      rvu <-
      rvu %>%
        rename(work_rvu = RVU,
               pe_rvu_nf = PE.RVU,
               pe_rvu_f = PE.RVU.1,
               mp_rvu = RVU.1,
               cf = FACTOR) %>%
        select(HCPCS, MOD, DESCRIPTION, 
               work_rvu, work_gpci,
               pe_rvu_nf, pe_rvu_f, pe_gpci,
               mp_rvu, mp_gpci, cf)

    # Facility practice expense RVUs will be used for services performed 
    # in inpatient or outpatient hospital settings, emergency rooms, skilled 
    # nursing facilities, or ambulatory surgical centers (ASCs). The 
    # non-facility practice expense relative value units will be used for 
    # services furnished in all other settings
    
    # Non-Facility Pricing Amount = 
    # [(Work RVU * Work GPCI) + (Non-Facility PE RVU * PE GPCI) + 
    # (MP RVU * MP GPCI)] * Conversion Factor (CF)

      rvu <-
      rvu %>%
        mutate(total_rvu = ((work_rvu * work_gpci) + 
                            (pe_rvu_nf * pe_gpci) +
                            (mp_rvu * mp_gpci)) * cf)
        
      sub_rvu <- 
      rvu %>% 
        semi_join(codemap, by = "HCPCS") %>%
        select(HCPCS, total_rvu) %>%
        filter(total_rvu > 0)

    # Summarize avg and med unit cost from 404 data
 
      open404 <-
      open404 %>%
        filter(CMHSP != "Detroit-Wayne" & FY == max(FY)) %>% # Filter out Detroit region // to RVU
        rename(HCPCS = Code) %>%
               # MOD = FirstOfModifier) %>% # Do not use MOD, results in dup rows after join
        group_by(HCPCS) %>%
        summarise(cost = sum(SumOfCost, na.rm = T),
                  units = sum(SumOfUnits, na.rm = T),
                  med_unitcost = median(CostPerUnit, na.rm = T),
                  q1_unitcost = quantile(CostPerUnit, .25, na.rm = T),
                  mad_unitcost = mad(CostPerUnit, na.rm = T )) %>%
        mutate(avg_unitcost = round(cost / units, digits =2),
#                adj_unitcost = round(med_unitcost - mad_unitcost, digits = 2), # adjust by 1 mad
#                adj_unitcost = ifelse(adj_unitcost > med_unitcost/2, # greater than half median
#                                      yes = adj_unitcost,
#                                      no = med_unitcost - (mad_unitcost/2)),
               adj_unitcost = q1_unitcost) %>%
        select(HCPCS, avg_unitcost, med_unitcost, 
               q1_unitcost, mad_unitcost, adj_unitcost)

    # Join RVU and 404 unit values to "codemap" table
      codemap <-
      codemap %>%
        left_join(sub_rvu, by = "HCPCS") %>%
        left_join(open404, by = "HCPCS") %>%
        mutate(rvu = round(ifelse(total_rvu %in% c(NA, 0), 
                              yes = adj_unitcost, 
                              no = total_rvu),
                           digits = 2),
               ServiceCode = as.factor(ServiceCode))  %>%
        filter(HCPCS != "90832" 
               & HCPCS != "90834") %>%
        distinct(ServiceCode) # Only distinct codes to avoid duplication of svs on join
  
    write.csv(codemap, "data/codemap.csv")
      
      
# MERGE SERVICE and EMPLOYEE DATA

  produce <- 
    services %>%
    filter(ServiceCode != 700) %>% # Filter out indirect svs
      # Issues w Ind: https://app.asana.com/0/20383833681148/36670714568594
    left_join(codemap) %>% #, by = "ServiceCode"
    left_join(employee) %>% #, by = "ProviderNum"
    filter(!grepl('^J', HCPCS)) %>% # Filter HCPCS J-codes, hrs under Med admin
    filter(HCPCS != "T1013" # Filter out Sign Language, not direct svs
           & ServiceCode != "310") %>% # Filter out Crisis on-call
    mutate(ServiceCode = as.factor(ServiceCode),
           ProviderNum = as.factor(ProviderNum),
           ServeHours = as.numeric(ServeHours),
           HCPCS = as.factor(HCPCS),
           Increment = as.factor(Increment))

produce$NumX <- ifelse(produce$NumInGroup %in% c(0, NA), 
                       yes = 1, no = produce$NumInGroup)
produce$serveHours_adj <- round(produce$ServeHours / produce$NumX, digits = 1)
produce$convert_units <- produce$ServeHours / produce$convert
# Include rounding logic based on Increment field
produce$adj_units_in <- ifelse(is.na(produce$convert_units), 
                      yes = produce$Units, 
                      no = produce$convert_units)
produce$adj_units_up <- ceiling(produce$adj_units_in)
produce$adj_units_dn <- floor(produce$adj_units_in)
produce$adj_units <- ifelse(produce$Increment == "15 min round down" 
                   | is.na(produce$Increment),
                   yes = produce$adj_units_dn,
                   no = produce$adj_units_up)
produce$value <- round(produce$rvu * produce$adj_units, digits = 2)
produce$paid <- round(produce$unitrate * produce$serveHours_adj, digits = 2)

#   produce <-
#   produce %>%
#     mutate(NumX = ifelse(NumInGroup %in% c(0, NA), 
#                          yes = 1, no = NumInGroup), # convert 0, avoid Inf
#            # Hours worked (divides total hours worked by # in group)
#            serveHours_adj = round(ServeHours / NumX, digits = 1),
#            # Adjusted units 
#            convert_units = ServeHours / convert,
#            # Include rounding logic based on Increment field
#            adj_units_in = ifelse(is.na(convert_units), 
#                               yes = Units, 
#                               no = convert_units),
#            adj_units_up = ceiling(adj_units_in),
#            adj_units_dn = floor(adj_units_in),
#            adj_units = ifelse(Increment == "15 min round down" 
#                               | is.na(Increment),
#                               yes = adj_units_dn,
#                               no = adj_units_up),
#            value = round(rvu * adj_units, digits = 2),
#            paid = round(unitrate * serveHours_adj, digits = 2)) 
#    # %>% droplevels

# Test for NA values
# View(produce %>% filter(is.na(value) == T & HCPCS != "Indirect"))

# Create Week, Month and Year variables
  produce$Week <- lubridate::week(produce$Date)
  produce$Month <- lubridate::month(produce$Date)
  produce$Year <- lubridate::year(produce$Date)
  # Modify dates for conversion to XTS
  produce$Week_Dt <- lubridate::floor_date(produce$Date, unit = "week")
  produce$Month_Dt <- lubridate::floor_date(produce$Date, unit = "month")
  
  
# Recode title with ampersand
  produce$title <- car::recode(produce$title, "'C&EPS'= 'CEPS'")
  produce$title <- as.factor(produce$title)
    
# Make a smaller joined table for use in analysis
  prod <- 
  produce %>%
    select(ServiceCode,HCPCS,ServiceDesc,
           Year,Month,Week,Month_Dt,Week_Dt,Date,
           ProviderNum,staffname,title,clin_pct,
           unitrate,hrs_pay_pd,fringe,ovr,
           gl_id,gl_desc,cc_id,cc_desc,
           direct_hrs = serveHours_adj, group = NumX, 
           rvu, units = adj_units, value)

# AGGREGATE
  
  # Aggregate Month data (group by Clinician, Month)
library(lubridate)
per_month <- 
  prod %>%
  group_by(title, ProviderNum, staffname, cc_desc, Year, Month, Month_Dt) %>%
  summarise(CP = max(clin_pct),
            Sch = max(hrs_pay_pd, na.rm = T)*2, # Multiply by 2 for month pd
            Srv = sum(direct_hrs),  # Hrs worked (divides total hrs by # in grp)
            Unt = sum(units), # Billable units
            Prt = round(max(unitrate), digits = 2), # Pay rate per hour
            Ovr = max(ovr)/12, # Monthly adjustment for annual overhead 
            Fri = max(fringe),
            Rvu = sum(value, na.rm = T)
            ) %>%
  mutate(ID = paste(ProviderNum, Month_Dt, sep = "-")) %>%
  left_join(paidhrs_month, by = "ID") %>%
  droplevels() %>%
  rename(Hrs = hours,
         Pay = wages) %>%
  mutate(Adm = Hrs - Srv, #Admin hours = Total paid hrs - direct service hrs
         # Pay = Hrs * Prt,
         # Adjusted costs for supervisors, a portion of which are spread across Ovr
         Pay = ifelse(title %in% c("Supervisor 101", "Supervisor 102", "Supervisor 104"),
                      yes = Pay * 0.5, no = Pay),
         # Adjusted costs for CMO, a portion of which are spread across Ovr
         Pay = ifelse(title %in% c("Director 116"),
                      yes = Pay * 0.85, no = Pay),
         Fri = Pay * Fri,
         Ovr = ifelse(title %in% c("Supervisor 101", "Supervisor 102", "Supervisor 104"),
                      yes = Ovr * 0.5, no = Ovr),
         Ovr = ifelse(title %in% c("Director 116"),
                      yes = Ovr * 0.85, no = Ovr),
         Exp = round(Pay + Fri + Ovr, digits = 2), # Budgeted expectation
         prod_raw = round(Srv / Hrs * 100, 
                          digits = 2),
         valu_cst = round(Rvu / Exp * 100,
                          digits = 2),
         RvuPerSrv = round(Rvu / Srv, digits = 2),
         SrvNeeded = round(Exp / RvuPerSrv, digits = 1), # how many direct hours needed to meet VPU = 100
         SrvNeededCP = round(Hrs * CP , digits = 1), # how many direct hours needed to meet CP at target
         CPNeeded = round(SrvNeeded / Hrs, digits = 1),
         prod_pct = round(Srv / SrvNeeded * 100, 
                          digits = 2),
         SrvtoCPGoal = round(SrvNeededCP - Srv, digits = 1),
         SrvtoGoal = round(SrvNeeded - Srv, digits = 1)) %>%
  select(ProviderNum,staffname,title,Year,Month,Month_Dt,
         Sch,Hrs,Srv,Adm,CP,Prt,Pay,Fri,Ovr,Exp,Unt,Rvu,
         prod_raw,prod_pct,valu_cst,
         RvuPerSrv,SrvNeeded,SrvNeededCP,CPNeeded,SrvtoCPGoal,SrvtoGoal) %>%
  filter(Pay > 0 # Provider has pay rate > $0
         & Srv <= Hrs) # Provider has more direct hrs than hrs paid 


# Write to file for app
  write.csv(per_month, "data/prod_per_month.csv")

  # Identify employees affected by data issues
    weird <- 
      prod %>%
      group_by(title, ProviderNum, staffname, cc_desc, Year, Month, Month_Dt) %>%
      summarise(CP = max(clin_pct),
                Sch = max(hrs_pay_pd, na.rm = T)*2, # Multiply by 2 for month pd
                Srv = sum(direct_hrs),  # Hrs worked (divides total hrs by # in grp)
                Unt = sum(units), # Billable units
                Prt = round(max(unitrate), digits = 2), # Pay rate per hour
                Ovr = max(ovr)/12, # Monthly adjustment for annual overhead 
                Fri = max(fringe),
                Rvu = sum(value, na.rm = T)
      ) %>%
      mutate(ID = paste(ProviderNum, Month_Dt, sep = "-")) %>%
      left_join(paidhrs_month, by = "ID") %>%
      droplevels() %>%
      rename(Hrs = hours,
             Pay = wages) %>%
      mutate(Adm = Hrs - Srv, #Admin hours = Total paid hrs - direct service hrs
             # Pay = Hrs * Prt,
             # Adjusted costs for supervisors, a portion of which are spread across Ovr
             Pay = ifelse(title %in% c("Supervisor 101", "Supervisor 102", "Supervisor 104"),
                          yes = Pay * 0.5, no = Pay),
             # Adjusted costs for CMO, a portion of which are spread across Ovr
             Pay = ifelse(title %in% c("Director 116"),
                          yes = Pay * 0.85, no = Pay),
             Fri = Pay * Fri,
             Ovr = ifelse(title %in% c("Supervisor 101", "Supervisor 102", "Supervisor 104"),
                          yes = Ovr * 0.5, no = Ovr),
             Ovr = ifelse(title %in% c("Director 116"),
                          yes = Ovr * 0.85, no = Ovr),
             Exp = round(Pay + Fri + Ovr, digits = 2), # Budgeted expectation
             prod_raw = round(Srv / Hrs * 100, 
                              digits = 2),
             valu_cst = round(Rvu / Exp * 100,
                              digits = 2),
             RvuPerSrv = round(Rvu / Srv, digits = 2),
             SrvNeeded = round(Exp / RvuPerSrv, digits = 1), # how many direct hours needed to meet out_in = 100
             CPNeeded = round(SrvNeeded / Hrs, digits = 1),
             prod_pct = round(Srv / SrvNeeded * 100, 
                              digits = 2),
             SrvtoGoal = round(SrvNeeded - Srv, digits = 1)) %>%
      select(ProviderNum,staffname,title,Year,Month,Month_Dt,
             Sch,Hrs,Srv,Adm,CP,Prt,Pay,Fri,Ovr,Exp,Unt,Rvu,
             prod_raw,prod_pct,valu_cst,
             RvuPerSrv,SrvNeeded,CPNeeded,SrvtoGoal) %>% 
      filter(pay = 0
             | Srv > Hrs)
  
    
    weird2 <- services %>% anti_join(employee) 
    #write.csv(weird, "no_pay_or_more_srv_than_hrs.csv")
    #write.csv(weird2, "no_info_in_employee.csv")
    
  # Aggregate Week data (group by Clinician, Week)

    per_week <- 
      prod %>%
      group_by(title, ProviderNum, staffname, cc_desc, Week_Dt) %>%
      summarise(CP = max(clin_pct),
                Sch = max(hrs_pay_pd, na.rm = T)*2, # Multiply by 2 for month pd
                Srv = sum(direct_hrs),  # Hrs worked (divides total hrs by # in grp)
                Unt = sum(units), # Billable units
                Prt = round(max(unitrate), digits = 2), # Pay rate per hour
                Ovr = max(ovr)/52.1429, # Weekly adjustment for annual overhead 
                Fri = max(fringe),
                Rvu = sum(value, na.rm = T)
      ) %>%
      mutate(ID = paste(ProviderNum, Week_Dt, sep = "-")) %>%
      left_join(paidhrs_week, by = "ID") %>%
      droplevels() %>%
      rename(Hrs = hours,
             Pay = wages) %>%
      mutate(Adm = Hrs - Srv, #Admin hours = Total paid hrs - direct service hrs
             # Pay = Hrs * Prt,
             # Adjusted costs for supervisors, a portion of which are spread across Ovr
             Pay = ifelse(title %in% c("Supervisor 101", "Supervisor 102", "Supervisor 104"),
                          yes = Pay * 0.5, no = Pay),
             # Adjusted costs for CMO, a portion of which are spread across Ovr
             Pay = ifelse(title %in% c("Director 116"),
                          yes = Pay * 0.85, no = Pay),
             Fri = Pay * Fri,
             Ovr = ifelse(title %in% c("Supervisor 101", "Supervisor 102", "Supervisor 104"),
                          yes = Ovr * 0.5, no = Ovr),
             Ovr = ifelse(title %in% c("Director 116"),
                          yes = Ovr * 0.85, no = Ovr),
             Exp = round(Pay + Fri + Ovr, digits = 2), # Budgeted expectation
             prod_raw = round(Srv / Hrs * 100, 
                              digits = 2),
             valu_cst = round(Rvu / Exp * 100,
                              digits = 2),
             RvuPerSrv = round(Rvu / Srv, digits = 2),
             SrvNeeded = round(Exp / RvuPerSrv, digits = 1), # how many direct hours needed to meet out_in = 100
             SrvNeededCP = round(Hrs * CP , digits = 1), # how many direct hours needed to meet CP at target
             CPNeeded = round(SrvNeeded / Hrs, digits = 1),
             prod_pct = round(Srv / SrvNeeded * 100, 
                              digits = 2),
             SrvtoCPGoal = round(SrvNeededCP - Srv, digits = 1),
             SrvtoGoal = round(SrvNeeded - Srv, digits = 1)) %>%
      select(ProviderNum,staffname,title,Week_Dt,
             Sch,Hrs,Srv,Adm,CP,Prt,Pay,Fri,Ovr,Exp,Unt,Rvu,
             prod_raw,prod_pct,valu_cst,
             RvuPerSrv,SrvNeeded,SrvNeededCP,CPNeeded,SrvtoCPGoal,SrvtoGoal) %>%
      filter(Pay > 0 # Provider has pay rate > $0
             & Srv <= Hrs) # Provider has more direct hrs than hrs paid 

# Write to file for app
  write.csv(per_week, "data/prod_per_week.csv")    
    
# Remove non-essential dataframes from source output
  rm(open404)
  rm(rvu)
  rm(sub_rvu)
  #rm(services)
  rm(paidhrs_month)
  rm(paidhrs_week)
  #rm(paidhrs)
  rm(weird)
  rm(weird2)
#Remove function
  rm(combinePaidHrs2)

# VPUbase = Srv / (Exp / (Ht * CP))

# CALCULATE

# Formula
  # Re = Cost for Staff (Hourly Rate * Hours Paid) = $9,000
  # CP = Clinical Percent = 62.5%
  # Ht = Total Work Hours per Month = 160 (for 1.0 FTE)
  # Ra = Actual Revenue (Relative Value) for Service = $100
  # Then the calculation is:
  # VPUbase = Ra / (Re / (Ht * CP))
# numerator = actual production, divisor = budgeted expectation

# production is measured in terms of dollars as income in FFS arrangements.  
  # It also serves the purpose of weighting by pay, since not all "hours" are created equal. 
  # But if we assign value to a service code based on expectated revenue and a clinician 
  # makes more per service, that clinician will more quickly attain 100 without working as much
  
# Expected revenue would account 
  # Re = Ht * Service Value

# < expected time = bad
# > expected time = bad for budget
# > "reasonable hours" = potential for burnout

# Are the Hours per pay period in the employee table based on budgeted amounts 
  # that a position was hired for?  If so, would "Hours Paid" in excess of this
  # be a concern for managing productivity relative to budget
# Differentiate between site based and community based staff positions?
# Calculate need / caseload

# Incorporating various quality measures, such as the requirement of completed 
# treatment plans, directly into the productivity system via data calculations 
# enhances compliance.