## server.R ##

# Load fun, data, libs, source files
library(car)
library(dplyr)
library(tidyr)
library(DT)
library(rcdimple)
library(dygraphs)
library(xts)
library(shinydashboard)

# Load Data
per_month <- read.csv("data/prod_per_month.csv")
per_week <- read.csv("data/prod_per_week.csv")
codemap <- read.csv("data/codemap.csv")

# Change class of Week_Dt var to "Date"
per_week <- per_week %>% mutate(Week_Dt = as.Date(Week_Dt))

# Make subset which filters out positions which may provide services but do not have 
# productivity expectations
expect <- 
  per_week %>% 
  filter(CP > 0) %>% # Exclude those without a current CP expectation
  droplevels()

# Summarise by cost center by month

by_cc <-
  per_month %>%
  group_by(cc_desc, Year, Month) %>%
  summarise(CP = mean(CP),
            Srv = sum(Srv),  # Hrs worked (divides total hrs by # in grp)
            Unt = sum(Unt),  # Billable units
            Pay = sum(Pay),
            Fri = sum(Fri),
            Rvu = sum(Rvu),
            Hrs = sum(Hrs),
            Ovr = sum(Ovr), # Monthly adjustment for annual overhead 
            Emp = n() # employees per CC
            ) %>%
  ungroup() %>%
  mutate(Adm = Hrs - Srv, #Admin hours = Total paid hrs - direct service hrs
         Exp = Pay + Fri + Ovr,
         prod_raw = round(Srv / Hrs * 100, 
                          digits = 2),
         prod_pct = round(Srv / (Hrs * CP) * 100, 
                          digits = 2),
         valu_cst = round(Rvu / (Pay + Fri + Ovr) * 100, digits = 2))


# Make summary table per CC per week
per_wk_cc <-
  per_week %>%
  group_by(cc_desc, Week_Dt) %>%
  summarise(Srv = sum(Srv),  # Hrs worked (divides total hrs by # in grp)
            Unt = sum(Unt), # Billable units
            Hrs = sum(Hrs),
            Pay = sum(Pay),
            Ovr = sum(Ovr, na.rm = T), 
            Fri = sum(Fri, na.rm = T),
            Rvu = sum(Rvu),
            CP = max(CP, na.rm = T),
            Emp = n(), # employees per CC
            Exp = round(Pay + Fri + Ovr, digits = 2), # Budgeted expectation
            prod_raw = round(Srv / Hrs * 100, digits = 2),
            valu_cst = round(Rvu / Exp * 100, digits = 2)
            ) 

# Make summary table per CC per week
per_wk_pos <-
  per_week %>%
  group_by(title, Week_Dt) %>%
  summarise(Srv = sum(Srv),  # Hrs worked (divides total hrs by # in grp)
            Unt = sum(Unt), # Billable units
            Hrs = sum(Hrs),
            Pay = sum(Pay),
            Ovr = sum(Ovr, na.rm = T), 
            Fri = sum(Fri, na.rm = T),
            Rvu = sum(Rvu),
            CP = max(CP, na.rm = T),
            Exp = round(Pay + Fri + Ovr, digits = 2), # Budgeted expectation
            prod_raw = round(Srv / Hrs * 100, digits = 2),
            valu_cst = round(Rvu / Exp * 100, digits = 2)
  )

# Make summary table per CC per week
per_wk_emp <-
  per_week %>%
  group_by(staffname, Week_Dt) %>%
  summarise(Srv = sum(Srv),  # Hrs worked (divides total hrs by # in grp)
            Unt = sum(Unt), # Billable units
            Hrs = sum(Hrs),
            Pay = sum(Pay),
            Ovr = sum(Ovr, na.rm = T), 
            Fri = sum(Fri, na.rm = T),
            Rvu = sum(Rvu),
            CP = max(CP, na.rm = T),
            Exp = round(Pay + Fri + Ovr, digits = 2), # Budgeted expectation
            prod_raw = round(Srv / Hrs * 100, digits = 2),
            valu_cst = round(Rvu / Exp * 100, digits = 2)
  )

# Make server structure

shinyServer(
  function(input, output) {
    
    output$horizbar <- renderDimple({
      
      min_mo <- min(expect$SrvtoGoal)
      max_mo <- max(expect$SrvtoGoal)
      
      expect %>%
        filter(title == input$var
               & Week_Dt == input$week) %>% 
        dimple(staffname ~ SrvtoGoal,
               type = "bar", 
               width = 600, height = 800
        ) %>%
        set_bounds(x = "25%", y = "5%",
                   width = "70%", height = "80%") %>% 
        xAxis(type = "addMeasureAxis",
              title = "Hours of Direct Service to Meet Goal", 
              fontSize = "80%",
              overrideMin = min_mo,
              overrideMax = max_mo) %>%
        yAxis(type = "addCategoryAxis", 
              orderRule = "staffname",
              title = "Employee Name",
              fontSize = "80%") %>%
        default_colors(c("#78B7C5","#F21A00")) %>%
        add_title( html = "<h4>Hours to Meet Value (VPU) Target, by Employee</h4>" )
    })
    
    output$table <- renderDataTable({
      expect %>%
        mutate(SrvPerWk = round(SrvNeeded / 4.34812, digits = 1), # based on avg wks per month
               SrvPerDay = round(SrvNeeded / 20.91667, digits = 1)) %>% 
        filter(title == input$var) %>%
        ungroup %>%
        select(staffname, Week_Dt, SrvNeeded, SrvPerWk, SrvPerDay) %>%
        datatable(filter = 'top', rownames = FALSE, 
                  colnames = c('Name', 'Week of', 'Direct Hrs per Month', 
                               'Direct Hrs per Week', 'Direct Hrs per Day'),
                  options = list(columnDefs = list(list(className = 'dt-center')),
                                 pageLength = 5, lengthMenu = c(5, 10, 25)))
    })
    
    output$vertbar_by_pos <- renderDimple({
      # Ensure fixed x-axis indepencent of time period selected
      # max_vpu <- max(per_month$valu_cst)
      
      expect %>% 
        ungroup %>% 
        select(title,Week_Dt,
               prod_raw,prod_pct,valu_cst) %>%
        droplevels %>%
        gather(Measure, Value, prod_raw,valu_cst) %>%
        arrange(title,Week_Dt,Measure) %>%
        mutate(Measure = car::recode(Measure, "'prod_raw' = 'Direct as % Total Hrs';
                                          'valu_cst' = 'Value (VPU)'")) %>%
        filter(Week_Dt == input$week) %>% 
        dimple(x = c("title","Measure"),
               y = "Value", groups = "Measure",
               type = "bar", 
               width = 600, height = 600
        ) %>%
        set_bounds(x = "10%", y = "5%",
                   width = "65%", height = "70%") %>%
        xAxis(type = "addCategoryAxis",
              title = "Position / Employee", 
              fontSize = "80%") %>%
        yAxis(type = "addMeasureAxis", 
              orderRule = "prod_vpu",
              title = "Measure Value",
              fontSize = "80%",
              overrideMax = 100) %>%
        add_legend( x = "20%", y = "1%", # dist from sides
                    width = "100%", height = "5%", # dim of legend
                    horizontalAlign = "left", fontSize = "80%") %>%
        default_colors(c("#46ACC8", "#E58601"))
    })
      
    output$vertbar <- renderDimple({
      # Ensure fixed x-axis indepencent of time period selected
      # max_vpu <- max(per_month$valu_cst)
      
      expect %>% 
        ungroup %>% 
        select(staffname,title,Week_Dt,
               prod_raw,prod_pct,valu_cst) %>%
        droplevels %>%
        gather(Measure, Value, prod_raw,valu_cst) %>%
        arrange(staffname,Week_Dt,Measure) %>%
        mutate(Measure = car::recode(Measure, "'prod_raw' = 'Direct as % Total Hrs';
                                          'valu_cst' = 'Value (VPU)'")) %>%
        filter(title == input$var
               & Week_Dt == input$week) %>% 
        dimple(x = c("staffname","Measure"),
               y = "Value", groups = "Measure",
               type = "bar", 
               width = 600, height = 600
               ) %>%
        set_bounds(x = "10%", y = "5%",
                   width = "65%", height = "70%") %>%
        xAxis(type = "addCategoryAxis",
              title = "Position / Employee", 
              fontSize = "80%") %>%
        yAxis(type = "addMeasureAxis", 
              orderRule = "prod_vpu",
              title = "Measure Value",
              fontSize = "80%",
              overrideMax = 100) %>%
        add_legend( x = "20%", y = "1%", # dist from sides
                    width = "100%", height = "5%", # dim of legend
                    horizontalAlign = "left", fontSize = "80%") %>%
        default_colors(c("#46ACC8", "#E58601"))  
    })

    output$dx_cc <- renderText({
      all_cc <-
        per_wk_cc %>% 
        filter(cc_desc == input$cc) %>% 
        summarise(CP = mean(CP),
                  Srv = sum(Srv),  # Hrs worked (divides total hrs by # in grp)
                  Unt = sum(Unt),  # Billable units
                  Pay = sum(Pay),
                  Fri = sum(Fri),
                  Rvu = sum(Rvu),
                  Hrs = sum(Hrs),
                  Ovr = sum(Ovr), # Monthly adjustment for annual overhead 
                  Emp = mean(Emp)
        ) %>%
        mutate(Adm = Hrs - Srv, #Admin hours = Total paid hrs - direct service hrs
               Exp = Pay + Fri + Ovr,
               prod_raw = round(Srv / Hrs * 100, digits = 2),
               prod_pct = round(Srv / (Hrs * CP) * 100, digits = 2),
               valu_cst = round(Rvu / (Pay + Fri + Ovr) * 100, digits = 2))
      
      paste("The actual ratio of value to cost for this program is: ",
            round(all_cc$Rvu 
                  / (all_cc$Pay 
                     + all_cc$Fri 
                     + all_cc$Ovr) * 100, 
                  digits = 1),
            "The changes you've selected would result in a ratio of: ",
            round(((all_cc$Rvu/all_cc$Srv) * input$slide_srv)
                  / (input$slide_pay 
                     + (input$slide_pay * all_cc$Fri/all_cc$Pay) 
                     + input$slide_ovr) * 100,
                  digits = 1) 
      ) 
      
    })
# 
#     output$table_cc <- renderDataTable({
#       by_cc %>%
#         filter(cc_desc == input$cc) %>%
#         ungroup %>%
#         select(Month, prod_raw, valu_cst) %>%
#         datatable(rownames = FALSE, 
#                   colnames = c('Month', 'Direct as % Total Hrs', 'Value (VPU)'),
#                   options = list(columnDefs = list(list(className = 'dt-center')),
#                                  pageLength = 5, lengthMenu = c(5, 10)))
#     })
#     
    
    output$line_emp <- renderDygraph({
      
      per_wk_emp_filt <- per_wk_emp %>% filter(staffname == input$emp) %>% droplevels()
      
            # Make xts object
      
      per_wk_xts <- as.xts(per_wk_emp_filt$prod_raw, 
                           order.by = as.POSIXct(per_wk_emp_filt$Week_Dt))
      
      names(per_wk_xts)[1]<-"prod_raw"
      per_wk_xts$valu_cst <- per_wk_emp_filt$valu_cst
      
      per_wk_xts %>%
        dygraph(main = "Productivity: Weekly Trend") %>%
        dySeries("prod_raw", label = "% direct service") %>%
        dySeries("valu_cst", label = "Value (VPU)") %>%
        dyAxis("y", valueRange = c(0, 120)) %>%
        dyOptions(colors = c("#46ACC8", "#E58601")) %>%
        dyLimit(round(max(per_wk_emp_filt$CP) * 100, digits = 1),
                "% direct service target", color = "#46ACC8") %>%
        dyLimit(100,"Value (VPU) target", color = "#E58601") %>%
        dyHighlight(highlightCircleSize = 5, 
                    highlightSeriesBackgroundAlpha = 0.2,
                    hideOnMouseOut = TRUE,
                    highlightSeriesOpts = list(strokeWidth = 3))
      
    })
    
    output$line_cc <- renderDygraph({
      
      per_wk_cc_filt <- per_wk_cc %>% filter(cc_desc == input$cc) %>% droplevels()
      
      # Make xts object
      
      per_wk_xts <- as.xts(per_wk_cc_filt$prod_raw, 
                           order.by = as.POSIXct(per_wk_cc_filt$Week_Dt))
      
      names(per_wk_xts)[1]<-"prod_raw"
      per_wk_xts$valu_cst <- per_wk_cc_filt$valu_cst

      per_wk_xts %>%
        dygraph(main = "Productivity: Weekly Trend") %>%
        dySeries("prod_raw", label = "% direct service") %>%
        dySeries("valu_cst", label = "Value (VPU)") %>%
        dyAxis("y", valueRange = c(0, 120)) %>%
        dyOptions(colors = c("#46ACC8", "#E58601")) %>%
        dyLimit(round(median(per_wk_cc_filt$CP) * 100, digits = 1),
                "% direct service target", color = "#46ACC8") %>%
        dyLimit(100,"Value (VPU) target", color = "#E58601") %>%
        dyHighlight(highlightCircleSize = 5, 
                    highlightSeriesBackgroundAlpha = 0.2,
                    hideOnMouseOut = TRUE,
                    highlightSeriesOpts = list(strokeWidth = 3))

    })
    
    output$line_pos <- renderDygraph({
      
      per_wk_pos_filt <- per_wk_pos %>% filter(title == input$var) %>% droplevels()
      
      # Make xts object
      
      per_wk_xts <- as.xts(per_wk_pos_filt$prod_raw, 
                           order.by = as.POSIXct(per_wk_pos_filt$Week_Dt))
      
      names(per_wk_xts)[1]<-"prod_raw"
      per_wk_xts$valu_cst <- per_wk_pos_filt$valu_cst
      
      per_wk_xts %>%
        dygraph(main = "Productivity: Weekly Trend") %>%
        dySeries("prod_raw", label = "% direct service") %>%
        dySeries("valu_cst", label = "Value (VPU)") %>%
        dyAxis("y", valueRange = c(0, 120)) %>%
        dyOptions(colors = c("#46ACC8", "#E58601")) %>%
        dyLimit(round(max(per_wk_pos_filt$CP) * 100, digits = 1),
                "% direct service target", color = "#46ACC8") %>%
        dyLimit(100,"Value (VPU) target", color = "#E58601") %>%
        dyHighlight(highlightCircleSize = 5, 
                    highlightSeriesBackgroundAlpha = 0.2,
                    hideOnMouseOut = TRUE,
                    highlightSeriesOpts = list(strokeWidth = 3))
      
    })
    
    output$vertbar_by_cc <- renderDimple({
      per_week %>% 
        ungroup %>% 
        select(cc_desc,Week_Dt,
               prod_raw,valu_cst) %>%
        droplevels %>%
        gather(Measure, Value, prod_raw, valu_cst) %>%
        arrange(cc_desc,Week_Dt,Measure) %>%
        mutate(Measure = car::recode(Measure, "'prod_raw' = 'Direct as % Total Hrs';
                                'valu_cst' = 'Value (VPU)'")) %>%
        filter(Week_Dt == input$week_cc) %>% 
        dimple(x = c("cc_desc","Measure"),
               y = "Value", groups = "Measure",
               type = "bar", 
               width = 600, height = 600
        ) %>%
        set_bounds(x = "10%", y = "5%",
                   width = "65%", height = "70%") %>%
        xAxis(type = "addCategoryAxis",
              title = "Team", 
              fontSize = "80%") %>%
        yAxis(type = "addMeasureAxis", 
              orderRule = "valu_cst",
              title = "Measure Value",
              fontSize = "80%",
              overrideMax = 100) %>%
        add_legend( x = "20%", y = "1%", # dist from sides
                    width = "100%", height = "5%", # dim of legend
                    horizontalAlign = "left", fontSize = "80%") %>%
        default_colors(c("#46ACC8", "#E58601"))
    })
    
    output$vertbar_cc <- renderDimple({
      per_week %>% 
        ungroup %>% 
        select(staffname,cc_desc,Week_Dt,
               prod_raw,valu_cst) %>%
        droplevels %>%
        gather(Measure, Value, prod_raw, valu_cst) %>%
        arrange(staffname,Week_Dt,Measure) %>%
        mutate(Measure = car::recode(Measure, "'prod_raw' = 'Direct as % Total Hrs';
                                'valu_cst' = 'Value (VPU)'")) %>%
        filter(cc_desc == input$cc
               & Week_Dt == input$week_cc) %>% 
        dimple(x = c("staffname","Measure"),
               y = "Value", groups = "Measure",
               type = "bar", 
               width = 600, height = 600
        ) %>%
        set_bounds(x = "10%", y = "5%",
                   width = "65%", height = "70%") %>%
        xAxis(type = "addCategoryAxis",
              title = "Employee", 
              fontSize = "80%") %>%
        yAxis(type = "addMeasureAxis", 
              orderRule = "valu_cst",
              title = "Measure Value",
              fontSize = "80%",
              overrideMax = 100) %>%
        add_legend( x = "20%", y = "1%", # dist from sides
                    width = "100%", height = "5%", # dim of legend
                    horizontalAlign = "left", fontSize = "80%") %>%
        default_colors(c("#46ACC8", "#E58601")) 
    })
    
    output$vpu_box <- renderValueBox({
      
      vpu <-
      expect %>% ungroup() %>% 
        filter(Week_Dt == max(Week_Dt) 
               & staffname == input$emp) %>%
        select(valu_cst)
      
      valueBox(
        paste0(as.character(vpu),"%"), 
        "Value (VPU)", 
        icon = icon(ifelse(vpu >= 100,
                      yes = "thumbs-up",
                      no = "thumbs-down")),
        color = ifelse(vpu >= 100,
                       yes = "green",
                       no = "red")
      )
    })
    
    output$cp_box <- renderValueBox({
      
      ds <-
        expect %>% ungroup() %>% 
        filter(Week_Dt == max(Week_Dt) 
               & staffname == input$emp) %>%
        select(prod_raw)
      
      cp <-
        expect %>% ungroup() %>% 
        filter(Week_Dt == max(Week_Dt) 
               & staffname == input$emp) %>%
        select(CP)
      
      valueBox(
        paste0(as.character(ds),"%"), 
        "Direct Service", 
        icon = icon(ifelse(ds >= cp*100,
                           yes = "thumbs-up",
                           no = "thumbs-down")),
        color = ifelse(ds >= cp*100,
                       yes = "green",
                       no = "red")
      )
    })
    
    output$cc_need_ds <- renderValueBox({
      
      cc_need_ds <-
      expect %>%
        group_by(Week_Dt, cc_desc) %>%
        summarize(SrvtoCPGoal = sum(SrvtoCPGoal)) %>%
        filter(cc_desc == input$cc
               & Week_Dt == input$week_cc)
      
      valueBox(
        paste0(as.character(cc_need_ds$SrvtoCPGoal)," hrs"), 
        "To Direct Service Target", 
        icon = icon("clock-o"),
        color = ifelse(cc_need_ds$SrvtoCPGoal <= 0,
                       yes = "green",
                       no = "red")
      )
      
    })
    
    output$cc_need_val <- renderValueBox({
      
      cc_need_val <-
        expect %>%
        group_by(Week_Dt, cc_desc) %>%
        summarize(SrvtoGoal = sum(SrvtoGoal)) %>%
        filter(cc_desc == input$cc
               & Week_Dt == input$week_cc)
      
      valueBox(
        paste0(as.character(cc_need_val$SrvtoGoal)," hrs"), 
        "To Value Target", 
        icon = icon("clock-o"),
        color = ifelse(cc_need_val$SrvtoGoal <= 0,
                       yes = "green",
                       no = "red")
      )
      
    })
    
    output$vpu_emp <- renderText({
      paste("Your value score was ",
            as.character(
              expect %>% ungroup() %>%
               filter(Week_Dt == max(Week_Dt) 
                      & staffname == input$emp) %>%
               select(valu_cst)), 
            " and you spent ",
            as.character(
              expect %>% ungroup() %>%
                filter(Week_Dt == max(Week_Dt) 
                       & staffname == input$emp) %>%
                select(prod_raw)),
            "% of your time providing direct services.")
      # the value of the services that you provided was x % of your cost to the organization
    })
    
    output$need_emp <- renderText({
      srv_remain <-
      expect %>% ungroup() %>%
        filter(Week_Dt == max(as.Date(Week_Dt)) 
               & staffname == input$emp) %>%
        select(SrvtoGoal)
      
      ds <-
      expect %>% ungroup() %>%
        filter(Week_Dt == max(as.Date(Week_Dt)) 
               & staffname == input$emp) %>%
        select(prod_raw)
      
      CP <-
        expect %>% ungroup() %>%
        filter(Week_Dt == max(as.Date(Week_Dt)) 
               & staffname == input$emp) %>%
        mutate(CP = CP * 100) %>%
        select(CP)

      if (srv_remain <= 0 & ds >= CP) {
        paste0("Awesome! You met your value goal and provided ",
               abs(srv_remain), " additional direct service hours!")
      } else if (srv_remain > 0 & ds >= CP) {
        paste0("You may not have met your value goal, but you've been working hard. ",
               ds, "% of time providing direct service!")
        # Get rid of prod in intro for
      } else if (srv_remain <= 0 & ds < CP) {
        paste0("You may have met your value goal, but you're only spending ",
               ds, "% of your time providing direct service.")
      } else if (srv_remain > 0 & ds < CP) {
        paste0("You have not met your value goal and are only spending ",
               ds, "% of your time providing direct service.")
      } else
        paste0("Unexpected combination of Productivity and Clinical Percentage")
      
    })
    
    output$change_emp <- renderText({
      
      srv_remain <-
        expect %>% ungroup() %>%
        filter(Week_Dt == max(as.Date(Week_Dt)) 
               & staffname == input$emp) %>%
        select(SrvtoGoal)
      
      ds <-
        expect %>% ungroup() %>%
        filter(Week_Dt == max(as.Date(Week_Dt)) 
               & staffname == input$emp) %>%
        select(prod_raw)
      
      CP <-
        expect %>% ungroup() %>%
        filter(Week_Dt == max(as.Date(Week_Dt)) 
               & staffname == input$emp) %>%
        mutate(CP = CP * 100) %>%
        select(CP)
      
      srv_needed <-
        expect %>% ungroup() %>%
        filter(Week_Dt == max(as.Date(Week_Dt)) 
               & staffname == input$emp) %>%
        mutate(SrvPerMo = round(SrvNeeded * 4.34524, digits = 1), # based on avg wks per month
               SrvPerDay = round(SrvNeeded / 5, digits = 1),
               AddSrvDay = round((SrvNeeded - Srv) / 20.91667, digits = 1),
               MinPerWk = round((CP * Hrs), digits = 1),
               MinPerDay = round((CP * Hrs) / 5, digits = 1)
               )
      
      if (srv_remain <= 0 & ds >= CP) {
        paste0("To stay on your streak, continue to average ",
               srv_needed$SrvPerDay, " direct service hours per day. ",
               "This adds up to ",srv_needed$SrvNeeded," per week, and ",
               srv_needed$SrvPerMo," per month, and requires that ",
               round(srv_needed$dsNeeded * 100, digits = 1),
               "% of the hours you work be spent in direct service. ",
               "In the most recent month, you spent ",srv_needed$prod_raw,
               "% of total hours in direct service.")
      } else if (srv_remain > 0 & ds >= CP) {
        paste0("Keep up the good work!")
        # direct services hours per day/week/month
      } else if (srv_remain <= 0 & ds < CP) {
        paste0("In order to meet minimum standards, you will need to provide at least ",
               srv_needed$MinPerDay, " direct service hours per day. ",
               "This adds up to ",srv_needed$MinPerWk," per week, and ",
               round(srv_needed$MinPerWk * 4.34524, digits = 1)," per month.")
      } else if (srv_remain > 0 & ds < CP) {
        paste0("In order to meet minimum standards, you will need to provide at least ",
               srv_needed$MinPerDay, " direct service hours per day. ",
               "This adds up to ",srv_needed$MinPerWk," per week, and ",
               round(srv_needed$MinPerWk * 4.34524, digits = 1)," per month.")
      } else
        paste0("Unexpected combination of Productivity and Clinical Percentage")  
      
    })
    
    output$cp_table <- renderDataTable({
      
      per_week %>%
        group_by(title,cc_desc) %>%
        summarize(CP = round(max(CP)*100), digits = 1) %>%
        filter(CP > 0) %>%
        select(title:CP) %>%
        datatable(filter = 'top', rownames = FALSE, 
                  colnames = c('Position', 'Team', 'Min CP Target'),
                  options = list(columnDefs = list(list(className = 'dt-center')),
                                 pageLength = 5, lengthMenu = c(5, 10, 25)))
      
    })
    
    output$rvu_table <- renderDataTable({
      
      codemap %>%
        select(ServiceCode,HCPCS,ServiceDesc,rvu) %>%
        datatable(filter = 'top', rownames = FALSE, 
                  colnames = c('WMCMH Code','HCPCS Code','Service','RVU'),
                  options = list(columnDefs = list(list(className = 'dt-center')),
                                 pageLength = 5, lengthMenu = c(5, 25, 50)))
      
    })

  }
)