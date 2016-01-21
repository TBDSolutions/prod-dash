## prod-dash ui.R ##

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

# Change class of Week_Dt var to "Date"
per_week <- per_week %>% mutate(Week_Dt = as.Date(Week_Dt))

# Make subset which filters out positions which may provide services but do not have 
# productivity expectations
expect <- 
  per_week %>% 
  filter(CP > 0) %>% # Exclude those without a current CP expectation
  droplevels()

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
         out_in = round(Rvu / Exp * 100, digits = 2))

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

current_emp <-
per_wk_emp %>%
  ungroup() %>%
  filter(Week_Dt == max(as.Date(Week_Dt))
         ) %>%
  select(staffname) %>% droplevels()

## Define UI ##

dashboardPage(
  skin = "black",
  dashboardHeader(
    title = "WMCMH Productivity"
  ),
  dashboardSidebar(
    sidebarMenu(
      menuItem("By Employee", tabName = "by_employee", icon = icon("user")),
      menuItem("By Position", tabName = "by_position", icon = icon("sitemap")),
      menuItem("By Team", tabName = "by_team", icon = icon("users")),
      menuItem("About", tabName = "about", icon = icon("file-text-o"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "by_employee",
              fluidRow(
                column(width = 4,
                  box(title = "Who are you looking for?", status = "warning",
                      width = NULL,
                      collapsible = TRUE, collapsed = TRUE,
                      selectInput("emp",
                                  label = "Pick a name",
                                  choices = levels(current_emp$staffname),
                                  selected = "")
                  ),
                  valueBoxOutput("vpu_box", width = NULL),
                  valueBoxOutput("cp_box", width = NULL)
                ),
                column(width = 4,
                  box(
                    title = "Feedback", status = "warning",
                    collapsible = TRUE, 
                    width = NULL,
                    tabBox(
                      width = NULL,
                      tabPanel("Personal Feedback", 
                               br(),
                               h4("Last week..."),
                               br(),
                               em(paste0("(the week of ", max(expect$Week_Dt), ")")),
                               br(),
                               h3(textOutput("vpu_emp")),
                               h3(textOutput("need_emp")),
                               br(),
                               h4(textOutput("change_emp")),
                               br(),
                               em("* Assuming that you continue to work the same 
                                  total number of hours and provide a similar 
                                  mix of services.")
                      ),
                      tabPanel("Notes", 
                               br(),
                               p("The best way that you can improve the value 
                                 measure is by increasing the percentage of time 
                                 spent providing direct services"),
                               p("Personal feedback is provided for each clinician,
                                 based on the hours for which they were paid.  
                                 Targets vary by job title and reflect productivity 
                                 assumptions for weeks during which the person 
                                 works 40 hours.  During weeks when a given clinician 
                                 takes PTO, it is understood that productivity may 
                                 not meet targets, and the targets here reflect that 
                                 assumption."),  
                               p("For further details on measures, please see", 
                                 em("About | Definitions | Measures"))
                      )
                    )
                  )
                ),
                column(width = 4,
                  box(title = "Getting Better?", status = "warning",
                      collapsible = TRUE,
                      width = NULL,
                      tabBox(
                        width = NULL, 
                        tabPanel("Chart", 
                                 dygraphOutput("line_emp")
                        ),
                        tabPanel("About", 
                                 br(),
                                 p("Am I improving?  How much does my performance change 
                                  from week to week?  Check out this chart  
                                  to see your individual scores.  You can hover 
                                   over the chart and you can see your direct 
                                   service percentage and VPU for each week."),
                                 p("For further details on measures, please see", 
                                   em("About | Definitions | Measures"))
                        )
                      )
                  )
                )
              )
      ) ,
      tabItem(tabName = "by_position",
              fluidRow(
                box(title = "Which Position?", status = "warning",
                    collapsible = TRUE, collapsed = TRUE,
                    p("Please select the position title that you want to analyze. 
                      The data on this page will update based on your selection."),
                    selectInput("var", 
                                label = "Choose a Position Title",
                                choices = levels(expect$title),
                                selected = levels(expect$title)[1]
                    ),
                    selectInput("week", 
                                label = "Select week of ...",
                                choices = as.character(unique(expect$Week_Dt)),
                                selected = as.character(max(expect$Week_Dt))
                    )
                ),
                box(title = "How Far From Goal?", status = "warning",
                    collapsible = TRUE, collapsed = TRUE,
                    tabBox(
                      width = NULL,
                      tabPanel("Table", 
                               dataTableOutput("table")),
                      tabPanel("Hours to Go", 
                               dimpleOutput("horizbar")),
                      tabPanel("About", 
                               br(),
                               h4("Table"),
                               p("The table here shows the number of direct service 
                                  hours required by each employee in order to meet 
                                  the value goal (VPU) of 100. Calculations
                                  are based on the average number of weeks and 
                                  business days per month in 2015."),
                               h4("Hours to Go"),
                               p("The chart here shows the number of direct service 
                                  hours needed to meet the value goal (VPU). It is 
                                 filtered by the week selected above.  Positive 
                                 numbers indicate direct hours worked above goal"),
                               p("For further details on measures, please see", 
                                 em("About | Definitions | Measures"))
                      )
                    )
                )
              ),
              fluidRow(
                box(title = "Compare", status = "warning",
                    collapsible = TRUE, 
                    tabBox(
                      width = NULL,
                      tabPanel("By position", 
                               dimpleOutput("vertbar_by_pos")),
                      tabPanel("By employee", 
                               dimpleOutput("vertbar")),
                      tabPanel("About", 
                               br(),
                               p("In the chart above, the y-axis has been fixed at 
                                    100 in order to consistently display performance 
                                    compared to goal.  It is filtered for the week 
                                    selected above.  You can see the actual score for 
                                    clinicians over 100 by hovering your cursor over 
                                    the Value (VPU) bar.")
                      )
                    )
                ),
                box(title = "Getting Better?", status = "warning",
                    collapsible = TRUE,
                    tabBox(
                      width = NULL,
                      tabPanel("Chart", 
                               dygraphOutput("line_pos")),
                      tabPanel("About", 
                               br(),
                               p("Is this position becoming more efficient?  How 
                                 much do we fluctuate from week to week?  Use this 
                                 chart to see cumulative weekly scores per position type."),
                               p("For further details on measures, please see", 
                                 em("About | Definitions | Measures"))
                      )
                    )
                )
              )
      ),
      tabItem(tabName = "by_team",
              fluidRow(
                box(title = "Filters", status = "warning",
                    collapsible = TRUE, collapsed = TRUE,
                    p("Please select the name of the team that you want to analyze. 
                      The data in the tabs to the right will update based on your 
                      selection."),
                    selectInput("cc", 
                                label = "Choose a Team",
                                choices = levels(per_week$cc_desc),
                                selected = levels(per_week$cc_desc)[1]
                    ),
                    selectInput("week_cc", 
                                label = "Select week of ...",
                                choices = as.character(unique(expect$Week_Dt)),
                                selected = as.character(max(expect$Week_Dt))
                    )
                ),
                box(title = "What If?", status = "warning",
                    collapsible = TRUE, collapsed = TRUE,
                    p("Use the sliding bars to ask questions about how changes in 
                      a given area would affect performance on the overall value 
                      of services."),
                    br(),
                    sliderInput("slide_srv", "Direct Service Hrs:", 
                                min = 0, max = round(max(per_wk_cc$Srv)*2, digits = 1), 
                                value = mean(per_wk_cc$Srv)),
                    sliderInput("slide_pay", "Employee Pay:", 
                                min = 0, max = round(max(by_cc$Pay)*1.15, digits = 2), 
                                value = mean(per_wk_cc$Pay)),
                    sliderInput("slide_ovr", "Program Overhead Costs:", 
                                min = 0, max = round(max(per_wk_cc$Ovr, na.rm = T)*1.25, digits = 2), 
                                value = mean(per_wk_cc$Ovr, na.rm = T)),
                    br(),
                    h4(textOutput("dx_cc"))
                )
              ),
              fluidRow(
                valueBoxOutput("cc_need_ds"),
                valueBoxOutput("cc_need_val")
                
              ),
              fluidRow(
                box(title = "Compare", status = "warning",
                    collapsible = TRUE,
                    tabBox(
                      width = NULL, 
                      tabPanel("By team", 
                               dimpleOutput("vertbar_by_cc")
                      ),
                      tabPanel("By employee", 
                               dimpleOutput("vertbar_cc")
                      ),
                      tabPanel("About", 
                               br(),
                               p("In the chart above, the y-axis has been fixed at 
                                  100 in order to consistently display performance 
                                  compared to goal.  It is filtered for the week 
                                  selected above.  You can see the actual score for 
                                  clinicians over 100 by hovering your cursor over 
                                  the Value (VPU) bar."),
                               p("For further details on measures, please see", 
                                 em("About | Definitions | Measures"))
                      )
                    )
                ),
                box(title = "Getting Better?", status = "warning",
                    collapsible = TRUE,
                    tabBox(
                      width = NULL, 
                      tabPanel("Chart", 
                               dygraphOutput("line_cc")
                      ),
                      tabPanel("About", 
                               br(),
                               p("Is my team improving?  How much do we fluctuate 
                                 from week to week?  Check out the chart below 
                                 to see cumulative weekly scores for your team."),
                               p("Keep in mind that improvement on the Value (VPU) 
                                 measure reflects the combined efforts of individual 
                                 clinicians and improvements in administrative 
                                 efficiency, while an increase in the percentage 
                                 of time providing direct services tends to reflect 
                                 efficiencies gained by clinicians and other 
                                 service providers."),
                               p("For further details on measures, please see", 
                                 em("About | Definitions | Measures"))
                      )
                    )
                )
              )
      ),
      tabItem(tabName = "about",
              fluidRow(
                column(width = 6,
                  box(title = "Definitions", status = "warning",
                      collapsible = TRUE, width = NULL,
                      tabBox(width = NULL,
                             tabPanel("Overview", 
                                      br(),
                                      p("Comprehensive tracking of productivity 
                                        is critical, with demand for increased 
                                        staff productivity fueled by multiple 
                                        developments in the funding environment:",
                                        br(),
                                        em("* Declining reimbursement and lower 
                                           margins"),
                                        br(),
                                        em("* Push to assume increasing risk 
                                           through risk sharing reimbursement 
                                           arrangements"),
                                        br(),
                                        em("* Rising human resource costs require 
                                           careful monitoring of investments in 
                                           people's time"),
                                        br(),
                                        em("* Shifts in future funding will lead 
                                           to competition for consumers based on 
                                           efficiency and value")
                                        ),
                                      br(),
                                      p("Productivity measurement asks how well an 
                                        organization can coordinate separate resources 
                                        (e.g. people, machines, paperwork) into useful 
                                        goods or services. It's a relationship between 
                                        inputs and outputs, or Units of Output / Units 
                                        of Input."),
                                      br(),
                                      p("Check out the", em("Measures"), "tab to 
                                        learn how productivity is calculated using 
                                        this model and the ", em("References"), 
                                        "tab to find links to related research and 
                                        documentation.")
                                      ),
                             tabPanel("Measures", 
                                      br(),
                                      p("Below you'll find definitions of the 
                                        measures and terms used in this model of 
                                        productivity measurement."),
                                      br(),
                                      h4("Value Per Unit (VPU)"),
                                      p("The goal is fairly simple (in concept, at 
                                        least): don't spend more to provide a service 
                                        than people are willing to spend to purchase 
                                        that service. This measure is calculated as 
                                        follows using the variables defined below:",
                                        code("VPU = Rvu / Exp")),
                                      p("The goal for everyone on the VPU measure 
                                        is 100.  A score of 100 means that you are 
                                        providing services at a value that is equal 
                                        or less than the amount you spend to provide 
                                        those services."),
                                      br(),
                                      h4("Relative Value Unit (RVU)"),
                                      p("But how do we know what the value of a 
                                        service is? The existing standard for RVUs 
                                        are in use by Medicare in the",
                                        a("Physician Fee Schedule.", 
                                          href = "https://www.cms.gov/apps/physician-fee-schedule/overview.aspx"),
                                        " But the current Physician Fee Schedule does 
                                        not price all HCPCS codes, and therefore 
                                        RVUs are only available for a subset of the 
                                        codes currently in use.  The rest were 
                                        supplemented by taking the 25 percentile 
                                        cost per unit from the most recent year of 
                                        the MDHHS Sub-Element Cost Report for the 
                                        Michigan region most closely corresponding 
                                        to the Medicare geographic areas (i.e. 
                                        excluding Detroit Wayne).  This means that 
                                        WMCMH has set a goal of having less expensive 
                                        services than 75% of other CMHSPs."),
                                      br(),
                                      h4("Expense"),
                                      p("If we really want the denominator of the 
                                        productivity equation to be ", 
                                        em("units in"), ", it needs to be broader 
                                        than just the cost of an employee's pay and 
                                        benefits.  WMCMH understands 
                                        that increasing direct service hours is a 
                                        critical component of creating value but 
                                        other factors also contribute.  The true 
                                        cost of providing services 
                                        includes benefits, capital expenses, 
                                        depreciation, a share of administrative 
                                        support, and other expenses, so these are 
                                        included in the denominator to get a true 
                                        sense of productivity.  In order to equitably 
                                        distribute these overhead costs across programs 
                                        and employees, we have applied: (a) Overhead 
                                        per FTE assumptions associated with each Cost 
                                        Center, and (b) Fringe cost assumptions 
                                        associated with each General Ledger ID")
                                      ),
                             tabPanel("References", 
                                      br(),
                                      p(a(href = "http://www.caseybennett.com/uploads/Clinical_productivity_PublishedVersion.pdf", 
                                          "Bennett, CC (2010). Clinical Productivity 
                                          System – A Decision Support Model. International 
                                          Journal of Productivity and Performance 
                                          Management. 60(3):311-319."),
                                        em("Note that the system is based completely 
                                           on open-source technology, and the design 
                                           is considered non-proprietary (p. 313)")),
                                      p(a(href = "http://www.cms.gov/Medicare/Medicare-Fee-for-Service-Payment/PhysicianFeeSched/PFS-Relative-Value-Files.html", 
                                          "Physician Fees Schedule (PFS) Relative 
                                          Value Files")),
                                      p("Data for unit costs were taken from the 
                                        Michigan Department of Health and Human 
                                        Services'",
                                        a(href = "http://www.michigan.gov/mdch/0,4612,7-132-2941_4868_4902-256889--,00.html", 
                                          "CMHSP Sub-element Cost Reports for Section 404"),
                                        ", augmented in the ",
                                        a(href = "https://github.com/j-hagedorn/open404", 
                                          "open404 format")),
                                      p(a(href = "http://www.emeraldinsight.com/doi/abs/10.1108/17410400710731446", 
                                          "Purbey, S., et al (2007) “Performance 
                                          measurement system for healthcare processes”, 
                                          International Journal of Productivity and 
                                          Performance Management, Vol. 56 No. 3, 
                                          pp. 241-51."))
                              ),
                             tabPanel("Update Process", 
                                      br(),
                                      h4("Updating RVU files"),
                                      p("The Medicare Physician Fee Schedule updates",
                                        a(href = "https://www.cms.gov/Medicare/Medicare-Fee-for-Service-Payment/PhysicianFeeSched/PFS-Relative-Value-Files.html",
                                          "files for calculating Relative Value Units"),
                                        "on a quarterly basis throughout the year.  
                                        Individuals maintaining this productivity 
                                        model should update the RVUs in the 
                                        productivity script when they are updated 
                                        by CMS."),
                                      br(),
                                      h4("Updating 404 files"),
                                      p("The Michigan Department of Health and 
                                        Human Services publishes the",
                                        a(href = "http://www.michigan.gov/mdhhs/0,5885,7-339-71550_2941_4868_4902-256889--,00.html",
                                          "CMHSP Sub-element Cost Reports for Section 404"),
                                        "annually on May 31st.  Individuals 
                                        maintaining this productivity model should 
                                        update the RVUs in the productivity script 
                                        when they are updated by CMS."),
                                      br(),
                                      h4("Updating fringe and overhead costs"),
                                      p("Calculations for overhead and fringe 
                                        percentage are updated at least annually 
                                        based on expense data.  Fringe and overhead 
                                        adjustments are applied to each employee's 
                                        expenses to calculate their toital cost 
                                        to the agency.  These adjustments must be 
                                        updated annually, or whenever major changes 
                                        in expenses occur.")
                             )
                            )
                          )
                ),
                column(width = 6,
                  box(title = "Targets", status = "warning",
                      collapsible = TRUE, width = NULL,
                      tabBox(width = NULL,
                             tabPanel("Direct Service %",
                                      dataTableOutput("cp_table")
                             ),
                             tabPanel("Value (VPU)",
                                      "The Value (VPU) formula is structured so that 
                                    all staff have the same target: 100 VPU’s per 
                                    month per clinician. A score of 100 means that 
                                    a clinician has provided services of a value 
                                    that equals 100% of their expenses.  This single 
                                    goal simplifies management, and allows for 
                                    staff doing different jobs to be directly 
                                    compared based on their value percentage."
                             ),
                             tabPanel("Value Per Unit (RVU)",
                                      "The table below displays the relative value 
                                      units (RVU) used to calculate the vlaue of 
                                      each HCPCS code for the VPU measure numerator:",
                                      br(),
                                      dataTableOutput("rvu_table")
                             )
                      )
                  )
                )
              )
      )
    )
  )
)