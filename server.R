# Click the small lights next to the publish button (top right of this code window) to navigate by headers

## Define a server for the Shiny app
function(input, output, session) {
  options(shiny.port = 1221) #Used for local testing, unused in shinyapps.
  useShinyjs()
  extendShinyjs(text = jscode, functions = c("closeWindow"))
  
  ###############################################.
  ## Server - UI Body  ----
  ###############################################.
  output$uibody <- renderUI({
    
    ###############################################.
    ## UI - Header NavBar Page ---- 
    ###############################################.
    navbarPage(title = "TargetCalc",
               theme = shinytheme("cerulean"),
               
               ###############################################.
               ## UI - Home ----
               ###############################################.           
               tabPanel("Home",
                        isolate({Header_Details}),
                        p(strong("How to use this calulator"), align="left", style="font-size:30px"),
                        p("1. Start by selecting the 'Add Location' tab where you provide a zip code and electricity rate in $/kWh.", align="left", style="font-size:30px"),
                        p("2. Then you can add a Greenhouse Design. Note: this information is discarded once you exit the calculator.", align="left", style="font-size:30px"),
                        p("3. Next, calculate your Days On Target by selecting a design for your location. ", align="left", style="font-size:30px"),
                        p("4. Voila! You've calculated your lighting costs!", align="left", style="font-size:30px"),
                        isolate({Footer_Details})
               ),#bracket Home tab panel 
               
               ###############################################.
               # UI - Locations - Add New ----
               ###############################################. 
               tabPanel("Add Location",
                        Header_Details,
                        sidebarPanel(titlePanel("Enter Greenhouse Location"), 
                                     div(title="Enter your Location Name Here",
                                         textInput(inputId ="New_Loc_Name",
                                                   label = shiny::HTML("<p>Location Name <br/> <span style='font-weight: 400'>(Enter a unique identifier for your Location)</span></p>"), width = '400px')),
                                     div(title="Enter the Zipcode Here",
                                         textInput(inputId ="New_Loc_Zip",
                                                   label = shiny::HTML("<p>Zip Code <br/> <span style='font-weight: 400'>(5 digits)</span></p>"), width = '125px')),
                                     div(title="Enter an Electricity Rate in $/kWh",
                                         textInput(inputId ="New_Loc_Elec_Rate",
                                                   label = p("Electricity Rate ($/kWh)",
                                                             bsButton("b2", label = "", icon = icon("question"), style = "info", size = "extra-small"), style = "width:200px")),
                                         bsPopover(id = "b2", title="", content = "Typically, between $0.05 and $0.30. Only use numbers with a decimal. Do NOT include a dollar sign ($)", placement="right", trigger="hover",options = list(container = "body"))),
                                     actionButton("Location_save", "Save")
                        ), #bracket sidebar Panel
                        p(strong("Enter Greenhouse Location"), align="left", style="font-size:30px"),
                        p("1. Add your location and electricity price ($/kWh).", align="left", style="font-size:30px"),
                        p("2. Do not add the dollar sign ($) in the electricity rate box.", align="left", style="font-size:30px"),
                        p("3. ", em("Not sure what your electricity rate is? Reference your most recent power bill for more information."), align="left", style="font-size:30px"),
                        Footer_Details
               ),#bracket Add New tab panel
               
               ###############################################.
               # UI - Greenhouse Designs - Add New ----
               ###############################################.
               tabPanel("Add Greenhouse Design",
                        Header_Details,
                        sidebarPanel(
                          titlePanel("Enter Greenhouse Design"),
                          p(strong("Update Gear Icon First"), align="center", style="font-size:20px"),
                          fluidRow(column(7,
                                          div(title="Enter your Design Name Here",
                                              textInput(inputId ="New_GHD_Name",
                                                        label = shiny::HTML("<p>Design Name <span style='font-weight: 400'></span></p>"), width = '300px')),
                                          div(title="Enter Greenhouse Length Here",
                                              numericInput(inputId ="New_GHD_Length",
                                                           label = shiny::HTML("<p>Length <span style='font-weight: 400'>(ft)</span></p>"),value=1,min=1, width = '100px')),
                                          div(title="Enter Greenhouse Width Here",
                                              numericInput(inputId ="New_GHD_Width",
                                                           label = shiny::HTML("<p>Width <span style='font-weight: 400'>(ft)</span></p>"),value=1,min=1, width = '100px')),
                                          div(title="Enter a % Transmission Here ",
                                              textInput(inputId ="New_GHD_Trans",
                                                        label = p("Greenhouse Transmission (%) ",
                                                                  bsButton("b3", label = "", icon = icon("question"), style = "info", size = "extra-small"), style = "width:175px")),
                                              bsPopover(id = "b3", title="", content = "Typically, between 40 and 90 percent. Use numbers only.", placement="right", trigger="hover",options = list(container = "body"))),
                                          div(title="Enter a Target DLI Here (mol/m2/day)",
                                              textInput(inputId ="New_GHD_TargetDLI",
                                                        label = p("Target DLI (mol/m2/day) ",
                                                                  bsButton("b4", label = "", icon = icon("question"), style = "info", size = "extra-small"), style = "width:200px")),
                                              bsPopover(id = "b4", title="", content = "Typically, between 5 and 30 mol/m2/day. Use numbers only.", placement="right", trigger="hover",options = list(container = "body"))),
                                          div(title="Enter Lighting Efficacy Here (umol/J)",
                                              textInput(inputId ="New_GHD_Efficacy",
                                                        label = p("Lighting Efficacy (umol/J) ",
                                                                  bsButton("b5", label = "", icon = icon("question"), style = "info", size = "extra-small"), style = "width:200px")),
                                              bsPopover(id = "b5", title = "", content = "Typically, between 1.2 and 3.5 umol/J. Use numbers only.", placement="right", trigger="hover",options = list(container = "body"))),
                                          div(title="Instantaneous PPFD",
                                              textInput(inputId ="New_GHD_PPFD_capability",
                                                        label = p("Supplemental Lighting Capacity (umol/m2/s)",
                                                                  bsButton("b6", label = "", icon = icon("question"), style = "info", size = "extra-small"), style = "width:200px")),
                                              bsPopover(id = "b6", title="", content = "The maximum amount of light that can be provided by the supplemental lighting system. This is typically provided as part of the lighting plan provided by your lighting supplier. If not available, this can be measured using a PAR sensor when there is no sunlight", placement="right", trigger="hover",options = list(container = "body"))),
                                          div(title="Photoperiod",
                                              textInput(inputId ="New_GHD_hours",
                                                        label = p("Hours on",
                                                                  bsButton("b7", label = "", icon = icon("question"), style = "info", size = "extra-small"), style = "width:200px")),
                                              bsPopover(id = "b7", title="", content = "How many hours each day will you have your lights on?", placement="right", trigger="hover",options = list(container = "body"))),
                                          div(title="Monthly Demand Charge Price",
                                              textInput(inputId ="New_GHD_demand_charge_price",
                                                        label = p("Monthly Demand Charge Price ($/kW)",
                                                                  bsButton("b8", label = "", icon = icon("question"), style = "info", size = "extra-small"), style = "width:200px")),
                                              bsPopover(id = "b8", title="", content = "This charge may range from $5 to over $15/kW. Use numbers only. Commercial customers typically pay a demand charge as part of the elecricity bill, based on their peak power usage. The price for this charge is typically shown on the power bill as $/kW", placement="right", trigger="hover",options = list(container = "body")))
                                          
                          ),#bracket column 1
                          #Gear icon for customize % of lighting area by month
                          column(5, 
                                 h6("Click gear to customize lighting % of area by month"),
                                 #Percent of the area to light by month
                                 shinyWidgets::dropdownButton(
                                   tags$h3("Percent of Greenhouse Area By Month"),
                                   uiOutput('percent_mat'), 
                                   shiny::actionButton(
                                     inputId = "update_permat", 
                                     label = "Update Percentages"
                                   ),
                                   circle = TRUE, status = "primary", icon = icon("gear"), width = "450",
                                   tooltip = tooltipOptions(title = "Click here to customize lighting area by month!")
                                 ),#bracket drop down Button
                          ),#bracket column 2
                          ),#bracket fluid row
                          actionButton("GHD_save", "Save")
                        ), #bracket sidebar Panel
                        p(strong("Enter Greenhouse Design"), align="left", style="font-size:30px"),
                        p("1. Add a unique greenhouse design to calculate electricity costs for your location.", align="left", style="font-size:30px"),
                        p("2. If desired, select the gear icon to customize lighting % of area by month.", align="left", style="font-size:30px"),
                        p("3. Indicate the square footage by providing the greenhouse length and width in feet. Provide your greenhouse transmission percentage,
                          target daily light integral (DLI) and lighting efficacy.", align="left", style="font-size:30px"),
                        p("4. Indicate the supplemental lighting capacity which is the maximum that the system is able to produce, the number of hours the system will be on, and the monthly demand charge price", align="left", style="font-size:30px"),
                        p("5. Once saved, select the 'Electricity Costs' tab to see your calculated costs for the provided greenhouse design.", align="left", style="font-size:30px"),
                        p("* Note: If running multiple tests, press 'Save' after updating gear icon for accurate results.",  align="left", style="font-size:30px"),
                        Footer_Details
               ),#bracket Add New tab panel
               
               ###############################################.
               ## UI - Electricity Costs ----
               ###############################################. 
               tabPanel("Days On Target",
                        h2("Days On Target"),
                        sidebarLayout(
                          sidebarPanel(
                            uiOutput('ui_CC_location'),
                            uiOutput('ui_CC_design'),
                            actionButton(inputId = 'CC_Choices', label = 'Update'),
                            downloadButton("CC_downloadData", "Download Results"),
                            width = "2",
                            h4("Press the Update Button to view results")
                          ),#bracket sidebar panel
                          mainPanel(
                            fluidRow(
                              column(8,
                                     h4("Monthly Area Lighting %"), 
                                     tableOutput("Percent_table_CC"),
                                     h4("Grower Input"),
                                     tableOutput('CC_table')
                              ),
                              column(4,
                                     h4("Annual Lighting Cost"),
                                     tableOutput("Annual_CC_Display_table"),
                                     h4("Annual Demand Charge"),
                                     tableOutput("DSC_Display_table")
                              )
                            )
                            , fluidRow(column(6,
                                              h4("Sunlight"),
                                              plotOutput({"CC_naturalDLI"})),
                                       column(6,
                                              h4("Supplemental Light"),
                                              plotOutput({"CC_supplementalDLI"}))
                            ), fluidRow(column(3,
                                               h4("Weekly Lighting Cost"),
                                               tableOutput({"CC_df_W_table"}),
                                               DT::dataTableOutput("trace_table"),
                                               style = "height:500px; overflow-y: scroll;"),
                                        column(4),
                                        column(3,
                                               valueBoxOutput("P_days_tile", width = NULL),
                                               valueBoxOutput("N_days_tile", width = NULL),
                                               valueBoxOutput("DLI_tile", width = NULL)))
                          )
                        )),
               tabPanel(title="FAQ",
                        FAQ_Details
               ),#Bracket FAQ Page
               tabPanel(title = "Quit",
                        actionButton("close", "Click Here to End Session"))
    )#Bracket UI navbar Page
    
  }) #bracket render UI
  
  ###############################################.
  ## Server - Quit Application ---- 
  ###############################################.
  observeEvent(input$close, {
    js$closeWindow()
    stopApp()
  })
  
  ###############################################.
  # Server - Locations - Add New ----
  ###############################################. 
  Loc_values <- reactiveValues() # empty list to store reactive values for new location try
  XTRA_values <- reactiveValues() # empty list to store radiation so it can be used downstream

  # Note that we use observeEvent() here, which depends on input$Location_save (the action button), so that the output is only updated when the grower clicks the button
  observeEvent(input$Location_save, { # When action button "Location_save" is clicked run code in this section
    
    elec_test <- FALSE #Electricity rate must be valid: no $. 
    geo_test <-FALSE #Location must be found
    field_test <- FALSE #All fields must be entered
    
    # get lat and long using street and zip code
    if(nchar(input$New_Loc_Zip)!=5 & input$New_Loc_Zip!=""){
      shinyalert("Please enter a 5 digit zipcode", type="error")
      updateTextInput(session,"New_Loc_Zip", value="")
    } else{ 
      t <- try(geo(postalcode=as.character(input$New_Loc_Zip), lat = "lat", long = "long", verbose = FALSE, method="geocodio"))
      if ("try-error" %in% class(t)) {
        shinyalert("Location not found please try a different address or zip code", type="error")
        updateTextInput(session,"New_Loc_Street", value="")
      } else{ 
        geoloc <- geo(postalcode=as.character(input$New_Loc_Zip), lat = "lat", long = "long", verbose = FALSE, method="geocodio")
        if(is.logical(geoloc$lat)){ #is.logical() is used if geocascade doesn't find address, it returns N/A
          shinyalert("Location not found please try a different address or zip code", type="error")
          updateTextInput(session,"New_Loc_Street", value="")
        } 
        else{
          geo_test <- TRUE #Set geo_test to true if location is found
          Loc_values$lat <- geoloc$lat[1] #Set lat to value from geo() function
          Loc_values$long <- geoloc$long[1] #Set long to value from geo() function
        }
      }
    }
    
    #Continue checking validity of inputs
    rate_test <- as.numeric(input$New_Loc_Elec_Rate)
    if(is.na(rate_test)){
      shinyalert("Please check Electricity Rate input: No '$' needed", type="error")
      updateTextInput(session,"New_Loc_Elec_Rate", value="")
    } else{
      elec_test <- TRUE #Set to true if electricity rate is a valid number 
    }
    if(input$New_Loc_Name=="" | input$New_Loc_Zip==""| input$New_Loc_Elec_Rate==""){
      shinyalert("Please make sure all fields are filled.", type="error")
    } else{
      field_test <- TRUE #Set to true if all inputs have been entered, no blanks
    }
    
    if(elec_test==TRUE & geo_test==TRUE & field_test==TRUE){ #If all tests are true, store the values. 
      Loc_values$Name <- input$New_Loc_Name
      Loc_values$Zip <- input$New_Loc_Zip
      Loc_values$Elec_Rate <- input$New_Loc_Elec_Rate  
      shinyalert("Success! Now add your Greenhouse Design.")
    }
    
    #Store the longitude and latitude to be used in the TMY data pull below
    lon <- as.numeric(Loc_values$long)
    lat <- as.numeric(Loc_values$lat)
    
    # Pull TMY data from NSRDB
    URL <- paste0(URLbase,'&wkt=POINT(', lon, '+', lat,')')#,'&utc=', 0) # utc=0 allows for timezone, but will crash the app fairly frequently, so it's commmented out
    req <- GET(url = URL)
    NSRDB <- content(req)
    radiation <- NSRDB[-(1:2),(1:6)]
    names(radiation)<- c("Y","M","D","H","Min","ghi")
    radiation$Y <- rep(year(Sys.Date()),nrow(radiation))
    radiation$M <- sprintf("%02d", as.numeric(radiation$M))
    radiation$D <- sprintf("%02d", as.numeric(radiation$D))
    radiation$oldTimeStamp <- paste0(radiation$M, "/", radiation$D, "/", radiation$Y, " ", radiation$H,":",radiation$Min)
    radiation$TimeStamp <- as.POSIXct(strptime(radiation$oldTimeStamp,"%m/%d/%Y %H:%M"))
    radiation$Month <- as.character(paste0(radiation$M))
    radiation <- radiation %>% 
      filter(TimeStamp >= FirstMonday) #remove this if you don't want to exclude the days prior to the first Monday of the year
    radiation <- radiation[c(9,8,6)]
    
    # Store the radiation table in a reactive list to be used outside of this observe event
    XTRA_values$radiation <- radiation
  })
  
  ###############################################.
  # Server - Greenhouse Designs - Add New ----
  ###############################################.
  ## Add New Greenhouse Design ####
  GHD_values <- reactiveValues() # empty list to store reactive values for new design qry
  AP_values <- reactiveValues() # empty list to store reactive values for percent of area to light by month qry
  
  #Ui output of matrix values default to 100%
  output$percent_mat <- renderUI({
    matrixInput(
      "per_area_input",#name of matrix input
      value = m_area,#value which is the matrix
      rows = list(names = TRUE,extend = FALSE),
      cols = list(names = TRUE),
      class = "numeric"
    )
  })
  
  
  #list of 100% area if the area percent matrix is never updated (did not click button)
  AP_values$percent <- m_area
  
  observeEvent(input$update_permat, {# When action button "update_permat" is clicked run code in this section
    #make a dataframe of the input matrix
    per_m <- data.frame(input$per_area_input)
    
    #conditional outcomes of update_permat button
    #all months must have a % entered and the % must be between 0 and 100
    if(all(!is.na(per_m))){#if all month have a % entered
      if(all(per_m >= 0,per_m <=  100)){ #if all % are between 0 and 100
        #replace default reactive list with the percent area input
        for (i in 1:length(per_m)){
          AP_values$percent[[i]] <- per_m[i][[1]]
        }
        
        #Ui output of matrix - change values to default on input
        output$percent_mat <- renderUI({
          matrixInput(
            "per_area_input",#name of matrix input
            value = input$per_area_input,#value which is the matrix
            rows = list(names = TRUE,extend = FALSE),
            cols = list(names = TRUE),
            class = "numeric"
          )
        })
        shinyalert("Success!")
      } else {
        #pop up warning message
        shinyalert("Oops!", "Precent must be between 0 and 100.", type = "error")
        
        #Ui output of matrix reset to values default to 100%
        output$percent_mat <- renderUI({
          matrixInput(
            "per_area_input",#name of matrix input
            value = m_area,#value which is the matrix
            rows = list(names = TRUE,extend = FALSE),
            cols = list(names = TRUE),
            class = "numeric"
          )
        })
        #return list to 100% area for all months
        AP_values$percent <- m_area
      }
    } else {
      #pop up warning message
      shinyalert("Oops!", "Each month must have a percent area.", type = "error")
      
      #Ui output of matrix reset to values default to 100%
      output$percent_mat <- renderUI({
        matrixInput(
          "per_area_input",#name of matrix input
          value = m_area,#vaule which is the matrix
          rows = list(names = TRUE,extend = FALSE),
          cols = list(names = TRUE),
          class = "numeric"
        )
      })
      #return list to 100% area for all months
      AP_values$percent <- m_area
    }
  })
  
  
  observeEvent(input$GHD_save, { # When action button "Design_save" is clicked run code in this section
    #Test to make sure all fields are filled
    field_test <- FALSE
    
    if(input$New_GHD_Name=="" | input$New_GHD_Length=="" | input$New_GHD_Width==""| input$New_GHD_Trans=="" | input$New_GHD_TargetDLI=="" | input$New_GHD_Efficacy=="" |input$New_GHD_PPFD_capability=="" | input$New_GHD_hours=="" | input$New_GHD_demand_charge_price ==""){ 
      shinyalert("Please make sure all fields are filled.", type="error")}
    else{
      field_test <- TRUE
    }
    #Test to make sure only numbers are used for inputs
    trans_test <- as.numeric(input$New_GHD_Trans)
    DLI_test <- as.numeric(input$New_GHD_TargetDLI)
    eff_test <- as.numeric(input$New_GHD_Efficacy)
    ppfd_test <- as.numeric(input$New_GHD_PPFD_capability)
    hours_test <- as.numeric(input$New_GHD_hours)
    demand_test <- as.numeric(input$New_GHD_demand_charge_price)
    length_test <- as.numeric(input$New_GHD_Length)
    width_test <- as.numeric(input$New_GHD_Width)
    f_trans_test <- FALSE
    f_DLI_test <- FALSE
    f_Eff_test <- FALSE
    f_ppfd_test <- FALSE
    f_hours_test <- FALSE
    f_demand_test <- FALSE
    lw_test <- FALSE
    
    if(is.na(trans_test) | trans_test <= 1 | trans_test > 100){
      shinyalert("Please check Transmission percentage: no % needed, enter as 90 if 90%", type="error")
      updateTextInput(session,"New_GHD_Trans", value="")
    } else{
      f_trans_test <- TRUE
    }
    
    if(is.na(DLI_test) | DLI_test <= 0){
      shinyalert("Please check Target DLI: only positive numerical values are accepted", type="error")
      updateTextInput(session,"New_GHD_TargetDLI", value="")
    } else{
      f_DLI_test <- TRUE
    }
    
    if(is.na(eff_test) | eff_test <= 0 | eff_test > 100){
      shinyalert("Please check Lighting Efficacy: only positive numerical values between 0 and 100 are accepted", type="error")
      updateTextInput(session,"New_GHD_Efficacy", value="")
    } else{
      f_Eff_test <- TRUE
    }
    
    if(is.na(ppfd_test) | ppfd_test <= 0){
      shinyalert("Please check Supplemental Lighting Capacity: only positive numerical values are accepted", type="error")
      updateTextInput(session,"New_GHD_PPFD_capability", value="")
    } else{
      f_ppfd_test <- TRUE
    }
    
    if(is.na(hours_test) | hours_test > 24 | hours_test < 0){
      shinyalert("Please check Hours on: only positive numerical values between 0 and 24 are accepted", type="error")
      updateTextInput(session,"New_GHD_hours", value="")
    } else{
      f_hours_test <- TRUE
    }
    
    if(is.na(demand_test) | demand_test < 0 | demand_test > 100){
      shinyalert("Please check Monthly Demand Charge Price: only positive numerical values between 0 and 100 are accepted", type="error")
      updateTextInput(session,"New_GHD_demand_charge_price", value="")
    } else{
      f_demand_test <- TRUE
    }
    
    if(is.na(length_test) | is.na(width_test) | length_test <= 0 | width_test <= 0 | input$New_GHD_Length=="0" | input$New_GHD_Width=="0"){
      shinyalert("Please enter non zero values for length and width", type="error")
      updateTextInput(session,"New_GHD_Length", value="1")
      updateTextInput(session,"New_GHD_Width", value="1")
    } else{
      lw_test <- TRUE
    }
    #store input values in recative list
    if(field_test==TRUE & f_trans_test==TRUE & f_DLI_test==TRUE & f_Eff_test==TRUE & lw_test==TRUE & f_ppfd_test==TRUE & f_hours_test==TRUE & f_demand_test==TRUE){
      GHD_values$Name <- input$New_GHD_Name
      GHD_values$Area <- isolate({input$New_GHD_Length}) * isolate({input$New_GHD_Width})
      GHD_values$Trans <- input$New_GHD_Trans
      GHD_values$TargetDLI <- input$New_GHD_TargetDLI
      GHD_values$Efficacy <- input$New_GHD_Efficacy
      GHD_values$PPFD_capability <- input$New_GHD_PPFD_capability
      GHD_values$hours <- input$New_GHD_hours
      GHD_values$demand_charge_price <- input$New_GHD_demand_charge_price
      
      shinyalert(paste0("Success! Now go to the Days On Target tab to view your results. Your lighting system can provide a supplemental DLI of ", round(as.numeric(input$New_GHD_PPFD_capability) * as.numeric(input$New_GHD_hours) * 0.0036,2), " (mol/m2/d)"))
    } 
    
    #convert percent to a decimal 
    AP_values$PerOfOne <- data.frame(AP_values$percent)/100
    
    #loop to add percent area by month records to percent_area table 
    percent <- list()
    month <- list()
    for (i in 1:12){ #for each month of the year
      PerOfOne_i <- AP_values$PerOfOne[1,i] #percent of area
      MonthNum_i <- i #month number
      percent[[i]] <- PerOfOne_i
      month[[i]] <- MonthNum_i
    }
    percent1 <- unlist(percent)
    month1 <- unlist(month)
    
    
    Area <- data.frame(percent1,month1)
    names(Area) <- c("Percent", "Month")
    
    #Make copy of area table to display to user
    Area2 <- Area
    Area2$Percent2 <- label_percent()(Area$Percent) 
    
    #Transpose the area table
    t_area <- t(Area2)
    colnames(t_area) = c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")
    
    #Remove extra columns to display only month and area %
    output$Percent_table_CC <- renderTable(t_area[3,, drop=F])
    
    # Create lists to store values used in the for-loop below to manipulate the start date of the year to that of the first Monday of the year
    val <- list()
    weeksum <- list()
    weekavg <- list()
    weekStart <- c(FirstMonday)
    
    # The outer loop iterate through the weeks while the inner loop adds the days
    for(i in 1:53){
      weekStart[[1 + (i)]]<- as_date(FirstMonday + weeks(i))
      for (j in seq(0,7,1)){
        val[j] <- AP_values$PerOfOne[[(month(weekStart[i] + days(j)))]]
      }
      weeksum[[i]] <- val
      weekavg[i] <- mean(sapply(weeksum[[i]],sum))
    }
    
    #loop to drop the 53rd week if it starts in the next year
    if (year(weekStart[53]) == year(Sys.Date() + years(1))){
      weeksum <- weeksum[-53]
      weekavg <- weekavg[-53]
    }
    length(weekStart) = length(weekavg) #remove the excess weeks from the defining loop
    
    #unlist the values
    weekavg1 <- unlist(weekavg)
    week1 <- unlist(weekStart)
    
    #Manually adjust overlapping months (easy). This and next year are done. Takes 3-5 mins.
    if (lubridate::year(as.Date(Sys.Date(), format = "%Y-%m-%d")) == 2022){
      weekavg1[5] <- (1*AP_values$PerOfOne[[1]] + 6*AP_values$PerOfOne[[2]])/7
      weekavg1[9] <- (1*AP_values$PerOfOne[[2]] + 6*AP_values$PerOfOne[[3]])/7
      weekavg1[13] <- (4*AP_values$PerOfOne[[3]] + 3*AP_values$PerOfOne[[4]])/7
      weekavg1[17] <- (6*AP_values$PerOfOne[[4]] + 1*AP_values$PerOfOne[[5]])/7
      weekavg1[22] <- (2*AP_values$PerOfOne[[5]] + 5*AP_values$PerOfOne[[6]])/7
      weekavg1[26] <- (4*AP_values$PerOfOne[[6]] + 3*AP_values$PerOfOne[[7]])/7
      weekavg1[35] <- (3*AP_values$PerOfOne[[8]] + 4*AP_values$PerOfOne[[9]])/7
      weekavg1[39] <- (5*AP_values$PerOfOne[[9]] + 2*AP_values$PerOfOne[[10]])/7
      weekavg1[44] <- (1*AP_values$PerOfOne[[10]] + 6*AP_values$PerOfOne[[11]])/7
      weekavg1[48] <- (3*AP_values$PerOfOne[[11]] + 4*AP_values$PerOfOne[[12]])/7
      weekavg1[52] <- AP_values$PerOfOne[[12]] 
    } else if (lubridate::year(as.Date(Sys.Date(), format = "%Y-%m-%d")) == 2023) {
      weekavg1[5] <- (2*AP_values$PerOfOne[[1]] + 5*AP_values$PerOfOne[[2]])/7
      weekavg1[9] <- (2*AP_values$PerOfOne[[2]] + 5*AP_values$PerOfOne[[3]])/7
      weekavg1[13] <- (5*AP_values$PerOfOne[[3]] + 2*AP_values$PerOfOne[[4]])/7
      weekavg1[22] <- (3*AP_values$PerOfOne[[5]] + 4*AP_values$PerOfOne[[6]])/7
      weekavg1[26] <- (5*AP_values$PerOfOne[[6]] + 2*AP_values$PerOfOne[[7]])/7
      weekavg1[31] <- (1*AP_values$PerOfOne[[7]] + 6*AP_values$PerOfOne[[8]])/7
      weekavg1[35] <- (4*AP_values$PerOfOne[[8]] + 3*AP_values$PerOfOne[[9]])/7
      weekavg1[39] <- (6*AP_values$PerOfOne[[9]] + 1*AP_values$PerOfOne[[10]])/7
      weekavg1[44] <- (2*AP_values$PerOfOne[[10]] + 5*AP_values$PerOfOne[[11]])/7
      weekavg1[48] <- (3*AP_values$PerOfOne[[11]] + 4*AP_values$PerOfOne[[12]])/7
      weekavg1[52] <- AP_values$PerOfOne[[12]]
    } else if (lubridate::year(as.Date(Sys.Date(), format = "%Y-%m-%d")) == 2024){ #Leap year
      weekavg1[5] <- (3*AP_values$PerOfOne[[1]] + 4*AP_values$PerOfOne[[2]])/7
      weekavg1[9] <- (4*AP_values$PerOfOne[[2]] + 3*AP_values$PerOfOne[[3]])/7
      weekavg1[18] <- (2*AP_values$PerOfOne[[4]] + 5*AP_values$PerOfOne[[5]])/7
      weekavg1[22] <- (5*AP_values$PerOfOne[[5]] + 2*AP_values$PerOfOne[[6]])/7
      weekavg1[31] <- (3*AP_values$PerOfOne[[7]] + 4*AP_values$PerOfOne[[8]])/7
      weekavg1[35] <- (6*AP_values$PerOfOne[[8]] + 1*AP_values$PerOfOne[[9]])/7
      weekavg1[40] <- (1*AP_values$PerOfOne[[9]] + 6*AP_values$PerOfOne[[10]])/7
      weekavg1[44] <- (4*AP_values$PerOfOne[[10]] + 3*AP_values$PerOfOne[[11]])/7
      weekavg1[48] <- (6*AP_values$PerOfOne[[11]] + 1*AP_values$PerOfOne[[12]])/7
      weekavg1[52] <- AP_values$PerOfOne[[12]]
    } else { # for 2025 and beyond, this needs to be recalculated. Currently is the same value from 2022
      weekavg1[5] <- (1*AP_values$PerOfOne[[1]] + 6*AP_values$PerOfOne[[2]])/7
      weekavg1[9] <- (1*AP_values$PerOfOne[[2]] + 6*AP_values$PerOfOne[[3]])/7
      weekavg1[13] <- (4*AP_values$PerOfOne[[3]] + 3*AP_values$PerOfOne[[4]])/7
      weekavg1[17] <- (6*AP_values$PerOfOne[[4]] + 1*AP_values$PerOfOne[[5]])/7
      weekavg1[22] <- (2*AP_values$PerOfOne[[5]] + 5*AP_values$PerOfOne[[6]])/7
      weekavg1[26] <- (4*AP_values$PerOfOne[[6]] + 3*AP_values$PerOfOne[[7]])/7
      weekavg1[35] <- (3*AP_values$PerOfOne[[8]] + 4*AP_values$PerOfOne[[9]])/7
      weekavg1[39] <- (5*AP_values$PerOfOne[[9]] + 2*AP_values$PerOfOne[[10]])/7
      weekavg1[44] <- (1*AP_values$PerOfOne[[10]] + 6*AP_values$PerOfOne[[11]])/7
      weekavg1[48] <- (3*AP_values$PerOfOne[[11]] + 4*AP_values$PerOfOne[[12]])/7
    }
    
    Area_week <- data.frame(weekavg1,week1)
    names(Area_week) <- c("Percent", "Week")
    
    #Make copy of area table to display to user
    Area_week_2 <- Area_week
    Area_week_2$Percent2 <- label_percent()(Area_week$Percent) 
    
    #Transpose the area table
    t_area_week <- t(Area_week_2)
    colnames(t_area_week) = week1
    
    #Remove extra columns to display only month and area %
    output$table_week <- renderTable(t_area_week[3,, drop=F])
    
  })
  
  ###############################################.
  ## Server - Electricity Costs ----
  ###############################################.
  
  # Location value to display on the results page
  output$ui_CC_location <- renderUI({
    textInput(inputId = 'CC_Select_location',
              label ='Location',
              width = "auto",
              value=Loc_values$Name)
  })
  
  # Design value to display on the results page
  output$ui_CC_design <- renderUI({
    textInput(inputId = 'CC_Select_design',
              label ='Design',
              width = "auto",
              value=GHD_values$Name)
  })
  
  ####################################.
  ## Calculations and Graphs  ####
  ####################################.
  observeEvent(input$CC_Choices, { # When action button "CC_Choices" is clicked run code in this section
    
    ####################################.
    ## Variables and Dataframes ####
    ####################################.
    # Upon clicking to view results, display a success message for user feedback
    shinyalert("Success! Updating your results.")
    
    # Then run this code below
    {
      # Assign variables from user inputs
      Location <- as.character(Loc_values$Name)
      ele_rate <- as.numeric(Loc_values$Elec_Rate)
      Design <- as.character(GHD_values$Name)
      trans <- as.numeric(GHD_values$Trans)
      target <- as.numeric(GHD_values$TargetDLI)
      area <- as.numeric(GHD_values$Area)
      efficacy <- as.numeric(GHD_values$Efficacy)
      PPFD_capability <- as.numeric(GHD_values$PPFD_capability)
      hours <- as.numeric(GHD_values$hours)
      demand_charge_price <- as.numeric(GHD_values$demand_charge_price)
      
      DLI_capability <- PPFD_capability/1000000*3600*hours
      
      #Calculate the area for each month based on percent of coverage
      PA_Months <- as.numeric(AP_values$percent)/100
      monthly_areaft2 <- vector(mode="numeric", length=12)
      monthly_aream2 <- vector(mode="numeric", length=12)
      demand_charge <- vector(mode="numeric", length=12)
      
      for(i in 1:12) {
        monthly_areaft2[i] <- area*(PA_Months[i])
        monthly_aream2[i] <- monthly_areaft2[i]/10.764
        demand_charge[i] <- demand_charge_price * (((PPFD_capability/efficacy)*monthly_aream2[i])/1000)
      }
      
      # This code below is repetitive but needed for the weekly costs table
      val <- list()
      weeksum <- list()
      weekavg <- list()
      weekStart <- c(FirstMonday)
      
      for(i in 1:53){
        weekStart[[1 + (i)]]<- as_date(FirstMonday + weeks(i))
        for (j in seq(0,7,1)){
          val[j] <- AP_values$PerOfOne[[(month(weekStart[i] + days(j)))]]
        }
        weeksum[[i]] <- val
        weekavg[i] <- mean(sapply(weeksum[[i]],sum))
      }
      
      #loop to drop the 53rd week if it starts in the next year
      if (year(weekStart[53]) == year(Sys.Date() + years(1))){
        weeksum <- weeksum[-53]
        weekavg <- weekavg[-53]
      }
      length(weekStart) = length(weekavg) #remove the excess weeks from the defining loop
      
      #unlist the values
      weekavg1 <- unlist(weekavg)
      week1 <- unlist(weekStart)
      
      #Manually adjust overlapping months (easy). This and next year are done. Takes 3-5 mins.
      if (lubridate::year(as.Date(Sys.Date(), format = "%Y-%m-%d")) == 2022){
        weekavg1[5] <- (1*AP_values$PerOfOne[[1]] + 6*AP_values$PerOfOne[[2]])/7
        weekavg1[9] <- (1*AP_values$PerOfOne[[2]] + 6*AP_values$PerOfOne[[3]])/7
        weekavg1[13] <- (4*AP_values$PerOfOne[[3]] + 3*AP_values$PerOfOne[[4]])/7
        weekavg1[17] <- (6*AP_values$PerOfOne[[4]] + 1*AP_values$PerOfOne[[5]])/7
        weekavg1[22] <- (2*AP_values$PerOfOne[[5]] + 5*AP_values$PerOfOne[[6]])/7
        weekavg1[26] <- (4*AP_values$PerOfOne[[6]] + 3*AP_values$PerOfOne[[7]])/7
        weekavg1[35] <- (3*AP_values$PerOfOne[[8]] + 4*AP_values$PerOfOne[[9]])/7
        weekavg1[39] <- (5*AP_values$PerOfOne[[9]] + 2*AP_values$PerOfOne[[10]])/7
        weekavg1[44] <- (1*AP_values$PerOfOne[[10]] + 6*AP_values$PerOfOne[[11]])/7
        weekavg1[48] <- (3*AP_values$PerOfOne[[11]] + 4*AP_values$PerOfOne[[12]])/7
        weekavg1[52] <- AP_values$PerOfOne[[12]] 
      } else if (lubridate::year(as.Date(Sys.Date(), format = "%Y-%m-%d")) == 2023) {
        weekavg1[5] <- (2*AP_values$PerOfOne[[1]] + 5*AP_values$PerOfOne[[2]])/7
        weekavg1[9] <- (2*AP_values$PerOfOne[[2]] + 5*AP_values$PerOfOne[[3]])/7
        weekavg1[13] <- (5*AP_values$PerOfOne[[3]] + 2*AP_values$PerOfOne[[4]])/7
        weekavg1[22] <- (3*AP_values$PerOfOne[[5]] + 4*AP_values$PerOfOne[[6]])/7
        weekavg1[26] <- (5*AP_values$PerOfOne[[6]] + 2*AP_values$PerOfOne[[7]])/7
        weekavg1[31] <- (1*AP_values$PerOfOne[[7]] + 6*AP_values$PerOfOne[[8]])/7
        weekavg1[35] <- (4*AP_values$PerOfOne[[8]] + 3*AP_values$PerOfOne[[9]])/7
        weekavg1[39] <- (6*AP_values$PerOfOne[[9]] + 1*AP_values$PerOfOne[[10]])/7
        weekavg1[44] <- (2*AP_values$PerOfOne[[10]] + 5*AP_values$PerOfOne[[11]])/7
        weekavg1[48] <- (3*AP_values$PerOfOne[[11]] + 4*AP_values$PerOfOne[[12]])/7
        weekavg1[52] <- AP_values$PerOfOne[[12]]
      } else if (lubridate::year(as.Date(Sys.Date(), format = "%Y-%m-%d")) == 2024){ #Leap year
        weekavg1[5] <- (3*AP_values$PerOfOne[[1]] + 4*AP_values$PerOfOne[[2]])/7
        weekavg1[9] <- (4*AP_values$PerOfOne[[2]] + 3*AP_values$PerOfOne[[3]])/7
        weekavg1[18] <- (2*AP_values$PerOfOne[[4]] + 5*AP_values$PerOfOne[[5]])/7
        weekavg1[22] <- (5*AP_values$PerOfOne[[5]] + 2*AP_values$PerOfOne[[6]])/7
        weekavg1[31] <- (3*AP_values$PerOfOne[[7]] + 4*AP_values$PerOfOne[[8]])/7
        weekavg1[35] <- (6*AP_values$PerOfOne[[8]] + 1*AP_values$PerOfOne[[9]])/7
        weekavg1[40] <- (1*AP_values$PerOfOne[[9]] + 6*AP_values$PerOfOne[[10]])/7
        weekavg1[44] <- (4*AP_values$PerOfOne[[10]] + 3*AP_values$PerOfOne[[11]])/7
        weekavg1[48] <- (6*AP_values$PerOfOne[[11]] + 1*AP_values$PerOfOne[[12]])/7
        weekavg1[52] <- AP_values$PerOfOne[[12]]
      } else { # for 2025 and beyond, this needs to be recalculated. Currently is the same value from 2022
        weekavg1[5] <- (1*AP_values$PerOfOne[[1]] + 6*AP_values$PerOfOne[[2]])/7
        weekavg1[9] <- (1*AP_values$PerOfOne[[2]] + 6*AP_values$PerOfOne[[3]])/7
        weekavg1[13] <- (4*AP_values$PerOfOne[[3]] + 3*AP_values$PerOfOne[[4]])/7
        weekavg1[17] <- (6*AP_values$PerOfOne[[4]] + 1*AP_values$PerOfOne[[5]])/7
        weekavg1[22] <- (2*AP_values$PerOfOne[[5]] + 5*AP_values$PerOfOne[[6]])/7
        weekavg1[26] <- (4*AP_values$PerOfOne[[6]] + 3*AP_values$PerOfOne[[7]])/7
        weekavg1[35] <- (3*AP_values$PerOfOne[[8]] + 4*AP_values$PerOfOne[[9]])/7
        weekavg1[39] <- (5*AP_values$PerOfOne[[9]] + 2*AP_values$PerOfOne[[10]])/7
        weekavg1[44] <- (1*AP_values$PerOfOne[[10]] + 6*AP_values$PerOfOne[[11]])/7
        weekavg1[48] <- (3*AP_values$PerOfOne[[11]] + 4*AP_values$PerOfOne[[12]])/7
      }
      
      PA_Weeks <- weekavg1
      
      # Load in the radiation data set from earlier to then use here
      radiation <- XTRA_values$radiation
      
      # Create a weekly version of the radiation data set
      radiationWeek <- radiation
      radiationWeek$Week <- week(date(radiation$TimeStamp) - day(FirstMonday - days(1)))
      radiationWeek$Month <- month(date(radiation$TimeStamp))
      
      ####################################.
      ## Calculations for Ele Cost ####
      ####################################.
      # Calculations for Daily Light Integral (DLI):
      # 1. Convert GHI value for one hour from Wh * m^-2 to micromoles * m^-2*h-1
      radiation$ghi <- (as.numeric(radiation$ghi)*7272)
      # 2. Calculate Daily Light Integral for each hour of the day (loop from hour 0 to hour 23) sum.
      CC_df <- radiation %>%
        group_by(date(TimeStamp),Month) %>%                   
        summarize(dli_umol = sum(ghi)) %>%
        na.omit
      colnames(CC_df) <- c("TimeStamp","Month","dli_umol")
      # 3. Convert micromoles to moles
      CC_df$dli_mol = CC_df$dli_umol/(1*10^6)
      # 4. Calculate Sunlight Present in Greenhouse (SPG):
      CC_df$trans <- trans # Grower Iput: Greenhouse Transmission -> Retrieve: Daily Light Integral
      CC_df$spg_mol <- CC_df$dli_mol * (trans/100) #transmission as %
      # 5. Calculate Supplemental Light Needed (SL):
      CC_df$dli_target <- target # Grower Input: Target Daily Light Integral (TDLI or plant light requirement) -> Retrieve: Daily Light
      CC_df$SL_mol <- ifelse(CC_df$spg_mol > target, 0, target - CC_df$spg_mol)
      CC_df$SL_mol <- ifelse(CC_df$SL_mol >= DLI_capability, yes = DLI_capability, no = CC_df$SL_mol) #We assign the capability DLI to the value because the user CAN'T go over their system's capacity DLI, so they shouldn't be charged over the supp DLI.
      # 6. Calculate Total light intake
      CC_df$totalDLImols <- CC_df$spg_mol + CC_df$SL_mol
      # 7. Calculate electricity cost in S/m^2d
      # Lighting System Efficacy input by grower in micromol/J
      # Lighting System Efficacy micromol/J x 3.6 = Lighting System Efficacy mol/(m^2d)
      CC_df$Elec_rate <- ele_rate # Electricity Cost $/(m^2d)
      CC_df$CC_m2d <- (CC_df$SL_mol/(efficacy*3.6))*ele_rate
      # 8. Convert the grower input of greenhouse size in square feet to square meters
      CC_df$aream2 <- (area/10.764) # Greenhouse Size m^2 = Greenhouse Size ft^2/10.764
      
      # Create a data set with these values grouped by month
      CC_df_M <- CC_df %>%
        group_by(Month) %>%
        summarize(CC_m2m  = sum(CC_m2d)) %>%
        na.omit
      # Create the columns below. PA_Months represents the PercentArea for each month, which is calculated in the percent matrix
      CC_df_M$MonthName <- month.abb[as.numeric(CC_df_M$Month)]
      CC_df_M$CC_acre <- CC_df_M$CC_m2m*4046.86*PA_Months 
      CC_df_M$CC_ftsq <- CC_df_M$CC_m2m/10.764*PA_Months 
      CC_df_M_Graph <- data.frame(CC_df_M$MonthName,CC_df_M$CC_acre)
      colnames(CC_df_M_Graph) <- c("Month","per_acre")
      CC_df_M$CC_design <- CC_df_M$CC_m2m*(monthly_aream2[as.numeric(CC_df_M$Month)]) 
      # Rename the column headers and round the values
      CC_df_M_Display <- CC_df_M[c("MonthName","CC_ftsq","CC_acre","CC_design")]
      colnames(CC_df_M_Display) <- c("Month","$ Per ft2","$ Per acre","$ Total Design")
      CC_df_M_Display$`$ Per ft2` <- prettyNum(round(CC_df_M_Display$`$ Per ft2`, digits = 3), big.mark = ',')
      CC_df_M_Display$`$ Per acre` <- prettyNum(round(CC_df_M_Display$`$ Per acre`, digits = 0), big.mark = ',')
      CC_df_M_Display$`$ Total Design` <- prettyNum(round(CC_df_M_Display$`$ Total Design`, digits = 0), big.mark = ',')
      
      # Weekly Electricity Cost Calculations
      # Calculations for Daily Light Integral (DLI):
      # 1. Convert GHI value for one hour from Wh * m^-2 to micromoles * m^-2*h-1
      radiationWeek$ghi <- (as.numeric(radiationWeek$ghi)*7272)
      # 2. Calculate Daily Light Integral for each hour of the day (loop from hour 0 to hour 23) sum.
      CC_df_Week <- radiationWeek %>%
        group_by(date(TimeStamp),Week) %>%
        summarize(dli_umol = sum(ghi)) %>%
        na.omit
      colnames(CC_df_Week) <- c("TimeStamp","Week","dli_umol")
      # 3. Convert micromoles to moles
      CC_df_Week$dli_mol = CC_df_Week$dli_umol/(1*10^6)
      CC_df_Week$trans <- trans # Grower Iput: Greenhouse Transmission -> Retrieve: Daily Light Integral
      # 4. Calculate Sunlight Present in Greenhouse (SPG):
      CC_df_Week$spg_mol <- CC_df_Week$dli_mol * (trans/100) #transmission as %
      # 5. Calculate Supplemental Light Needed (SL):
      CC_df_Week$dli_target <- target # Grower Input: Target Daily Light Integral (TDLI or plant light requirement) -> Retrieve: Daily Light
      CC_df_Week$SL_mol <- ifelse(CC_df_Week$spg_mol > target, 0, target - CC_df_Week$spg_mol)
      CC_df_Week$SL_mol <- ifelse(CC_df_Week$SL_mol >= DLI_capability, yes = DLI_capability, no = CC_df_Week$SL_mol) #We assign the capability DLI to the value because the user CAN'T go over their system's capacity DLI, so they shouldn't be charged over the supp DLI.
      # 6. Calculate Total light intake
      CC_df_Week$totalDLImols <- CC_df_Week$spg_mol + CC_df_Week$SL_mol
      # 7. Calculate electricity cost in S/m^2d
      # Lighting System Efficacy input by grower in micromol/J
      # Lighting System Efficacy micromol/J x 3.6 = Lighting System Efficacy mol/(m^2d)
      CC_df_Week$Elec_rate <- ele_rate # Electricity Cost $/(m^2d)
      CC_df_Week$CC_m2d <- (CC_df_Week$SL_mol/(efficacy*3.6))*ele_rate
      # 8. Convert the grower input of greenhouse size in square feet to square meters
      CC_df_Week$aream2 <- (area/10.764) # Greenhouse Size m^2 = Greenhouse Size ft^2/10.764

      # Create two empty vectors equal to the number of weeks in the year and fill them with the weekly area lit by interacting PA_Weeks with area
      weekly_areaft2 <- vector(mode="numeric", length = length(weekStart))
      weekly_aream2 <- vector(mode="numeric", length = length(weekStart))
      for(i in 1:length(weekStart)) {
        weekly_areaft2[i] <- area*(PA_Weeks[i])
        weekly_aream2[i] <- weekly_areaft2[i]/10.764
      }
      
      # Create a weekly table to summarize costs by week
      CC_df_W <- CC_df_Week %>%
        group_by(Week) %>%
        summarize(CC_m2m  = sum(CC_m2d)) %>% 
        na.omit
      
      # Create a weekly cost table to display to the user
      CC_df_W$WeekNum <- paste(CC_df_W$Week)  
      CC_df_W$CC_acre <- CC_df_W$CC_m2m*4046.86*PA_Weeks
      CC_df_W$CC_ftsq <- CC_df_W$CC_m2m/10.764*PA_Weeks
      CC_df_W$CC_design <- rep(CC_df_W$CC_m2m*(weekly_aream2[as.numeric(CC_df_W$Week)]))
      # Create a list of start dates
      a <- list()
      for (i in 1:52) {
        a[i] = toString(x = weekStart[i])
      }
      for (i in 1:52) {
        a[i] = substr(weekStart[i],nchar(weekStart[i])+1, 10)
      }

      # Rename the columns and round the values
      CC_df_W_Display <- CC_df_W[c("WeekNum","CC_ftsq","CC_acre","CC_design")]
      colnames(CC_df_W_Display) <- c("Week","$ Per ft2","$ Per acre","$ Total Design")
      CC_df_W_Display$`$ Per ft2` <- prettyNum(round(CC_df_W_Display$`$ Per ft2`, digits = 3), big.mark = ',')
      CC_df_W_Display$`$ Per acre` <- prettyNum(round(CC_df_W_Display$`$ Per acre`, digits = 0), big.mark = ',')
      CC_df_W_Display$`$ Total Design` <- prettyNum(round(CC_df_W_Display$`$ Total Design`, digits = 0), big.mark = ',')
      
      # Calculate the estimated annual electricity cost & round the values
      Annual_CC <- data.frame(sum(CC_df_W$CC_ftsq),sum(CC_df_W$CC_acre),sum(CC_df_W$CC_design))
      colnames(Annual_CC) <- c("ACC_df_ft2m","ACC_df_acre","ACC_df_design")
      Annual_CC_Display <- Annual_CC
      colnames(Annual_CC_Display) <- c("$ Per ft2","$ Per acre","$ Total Design")
      Annual_CC_Display$`$ Per ft2` <- prettyNum(round(Annual_CC_Display$`$ Per ft2`, digits = 3), big.mark = ',')
      Annual_CC_Display$`$ Per acre` <- prettyNum(round(Annual_CC_Display$`$ Per acre`, digits = 0), big.mark = ',')
      Annual_CC_Display$`$ Total Design` <- prettyNum(round(Annual_CC_Display$`$ Total Design`, digits = 0), big.mark = ',')

      CC_df_M$CC_demand_design <- demand_charge
      
      Annual_CC_demand <- data.frame(sum(CC_df_M$CC_demand_design)/area,sum(CC_df_M$CC_demand_design)/(area/43560),sum(CC_df_M$CC_demand_design))
      colnames(Annual_CC_demand) <- c("ADCC_df_ft2m","ADCC_df_acre","ADCC_df_design")
      Annual_CC_demand_Display <- Annual_CC_demand
      colnames(Annual_CC_demand_Display) <- c("$ Per ft2","$ Per acre","$ Total Design")
      Annual_CC_demand_Display$`$ Per ft2` <- prettyNum(round(Annual_CC_demand_Display$`$ Per ft2`, digits = 3), big.mark = ',')
      Annual_CC_demand_Display$`$ Per acre` <- prettyNum(round(Annual_CC_demand_Display$`$ Per acre`, digits = 0), big.mark = ',')
      Annual_CC_demand_Display$`$ Total Design` <- prettyNum(round(Annual_CC_demand_Display$`$ Total Design`, digits = 0), big.mark = ',')
      
      # Grower Input Table
      output$CC_table <- renderTable({
        # Create a dataframe that will be used to display the user's inputs back to them
        CC_table_output <- data.frame(trans,target,efficacy,ele_rate, demand_charge_price, PPFD_capability, hours, area)
        names(CC_table_output) <- (c("Greenhouse Transmission (%)","Target DLI (mol/m2/day)","Lighting Efficacy (umol/J)", "Electricity Cost ($/kWh)", "Demand Charge ($/kW/month)", "PPFD (umol/m2/s)", "Hours", "Area (ft2)"))
        # Needed to keep the formatting pretty
        CC_table_output
      })
      
      # Create an hourly DLI table
      DLI_Hourly <- radiation %>%
        group_by(date(TimeStamp),hour(TimeStamp)) %>%                   
        summarize(dli_h_umol = sum(ghi)) %>% 
        na.omit
      colnames(DLI_Hourly) <- c("TimeStamp","Month","dli_h_umol")
      
      # Convert micromoles to moles to get DLI (or HLI since this is an hourly value)
      DLI_Hourly$dli_h_mol = DLI_Hourly$dli_h_umol/(1*10^6)
      
      # Create a DLI table
      DLI_table <- DLI_Hourly %>%
        group_by(date(TimeStamp)) %>%
        summarize(dli_mol = sum(dli_h_mol)*(trans/100)) %>% 
        na.omit()
      colnames(DLI_table) <- c("Date", "Natural_DLI", "DLI_Cost", "Aream2") #natural accounts for efficacy
      
      # Create a plot that visualized the natural sunlight that enters the greenhouse after accounting for transmission for each day, and create an intercept representing the target DLI
      DLI_table$Natural_DLI_Reached <- DLI_table$Natural_DLI >= target 
      #DLI_table$Month <- format(as.Date(df$Date), "%d-%m")
      # Visualize the data
      output$CC_naturalDLI <- renderPlot({
        ggplot(DLI_table,aes(Date,Natural_DLI)) +
          geom_point(aes(color = Natural_DLI_Reached)) + # Meaning does the natural sunlight entering the greenhouse fall above or below the target DLI
          geom_hline(yintercept = target, color = "red", size = .5) +
          labs(title = paste0("Days at or below a DLI of ", target),
               caption = paste0("Red line indicates the target DLI of ",target, "(mol/m2/d)")) +
          theme(plot.caption = element_text(size = 12)) +
          ylab("Sunlight DLI (mol/m2/d)") + xlab("Date") + scale_x_date(name = "Date", date_breaks = '1 month', date_labels = '%b')
      })
      
      # Create a plot to visualize the amount of supplemental light needed each day where the intercept represents the DLI capacity needed to cover X percent of days
      # A. calculate the amount of supplemental light needed each day
      DLI_table$Supplemental <- target - DLI_table$Natural_DLI
      # If there is ample light for the day, assign 0 to supplemental light needed
      DLI_table$Supplemental[DLI_table$Supplemental < 0] <- 0
      # Create a column for T/F if the target DLI was reached (this is used to color the points in ggplot)
      DLI_table$Target_DLI_Reached <- DLI_table$Supplemental <= DLI_capability
      # Calculate the percent of days that the target DLI is reached
      days_reached <- 0
      for (i in 1:length(DLI_table$Target_DLI_Reached)) {
        if(DLI_table$Target_DLI_Reached[i] == TRUE){days_reached <- days_reached + 1} else {}
      }
      rm(i)
      days_reached_percent <- round(days_reached/length(DLI_table$Target_DLI_Reached)*100,1)
      
      # B. Visualize the data
      output$CC_supplementalDLI <- renderPlot({
        ggplot(DLI_table,aes(Date,Supplemental)) +
          geom_point(aes(color = Target_DLI_Reached)) + # Meaning does the supplemental light needed to reach the target DLI fall above or below the calculated system capacity
          geom_hline(yintercept = DLI_capability, color = "red", size = .5) +
          labs(title = paste0("You'll reach your target DLI ",
                              days_reached, " days of the year, which is ",
                              days_reached_percent, "% of the year"),
               caption = paste0("Red line indicates the maximum DLI that can be provided by the system, which is ",round(DLI_capability,2), "(mol/m2/d)")) +
          theme(plot.caption = element_text(size = 12)) +
          ylab("Supplemental DLI needed (mol/m2/d)") + xlab("Date") + scale_x_date(name = "Date", date_breaks = '1 month', date_labels = '%b')
          
      })
      
      # Output the percent of days, # of days, and DLI capacity as a valueBox for users to see
      output$P_days_tile <- renderValueBox({
        valueBox(value = tags$p("% of days that the target DLI is reached", style = "font-size: 80%;"), 
                 subtitle = tags$p(paste0(round(days_reached_percent, digits = 1), " %"), style = "font-size: 200%;"))
      })
      output$N_days_tile <- renderValueBox({
        valueBox(value = tags$p("# of days that the target DLI is reached", style = "font-size: 80%;"), 
                 subtitle = tags$p(paste0(round(days_reached, digits = 0), " days"), style = "font-size: 200%;"))
      })
      output$DLI_tile <- renderValueBox({
        valueBox(value = tags$p("Max Supplemental DLI Capacity", style = "font-size: 80%;"), 
                 subtitle = tags$p(paste0(round(DLI_capability, digits = 2), " mol/m2/d"), style = "font-size: 200%;"))
      })
      
      # #####################################.
      # #### Download Results #####
      # #####################################.
      # Create a dataframe with the original user inputs
      Grower_input <- data.frame(trans,target,efficacy,ele_rate, demand_charge_price, PPFD_capability, hours, area)
      names(Grower_input) <- (c("Greenhouse Transmission (%)","Target DLI (mol/m2/day)","Lighting Efficacy (umol/J)", "Electricity Cost ($/kWh)", "Demand Charge ($/kW/month)", "PPFD (umol/m2/s)", "Hours", "Area (ft2)"))
      
      # Load in the detailed values based on the original user inputs
      CC_df2 <- CC_df
      names(CC_df2) <- c("TimeStamp", "Month", "DLI_umol", "DLI_mol", "Transmission (%)", "Sunlight Present in Greenhouse (mol)", "DLI Target",
                         "Supplemental Light Needed (mol)", "Total DLI Mols", "Electricity Rate", "Cost per m2 per day", "Area (m2)")
      CC_df2 <- CC_df2[-c(3)]
      CC_df2 <- CC_df2[-c(11)]
      CC_df2$`Cost per m2 per day` <- round( CC_df2$`Cost per m2 per day`, digits = 2)
      
      # Create an area matrix dataframe
      months <-  c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")
      Monthly_Area_Percent <- data.frame(months, label_percent()(PA_Months))
      names(Monthly_Area_Percent) <- c("Month", "Percent Area")
      
      # Create a dataframe for the % of year and # of days
      Value <- c(paste0(round(days_reached_percent,1), " %"), paste0(round(days_reached,0)," days"), paste0(round(DLI_capability,2)," mol/m2/d"))
      Name <- c("% of year that the target DLI is reached", "# of days that the target DLI is reached", "Max Supplemental DLI Capacity")
      Summary_Frame <- data.frame(Name, Value)
      
      # Create a list to represent a workbook where each line is an excel sheet based on adjusted values
      data_list <- reactive({
        list(
          Grower_Input = Grower_input,
          Monthly_Area = Monthly_Area_Percent,
          Monthly_Costs = CC_df_M_Display,
          Weekly_Costs = CC_df_W_Display,
          Demand_Costs = Annual_CC_demand_Display,
          Detailed_Electricity_Data = CC_df2,
          Summary_Values = Summary_Frame
        )
      })
      #Button to Download Results
      output$CC_downloadData <- downloadHandler(
        filename = paste0("TargetCalc_", as.character(Sys.Date()), ".xlsx"), #File with current date
        content = function(file) {
          write_xlsx(data_list(), path=file, col_names = TRUE)
        }
      )
      
    }
    
    #################################.
    ### Render Tables and Graph ###
    #################################.
    # Send the tables to the UI
    output$Annual_CC_Display_table<- renderTable({
      Annual_CC_Display # Estimated Annual Electricity Summary Costs
    })
    output$DSC_Display_table <- renderTable({
      Annual_CC_demand_Display # Estimated Annual Demand Summary Costs
    })
    output$CC_df_W_table <- renderTable({
      CC_df_W_Display # Estimated Weekly Electricity Costs
    })
  })
} #bracket server