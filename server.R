library(shiny)
library(shinydashboard)
library(ggplot2)
library(RSQLite)
library(shinyjs)
library(xts)
library(dygraphs)
library(V8)

#necessary for remote box-collapsing
jscode <- "
shinyjs.collapse = function(boxid) {
$('#' + boxid).closest('.box').find('[data-widget=collapse]').click();
}
"

#sqlite <- dbConnect(SQLite(), "db.sqlite")
sqlite <- dbConnect(SQLite(), "options.sqlite")

Nd20 <- 0
Time_to_maturity_0 <- 0

Nd1_old <- 0
A_Value_old <- 0
L_Value_old <- 0


date_act <- 0
Nd1_act <- 0
A_Value_act <- 0
L_Value_act <- 0

server <- function(input, output, session) {

  observeEvent(input$ab_Initial_Pricing, {
    js$collapse("box_Measure")
    hide(id = "box_Initial_Pricing", anim = FALSE)
    
    temp_db_derivative_instrument_master <-
      cbind.data.frame(
        input$ti_Stock_ISIN,
        input$ti_Type_Of_Stock_Derivative,
        input$ti_Exercise_Or_Forward_Price,
        
        as.character(input$ti_Contracting_Date),
        as.character(input$ti_Expiration_Date),
        input$ti_Contracting_Security_Price,
        
        input$ti_Contract_Size,
        input$ti_Number_Of_Contracts,
        input$ti_Stock_Volatility,
        
        input$ti_Interest_Rate,
        input$ti_Mark_To_Model
      )
    
    names(temp_db_derivative_instrument_master) <-
      c(
        "isin", 
        "type_of_stock_derivative",
        "exercise_or_forward_price", 
        "contracting_date", 
        "expiration_date", 
        "contracting_security_price", 
        "contracting_size", 
        "number_of_contracts", 
        "volatility", 
        "interest_rate", 
        "mark_to_model"
      )
    
    dbWriteTable(sqlite,
                 "derivative_instrument_master",
                 temp_db_derivative_instrument_master,
                 append = TRUE)
    
  
    
    P <- as.numeric(input$ti_Contracting_Security_Price) #contracting_security_price
    X <- as.numeric(input$ti_Exercise_Or_Forward_Price) #exercise_or_forward_price
    r <- as.numeric(input$ti_Interest_Rate)/100 #interest_rate
    sigma <- as.numeric(input$ti_Stock_Volatility)/100 # volatility
    
    #cat(file=stderr(), "p: ", P, "\n")
    #cat(file=stderr(), "X: ", X, "\n")
    #cat(file=stderr(), "r: ", r, "\n")
    #cat(file=stderr(), "sigma: ", sigma, "\n")

    Time_to_maturity <- as.numeric(difftime(toString(isolate(input$ti_Expiration_Date)),
                                                  toString(isolate(input$ti_Contracting_Date))))/365
  
    Time_to_maturity_0 <<- Time_to_maturity
    #cat(file=stderr(), "contdate: ", toString(isolate(input$ti_Contracting_Date)), "\n")
    #cat(file=stderr(), "expdate: ", toString(isolate(input$ti_Expiration_Date)), "\n")
    #cat(file=stderr(), "TtM: ", Time_to_maturity, "\n")
    
    d1 <- (log(P/X) + (r + sigma*sigma/2) * Time_to_maturity) /( sigma*sqrt(Time_to_maturity))
    Nd1 <- pnorm(d1)
    #cat(file=stderr(), "Nd1: ", Nd1, "\n")
    
    d2 <- d1 - sigma * sqrt(Time_to_maturity)
    Nd2 <- pnorm(d2)
    
    Nd20 <<- Nd2
    #cat(file=stderr(), "Nd2: ", Nd2, "\n")
    
    Asset <- P * Nd1
    Liability <- -X*exp(-r*Time_to_maturity)*Nd2
    
    #cat(file=stderr(), "Asset: ", Asset, "\n")
    #cat(file=stderr(), "Liability: ", Liability, "\n")
    
    Nd1_old <<- Nd1
    A_Value_old <<- Asset
    L_Value_old <<- Liability
    
    #database
    temp_db_derivative_instrument_transactional <-
      cbind.data.frame(
        1,
        1,
        as.character(input$ti_Contracting_Date),
        Asset + Liability
      )
    
    names(temp_db_derivative_instrument_transactional) <-
      c(
        "derivate_instrument_master_id",
        "event_id",
        "timestamp_",
        "fair_value"
      )
    
    dbWriteTable(sqlite,
                 "derivative_instrument_transactional",
                 temp_db_derivative_instrument_transactional,
                 append = TRUE)
    
    ##Risky income
    temp_db_economic_resource_risky_income <-
      cbind.data.frame(
        1,
        1,
        Nd1,
        Asset,
        0
      )
    
    names(temp_db_economic_resource_risky_income) <-
      c(
        "derivative_instrument_transactional_id",
        "resource_id",
        "nd1t",
        "value",
        "asset_or_liability"
      )
    
    dbWriteTable(sqlite,
                 "economic_resource_risky_income",
                 temp_db_economic_resource_risky_income,
                 append = TRUE)
    
    ##Fixed income
    temp_db_economic_resource_fixed_income <-
      cbind.data.frame(
        1,
        1,
        Liability,
        1
      )
    
    names(temp_db_economic_resource_fixed_income) <-
      c(
        "derivative_instrument_transactional_id", 
        "resource_id", 
        "present_value", 
        "asset_or_liability"
      )
    
    dbWriteTable(sqlite,
                 "economic_resource_fixed_income",
                 temp_db_economic_resource_fixed_income,
                 append = TRUE)
  
  })
    
  #event Measure  
  observeEvent(input$button_Measure, {
    
    temp_db_financial_security_pricing_transactional <-
      cbind.data.frame(
        input$ti_Stock_ISIN,
        as.character(input$ti_Measure_timestamp),
        input$ti_Measure_Stock_Price
      )
    names(temp_db_financial_security_pricing_transactional) <-
      c("isin",
        "timestamp_",
        "price")
    dbWriteTable(sqlite,
                 "financial_security_pricing_transactional",
                 temp_db_financial_security_pricing_transactional,
                 append = TRUE)
    
    
    js$collapse("box_Plan")
  })
  
  
  
  
  observeEvent(input$button_Plan, {
    
    
    resultset = dbGetQuery(sqlite, "select timestamp_, price from financial_security_pricing_transactional order by timestamp_ desc limit 1")
    dbDisconnect(sqlite)
    P = as.numeric(resultset$price)

    X <- as.numeric(input$ti_Exercise_Or_Forward_Price) #exercise_or_forward_price
    r <- as.numeric(input$ti_Interest_Rate)/100 #interest_rate
    sigma <- as.numeric(input$ti_Stock_Volatility)/100 # volatility
    
    cat(file=stderr(), "p: ", P, "\n")
    cat(file=stderr(), "X: ", X, "\n")
    cat(file=stderr(), "r: ", r, "\n")
    cat(file=stderr(), "sigma: ", sigma, "\n")
    
    Time_to_maturity <- as.numeric(difftime(toString(isolate(input$ti_Expiration_Date)),
                                            toString(resultset$timestamp_)))/365
    
    cat(file=stderr(), "contdate: ", toString(isolate(input$ti_Contracting_Date)), "\n")
    cat(file=stderr(), "expdate: ", toString(isolate(input$ti_Expiration_Date)), "\n")
    cat(file=stderr(), "TtM: ", Time_to_maturity, "\n")
    
    d1 <- (log(P/X) + (r + sigma*sigma/2) * Time_to_maturity) /( sigma*sqrt(Time_to_maturity))
    Nd1 <- pnorm(d1)
    cat(file=stderr(), "Nd1: ", Nd1, "\n")

    Nd1_act <<- Nd1
    date_act <<- toString(resultset$timestamp_)
    
    cat(file=stderr(), "date_act", date_act, "\n")
    cat(file=stderr(), "d1, t: ", d1, "\n")
    cat(file=stderr(), "Nd1, t: ", Nd1, "\n")
    
    Asset <- P * Nd1
    Liability <- L_Value_old * exp (r*(Time_to_maturity_0-Time_to_maturity))

    A_Value_act <<- Asset
    L_Value_act <<- Liability - (Nd1_act - Nd1_old)*P

    
    
    output$to_Plan <- renderText(paste("N(d1) = ", Nd1))
    js$collapse("box_Check")
  })
  
  #https://stackoverflow.com/questions/19611254/r-shiny-disable-able-shinyui-elements
  
  
  observeEvent(input$button_Check, {
    
    
    cat(file=stderr(), "Nd1, t-1: ", Nd1_old, "\n")
    cat(file=stderr(), "Delta Nd1, t: ", Nd1_act - Nd1_old, "\n")

    
    output$to_Check <- renderText(paste("Delta N(d1) = ", Nd1_act - Nd1_old))
    js$collapse("box_Act")
  })
  
  observeEvent(input$button_Act, {
    

    
    cat(file=stderr(), "Asset value after transaction: ", A_Value_act, "\n")
    cat(file=stderr(), "Liability value after transaction: ", L_Value_act, "\n")
    cat(file=stderr(), "Portfolio value after transaction: ", +A_Value_act+L_Value_act, "\n")
    
    cat(file=stderr(), "as.character(date_act) ", as.character(date_act), "\n")
    cat(file=stderr(), "(A_Value_act+L_Value_act) ", (A_Value_act+L_Value_act), "\n")
    cat(file=stderr(), "Nd1_act ", Nd1_act, "\n")
    cat(file=stderr(), "A_Value_act ", A_Value_act, "\n")
    cat(file=stderr(), "L_Value_act ", L_Value_act, "\n")
    
    
   output$to_Act <- renderText("Rebalancing executed!")
  #  v$doCalcAndPlot <- input$button_Act #CalcAndPlot
  })
  
  observeEvent(input$button_Act_Continue, {
    
    
    js$collapse("box_Act")
    js$collapse("box_Plan")
    js$collapse("box_Check")
    
    output$to_Plan <- renderText("")
    output$to_Check <- renderText("")
    output$to_Act <- renderText("")
    
  })
  
  observeEvent(input$reset_db, {
    dbSendStatement(sqlite, "DELETE from derivative_instrument_master")
    dbSendStatement(sqlite, "DELETE from economic_resource_fixed_income")
    dbSendStatement(sqlite, "DELETE from economic_resource_risky_income")
    dbSendStatement(sqlite, "DELETE from derivative_instrument_transactional")
    dbSendStatement(sqlite, "DELETE from financial_security_pricing_transactional")
    #dbSendStatement(sqlite, "DELETE from Asset")
    #dbSendStatement(sqlite, "DELETE from Liability")
    #dbSendStatement(sqlite, "DELETE from Off_Balance")
  })
  

  
  v <- reactiveValues(doCalcAndPlot = FALSE) #recalc and redraw
  
  output$timeline <- renderDygraph({
    if (v$doCalcAndPlot == FALSE)
      return()
    isolate({
      temp_db_draw <- dbReadTable(sqlite, "Stock_Pricing_Dynamic")
      temp_db_draw$Pricing_Date <-
        as.Date(as.POSIXct(temp_db_draw$timestamp))
      
      #legacy calc
      temp_db_draw$TtM <-
        as.numeric(difftime(
          as.Date(isolate(input$ti_Expiration_Date)),
          as.Date(temp_db_draw$Pricing_Date),
          unit = "weeks"
        )) / 52.1775
      temp_db_draw$Interest_Rate <-
        as.numeric(input$ti_Interest_Rate) / 100
      temp_db_draw$Interest_Rate_Cont <-
        log(1 + temp_db_draw$Interest_Rate)
      temp_db_draw$F_Price <-
        temp_db_draw[1, 3] * (1 + as.numeric(input$ti_Interest_Rate) / 100) ^ (as.numeric(difftime(
          as.Date(input$ti_Expiration_Date),
          as.Date(input$ti_Contracting_Date),
          unit = "weeks"
        )) / 52.1775)
      temp_db_draw$Liability <-
        -temp_db_draw$F_Price * exp(-temp_db_draw$Interest_Rate_Cont * temp_db_draw$TtM)
      temp_db_draw$Asset <- temp_db_draw$Stock_Price
      temp_db_draw$'Forward Value' <-
        round(temp_db_draw$Liability + temp_db_draw$Stock_Price, 1)

      #Composing XTS
      temp_xts_draw <-
        xts(x = temp_db_draw[, c("Asset", "Liability", "Forward Value")], order.by =
              temp_db_draw[, 5])
      
      #Derivative_Instrument_Dynamic entry
      temp_Stock_Derivative_Static <-
        dbReadTable(sqlite, "Stock_Derivative_Static")
      temp_db_Derivative_Instrument_Dynamic <-
        cbind.data.frame(
          tail(temp_Stock_Derivative_Static$Stock_Derivative_Static_ID, 1),
          as.character(input$ti_Do_timestamp),
          tail(temp_db_draw$'Forward Value', 1)
        )
      names(temp_db_Derivative_Instrument_Dynamic) <-
        c("Stock_Derivative_Static_ID",
          "timestamp",
          "Fair_Value")
      dbWriteTable(
        sqlite,
        "Derivative_Instrument_Dynamic",
        temp_db_Derivative_Instrument_Dynamic,
        append = TRUE
      )
      
      #Economic_Resource_Risky_Income entry
      temp_Derivative_Instrument_Dynamic <-
        dbReadTable(sqlite, "Derivative_Instrument_Dynamic")
      temp_db_Economic_Resource_Risky_Income <-
        cbind.data.frame(
          tail(
            temp_Derivative_Instrument_Dynamic$Derivative_Instrument_Dynamic_ID,
            1
          ),
          as.character(input$ti_Do_timestamp),
          1,
          tail(temp_db_draw$'Asset', 1),
          1
        )
      names(temp_db_Economic_Resource_Risky_Income) <-
        c(
          "Derivative_Instrument_Dynamic_ID",
          "timestamp",
          "Nd1t",
          "Value",
          "Asset_Or_Liability"
        )
      dbWriteTable(
        sqlite,
        "Economic_Resource_Risky_Income",
        temp_db_Economic_Resource_Risky_Income,
        append = TRUE
      )
      
      #Economic_Resource_Fixed_Income entry
      temp_Derivative_Instrument_Dynamic <-
        dbReadTable(sqlite, "Derivative_Instrument_Dynamic")
      temp_db_Economic_Resource_Fixed_Income <-
        cbind.data.frame(
          tail(
            temp_Derivative_Instrument_Dynamic$Derivative_Instrument_Dynamic_ID,
            1
          ),
          as.character(input$ti_Do_timestamp),
          tail(temp_db_draw$'Liability', 1),
          1
        )
      names(temp_db_Economic_Resource_Fixed_Income) <-
        c(
          "Derivative_Instrument_Dynamic_ID",
          "timestamp",
          "Present_Value",
          "Asset_Or_Liability"
        )
      dbWriteTable(
        sqlite,
        "Economic_Resource_Fixed_Income",
        temp_db_Economic_Resource_Fixed_Income,
        append = TRUE
      )
      
      #Asset, Liability of Off Balance
      if (tail(temp_db_draw$'Forward Value', 1) > 0) {
        #Asset
        temp_Derivative_Instrument_Dynamic <-
          dbReadTable(sqlite, "Derivative_Instrument_Dynamic")
        temp_db_asset <-
          cbind.data.frame(
            tail(
              temp_Derivative_Instrument_Dynamic$Derivative_Instrument_Dynamic_ID,
              1
            ),
            as.character(input$ti_Do_timestamp),
            tail(temp_Derivative_Instrument_Dynamic$Fair_Value, 1)
          )
        names(temp_db_asset) <-
          c("Derivative_Instrument_Dynamic_ID",
            "timestamp",
            "Fair_Value")
        dbWriteTable(sqlite, "Asset", temp_db_asset, append = TRUE)
      } else if (tail(temp_db_draw$'Forward Value', 1) < 0) {
        #Liability
        temp_Derivative_Instrument_Dynamic <-
          dbReadTable(sqlite, "Derivative_Instrument_Dynamic")
        temp_db_liability <-
          cbind.data.frame(
            tail(
              temp_Derivative_Instrument_Dynamic$Derivative_Instrument_Dynamic_ID,
              1
            ),
            as.character(input$ti_Do_timestamp),
            tail(temp_Derivative_Instrument_Dynamic$Fair_Value, 1)
          )
        names(temp_db_liability) <-
          c("Derivative_Instrument_Dynamic_ID",
            "timestamp",
            "Fair_Value")
        dbWriteTable(sqlite, "Liability", temp_db_liability, append = TRUE)
      }
      else {
        # Off_Balance
        temp_Derivative_Instrument_Dynamic <-
          dbReadTable(sqlite, "Derivative_Instrument_Dynamic")
        temp_db_off_balance <-
          cbind.data.frame(
            tail(
              temp_Derivative_Instrument_Dynamic$Derivative_Instrument_Dynamic_ID,
              1
            ),
            as.character(input$ti_Do_timestamp)
          )
        names(temp_db_off_balance) <-
          c("Derivative_Instrument_Dynamic_ID",
            "timestamp")
        dbWriteTable(sqlite, "Off_Balance", temp_db_off_balance, append = TRUE)
      }
      
      #Plotting XTS
      dygraph(temp_xts_draw) %>%
        dyRangeSelector()
    })
  })
  
  observeEvent(
    input$load_table_Stock_Pricing_Dynamic,
    output$table_Stock_Pricing_Dynamic <- renderDataTable({
      dbReadTable(sqlite, "Stock_Pricing_Dynamic")
    })
  )
  
  observeEvent(
    input$load_table_Stock_Information_Static,
    output$table_Stock_Information_Static <- renderDataTable({
      dbReadTable(sqlite, "Stock_Information_Static")
    })
  )
  
  observeEvent(
    input$load_table_Stock_Derivative_Static,
    output$table_Stock_Derivative_Static <-
      renderDataTable({
        dbReadTable(sqlite, "Stock_Derivative_Static")
      })
  )
  
  observeEvent(
    input$load_table_Derivative_Instrument_Dynamic,
    output$table_Derivative_Instrument_Dynamic <-
      renderDataTable({
        dbReadTable(sqlite, "Derivative_Instrument_Dynamic")
      })
  )
  
  observeEvent(
    input$load_table_Economic_Resource_Risky_Income,
    output$table_Economic_Resource_Risky_Income <-
      renderDataTable({
        dbReadTable(sqlite, "Economic_Resource_Risky_Income")
      })
  )
  
  observeEvent(
    input$load_table_Economic_Resource_Fixed_Income,
    output$table_Economic_Resource_Fixed_Income <-
      renderDataTable({
        dbReadTable(sqlite, "Economic_Resource_Fixed_Income")
      })
  )
  
  observeEvent(input$load_table_Asset,
               output$table_Asset <- renderDataTable({
                 dbReadTable(sqlite, "Asset")
               }))
  
  observeEvent(input$load_table_Liability,
               output$table_Liability <- renderDataTable({
                 dbReadTable(sqlite, "Liability")
               }))
  
  observeEvent(input$load_table_Off_Balance,
               output$table_Off_Balance <- renderDataTable({
                 dbReadTable(sqlite, "Off_Balance")
               }))
}