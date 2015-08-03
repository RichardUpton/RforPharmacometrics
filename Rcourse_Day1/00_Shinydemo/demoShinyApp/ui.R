#Define UI for IbuprofenNeonates App
fixedPage(

    #Application Title and Logo
    fixedRow(
        column(4,
        img(src = "ACP_UniSA_Logo.png"), align = "left"),
        column(8,
        h2("Ibuprofen Dosing Regimens in Pre-Term Neonates for Patent Ductus Arteriosus"),
        h6("Reference: Gregoire N, Desfrere L, Roze JC, Kibleur Y, Koehne P. 
        Population pharmacokinetic analysis of Ibuprofen enantiomers in 
        preterm newborn infants. Journal of clinical pharmacology. 2008;48(12):1460-8."), align = "center")
        ),    #Brackets closing "fixedRow"
                
    hr(),    #Add a break with a horizontal line
    
    #Sidebar panel with widgets
    sidebarLayout(
        sidebarPanel(
            
            #Selection box for dosing regimen
            radioButtons("SELECT",
                        "Dosing Regimen:",
                        choices = list("Loading-Bolus-Bolus" = 1,
                                        "Loading-Continuous Infusion" = 2),
                        selected = 1),
                        
                br(),    #Add a blank break between widgets
                                        
                conditionalPanel(condition = "input.SELECT == 1",
                        
            #Slider input for dose
            #Bolus dose for Loading-Bolus-Bolus regimen
            sliderInput("BDOSE",
                        "Bolus Dose at 24 and 48 hours (mg/kg):",
                        min = 0,
                        max = 40,
                        value = 5,
                        step = 1,
                        width = 500)
                        
                ),    #Brackets closing "conditionalPanel"
                
                conditionalPanel(condition = "input.SELECT == 2",
                                        
            #Slider input for dose
            #Dose to be infused over 72 hours for Loading-Continuous Infusion regimen
            sliderInput("CDOSE",
                        "Dose continuously infused over 72 hours (mg/kg):",
                        min = 0,
                        max = 40,
                        value = 12,
                        step = 1,
                        width = 500),
                        
            #Numeric output for infusion rate
            textOutput("RATE")
                
                ),    #Brackets closing "conditionalPanel"
            
            br(),
            
            #Slider input for loading dose
            sliderInput("LDOSE",
                        "Loading Dose (mg/kg):",
                        min = 0,
                        max = 40,
                        value = 10,
                        step = 1,
                        width = 500),    
                        
            br(),
            
            #Slider input for age
            sliderInput("AGE",
                        "Postnatal Age (Hours):",
                        min = 0,
                        max = 72,
                        value = 12,
                        step = 6,
                        width = 500),
            
            br(),
            
            #Slider input for number of individuals
            sliderInput("n",
                        "Number of Individuals:",
                        min = 10,
                        max = 1000,
                        value = 10,
                        step = 10,
                        width = 500),
            
            br(),
            
            #Selection box for prediction intervals
            selectInput("CI",
                        "Prediction Intervals %:",
                        choices = list("80% - 10th and 90th percentiles" = 1,
                                        "90% - 5th and 95th percentiles" = 2,
                                        "95% - 2.5th and 97.5th percentiles" = 3),
                        selected = 1,
                        width = 500),
                        
            br(),
            
            #Button to initiate simulation
            submitButton("Simulate"),
        
        align = "left"),    #Brackets closing "sidebarPanel"
                            
        mainPanel(
        
            #Plot output for concentration-time profile
            plotOutput("plotCONC", height = 600, width = 800),
        
        align = "center")    #Brackets closing "mainPanel"
            
    )    #Brackets closing "sidebarLayout"

)    #Brackets closing "fixedPage"
