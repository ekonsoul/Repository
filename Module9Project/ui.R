
shinyUI(fluidPage(
  titlePanel(h3("Interactive Probability Distributions Gallery", align = "center")),
  
  sidebarLayout(
    sidebarPanel(
      helpText("Select a pdf to examine. 
          Pdf or Pmf Information will be loaded from existing distributions in the ", 
          span("stats", style = "color:blue"), " package."),
      
    
      selectInput("selectedpdf", label = h5("Select Distribution"), 
                  choices = list("Normal"      = 1,  "Beta"        = 2,  "Gamma"       = 3,
                                 "Cauchy"      = 4,  "Binomial"    = 5,  "Exponential" = 6,
                                 "Poison"      = 7,  "Log-Normal"  = 8,  "Weibull"     = 9, 
                                 "Student's t" = 10, "Chi-Squared" = 11, "Uniform"     = 12),
                                                                               selected = 1),
      
      conditionalPanel(condition = "input.selectedpdf == '1'",
                
                h5('Histogram Parameters'),
                sliderInput("N1",       label = "Number of Random Samples",
                                                 min = 100, max = 100000, value = 1000, step=100),       
                       
                sliderInput("Nbreaks1", label = "Number of Scaled Histogram bins", 
                                                 min = 20,  max = 600, value = 100),
                
                br(),
                h5('Normal Distribution Parameters'),
                sliderInput("mean",     label = "Mean", min = -10, max = 10, value = 0, step = 0.1),
                
                sliderInput("sd",       label = "Standard Deviation", 
                                                 min = 1e-10, max = 20, value = 3, step = 0.1),   
                
                br(),
                h5('Probability Calculator'),
                # Probability Interval Bounds
                sliderInput("x11", label = "Select Lower Bound x1", 
                                            min = -10, max = 10, value = -3, step = 0.01),
                sliderInput("x12", label = "Select Upper Bound x2>=x1", 
                                            min = -10, max = 10, value = 3, step = 0.01)
                
      ),

      conditionalPanel(condition = "input.selectedpdf == '2'",
                
                h5('Histogram Parameters'),   
                sliderInput("N2",       label = "Number of Random Samples",
                                                 min = 100, max = 100000, value = 1000, step=100),          
                    
                sliderInput("Nbreaks2", label = "Number of Scaled Histogram bins",
                                                 min = 20, max = 600, value = 100),
                
                br(),
                h5('Beta Distribution Parameters'),
                sliderInput("shape21",  label = "Beta pdf 1st shape parameter",
                                                 min = 1e-10, max = 5, value = 1/2, step = 0.1),
                sliderInput("shape22",  label = "Beta pdf 2nd shape parameter",
                                                 min = 1e-10, max = 5, value = 1/2, step = 0.1),
                    
                br(),
                h5('Probability Calculator'),
                # Probability Interval Bounds
                sliderInput("x21", label = "Select Lower Bound x1", 
                                            min = 0, max = 1, value = 0.2, step = 0.01),
                sliderInput("x22", label = "Select Upper Bound x2>=x1", 
                                            min = 0, max = 1, value = 0.8, step = 0.01)
   
      ),
   
      conditionalPanel(condition = "input.selectedpdf == '3'",
                
                h5('Histogram Parameters'),          
                sliderInput("N3",       label = "Number of Random Samples",
                                                 min = 100, max = 100000, value = 1000, step=100),                    
               
                sliderInput("Nbreaks3", label = "Number of Scaled Histogram bins",
                                                 min = 20, max = 600, value = 100),
                
                br(),
                h5('Gamma Distribution Parameters'),
                sliderInput("scale3",   label = "Gamma pdf scale parameter",
                                                 min = 1e-10, max = 3,  value = 1/4, step = 0.1),
                sliderInput("shape3",   label = "Gamma pdf shape parameter",
                                                 min = 1e-10, max = 10, value = 1/2, step = 0.1),
                
                br(),
                h5('Probability Calculator'),
                # Probability Interval Bounds
                sliderInput("x31", label = "Select Lower Bound x1", 
                                            min = 0, max = 20, value = 1, step = 0.01),
                sliderInput("x32", label = "Select Upper Bound x2>=x1", 
                                            min = 0, max = 20, value = 4, step = 0.01)
  
      ),
   
      conditionalPanel(condition = "input.selectedpdf == '4'",
                    
                h5('Histogram Parameters'),                      
                sliderInput("N4",       label = "Number of Random Samples",
                                                 min = 100, max = 100000, value = 1000, step=100),            
                sliderInput("Nbreaks4", label = "Number of Scaled Histogram bins",
                                                 min = 2000, max = 10000, value = 4000),
                    
                br(),
                h5('Cauchy Distribution Parameters'),
                sliderInput("location", label = "Cauchy pdf location parameter",
                                                 min = -10 ,  max = 10, value = 0, step = 0.1),
                sliderInput("scale4"  , label = "Cauchy pdf scale parameter",
                                                 min = 1e-10, max = 10, value = 1, step = 0.1),
                    
                br(),
                h5('Probability Calculator'),
                # Probability Interval Bounds
                sliderInput("x41", label = "Select Lower Bound x1", 
                                            min = -20, max = 20, value = -5, step = 0.01),
                sliderInput("x42", label = "Select Upper Bound x2>=x1", 
                                            min = -20, max = 20, value = 5, step = 0.01)
      ),
   
      conditionalPanel(condition = "input.selectedpdf == '5'",
                      
                h5('Histogram Parameters'),                      
                sliderInput("N5",      label = "Number of Random Samples",
                                                min = 100, max = 100000, value = 1000, step=100),                
                                
                br(),
                h5('Binomial Distribution Parameters'),
                sliderInput("prob",    label = "Probability of Success",
                                                min = 0, max = 1, value = 0.5, step = 0.1),
                sliderInput("ntrials", label = "Number of Bernoulli Trials",
                                                min = 1, max = 100, value = 10),
                                            
                br(),
                h5('Probability Calculator'),
                # Probability Interval Bounds
                sliderInput("x51", label = "Select Lower Bound x1", 
                                            min = 0, max = 100, value = 1, step = 0.01),
                sliderInput("x52", label = "Select Upper Bound x2>=x1", 
                                            min = 0, max = 100, value = 5, step = 0.01)
      ),
   
      conditionalPanel(condition = "input.selectedpdf == '6'",
                
                h5('Histogram Parameters'),                          
                sliderInput("N6",       label = "Number of Random Samples",
                                                 min = 100, max = 100000, value = 1000, step=100),            
                      
                sliderInput("Nbreaks6", label = "Number of Scaled Histogram bins",
                                                 min = 100, max = 1000, value = 200),
                
                br(),
                h5('Exponential Distribution Parameters'),
                sliderInput("rate",     label = "Rate (lambda) of Events",
                                                 min = 1e-10, max = 4, value = 1, step = 0.1),
                      
                br(),
                h5('Probability Calculator'),
                # Probability Interval Bounds
                sliderInput("x61", label = "Select Lower Bound x1", 
                                            min = 0, max = 10, value = 1, step = 0.01),
                sliderInput("x62", label = "Select Upper Bound x2>=x1", 
                                            min = 0, max = 10, value = 5, step = 0.01)
                                          
      ),
   
      conditionalPanel(condition = "input.selectedpdf == '7'",
                    
                h5('Histogram Parameters'),   
                sliderInput("N7",       label = "Number of Random Samples",
                                            min = 100, max = 100000, value = 1000, step=100),         
                    
                br(),
                h5('Poisson Distribution Parameters'),
                sliderInput("lambda", label = "Mean (lambda) of Poisson pmf",
                                                   min = 0, max = 40, value = 10, step = 0.1),
             
                br(),
                h5('Probability Calculator'),
                # Probability Interval Bounds
                sliderInput("x71", label = "Select Lower Bound x1", 
                                    min = 0, max = 80, value = 4, step = 0.01),
                sliderInput("x72", label = "Select Upper Bound x2>=x1", 
                                    min = 0, max = 80, value = 12, step = 0.01)
                    
      ),
   
      conditionalPanel(condition = "input.selectedpdf == '8'",
                    
                h5('Histogram Parameters'),   
                sliderInput("N8",       label = "Number of Random Samples",
                                                 min = 100, max = 100000, value = 1000, step=100),            
                
                sliderInput("Nbreaks8", label = "Number of Scaled Histogram bins",
                                                 min = 100, max = 20000, value = 200),
                
                br(),
                h5('Log-Normal Distribution Parameters'),
                sliderInput("logmean",  label = "Mean on log scale", 
                                                 min = 1e-10, max = 4, value = 0.5, step = 0.1),
                    
                sliderInput("logsd",    label = "Standard Deviation on log scale", 
                                                 min = 1e-10, max = 2, value = 0.25, step = 0.1),
                                        
                br(),
                h5('Probability Calculator'),
                # Probability Interval Bounds
                sliderInput("x81", label = "Select Lower Bound x1", 
                                            min = 0, max = 50, value = 1, step = 0.01),
                sliderInput("x82", label = "Select Upper Bound x2>=x1", 
                                            min = 0, max = 50, value = 4, step = 0.01)
      ),
   
      conditionalPanel(condition = "input.selectedpdf == '9'",
                    
                h5('Histogram Parameters'),   
                sliderInput("N9",       label = "Number of Random Samples",
                                                 min = 100, max = 100000, value = 1000, step=100),          
                       
                sliderInput("Nbreaks9", label = "Number of Scaled Histogram bins",
                                                 min = 20, max = 600, value = 100),
                
                br(),
                h5('Weibull Distribution Parameters'),            
                
                sliderInput("scale9",   label = "Weibull pdf scale parameter",
                                                 min = 1e-10, max = 20, value = 8, step=0.1),
                sliderInput("shape9",   label = "Weibull pdf shape parameter",
                                                 min = 1e-10, max = 20, value = 4, step=0.1),
                    
                br(),
                h5('Probability Calculator'),
                # Probability Interval Bounds
                sliderInput("x91", label = "Select Lower Bound x1", 
                                            min = 0, max = 20, value = 5, step = 0.01),
                sliderInput("x92", label = "Select Upper Bound x2>=x1", 
                                            min = 0, max = 20, value = 10, step = 0.01)
                    
      ),
      
      conditionalPanel(condition = "input.selectedpdf == '10'",
                    
                h5('Histogram Parameters'),
                sliderInput("N10",       label = "Number of Random Samples",
                                                  min = 100, max = 100000, value = 1000, step=100),                
                   
                sliderInput("Nbreaks10", label = "Number of Scaled Histogram bins",
                                                  min = 20, max = 1000, value = 100),
                br(),
                h5('Student\'s t-Distribution Parameters'),
                sliderInput("Ndegrees",  label = "Degrees of Freedom",
                                                  min = 1, max = 20, value = 5),
                sliderInput("delta",     label = "Non-Centrality Parameter",
                                                  min = -5, max = 5, value = 0, step = 0.1),
                    
                br(),
                h5('Probability Calculator'),
                # Probability Interval Bounds
                sliderInput("x101", label = "Select Lower Bound x1", 
                                             min = -10, max = 10, value = -3, step = 0.01),
                sliderInput("x102", label = "Select Upper Bound x2>=x1", 
                                             min = -10, max = 10, value = 3, step = 0.01)
                                              
      ),
      
      conditionalPanel(condition = "input.selectedpdf == '11'",
                       
                h5('Histogram Parameters'),
                sliderInput("N11",       label = "Number of Random Samples",
                                                  min = 100, max = 100000, value = 1000, step=100),                
                       
                sliderInput("Nbreaks11", label = "Number of Scaled Histogram bins",
                                                  min = 20, max = 1000, value = 100),
                       
                br(),
                h5('Chi-Squared Distribution Parameters'),
                sliderInput("Ndegrees11",        label = "Degrees of Freedom",
                                                  min = 1,  max = 20, value = 5, step = 0.1),
                sliderInput("ncp11",             label = "Non-Centrality Parameter",
                                                  min = 0,  max = 10, value = 0, step = 0.1),
                       
                br(),
                h5('Probability Calculator'),
                # Probability Interval Bounds
                sliderInput("x111", label = "Select Lower Bound x1", 
                                     min = 0, max = 40, value = 1, step = 0.01),
                sliderInput("x112", label = "Select Upper Bound x2>=x1", 
                                     min = 0, max = 40, value = 3, step = 0.01)
                       
      ),
      
      conditionalPanel(condition = "input.selectedpdf == '12'",
                       
                h5('Histogram Parameters'),
                sliderInput("N12",       label = "Number of Random Samples",
                                                  min = 100, max = 100000, value = 1000, step=100),       
                       
                sliderInput("Nbreaks12", label = "Number of Scaled Histogram bins",
                                                  min =  20, max = 1000, value = 100),
                
                br(),
                h5('Uniform Distribution Parameters'),                         
                sliderInput("alpha",     label = "Lower Bound",
                                                  min = -20, max = 0, value = 0, step = 0.1),
                sliderInput("beta",      label = "Upper Bound",
                                                  min = 0, max = 20, value = 1, step = 0.1),
                       
                br(),
                h5('Probability Calculator'),
                # Probability Interval Bounds
                sliderInput("x121", label = "Select Lower Bound x1", 
                                             min = -20, max = 20, value = 0, step = 0.01),
                sliderInput("x122", label = "Select Upper Bound x2>=x1", 
                                             min = -20, max = 20, value = 1, step = 0.01)
                       
      ),
      
      textOutput("text4"),
      textOutput("text5")
            
    ),  # End of SideBarPanel()
    
      mainPanel(# plotOutput("plotpdf", width = "100%", height = "300px"),
                # plotOutput("plotcdf", width = "100%", height = "300px"),
                plotOutput("plotpdf"),
                plotOutput("plotcdf"),
                textOutput("text1"),
                textOutput("text2"),
                textOutput("text3"))
                
        
)))