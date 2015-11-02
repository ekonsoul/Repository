# server.R
library(shiny)
library(stats)

shinyServer(function(input, output) {
    
  output$plotpdf <- renderPlot({
     
     switch(input$selectedpdf,
                       
        '1' = { # Plot Histogram (density) of Gaussian Generated Random Points.
                xmin = -10; xmax = 10; xinc = 0.05;
                hist(rnorm(input$N1,input$mean,input$sd), breaks = input$Nbreaks1, 
                                    freq = FALSE, xlim = c(xmin,xmax), xlab = "x", main = NULL)
                
                # Plot Gaussian pdf 
                # Create x-coordinate grid.
                x <- seq(from=xmin, to=xmax, by=xinc);
                lines(x,dnorm(x,input$mean,input$sd, log = FALSE), type = "l", col = 'red', lwd = 3, pin = c(5,3.5))
                title("Normal pdf", sub = NULL, cex.main = 1.5, font.main= 6, col.main= "blue")
                
                # Plot Probability Interval Bounds.
                x1 <- input$x11
                lines(c(x1,x1), c(0,100),col = "cyan",lwd=2)
                x2 <- input$x12
                lines(c(x2,x2), c(0,100),col = "magenta",lwd=2)
                
                # Calculate and Print Probability.
                cdf2 <- pnorm(x2,input$mean,input$sd,log = FALSE)
                cdf1 <- pnorm(x1,input$mean,input$sd,log = FALSE)
                prob <- cdf2 - cdf1 
                
                br();
                output$text1 <- renderText({ 
                  paste("You have selected",input$Nbreaks1,"scaled histogram bins." ) 
                })    
                
                output$text2 <- renderText({ 
                  paste("You have chosen",input$mean,"as mean value." ) 
                })     
                
                output$text3 <- renderText({ 
                  paste("You have chosen",format(input$sd,digits=4),"as standard deviation." ) 
                })
                
        },
        
        
        '2' = { # Plot Histogram (density) of Beta Generated Random Points.
                xmin = 0; xmax = 1; xinc = 0.005;      
                hist(rbeta(input$N2,input$shape21,input$shape22, ncp=0), breaks = input$Nbreaks2, 
                                           freq = FALSE, xlim = c(xmin,xmax), xlab = "x", main = NULL)
          
                # Plot Beta pdf 
                # Create x-coordinate grid.
                x <- seq(from=xmin, to=xmax, by=xinc);
                lines(x,dbeta(x,input$shape21,input$shape22, ncp=0), type = "l", col = 'red', lwd = 3)
                title("Beta pdf", sub = NULL, cex.main = 1.5,  font.main= 6, col.main= "blue")
                
                # Plot Probability Interval Bounds.
                x1 <- input$x21
                lines(c(x1,x1), c(0,100),col = "cyan",lwd=2)
                x2 <- input$x22
                lines(c(x2,x2), c(0,100),col = "magenta",lwd=2)
                
                # Calculate and Print Probability.
                cdf2 <- pbeta(x2,input$shape21,input$shape22,ncp=0)
                cdf1 <- pbeta(x1,input$shape21,input$shape22,ncp=0)
                prob <- cdf2 - cdf1 
                                
                br();
                output$text1 <- renderText({ 
                  paste("You have selected",input$Nbreaks2,"scaled histogram bins." ) 
                })    
                
                output$text2 <- renderText({ 
                  paste("You have chosen",round(input$shape21,4),"as first shape parameter." ) 
                })
                
                output$text3 <- renderText({ 
                  paste("You have chosen",round(input$shape22,4),"as second shape parameter." ) 
                })     
                
        },   # end of case 2 code block.
        
        
        '3' = { # Plot Histogram (density) of Gamma Generated Random Points.
                xmin = 0; xmax = 20; xinc = 0.05;      
                hist(rgamma(input$N3,input$shape3,input$scale3), breaks = input$Nbreaks3, 
                                   freq = FALSE, xlim = c(xmin,xmax), xlab = "x", main = NULL)
          
                # Plot Gamma pdf 
                # Create x-coordinate grid.
                x <- seq(from=xmin, to=xmax, by=xinc);
                lines(x,dgamma(x,input$shape3,input$scale3), type = "l", col = 'red', lwd = 3)
                title("Gamma pdf", sub = NULL, cex.main = 1.5,  font.main= 6, col.main= "blue")
          
                # Plot Probability Interval Bounds.
                x1 <- input$x31
                lines(c(x1,x1), c(0,100),col = "cyan",lwd=2)
                x2 <- input$x32
                lines(c(x2,x2), c(0,100),col = "magenta",lwd=2)
                
                # Calculate and Print Probability.
                cdf2 <- pgamma(x2,input$shape3,input$scale3)
                cdf1 <- pgamma(x1,input$shape3,input$scale3)
                prob <- cdf2 - cdf1 
                
                br();
                output$text1 <- renderText({ 
                paste("You have selected",input$Nbreaks3,"scaled histogram bins." ) 
                })    
          
                output$text2 <- renderText({ 
                  paste("You have chosen",round(input$scale3,4),"as scale parameter." ) 
                })
          
                output$text3 <- renderText({ 
                  paste("You have chosen",round(input$shape3,4),"as shape parameter." ) 
                })     
          
        },   # end of case 3 code block.
        
        
        '4' = { # Plot Histogram (density) of Cauchy Generated Random Points.
                xmin = -20; xmax = 20; xinc = 0.05;      
                a <- rcauchy(input$N4,input$location,input$scale4);
                b <- a[abs(a)<=1000];
                hist(b,breaks = input$Nbreaks4, freq = FALSE, xlim = c(xmin,xmax)
                                            , xlab = "x", main = NULL)
          
                # Plot Cauchy pdf 
                # Create x-coordinate grid.
                x <- seq(from=xmin, to=xmax, by=xinc);
                lines(x,dcauchy(x,input$location,input$scale4), type = "l", col = 'red', lwd = 3)
                title("Cauchy pdf", sub = NULL, cex.main = 1.5,  font.main= 6, col.main= "blue")
                
                # Plot Probability Interval Bounds.
                x1 <- input$x41
                lines(c(x1,x1), c(0,100),col = "cyan",lwd=2)
                x2 <- input$x42
                lines(c(x2,x2), c(0,100),col = "magenta",lwd=2)
                
                # Calculate and Print Probability.
                cdf2 <- pcauchy(x2,input$location,input$scale4)
                cdf1 <- pcauchy(x1,input$location,input$scale4)
                prob <- cdf2 - cdf1 
          
                br();
                output$text1 <- renderText({ 
                  paste("You have selected",input$Nbreaks4,"scaled histogram bins." ) 
                })    
          
                output$text2 <- renderText({ 
                  paste("You have chosen",round(input$location,4),"as location parameter." ) 
                })
          
                output$text3 <- renderText({ 
                  paste("You have chosen",round(input$scale4,4),"as scale parameter." ) 
                })     
          
        },   # end of case 4 code block.
        
        
        '5' = { # Plot Histogram (density) of Binomially Generated Random Points.
                  xmin = 0; xmax = 100; xinc = 1;      
                  hist(rbinom(input$N5,input$ntrials,input$prob),breaks = seq(xmin,xmax,1), 
                                   freq = FALSE, xlim = c(xmin,xmax), xlab = "x", main = NULL)
                
                  # Plot Binomial pmf 
                  # Create x-coordinate grid.
                  x <- seq(from=xmin, to=xmax, by=xinc);
                  points(x,dbinom(x,input$ntrials,input$prob), col = 'red', pch = 19)
                  title("Binomial pmf", sub = NULL, cex.main = 1.5, font.main= 6, col.main= "blue")
                  
                  # Plot Probability Interval Bounds.
                  x1 <- input$x51
                  lines(c(x1,x1), c(0,100),col = "cyan",lwd=2)
                  x2 <- input$x52
                  lines(c(x2,x2), c(0,100),col = "magenta",lwd=2)
                  
                  # Calculate and Print Probability.
                  cdf2 <- pbinom(x2,input$ntrials,input$prob)
                  cdf1 <- pbinom(x1,input$ntrials,input$prob)
                  prob <- cdf2 - cdf1 
          
                  br();
                  output$text1 <- renderText({ 
                    paste("Scaled histogram bins is equal to the integer sequence xmin:1:xmax." ) 
                  })    
          
                  output$text2 <- renderText({ 
                    paste("You have chosen",round(input$prob,4),"as probability of success." ) 
                  })
          
                  output$text3 <- renderText({ 
                    paste("You have chosen",input$ntrials,"as Number of Trials." ) 
                  })     
          
        },   # end of case 5 code block.
        
        
        '6' = { # Plot Histogram (density) of Exponentially Generated Random Points.
                xmin = 0; xmax = 10; xinc = 0.05;      
                hist(rexp(input$N6,input$rate),breaks = input$Nbreaks6, 
                                      freq = FALSE, xlim = c(xmin,xmax), xlab = "x", main = NULL)
          
                # Plot Exponential pdf 
                # Create x-coordinate grid.
                x <- seq(from=xmin, to=xmax, by=xinc);
                lines(x,dexp(x,input$rate), type = "l", col = 'red', lwd = 3)
                title("Exponential pdf", sub = NULL, cex.main = 1.5, font.main= 6, col.main= "blue")
                
                # Plot Probability Interval Bounds.
                x1 <- input$x61
                lines(c(x1,x1), c(0,100),col = "cyan",lwd=2)
                x2 <- input$x62
                lines(c(x2,x2), c(0,100),col = "magenta",lwd=2)
                
                # Calculate and Print Probability.
                cdf2 <- pexp(x2,input$rate)
                cdf1 <- pexp(x1,input$rate)
                prob <- cdf2 - cdf1 
                          
                br();
                output$text1 <- renderText({ 
                  paste("You have selected",input$Nbreaks6,"scaled histogram bins." ) 
                })    
          
                output$text2 <- renderText({ 
                  paste("You have chosen",round(input$rate,4),"as rate parameter (lambda)." ) 
                })
          
                output$text3 <- renderText({ 
                })     

          },   # end of case 6 code block.
        
        
        '7' = { # Plot Histogram (density) of Poisson Generated Random Points.
                # xmin = 0; xmax = input$xmax7; xinc = 1;    
                xmin = 0; xmax = 80; xinc = 1;    
                hist(rpois(input$N7,input$lambda),breaks = seq(xmin,xmax,1), 
                                    freq = FALSE, xlim = c(xmin,xmax), xlab = "x", main = NULL)
          
                # Plot Poisson pmf 
                # Create x-coordinate grid.
                x <- seq(from=xmin, to=xmax, by=xinc);
                points(x,dpois(x,input$lambda, log = FALSE), col = 'red', pch = 19)
                title("Poisson pmf", sub = NULL, cex.main = 1.5, font.main= 6, col.main= "blue")
                
                # Plot Probability Interval Bounds.
                x1 <- input$x71
                lines(c(x1,x1), c(0,100),col = "cyan",lwd=2)
                x2 <- input$x72
                lines(c(x2,x2), c(0,100),col = "magenta",lwd=2)
                
                # Calculate and Print Probability.
                cdf2 <- ppois(x2,input$lambda)
                cdf1 <- ppois(x1,input$lambda)
                prob <- cdf2 - cdf1 
          
                br();
                output$text1 <- renderText({ 
                  paste("Scaled histogram bins is equal to the integer sequence xmin:1:xmax." ) 
                })    
          
                output$text2 <- renderText({ 
                  paste("You have chosen",round(input$lambda,4),"as mean of pmf." ) 
                })
          
                output$text3 <- renderText({ 
               #   paste("You have chosen",input$xmax7,"as maximum x value." )
                })     
        
        },   # end of case 7 code block.
        
        
        '8' = { # Plot Histogram (density) of Log-Normally Generated Random Points.
                xmin = 0; xmax = 50; xinc = 0.05;
                hist(rlnorm(input$N8,input$logmean,input$logsd), breaks = input$Nbreaks8, 
                                      freq = FALSE, xlim = c(xmin,xmax), xlab = "x", main = NULL)
          
                # Plot Log Normal pdf 
                # Create x-coordinate grid.
                x <- seq(from=xmin, to=xmax, by=xinc);
                lines(x,dlnorm(x,input$logmean,input$logsd, log = FALSE), type = "l", col = 'red', lwd = 3)
                title("Log-Normal pdf", sub = NULL, cex.main = 1.5, font.main= 6, col.main= "blue")
          
                # Plot Probability Interval Bounds.
                x1 <- input$x81
                lines(c(x1,x1), c(0,100),col = "cyan",   lwd=2)
                x2 <- input$x82
                lines(c(x2,x2), c(0,100),col = "magenta",lwd=2)
                
                # Calculate and Print Probability.
                cdf2 <- plnorm(x2,input$logmean,input$logsd, log = FALSE)
                cdf1 <- plnorm(x1,input$logmean,input$logsd, log = FALSE)
                prob <- cdf2 - cdf1 
                
                br();
                output$text1 <- renderText({ 
                   paste("You have selected",input$Nbreaks8,"scaled histogram bins." ) 
                })    
          
                output$text2 <- renderText({ 
                   paste("You have chosen",round(input$logmean,4),"as log-mean value." ) 
                })     
          
                output$text3 <- renderText({ 
                   paste("You have chosen",round(input$logsd,4),"as log-standard deviation." ) 
                })
                
        }, # end of case 8 code block.
      
        
        '9' = { # Plot Histogram (density) of Weibull Generated Random Points.
                xmin = 0; xmax = 20; xinc = 0.05;      
                hist(rweibull(input$N9,input$shape9,input$scale9), breaks = input$Nbreaks9, 
                       freq = FALSE, xlim = c(xmin,xmax), xlab = "x", main = NULL)
          
                # Plot Weibull pdf 
                # Create x-coordinate grid.
                x <- seq(from=xmin, to=xmax, by=xinc);
                lines(x,dweibull(x,input$shape9,input$scale9), type = "l", col = 'red', lwd = 3)
                title("Weibull pdf", sub = NULL, cex.main = 1.5,  font.main= 6, col.main= "blue")
          
                # Plot Probability Interval Bounds.
                x1 <- input$x91
                lines(c(x1,x1), c(0,100),col = "cyan",   lwd=2)
                x2 <- input$x92
                lines(c(x2,x2), c(0,100),col = "magenta",lwd=2)
                
                # Calculate and Print Probability.
                cdf2 <- pweibull(x2,input$shape9,input$scale9)
                cdf1 <- pweibull(x1,input$shape9,input$scale9)
                prob <- cdf2 - cdf1                 
                
                br();
                output$text1 <- renderText({ 
                  paste("You have selected",input$Nbreaks9,"scaled histogram bins." ) 
                })    
            
                output$text2 <- renderText({ 
                    paste("You have chosen",round(input$scale9,4),"as scale parameter." ) 
                })
          
                output$text3 <- renderText({ 
                  paste("You have chosen",round(input$shape9,4),"as shape parameter." ) 
                })     
          
        }, # end of case 9 code block.  
      
      '10' = { # Plot Histogram (density) of Central Student's t Generated Random Points.
                 xmin = -10; xmax = 10; xinc = 0.05;      
                 hist(rt(input$N10,input$Ndegrees, input$delta), breaks = input$Nbreaks10, 
                           freq = FALSE, xlim = c(xmin,xmax), xlab = "x", main = NULL)
          
               # Plot Central Student's t-distribution pdf 
               # Create x-coordinate grid.
                 x <- seq(from=xmin, to=xmax, by=xinc);
                 lines(x,dt(x,input$Ndegrees, input$delta,log = FALSE), type = "l", col = 'red', lwd = 3)
                 title("Student's t-distribution pdf", sub = NULL, cex.main = 1.5,  font.main= 6, col.main= "blue")
          
               # Plot Probability Interval Bounds.
                 x1 <- input$x101
                 lines(c(x1,x1), c(0,100),col = "cyan",   lwd=2)
                 x2 <- input$x102
                 lines(c(x2,x2), c(0,100),col = "magenta",lwd=2)
               
               # Calculate and Print Probability.
                 cdf2 <- pt(x2,input$Ndegrees,input$delta,log = FALSE)
                 cdf1 <- pt(x1,input$Ndegrees,input$delta,log = FALSE)
                 prob <- cdf2 - cdf1   
               
                 br();
                 output$text1 <- renderText({ 
                     paste("You have selected",input$Nbreaks10,"scaled histogram bins." ) 
                 })    
          
                 output$text2 <- renderText({ 
                    paste("You have chosen",input$Ndegrees,"degrees of freedom." ) 
                 })
          
                 output$text3 <- renderText({ 
                    paste("You have chosen",round(input$delta,4),"as non-centrality parameter." ) 
                 })     
                     
      }, # end of case 10 code block.    
        
      '11' = { # Plot Histogram (density) of Chi-Squared Generated Random Points.
               xmin = 0; xmax = 40; xinc = 0.05;      
               hist(rchisq(input$N11,input$Ndegrees11, input$ncp11), breaks = input$Nbreaks11, 
               freq = FALSE, xlim = c(xmin,xmax), xlab = "x", main = NULL)
        
               # Plot Chi-Squared pdf 
               # Create x-coordinate grid.
               x <- seq(from=xmin, to=xmax, by=xinc);
               lines(x,dchisq(x,input$Ndegrees11, input$ncp11,log = FALSE), type = "l", col = 'red', lwd = 3)
               title("Non-Central Chi-Squared pdf", sub = NULL, cex.main = 1.5,  font.main= 6, col.main= "blue")
        
               # Plot Probability Interval Bounds.
               x1 <- input$x111
               lines(c(x1,x1), c(0,100),col = "cyan",   lwd=2)
               x2 <- input$x112
               lines(c(x2,x2), c(0,100),col = "magenta",lwd=2)
        
               # Calculate and Print Probability.
               cdf2 <- pchisq(x2,input$Ndegrees11,input$ncp11,log = FALSE)
               cdf1 <- pchisq(x1,input$Ndegrees11,input$ncp11,log = FALSE)
               prob <- cdf2 - cdf1   
        
               br();
               output$text1 <- renderText({ 
                    paste("You have selected",input$Nbreaks11,"scaled histogram bins." ) 
               })    
        
               output$text2 <- renderText({ 
                    paste("You have chosen",input$Ndegrees11,"degrees of freedom." ) 
               })
        
               output$text3 <- renderText({ 
                    paste("You have chosen",round(input$ncp11,4),"as non-centrality parameter." ) 
               })     
      },
      
      '12' = { # Plot Histogram (density) of Uniformly Distributed Random Points.
               xmin = -20; xmax = 20; xinc = 0.05;      
               hist(runif(input$N12,input$alpha,input$beta), breaks = input$Nbreaks12, 
                                         freq = FALSE, xlim = c(xmin,xmax), xlab = "x", main = NULL)
        
               # Plot Central Student's t-distribution pdf 
               # Create x-coordinate grid.
               x <- seq(from=xmin, to=xmax, by=xinc);
               lines(x,dunif(x,input$alpha, input$beta), type = "l", col = 'red', lwd = 3)
               title("Uniform pdf", sub = NULL, cex.main = 1.5,  font.main= 6, col.main= "blue")
        
               # Plot Probability Interval Bounds.
               x1 <- input$x121
               lines(c(x1,x1), c(0,100),col = "cyan",   lwd=2)
               x2 <- input$x122
               lines(c(x2,x2), c(0,100),col = "magenta",lwd=2)
        
               # Calculate and Print Probability.
               cdf2 <- punif(x2,input$alpha,input$beta)
               cdf1 <- punif(x1,input$alpha,input$beta)
               prob <- cdf2 - cdf1   
        
               br();
               output$text1 <- renderText({ 
                               paste("You have selected",input$Nbreaks12,"scaled histogram bins." ) 
               })    
        
               output$text2 <- renderText({ 
                               paste("You have chosen",round(input$alpha,3),"as lower limit of the distribution." ) 
               })
        
               output$text3 <- renderText({ 
                               paste("You have chosen",round(input$beta,3),"as upper limit of the distribution." ) 
               })     
        }
              
     ) # end of switch()
    
     output$text4 <- renderText({paste("Probability of x in [",round(x1,3)," ",round(x2,3),"] = ", round(prob,3))})                                 
     output$text5 <- renderText({paste("is equal to the vertical blue line segment length.")})                                         
     
  }) # End of renderPlot.
  
  
  output$plotcdf <- renderPlot({
        
     switch(input$selectedpdf,
           
           '1' = { # Plot Gaussian cdf 
                   # Create x-coordinate grid.
                   xmin = -10; xmax = 10; xinc = 0.05;
                   x <- seq(from=xmin, to=xmax, by=xinc); par("tck" = 1); 
                   plot(x,pnorm(x,input$mean,input$sd,log = FALSE), type = "l", col = 'green', lwd = 2, ylab = "Fx(x)")
                   title("Normal cdf", sub = NULL, cex.main = 1.5, font.main= 6, col.main= "blue")
                   
                   # Plot Probability Interval Bounds
                   x1 <- input$x11
                   cdf1 <- pnorm(x1,input$mean,input$sd,log = FALSE)
                   lines(c(x1,x1), c(0,cdf1),col = "cyan",lwd=2)
                   x2 <- input$x12
                   cdf2 <- pnorm(x2,input$mean,input$sd,log = FALSE)
                   lines(c(x2,x2), c(0,cdf2),col = "magenta",lwd=2)
                   
                   # Plot Horizontal cdf Lines.
                   lines(c(xmin,x1), c(cdf1,cdf1),col = "cyan",lwd=2)             
                   lines(c(xmin,x2), c(cdf2,cdf2),col = "magenta",lwd=2)
                   
                   # Plot Line Segment equal to Probability Measure
                   lines(c(xmin,xmin), c(cdf1,cdf2),col = "royalblue",lwd=8)             
           },
           
           '2' = { # Plot Beta cdf 
                   # Create x-coordinate grid.
                   xmin = 0; xmax = 1; xinc = 0.01;
                   x <- seq(from=xmin, to=xmax, by=xinc); par("tck" = 1);
                   plot(x,pbeta(x,input$shape21,input$shape22, ncp=0), type = "l", col = 'green', lwd = 2, ylab = "Fx(x)")
                   title("Beta cdf", sub = NULL, cex.main = 1.5,  font.main= 6, col.main= "blue")
                   
                   # Plot Probability Interval Bounds
                   x1 <- input$x21
                   cdf1 <- pbeta(x1,input$shape21,input$shape22,ncp=0)
                   lines(c(x1,x1), c(0,cdf1),col = "cyan",lwd=2)
                   x2 <- input$x22
                   cdf2 <- pbeta(x2,input$shape21,input$shape22,ncp=0)
                   lines(c(x2,x2), c(0,cdf2),col = "magenta",lwd=2)
                   
                   # Plot Horizontal cdf Lines.
                   lines(c(xmin,x1), c(cdf1,cdf1),col = "cyan",lwd=2)             
                   lines(c(xmin,x2), c(cdf2,cdf2),col = "magenta",lwd=2)
                   
                   # Plot Line Segment equal to Probability Measure
                   lines(c(xmin,xmin), c(cdf1,cdf2),col = "royalblue",lwd=8)  
                   
           },   # end of case 2 code block.
           
           
           '3' = { # Plot Gamma cdf 
                   # Create x-coordinate grid.
                   xmin = 0; xmax = 20; xinc = 0.05;
                   x <- seq(from=xmin, to=xmax, by=xinc); par("tck" = 1);
                   plot(x,pgamma(x,input$shape3,input$scale3), type = "l", col = 'green', lwd = 2, ylab = "Fx(x)")
                   title("Gamma cdf", sub = NULL, cex.main = 1.5,  font.main= 6, col.main= "blue")
                   
                   # Plot Probability Interval Bounds
                   x1 <- input$x31
                   cdf1 <- pgamma(x1,input$shape3,input$scale3)
                   lines(c(x1,x1), c(0,cdf1),col = "cyan",lwd=2)
                   x2 <- input$x32
                   cdf2 <- pgamma(x2,input$shape3,input$scale3)
                   lines(c(x2,x2), c(0,cdf2),col = "magenta",lwd=2)
                   
                   # Plot Horizontal cdf Lines.
                   lines(c(xmin,x1), c(cdf1,cdf1),col = "cyan",lwd=2)             
                   lines(c(xmin,x2), c(cdf2,cdf2),col = "magenta",lwd=2)
                   
                   # Plot Line Segment equal to Probability Measure
                   lines(c(xmin,xmin), c(cdf1,cdf2),col = "royalblue",lwd=8)  
             
           },   # end of case 3 code block.
           
           
           '4' = { # Plot Cauchy cdf 
                   # Create x-coordinate grid.
                   xmin = -20; xmax = 20; xinc = 0.05;
                   x <- seq(from=xmin, to=xmax, by=xinc); par("tck" = 1);
                   plot(x,pcauchy(x,input$location,input$scale4), type = "l", col = 'green', lwd = 2, ylab = "Fx(x)")
                   title("Cauchy cdf", sub = NULL, cex.main = 1.5,  font.main= 6, col.main= "blue")
                   
                   # Plot Probability Interval Bounds
                   x1 <- input$x41
                   cdf1 <- pcauchy(x1,input$location,input$scale4)
                   lines(c(x1,x1), c(0,cdf1),col = "cyan",lwd=2)
                   x2 <- input$x42
                   cdf2 <- pcauchy(x2,input$location,input$scale4)
                   lines(c(x2,x2), c(0,cdf2),col = "magenta",lwd=2)
                   
                   # Plot Horizontal cdf Lines.
                   lines(c(xmin,x1), c(cdf1,cdf1),col = "cyan",lwd=2)             
                   lines(c(xmin,x2), c(cdf2,cdf2),col = "magenta",lwd=2)
                   
                   # Plot Line Segment equal to Probability Measure
                   lines(c(xmin,xmin), c(cdf1,cdf2),col = "royalblue",lwd=8)  
             
           },   # end of case 4 code block.
           
           
           '5' = { # Plot Binomial cdf 
                   # Create x-coordinate grid.
                   xmin = 0; xmax = 100; xinc = 1;
                   x <- seq(from=xmin, to=xmax, by=xinc);  par("tck" = 1);
                   plot(x,pbinom(x,input$ntrials,input$prob), type = "s", col = 'green', lwd = 2, ylab = "Fx(x)")
                   title("Binomial cdf", sub = NULL, cex.main = 1.5, font.main= 6, col.main= "blue")
                   
                   # Plot Probability Interval Bounds
                   x1 <- input$x51
                   cdf1 <- pbinom(x1,input$ntrials,input$prob)
                   lines(c(x1,x1), c(0,cdf1),col = "cyan",lwd=2)
                   x2 <- input$x52
                   cdf2 <- pbinom(x2,input$ntrials,input$prob)
                   lines(c(x2,x2), c(0,cdf2),col = "magenta",lwd=2)
                   
                   # Plot Horizontal cdf Lines.
                   lines(c(xmin,x1), c(cdf1,cdf1),col = "cyan",lwd=2)             
                   lines(c(xmin,x2), c(cdf2,cdf2),col = "magenta",lwd=2)
                   
                   # Plot Line Segment equal to Probability Measure
                   lines(c(xmin,xmin), c(cdf1,cdf2),col = "royalblue",lwd=8)  
             
           },   # end of case 5 code block.
           
           
           '6' = { # Plot Exponential cdf 
                   # Create x-coordinate grid.  
                   xmin = 0; xmax = 10; xinc = 0.05;      
                   x <- seq(from=xmin, to=xmax, by=xinc); par("tck" = 1);
                   plot(x,pexp(x,input$rate), type = "l", col = 'green', lwd = 2, ylab = "Fx(x)")
                   title("Exponential cdf", sub = NULL, cex.main = 1.5, font.main= 6, col.main= "blue")
                   
                   # Plot Probability Interval Bounds
                   x1 <- input$x61
                   cdf1 <- pexp(x1,input$rate)
                   lines(c(x1,x1), c(0,cdf1),col = "cyan",lwd=2)
                   x2 <- input$x62
                   cdf2 <- pexp(x2,input$rate)
                   lines(c(x2,x2), c(0,cdf2),col = "magenta",lwd=2)
                   
                   # Plot Horizontal cdf Lines.
                   lines(c(xmin,x1), c(cdf1,cdf1),col = "cyan",lwd=2)             
                   lines(c(xmin,x2), c(cdf2,cdf2),col = "magenta",lwd=2)
                   
                   # Plot Line Segment equal to Probability Measure
                   lines(c(xmin,xmin), c(cdf1,cdf2),col = "royalblue",lwd=8)  
             
           },   # end of case 6 code block.
           
           
           '7' = { # Plot Poisson cdf 
                   # Create x-coordinate grid.
                   # xmin = 0; xmax = input$xmax7; xinc = 1;    
                   xmin = 0; xmax = 80; xinc = 1;    
                   x <- seq(from=xmin, to=xmax, by=xinc); par("tck"=1);
                   plot(x,ppois(x,input$lambda, log = FALSE),type = "s", col = 'green', lwd=2, ylab = "Fx(x)")
                   title("Poisson cdf", sub = NULL, cex.main = 1.5, font.main= 6, col.main= "blue")
                   
                   # Plot Probability Interval Bounds
                   x1 <- input$x71
                   cdf1 <- ppois(x1,input$lambda)
                   lines(c(x1,x1), c(0,cdf1),col = "cyan",lwd=2)
                   x2 <- input$x72
                   cdf2 <- ppois(x2,input$lambda)
                   lines(c(x2,x2), c(0,cdf2),col = "magenta",lwd=2)
                   
                   # Plot Horizontal cdf Lines.
                   lines(c(xmin,x1), c(cdf1,cdf1),col = "cyan",lwd=2)             
                   lines(c(xmin,x2), c(cdf2,cdf2),col = "magenta",lwd=2)
                   
                   # Plot Line Segment equal to Probability Measure
                   lines(c(xmin,xmin), c(cdf1,cdf2),col = "royalblue",lwd=8)  
             
           },   # end of case 7 code block.
           
           
           '8' = { # Plot Log Normal cdf 
                   # Create x-coordinate grid.  
                   xmin = 0; xmax = 50; xinc = 0.05;
                   x <- seq(from=xmin, to=xmax, by=xinc); par("tck" = 1);
                   plot(x,plnorm(x,input$logmean,input$logsd, log = FALSE), type = "l", col = 'green', lwd = 2, ylab = "Fx(x)")
                   title("Log-Normal cdf", sub = NULL, cex.main = 1.5, font.main= 6, col.main= "blue")
             
                   # Plot Probability Interval Bounds
                   x1 <- input$x81
                   cdf1 <- plnorm(x1,input$logmean,input$logsd,log = FALSE)
                   lines(c(x1,x1), c(0,cdf1),col = "cyan",lwd=2)
                   x2 <- input$x82
                   cdf2 <- plnorm(x2,input$logmean,input$logsd,log = FALSE)
                   lines(c(x2,x2), c(0,cdf2),col = "magenta",lwd=2)
                   
                   # Plot Horizontal cdf Lines.
                   lines(c(xmin,x1), c(cdf1,cdf1),col = "cyan",lwd=2)             
                   lines(c(xmin,x2), c(cdf2,cdf2),col = "magenta",lwd=2)
                   
                   # Plot Line Segment equal to Probability Measure
                   lines(c(xmin,xmin), c(cdf1,cdf2),col = "royalblue",lwd=8) 
                   
           }, # end of case 8 code block.
           
           
           '9' = { # Plot Weibull cdf 
                   # Create x-coordinate grid.      
                   xmin = 0; xmax = 20; xinc = 0.05;      
                   x <- seq(from=xmin, to=xmax, by=xinc); par("tck" = 1);
                   plot(x,pweibull(x,input$shape9,input$scale9), type = "l", col = 'green', lwd = 2, ylab = "Fx(x)")
                   title("Weibull cdf", sub = NULL, cex.main = 1.5,  font.main= 6, col.main= "blue")
                   
                   # Plot Probability Interval Bounds
                   x1 <- input$x91
                   cdf1 <- pweibull(x1,input$shape9,input$scale9)
                   lines(c(x1,x1), c(0,cdf1),col = "cyan",lwd=2)
                   x2 <- input$x92
                   cdf2 <- pweibull(x2,input$shape9,input$scale9)
                   lines(c(x2,x2), c(0,cdf2),col = "magenta",lwd=2)
                   
                   # Plot Horizontal cdf Lines.
                   lines(c(xmin,x1), c(cdf1,cdf1),col = "cyan",lwd=2)             
                   lines(c(xmin,x2), c(cdf2,cdf2),col = "magenta",lwd=2)
                   
                   # Plot Line Segment equal to Probability Measure
                   lines(c(xmin,xmin), c(cdf1,cdf2),col = "royalblue",lwd=8) 
             
           }, # end of case 9 code block.  
           
           '10' = { # Plot Central Student's t-distribution cdf 
                    # Create x-coordinate grid.
                    xmin = -10; xmax = 10; xinc = 0.05;      
                    x <- seq(from=xmin, to=xmax, by=xinc);  par("tck" = 1);
                    plot(x,pt(x,input$Ndegrees, input$delta,log = FALSE), type = "l", col = 'green', lwd = 2, ylab = "Fx(x)")
                    title("Student's t-distribution cdf", sub = NULL, cex.main = 1.5,  font.main= 6, col.main= "blue")
                    
                    # Plot Probability Interval Bounds
                    x1 <- input$x101
                    cdf1 <- pt(x1,input$Ndegrees,input$delta,log = FALSE)
                    lines(c(x1,x1), c(0,cdf1),col = "cyan",lwd=2)
                    x2 <- input$x102
                    cdf2 <- pt(x2,input$Ndegrees,input$delta,log = FALSE)
                    lines(c(x2,x2), c(0,cdf2),col = "magenta",lwd=2)
                    
                    # Plot Horizontal cdf Lines.
                    lines(c(xmin,x1), c(cdf1,cdf1),col = "cyan",lwd=2)             
                    lines(c(xmin,x2), c(cdf2,cdf2),col = "magenta",lwd=2)
                    
                    # Plot Line Segment equal to Probability Measure
                    lines(c(xmin,xmin), c(cdf1,cdf2),col = "royalblue",lwd=8) 
             
           }, # end of case 10 code block.    
           
           '11' = { # Plot the non-central Chi-Squared Distribution cdf 
                    # Create x-coordinate grid.
                    xmin = 0; xmax = 40; xinc = 0.05;      
                    x <- seq(from=xmin, to=xmax, by=xinc);  par("tck" = 1);
                    plot(x,pchisq(x,input$Ndegrees11, input$ncp11,log = FALSE), type = "l", col = 'green', lwd = 2, ylab = "Fx(x)")
                    title("Non-Central Chi-Squared cdf", sub = NULL, cex.main = 1.5,  font.main= 6, col.main= "blue")
             
                    # Plot Probability Interval Bounds
                    x1 <- input$x111
                    cdf1 <- pchisq(x1,input$Ndegrees11,input$ncp11,log = FALSE)
                    lines(c(x1,x1), c(0,cdf1),col = "cyan",lwd=2)
                    x2 <- input$x112
                    cdf2 <- pchisq(x2,input$Ndegrees11,input$ncp11,log = FALSE)
                    lines(c(x2,x2), c(0,cdf2),col = "magenta",lwd=2)
             
                    # Plot Horizontal cdf Lines.
                    lines(c(xmin,x1), c(cdf1,cdf1),col = "cyan",lwd=2)             
                    lines(c(xmin,x2), c(cdf2,cdf2),col = "magenta",lwd=2)
             
                    # Plot Line Segment equal to Probability Measure
                    lines(c(xmin,xmin), c(cdf1,cdf2),col = "royalblue",lwd=8) 
             
           }, # end of case 11 code block.       
           
           
           '12' = { # Plot the Uniform Distribution cdf 
                    # Create x-coordinate grid.
                    xmin = -20; xmax = 20; xinc = 0.05;      
                    x <- seq(from=xmin, to=xmax, by=xinc);  par("tck" = 1);
                    plot(x,punif(x,input$alpha, input$beta), type = "l", col = 'green', lwd = 2, ylab = "Fx(x)")
                    title("Uniform cdf", sub = NULL, cex.main = 1.5,  font.main= 6, col.main= "blue")
             
                    # Plot Probability Interval Bounds
                    x1 <- input$x121
                    cdf1 <- punif(x1,input$alpha,input$beta)
                    lines(c(x1,x1), c(0,cdf1),col = "cyan",lwd=2)
                    x2 <- input$x122
                    cdf2 <- punif(x2,input$alpha,input$beta)
                    lines(c(x2,x2), c(0,cdf2),col = "magenta",lwd=2)
             
                    # Plot Horizontal cdf Lines.
                    lines(c(xmin,x1), c(cdf1,cdf1),col = "cyan",lwd=2)             
                    lines(c(xmin,x2), c(cdf2,cdf2),col = "magenta",lwd=2)
             
                    # Plot Line Segment equal to Probability Measure
                    lines(c(xmin,xmin), c(cdf1,cdf2),col = "royalblue",lwd=8) 
             
           } # end of case 12 code block.     
         
    ) # End of switch
  
  })  # End of renderPlot
 
})
