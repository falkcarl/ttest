######################################################################
## Copyright 2024 Carl F. Falk
##
## This program is free software: you can redistribute it and/or
## modify it under the terms of the GNU General Public License as
## published by the Free Software Foundation, either version 3 of
## the License, or (at your option) any later version.
##
## This program is distributed in the hope that it will be useful,
## but WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
## GNU General Public License for more details.
## <http://www.gnu.org/licenses/>

library(shiny)
library(xtable)
#library(knitr)
#library(kableExtra)
library(effectsize)
#library(report) # auto-generates APA-style report...
#library(shinyjs) # not used
#library(ggplot2) # not used

# Define server logic
server <- function(input, output, session) {
  
  # create dataset and descriptives
  dat <- eventReactive({input$newdat},{
    Y1 <- round(rnorm(input$pop1n, input$pop1mean, input$pop1sd),2)
    Y2 <- round(rnorm(input$pop2n, input$pop2mean, input$pop2sd),2)
    Y <- c(Y1,Y2)
    X <- c(rep("Group 1", input$pop1n),rep("Group 2", input$pop2n))
    data <- data.frame(X=X,Y=Y)
    out <- list(data = data,
                Gnames = c("Group 1","Group 2"),
                N1 = input$pop1n,
                N2 = input$pop2n,
                M1 = mean(Y1),
                M2 = mean(Y2),
                Med1 = median(Y1),
                Med2 = median(Y2),
                SD1 = sd(Y1),
                SD2 = sd(Y2),
                Var1 = var(Y1),
                Var2 = var(Y2)
            )
    out
  })
  
  # display of raw data
  output$rawData <- renderDataTable({
    data <- dat()
    data$data}
  )
  
  # display of descriptive statistics
  output$desc <- function(){
    d <- dat()

    #kableExtra::kable_styling(
    #knitr::kable(data.frame(Group = d$Gnames,
    #                 N = c(d$N1,d$N2),
    #                 M = c(d$M1,d$M2),
    #                 #Median = c(d$Med1, d$Med2),
    #                 SD = c(d$SD1,d$SD2),
    #                 Var = c(d$Var1, d$Var2)),
    #      "html",
    #      digits = 3),
    #"striped", full_width=T)
    print(xtable(data.frame(Group = d$Gnames,
                     N = c(d$N1,d$N2),
                     M = c(d$M1,d$M2),
                     #Median = c(d$Med1, d$Med2),
                     SD = c(d$SD1,d$SD2),
                     Var = c(d$Var1, d$Var2)),
           digits = 3),type="html",
          include.rownames=FALSE,
          booktabs=TRUE)
    
  }
  
    # display results of t-test
  output$results <- renderPrint({
    data <- dat()
    alpha <- as.numeric(input$alpha)
    
    # df and critical t-value
    df <- nrow(data$data) - 2
    t.crit <- abs(qt(alpha/2, df=df))
    
    test <- t.test(Y ~ X, data=data$data, var.equal=TRUE, conf.level=1-alpha)
    
    reject <- test$p.value < alpha
    #out <- report(t.test(Y ~ X, data = data$data, var.equal=TRUE))
    
    D <- unname(test$estimate[1]-test$estimate[2])
    pooledvar <- (data$Var1*(data$N1-1)+data$Var2*(data$N2-1))/df
    cohend <- D/sqrt(pooledvar)
    dsize <- unname(interpret_cohens_d(cohend, rules="cohen1988"))
    
    out <- ""
    out <- paste0(out, "alpha (two-tailed): ", input$alpha, "\n")
    out <- paste0(out, "degrees of freedom (df) = ", df,"\n")
    out <- paste0(out, "critical t = ", round(t.crit, digits=4), "\n")    
    out <- paste0(out, "difference (D; Group 1 - Group 2) = ", round(D, 4), "\n")
    out <- paste0(out, "estimated standard error = ", round(test$stderr,4), "\n")    
    out <- paste0(out, "obtained t = ", round(test$statistic, digits=4), "\n")
    out <- paste0(out, "reject H0? ", ifelse(reject, "Yes", "No"), "\n")
    
    out <- paste0(out, "\n\n")
    out <- paste0(out, "Basic APA-style report\n\n")
    
    group1diff <- ifelse(D > 0, "higher", "lower")
    group1 <- paste0("Group 1 (M = ", round(data$M1,2), ", SD = ", round(data$SD1,2), ")" )
    group2 <- paste0("Group 2 (M = ", round(data$M2,2), ", SD = ", round(data$SD2,2), ")" )

    if(reject){
      stats <- paste0("t(",df, ") = ", round(test$statistic, 2), ", p < ", alpha,
                      ", ", (1-alpha)*100, "% CI[",round(test$conf.int[1],2),",",round(test$conf.int[2],2),"]")      
      out <- paste0(out, group1, " had significantly ", group1diff, " scores in
comparison to ", group2, ", ", stats, ",
with a ", dsize, " effect size, Cohen's d = ", round(cohend,2), ".")
    } else {
      stats <- paste0("t(",df, ") = ", round(test$statistic, 2), ", p > ", alpha,
                      ", ", (1-alpha)*100, "% CI[",round(test$conf.int[1],2),",",round(test$conf.int[2],2),"]")            
      out <- paste0(out, "Although ", group1, " tended to have ", group1diff, " scores
in comparison to ", group2, ", this difference was
not significant, ", stats, ",
and had a ", dsize, " effect size, Cohen's d = ", round(cohend,2), ".")
    }
    cat(out)
  })
  
  
}

ui <- fluidPage(

    # Application title
    #titlePanel(""),

    # Layout of UI
    sidebarLayout(
      sidebarPanel(strong("Population 1"),
                   sliderInput("pop1n",
                       "Sample size (N):",
                       min = 3, max=15,
                       value = 5, step=1),
                   numericInput("pop1mean",
                       "Mean:",
                       value = 5,
                       step = .5),
                   numericInput("pop1sd",
                       "Standard Deviation:",
                       value = 1,
                       step = .1,
                       min = .1),
                   br(),
                   br(),
                   strong("Population 2"),
                   sliderInput("pop2n",
                       "Sample size (N):",
                       min = 3, max=15,
                       value = 5, step=1),                   
                   numericInput("pop2mean",
                       "Mean:",
                       value = 5,
                       step = .5),
                   numericInput("pop2sd",
                       "Standard Deviation:",
                       value = 1,
                       step = .1,
                       min = .1),
                   actionButton("newdat",
                        "Generate New Dataset"),
                   br(),
                   br(),
                   strong("Analysis options"),
                   selectInput("alpha",
                               label = "alpha (two-tailed):",
                               choices = c(.05,.005,.01,.02, .1, .2, .3, .4, .5)
                   )
      ),
      # Main Panel
      mainPanel(
          tabsetPanel(
            tabPanel("Raw Data",
                     dataTableOutput("rawData")
            ),
            tabPanel("Descriptive Stats",
                     tableOutput("desc")
            ),
            tabPanel("Results",
                     verbatimTextOutput("results")                     
            )#,
            #tabPanel("Visualization"
            #)
          )
      )
    ) # end sidebarLayout
)

shinyApp(ui = ui, server = server)

