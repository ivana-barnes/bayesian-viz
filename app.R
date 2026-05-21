library(shiny)

# Define UI for application
ui <- fluidPage(
  
  # Application title
  titlePanel("Bayesian Visualization"),
  
  # Sidebar
  sidebarLayout(
    sidebarPanel(
      # Slider input for number of flips
      sliderInput("nflips",
                  "Number of flips:",
                  min = 5,
                  max = 1000,
                  value = 50),
      # Slider input for p: the probability of heads
      sliderInput("prob",
                  "True p (probability of heads):",
                  min = 0,
                  max = 1,
                  value = .5),
      # Multiple choice input for which type of prior
      radioButtons("prior",
                   "Prior Distribution",
                   choiceNames = c("Uniform (flat)",
                                   "Beta (Shape 1 = Shape 2 = 0.5)",
                                   "Normal (mean = 0.5, sd = 0.05)"),
                   choiceValues = c("unif","beta","norm")),
      # Checkbox input for which of the distributions should be included
      checkboxGroupInput("which_plot", "Distributions to show:",
                         c("Prior" = "pri",
                           "Likelihood" = "lik",
                           "Posterior" = "post")),
      width = 3
      
    ),
    
    # Display the output plot (generated within the server chunk below)
    mainPanel(
      plotOutput("myPlot")
    )
  )
)

# Define server logic required to make the plot
server <- function(input, output) {
  
  output$myPlot <- renderPlot({
    
    # Set seed so it doesn't redraw a new sample when you change the prior
    # or which distributions to show - only when you change Nflips or p
    set.seed(round((input$prob + input$nflips)*1000))
    
    #########------------------#########
    #########-DEFINE FUNCTIONS-#########
    #########------------------#########
    
    # Define function for log likelihood
    # test_data is the data: a vector of 0s and 1s representing the coin flips
    # test_p is the hypothesis: a guess at the true value of p
    LogLikCoin <- function(test_data, test_p){
      k <- sum(test_data == 1) # k = number of heads
      n <- length(test_data) # n = number of flips
      # Calculate the log likelihood of the data given n and k, and return it
      return(log((test_p^k) * ((1-test_p)^(n-k))))
    }
    
    
    # Define function for simulating data and making the plot
    # nflips is the number of coin flips
    # prob is the true p used to simulate the data
    # prior is a function - it gives the prior probability as a function of p
    # which_plot is a vector of which distributions to plot 
    # (prior, likelihood, posterior, or any combination of these)
    
    sim_and_make_plot <- function(nflips, prob, prior, which_plot){
      
      # Simulate the data: each draw from the binomial distribution represents
      # one coin flip, so we need nflips draws, with N = 1 and prob = p
      data <- rbinom(n=nflips, size = 1, prob = prob)
      
      # Calculating likelihood, prior, and posterior from p and the data
      
      # Do a grid search over possible values of p
      p_vals <- seq(0,1,by = .001)
      
      # Initialize empty vectors to store likelihood and posterior
      lik <- rep(NA,length(p_vals))
      posterior <- rep(NA,length(p_vals))
      
      # Calculate likelihood and posterior for each p to fill these empty vectors
      for(i in 1:length(p_vals)){
        # Index p from the vector
        p <- p_vals[i]
        # Calculate likelihood and save it to the correct slot in the 
        # likelihood vector (exp() because LogLikCoin() returns LOG likelihood)
        likelihood <- exp(LogLikCoin(data,p))
        lik[i] <- likelihood
        # Calculate the prior probability
        pri <- prior(p)
        # Calculate the numerator of posterior probability and save it to 
        # the correct slot in the posterior vector
        # (bc this is just the numerator it isn't actually the posterior
        # probability, but the denominator is a constant so this is 
        # proportional to the posterior probability - ask me if you 
        # want more detail on this!)
        posterior[i] <- likelihood*pri
      }
      
      # Calculate the prior probability for every p and save to a vector
      pri <- prior(p_vals)
      
      # Scale the vectors so that all of them have a max value of 1 - this 
      # puts them on a similar scale so that they can be plotted together
      # (the indexing is to make sure the max isn't a weird value like Inf or NA)
      post_vec <- posterior/max(posterior[posterior < 1 & !is.na(posterior)])
      lik_vec <- lik/max(lik[lik < 1 & !is.na(lik)])
      pri_vec <- pri/max(pri[pri != Inf & !is.na(pri)])
      
      # Set up plot with axis limits, labels, ...
      plot(NA, xlim = c(0,1), ylim = c(0,1),
           xlab = "p",
           ylab = "Relative Probability",
           main = paste("# flips = ", nflips))
      
      
      # If prior is one of the selected distributions, add it to the plot
      if("pri" %in% which_plot){
        lines(pri_vec ~ p_vals, col = "cornflowerblue", lwd = 2)
      }
      
      # If likelihood is one of the selected distributions, add it to the plot
      if("lik" %in% which_plot){
        lines(lik_vec ~ p_vals, col = "red", lwd = 2)
      }
      
      # If posterior is one of the selected distributions, add it to the plot
      if("post" %in% which_plot){
        lines(post_vec ~ p_vals, col = "purple",lwd = 2)
      }
      
      # Add a vertical line at the true p value used to simulate the data
      abline(v=prob,col = "grey",lty="dashed")
      
      # Add a legend for which distribution is which
      legend("topright", legend = c("Prior","Posterior","Likelihood"),
             fill = c("cornflowerblue","purple","red"))
    }
    
    #########---------------#########
    #########-GENERATE PLOT-#########
    #########---------------#########
    
    
    # Pick which function to use for the prior based on the multiple choice 
    # input from the shiny app
    if(input$prior == "unif"){
      my_prior <- function(test_p){return(dunif(test_p,0,1))}
    } else if(input$prior == "beta"){
      my_prior <- function(test_p){return(dbeta(test_p,0.5,0.5))}
    } else if(input$prior == "norm"){
      my_prior <- function(test_p){return(dnorm(test_p,0.5,0.05))}
    }
    
    # Call sim_and_make_plot() as a function of what was input in the shiny app
    sim_and_make_plot(nflips = input$nflips, 
                      prob = input$prob,
                      which_plot = input$which_plot,
                      prior = my_prior)
    
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
