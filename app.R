# Load necessary libraries

library(shiny)
library(interplot)
library(tidyverse)
library(foreign)
library(shinythemes)
library(dotwhisker)
library(ggthemes)

# Custom function for styling
theme_efs <- function() {
    theme_fivethirtyeight() +
        theme(axis.text.x = element_text(size = 14), 
              axis.title.x = element_text(size = 14),
              axis.text.y = element_text(size = 14), 
              axis.title.y = element_text(size=14, margin = margin(t = 0, r = 20, b = 0, l = 0)),
              plot.title = element_text(size = 20, hjust=0.5)) +
        theme(text=element_text(color="white"),
              plot.caption = element_text(color = "white", hjust=-0.3, face="italic"),
              plot.background = element_rect(fill = "#292828"),
              panel.background = element_rect(fill = '#292828'),
              legend.background = element_rect(fill = '#292828'),
              legend.justification=c(0,1), 
              legend.position=c(0.00, 1),
              legend.text = element_text(size = 14, margin = margin(r = 30, unit = "pt")),
              legend.box.background = element_blank(),
              legend.direction = "vertical",
              legend.box = "horizontal",
              legend.key = element_rect(fill = "#292828"),
              legend.key.size = unit(0.75, "cm"))
}


# Read Data
rd <- read.dta("C:/Users/afisher/Documents/R Code/Resources/Interviews/russiadata.dta")

# Get Full Sample
rd <- rd %>%
    select(foreign_treat, putin_approve, vote_navalny,
           putin, democracy, female, age, russian,
           married, city, education, polinterest4, howreligious,
           orthodox, media_use3, mediachoice, socialmediause, 
           angryinsultputin, usattitudes, regimesupport, pass) %>% 
    mutate(approve_putin = case_when(putin_approve == "Fully approve" ~ 5,
                                     putin_approve == "Approve" ~ 4,
                                     putin_approve == "Neither approve or disapprove" ~ 3,
                                     putin_approve == "Disapprove" ~ 2,
                                     putin_approve == "Do not approve at all" ~ 1),
           vote_navalny = case_when(vote_navalny == "No" ~ 0,
                                    vote_navalny == "Yes" ~ 1)) %>%
    na.omit() 

# individuals who took proper amount of time to take survey
rd2 <-  rd  %>%
    filter(pass==1)

################################
# Front end of the application #
################################

ui <- fluidPage(
    titlePanel(h1(strong("American Digital Media Effects in Russia: An Experimental Approach"),
               h4("- Select the outcome to change the dependent variable."),
               h4("- Select the sample to filter individuals who did not pass reading checks."),
               h4("- Select an interaction variable to examine hetergenous treatement effects."))),
    theme = shinytheme("flatly"),
    fluidRow(
        sidebarPanel(
            selectInput("outcome", label = h3("Outcome"),
                        choices = list("Support for Vladimir Putin" = "putin",
                                       "Belief that Russia is a Democracy" = "democracy",
                                       "Likelihood of Voting for Opposition Candidate" = "vote_navalny"), selected = 1),
            selectInput("model", label = h3("Sample"),
                        choices = list("Full Sample" = "rd",
                                       "Passed Screening test" = "rd2"), selected = 1),
            selectInput("interaction", label = h3("Interactions"),
                        choices = list("Education" = "education",
                                       "Age" = "age",
                                       "Interest in Politics" = "polinterest4"), selected = 1)
        ),
        
        mainPanel(cellArgs = list(style = "vertical-align: bottom"),
        tabsetPanel(type = "tabs",
            tabPanel("Coefficient Plot", plotOutput('plotsupport')),
            tabPanel("Barplot", plotOutput('plotsupport2')),
            tabPanel("Interaction Plot", plotOutput('plotsupport3'))
            
        )
    )
  )
)

###############################
# Back end of the application #
###############################


server <- function(input, output, session) {
    
    
    #model = reactive({lm(rd[,input$outcome] ~ rd[,"foreign_treat"], data = rd)})
    

    # Data Frame Names
    df <- reactive({
        x <- get(input$model)
    })
    
    # regression formula
    regFormula <- reactive({
        as.formula(paste(input$outcome, '~', 'foreign_treat'))
    })
    
    # bivariate model
    model <- reactive({
        lm(regFormula(), data = df())
    })
    
    
    # regression formula Interaction
    regFormulaInt <- reactive({
      as.formula(paste(input$outcome, '~', input$interaction, ' * foreign_treat'))
    })
    
    # Interaction model
    modelInt <- reactive({
      lm(regFormulaInt(), data = df())
    })
    
    
    #Bar plot data
    rd_group = reactive({
        
        df() %>%
            group_by(foreign_treat) %>%
            summarize(dv = mean(!!rlang::sym(input$outcome)),
                      sd = sd(!!rlang::sym(input$outcome)),
                      n = n(),
                      se = sd/sqrt(n),
                      ci = se * 1.96)
        
    })
    
    # Function to plot coefficient plot
    
    output$plotsupport <- renderPlot({
        
        dwplot(model(), 
               vline = geom_vline(xintercept = 0, colour = "black", linetype = 2, size=1),
               dot_args = list(aes(shape = model), size=6, color="black"),
               whisker_args = list(aes(linetype = model), size=1.8, color="black")) +
            theme_classic() + xlab("Coefficient Estimate") + ylab("")+
            guides(col = guide_legend(nrow=1))+
            ggtitle("") +
            theme(plot.title = element_text(face="bold", hjust=0.5),
                  legend.position = "none",
                  legend.justification = c(0.22, 0.01), 
                  legend.background = element_rect(colour="grey80"),
                  strip.background = element_blank(),
                  panel.grid.minor.y = element_blank(), 
                  panel.grid.major.x = element_blank(),
                  panel.grid.major = element_blank(), 
                  panel.grid.minor = element_blank(),
                  panel.background = element_blank(),
                  legend.title = element_blank()) +
            scale_y_discrete(labels = c('Balanced Criticism','One-sided Criticism')) +
            scale_color_brewer(type = 'qual', palette = 3) +
            theme(axis.text.x = element_text(size = 14), 
                  axis.title.x = element_text(size = 14),
                  axis.text.y = element_text(size = 14), 
                  axis.title.y = element_text(size=14, margin = margin(t = 0, r = 20, b = 0, l = 0)))
        
    }, height=400, width=600) 
    
    # Function to plot bar graphs
    
    output$plotsupport2 <- renderPlot({
        
        ggplot(rd_group(), aes(x=foreign_treat, y=dv)) + 
            geom_bar(position=position_dodge(), stat="identity",
                     colour="black", # Use black outlines,
                     size=1) +      # Thinner lines
            geom_errorbar(aes(ymin=dv-ci, ymax=dv+ci),
                          size=1.5,    # Thinner lines
                          width=.2,
                          position=position_dodge(.9)) +
            xlab("") +
            ylab("") +
            ggtitle("") +
            ylim(0,1) +
            theme_classic() +
            theme(plot.title = element_text(size=16, hjust = 0.5,  
                                            face="bold", margin = margin(t = 0, r = 0, b = 10, l = 0)),
                  axis.title.y = element_text(size=12, margin = margin(t = 0, r = 20, b = 0, l = 0)),
                  axis.text=element_text(size=12))
        
    }, height=600, width=600)
    
    # Function to plot interactions
    
  
    
    
    
    output$plotsupport3 <- renderPlot({
      
      interplot(m = modelInt(), var1 = "foreign_treat", var2 = input$interaction, hist=T, facet_labs = c("One-Sided", "Balanced")) +
        # Add labels for X and Y axes
        xlab("Interaction Variable") +
        ylab("Treatment Effect") +
        # Change the background
        theme_classic() +
        # Add the title
        ggtitle("") +
        theme(plot.title = element_text(face="bold")) +
        # Add a horizontal line at y = 0
        geom_hline(yintercept = 0, linetype = "dashed", size=1) +
        theme(plot.title = element_text(size=16, hjust = 0.5,  
                                      face="bold", margin = margin(t = 0, r = 0, b = 10, l = 0)),
            axis.title.y = element_text(size=12, margin = margin(t = 0, r = 20, b = 0, l = 0)),
            axis.title.x = element_text(size=12, margin = margin(t = 0, r = 20, b = 0, l = 0)),
            axis.text=element_text(size=12),
            strip.text.x = element_text(size = 14))
      
    }, height=600, width=600)
}

shinyApp(ui = ui, server = server)


