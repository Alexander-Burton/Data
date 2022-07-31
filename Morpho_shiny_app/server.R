library(shiny)

# This code is in the serve source code as the server function uses it and doesnt have access to it
# In the Ui source code, even when outside all functions

# Get the morphometris data from where it is uploaded to github
C_Data <- read.csv(url("https://raw.githubusercontent.com/Alexander-Burton/Data/master/Citrus_Data/Citrus%20Morphometrics.csv"))

#Get only instars with thickness measurments (some bugs didnt have their weight and thickness measured)
C_Data <- C_Data[!is.na(C_Data$Thickness_mm),]

#Load Citrus top Leaf raw colour data from github
Citrus_Raw_Col_Top <- read.csv(url("https://raw.githubusercontent.com/Alexander-Burton/Data/master/Citrus_Data/Citrus%20Colour%20-%20Top%20Leaf%20-%20Nikon%20D7000%20Novoflex%2035mm%20Peafowl%20D65.csv"))

#Get bug ID and each ROI
Citrus_Raw_Col_Top$ID <- as.numeric(substr(Citrus_Raw_Col_Top$Label,1,3))
Citrus_Raw_Col_Top$Region <- NA
for (i in 1:nrow(Citrus_Raw_Col_Top)) {
  
  Citrus_Raw_Col_Top$Region[i] <- substr(Citrus_Raw_Col_Top$Label[i],12,
                                         nchar(as.character(Citrus_Raw_Col_Top$Label[i])))
}

#Trim data to required columns
Citrus_Raw_Col_Top <- Citrus_Raw_Col_Top[Citrus_Raw_Col_Top$Region == "Abdomen",
                                         c("ID","Region","vsMean","swMean","mwMean","lwMean")]

#Assign Raw colours and remove  NAs
C_Data$UV <- NA
C_Data$SW <- NA
C_Data$MW <- NA
C_Data$LW <- NA

for(i in 1:nrow(C_Data)) {
  
  for(j in 1:nrow(Citrus_Raw_Col_Top)) {
    
    if(as.character(C_Data$ID[i]) == as.character(Citrus_Raw_Col_Top$ID[j])) {
      
      C_Data$UV[i] <- Citrus_Raw_Col_Top$vsMean[j]
      C_Data$SW[i] <- Citrus_Raw_Col_Top$swMean[j]
      C_Data$MW[i] <- Citrus_Raw_Col_Top$mwMean[j]
      C_Data$LW[i] <- Citrus_Raw_Col_Top$lwMean[j]
      
    }
    
  }
  
}

# remove rows missing data and trim to required columns
C_Data <- C_Data[!is.na(C_Data$UV),-c(2,4)]

# Calculate Hue with (LW)/(LW+UV) (Based on PCA loadings of SW, MW, LW. UV data)
# Hue here is determined from the greatest axis of colour variation
C_Data$Hue <- (C_Data$LW)/(C_Data$LW+C_Data$UV)


# create plot names
# the numbers, 3, 4, 6, and 13 correspond to the column numbers of the required morphometric characteristics
plot_names <- rep("NA", 15)
plot_names[13] <- "Hue" 
plot_names[6] <- "Area"
plot_names[4] <- "Weight"
plot_names[3] <- "Thickness"




# Define server logic required to draw the scatter plot ----
server <- function(input, output, session) {
  
  # the scatter plots render
  output$scat_plot <- renderPlot({
    
    # Check to see if any instars are selected
    if (length(input$var_stages) > 0) {
    
      # create a new data frame that only includes the required instars
      C_Data_in = C_Data[C_Data$Instar %in% as.numeric(input$var_stages) ,]
      
      # Find the max and min hue values, used to create a range from 0-1
      max_hue = max(C_Data_in$Hue)
      min_hue = min(C_Data_in$Hue)
      
      # Use max and min to work out where each hue value falls between max and min (range = 0-1)
      C_Data_in$Rel_Hue <- (C_Data_in$Hue-min_hue)/(max_hue-min_hue)
      
      # Create a green-red gradient the same legth as the required data
      col_ramp <- colorRampPalette(c("green", "red"))
      col_list <- col_ramp(length(C_Data_in$Rel_Hue))
      
      # create a new col list that is all red and work out what order the gradient colours should be 
      # based on the hue order of the required bugs
      col_list_new <- rep("red", length(C_Data_in$Rel_Hue))
      rel_order <- order(C_Data_in$Rel_Hue)
      
      for (i in 1:length(C_Data_in$Rel_Hue)) {
        
        if (input$var_binary == FALSE){
          
          # If using a gradient: assign the correct colour for each value in the new colour list
          col_list_new[rel_order[i]] <- col_list[i]
          
        }
        else {
          
          # If NOT using a gradient: assign colours below a pre-defined threshold to green
          if (C_Data_in$Rel_Hue[i] <= as.numeric(input$var_slider)/100) {
            col_list_new[i] = "green"
          } 
          else {
            col_list_new[i] = "red"
          }
        
        }
        
      }
      
      # Create the plot title using the selected stages
      name_list <- "Stages: "
      the_sep <- " "
      for (i in 1:length(input$var_stages)) {
        name_list <- paste(name_list, input$var_stages[i], sep = the_sep)
        the_sep <- ", "
      }
      
    }
    
    # Plot the data
    plot(C_Data_in[,as.numeric(input$var_y)] ~ C_Data_in[,as.numeric(input$var_x)], 
         bg = col_list_new, 
         xlab = plot_names[as.numeric(input$var_x)], 
         ylab = plot_names[as.numeric(input$var_y)], 
         pch = 21, 
         main = name_list)
    
  })
  
}
