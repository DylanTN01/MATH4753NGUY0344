#' @title DDT Graph
#'
#' @param df Dataframe
#' @param x x label
#' @param y y label
#' @param SPECIES Species of fish
#' @param col Color
#'
#' @return Detailed plot of data for a specific species
#' @Imports:
ggplot2
#' @export
#'
#'
#' @examples
#' \dontrun{myddt(df = ddt, SPECIES = "CCATFISH")}

myddt <- function(df, x = "LENGTH", y = "WEIGHT", SPECIES, col = "RIVER"){ # Function declaration
  RIVER <- WEIGHT <- LENGTH <- NULL # Initializing variables to null
  df1 = ddt[df$SPECIES == SPECIES,] # Filtering dataframe to a specific species
  g <- ggplot(df1, aes_string(x=x,y=y)) + # Create plot with x and y labeling the x and y axes respectively using df1 data
    geom_point(aes_string(color = col )) + # Color code points according to river
    geom_smooth(formula = y~x +I(x^2), method = "lm") + # Fit a quadratic curve onto the data
    ggtitle("Dylan Nguyen") # Insert title onto graph
  print(g) # Print out the graph
  head(df1) # Display the first 6 elements in df1
  write.csv(df1, paste("LvsWfor",SPECIES,".csv",sep="")) # Save df1 as a csv file in the directory
}
