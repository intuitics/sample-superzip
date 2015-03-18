#library(dplyr)
#library(digest)
#library(RColorBrewer)

superzip.threshold <- 5;

get.superzip.data.filename <- function(){
  paste(c("data/", "superzip.rds"))
}

load.data <- function(data.path){
  allzips <- readRDS("data/superzip.rds")
  allzips$latitude <- jitter(allzips$latitude)
  allzips$longitude <- jitter(allzips$longitude)
  allzips$college <- allzips$college * 100
  allzips$zipcode <- formatC(allzips$zipcode, width=5, format="d", flag="0")
  row.names(allzips) <- allzips$zipcode
  
  allzips
}

clean.table <- function(data){
  cleantable <- data %>%
    select(
      City = city.x,
      State = state.x,
      Zipcode = zipcode,
      Rank = rank,
      Score = centile,
      Superzip = superzip,
      Population = adultpop,
      College = college,
      Income = income,
      Lat = latitude,
      Long = longitude
    )
}

sample.data <- function(allzips){
  set.seed(100)
  zipdata <- allzips[sample.int(nrow(allzips), 10000),]
  zipdata <- zipdata[order(zipdata$centile),]
}

get.options <- function(){
  list("Is Super ZIP?" = "superzip", 
       "Centile Score" = "centile", 
       "College Education" = "college", 
       "Median Income" = "income", 
       "Population" = "adultpop")
}

get.option.names <- function(){
  names(get.options());
}

get.option.value.by.name <- function(name){
  get.options()[[name]]
}

get.viz.data <- function(data, color, size, threshold=1){
  colorBy <- get.option.value.by.name(color)
  sizeBy <- get.option.value.by.name(size)
  maxSizeBy <- max(data[[sizeBy]])
  colors <- get.colors(colorBy, data, threshold)
  sizes <- get.sizes(sizeBy, data)
  dataHash <- digest(data)
  
  c(list(sizeBy=sizeBy, threshold=threshold),get.changed.viz.data(data,colors,sizes))
}

get.colors <- function(colorBy, data, threshold){
  colorData <- if (colorBy == "superzip") {
    as.numeric(data$centile > (100 - threshold))
  } else {
    data[[colorBy]]
  }
  colors <- brewer.pal(7, "Spectral")[cut(colorData, 7, labels = FALSE)]
  colors
}

get.sizes <- function(sizeBy, data){
  (data[[sizeBy]] / max(data[[sizeBy]])) * 30000
}

get.changed.viz.data <- (function(){
  colors.hash <- NULL
  sizes.hash <- NULL
  data.hash <- NULL
  
  function(data, colors, sizes){
    new.data.hash <- digest(colors)
    new.colors.hash <- digest(colors)
    new.sizes.hash <- digest(sizes)
    
    result = list();
    
    #return the data if it has changed
    if(is.null(data.hash) || new.data.hash != data.hash){
      data.hash <<- new.data.hash;
    }
    
    result$data <- data
    
    #return the colors if they have changed
    if(is.null(colors.hash) || new.colors.hash != colors.hash){
      colors.hash <<- new.colors.hash;
    }
    
    result$colors <- colors
    
    #return the colors if they have changed
    if(is.null(sizes.hash) || new.sizes.hash != sizes.hash){
      sizes.hash <<- new.sizes.hash;
    }
    
    result$sizes <- sizes
    
    result
  }
})()

not.sizing.by.superzip <- function(colorBy=NULL){
  if(is.null(colorBy) || get.option.value.by.name(colorBy) != "superzip"){
    T
  } else {
    F
  }
}

relay <- function(x){
  x
}