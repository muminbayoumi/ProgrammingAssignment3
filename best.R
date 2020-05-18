
best <- function(State=as.character(),Outcome=as.character(),file="outcome-of-care-measures.csv"){
        #Loading Data
         data <- read.csv(file,na.strings = "Not Available")
        # Setting variable standards/Conditions to check against
        states = unique(data[,"State"])
        Out.Choice <- c(11,17,23)
        #  Setting Mortality Columns as numeric values
        data[,Out.Choice] <- lapply(FUN=as.numeric,data[,Out.Choice])
        #Providing character names to compate against
        names(Out.Choice) <-  c("heart attack","heart failure","pneumonia")
       #Initially checking  if outcome one of 3 set options
        if(!Outcome%in%names(Out.Choice)){
                stop("Invalid Outcome")}
        # Then checking if state is valid
        if(!State%in%states) {
                stop(" Invalid State")
        }
        data <- data[data$State==State,]
        LowMort <- which(data[,Out.Choice[Outcome]]==min(data[,Out.Choice[Outcome]],na.rm = T))
        a <- data[LowMort,"Hospital.Name"]

        a <- a[order(a)]
        return(a[1])
}






