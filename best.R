
best <- function(State=as.character(),Outcome=as.character(),file="outcome-of-care-measures.csv"){
        #Loading Data
         data <- read.csv(file)
        # Setting variable standards/Conditions to check against
        states = unique(data[,"State"])
        Out.Choice <- c(17,23,11)
        # Setting Mortality Columns as numeric values
        data[,Out.Choice] <- lapply(FUN=as.numeric,data[,Out.Choice])
        #Providing character names to compate against
        names(Out.Choice) <-  c("heart failure","pneumonia","heart attack")
       #Initially checking  if outcome one of 3 set options
        if(!Outcome%in%names(Out.Choice)){
                stop("Invalid Outcome")}
        # Then checking if state is valid
        if(!State%in%states) {
                stop(" Invalid State")
        }
        data <- data[which(data$State==State),]
        LowMort <- which(data[,Out.Choice[Outcome]]<=min(data[,Out.Choice[Outcome]]))
        a <- data[LowMort,"Hospital.Name"]

        a <- a[order(a)]
        return(a[1])
}






