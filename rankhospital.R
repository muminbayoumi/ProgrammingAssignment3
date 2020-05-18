rankhospital <- function(State=as.character(),Outcome=as.character(), num){
        ## Read outcome data
        data <- read.csv(file = "outcome-of-care-measures.csv",na.strings = "Not Available")
        data <- data[,c(2,7,11,17,23)]
        # Initializing Rank Column
        data$Rank <- NA


        # Setting variable standards/Conditions to check against
        states = unique(data[,"State"])
        Out.Choice <- c(3,4,5)





        #     Checking provided num var is correct
        if(!is.numeric(num) & num!="Best" & num!="Worst"){
                stop("invalid ranking entry. Allowed options are either
                     integer or 'Best'or 'Worst'")}
        # Setting Mortality Columns as numeric values
        data[,Out.Choice] <- lapply(FUN=as.numeric,data[,Out.Choice])
        #Providing character names to compate against
        names(Out.Choice) <-  c("heart attack","heart failure","pneumonia")
        ## Check that state and outcome are valid
        if(!Outcome%in%names(Out.Choice)){
                stop("Invalid Outcome")}
                # Then checking if state is valid
        if(!State%in%states) {
                        stop(" Invalid State")
                }
                data <- data[which(data$State==State),c(1:6)]
                o.data<- order(data[,Out.Choice[Outcome]],data[,1])
                data <-  data[o.data,]
                data$Rank <- 1:length(data[,1])


        if(num > length(data[,1])&num!="Best"&num!="Worst"){
                data[1,6] <- NA
                print(data[1,6])
                stop("Rank greater than length of data")
                }


        ## Return hospital name in that state with the given rank
        if(num=="Best"){
                data <- data[1,c(1,Out.Choice[[Outcome]],6)]}


         if(num=="Worst"){
                data <- data[!is.na(data[,Out.Choice[Outcome]]),]
                data <- data[data$Rank==length(data$Rank),c(1,Out.Choice[[Outcome]],6)]}
        if(is.numeric(num)){
                data <- data[!is.na(data[,Out.Choice[Outcome]]),]
                data <- data[data$Rank[num],c(1,Out.Choice[[Outcome]],6)]}

return(data)
}
