rankall <- function(Outcome, num = "best") {
        ## Read outcome data
        data  <- read.csv(file = "outcome-of-care-measures.csv",na.strings = "Not Available")
        data <- data[,c(2,7,11,17,23)]
        data$Rank <- NA

        states = unique(data[,"State"])
        #     Checking provided num var is correct
        if(!is.numeric(num) & num!="Best" & num!="Worst"){
                stop("invalid ranking entry. Allowed options are either
                     integer or 'Best'or 'Worst'")}
        # Setting Mortality Columns as numeric values
        #Providing character names to compate against
        Out.Choice <- c(3,4,5)
        data[,Out.Choice] <- lapply(FUN=as.numeric,data[,Out.Choice])
        names(Out.Choice) <-  c("heart attack","heart failure","pneumonia")
        ## Check that state and outcome are valid
        if(!Outcome%in%names(Out.Choice)){
                stop("Invalid Outcome")}

        Report <-  data.frame(State=states,
                              Hospital= NA)

        ## For each state, find the hospital of the given rank
        for(i in 1:dim(Report)[1]){
                s <- as.character(states[i])
                rdata <- data[data$State==s,c(1:6)]
                rdata <- rdata[!is.na(rdata[,Out.Choice[Outcome]]),]
                o.data<- order(rdata[,Out.Choice[Outcome]],rdata[,1])
                rdata <-  rdata[o.data,]
                rdata <- rdata[!is.na(rdata[,Out.Choice[Outcome]]),]
                rdata$Rank <- 1:length(rdata[,1])

                if(num > length(rdata[,1])&num!="Best"&num!="Worst"){
                        HN <- NA
                         }
                if(num=="Best"){
                       HN <- as.character(rdata[1,1])}


               if(num=="Worst"){
                       rdata <- rdata[!is.na(rdata[,Out.Choice[Outcome]]),]
                       HN <- as.character(rdata[rdata$Rank==length(rdata$Rank),1])}

               if(is.numeric(num)){
                        rdata <- rdata[!is.na(rdata[,Out.Choice[Outcome]]),]
                        HN <- as.character(rdata[rdata$Rank[num],1])}

        ## Return a data frame with the hospital names and the
                Report[i,2] <- HN


        }
        ## Return a data frame with the hospital names and the
        ## (abbreviated) state name
        Report <- Report[,c("Hospital","State")]
        if(sum(is.na(Report[,1]))>0){
                warning("Rank requested greater Than No. Of Hospitals per state, NA returned")}
       return(Report)
}
