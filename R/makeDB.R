makeDB <- function() {

    # Load required libraries for all functions
    library("jsonlite"); library("lubridate"); library("plyr"); library("dplyr")
    library("doMC")
    
    # Register the number of available cores on the system
    registerDoMC(cores = 32)
    
    # Load all of the functions needed to build the DB
    lapply(list("R/fileLists.R", "R/fileStructure.R", "R/legiBillJSON.R", 
              "R/legiPersonJSON.R", "R/legiVotesJSON.R"), source)
    
    # Build a directory tree object for the file locations
    dirTree <- fileStructure("~/legiscanR/data/msHistoricalJSON/")
    
    # Build a files object w/full path/name specified
    files <- fileLists(dirTree)
    
    # Create a peoples object to store all of the parsed
    # People directory data
    people <- list(); votes <- list(); bills <- list()
    for (i in names(files[["people"]])) {
    	people[[i]] <- ldply(files[["people"]][[i]], legiPersonJSON, .parallel = TRUE, 
    						 .paropts = list(.errorhandling = 'pass'))
    	votes[[i]] <- llply(files[["votes"]][[i]], legiVotesJSON, .parallel = TRUE, 
    	                    .paropts = list(.errorhandling = 'pass'))
    	bills[[i]] <- llply(files[["bills"]][[i]], legiBillJSON, .parallel = TRUE, 
    	                    .paropts = list(.errorhandling = 'pass'))
    }
    
    
    # Create a single dataframe with all the people data
    allPeople <- ldply(people, .parallel = TRUE, .id = "session_name",
    				   FUN = function(x) {
    				   	rbind(x, stringsAsFactors = FALSE)
    				   }
    )
    
    # Set the session name variable to a character vector
    allPeople$session_name <- toString(allPeople$session_name)
    
    # Generate all of the attributes needed to assign to the data frame
    # Generate variable labels for the variables
    # These will be used later to add column comments to the data system
    peopleAttr <- list(names = names(allPeople), class = "data.frame",
    				row.names = as.integer(seq(1, nrow(allPeople), 1)), 
    				var.labels = list(session_name = "Name of the legislative session",
    				people_id = "Legiscan People ID value from the Legiscan API", 
    				role_id = "Legiscan Role ID from the Legiscan API",
    				name = "Full Name of the Legislator", 
    				ftm_id = "Follow the Money ID from Legiscan API",
    				committee_id = "Legiscan Chamber Committee ID from Legiscan API",
    				party_id = "Political Party Indicator from Legiscan API", 
    				first_name = "First Name of the Legislator",
    				middle_name = "Middle Name of the Legislator", 
    				last_name = "Last Name of the Legislator",
    				suffix = "Suffix for Legislator if applicable", 
    				nickname = "Nickname used by the Legislator",
    				ftm_eid = "Follow the Money alternate ID from the Legiscan API"))
    
    # Assign the attributes to the single data frame object
    attributes(allPeople) <- peopleAttr
    
    # Remove a level of lists from the votes object
    votes2 <- llply(votes, unlist, recursive = FALSE, .parallel = TRUE)
    
    # Remove unnecessary list layer from the bills object
    bills2 <- llply(bills, unlist, recursive = FALSE, .parallel = TRUE)
    
    # Initiate the metaVotes and recordVotes objects that will later become
    # separate data frames
    metaVotes <- list()
    recordVotes <- list()
    billHist <- list()
    billProg <- list()
    billSpons <- list()
    billTxt <- list()
    billMeta <- list()
    
    # Loop over the sessions
    for (i in names(votes2)) {
    	
    	# Combine all of the meta vote data frames into a single data frame 
    	# per legislative session
    	metaVotes[[i]] <- ldply(votes2[[i]][names(votes2[[i]]) == "meta"], rbind.fill, 
    							.parallel = TRUE)
    	
    	# Combine all of the individual voting records into a single data frame
    	# per legislative session
    	recordVotes[[i]] <- ldply(votes2[[i]][names(votes2[[i]]) == "records"], 
    							  rbind.fill, .parallel = TRUE)
    	# Combine all of the bill metadata into a single data frame per 
    	# legislative session
    	billMeta[[i]] <- ldply(bills2[[i]][names(bills2[[i]]) == "billMetaData"], 
    	                       rbind.fill, .parallel = TRUE)
    	
    	# Combine all of the bill history data into a single data frame per 
    	# legislative session
    	billHist[[i]] <- ldply(bills2[[i]][names(bills2[[i]]) == "billHistory"], 
    	                       rbind.fill, .parallel = TRUE)
    	
    	# Combine all of the bill progress data into a single data frame per 
    	# legislative session
    	billProg[[i]] <- ldply(bills2[[i]][names(bills2[[i]]) == "billProgress"], 
    	                       rbind.fill, .parallel = TRUE)
    	
    	# Combine all of the bill sponsors data into a single data frame per 
    	# legislative session
    	billSpons[[i]] <- ldply(bills2[[i]][names(bills2[[i]]) == "billSponsors"], 
    	                        rbind.fill, .parallel = TRUE)
    	
    	# Combine all of the bill text data into a single data frame per 
    	# legislative session
    	billTxt[[i]] <- ldply(bills2[[i]][names(bills2[[i]]) == "billText"], 
    	                      rbind.fill, .parallel = TRUE)
    	
    } # End of Loop
    
    # Remove the object i
    rm(i)
   
    # Convert the list of voting metadata data frames into a single data frame 
    # object including the session name in the data frame
    metaVotes2 <- ldply(metaVotes, .id = "session_name", .parallel = TRUE, 
    				   FUN = function(x) {
    				   	rbind.fill(x, stringsAsFactors = FALSE)
    				   })
    
    # Convert the list of individual voting records data frames into a single 
    # data frame object including the session name in the data frame
    recordVotes2 <- ldply(recordVotes, .id = "session_name", .parallel = TRUE, 
    					 FUN = function(x) {
    					 	rbind.fill(x, stringsAsFactors = FALSE)
    					 })
    
    # Remove unneeded variables from these objects
    metaVotes3 <- metaVotes2[, -2]
    recordVotes3 <- recordVotes2[, -2]
    
    metaVotes4 <- metaVotes3
    recordVotes4 <- recordVotes3
    
    # Recast the session name variables
    metaVotes4$session_name <- toString(metaVotes3$session_name)
    recordVotes4$session_name <- toString(recordVotes3$session_name)
    
    
    # Generate all of the attributes needed to assign to the voting metadata 
    # data frame, generate variable labels for the variables
    # These will be used later to add column comments to the data system
    metaVotesAttr <- list(names = names(metaVotes4), class = "data.frame",
    					row.names = as.integer(seq(1, nrow(metaVotes4), 1)), 
    					var.labels = list(session_name = "Name of the legislative session",
    					roll_call_id = "Legiscan Roll Call ID value from the Legiscan API", 
    					bill_id = "Legiscan Bill ID from the Legiscan API",
    					date = "Date the voting on the bill took place", 
    					desc = "Short description of the outcome of the vote",
    					yea = "Total number of votes for the bill",
    					nay = "Total number of votes against the bill", 
    					nv = "Total number of non-voters/abstainers",
    					absent = "Total number of legislators absent", 
    					passed = "Indicator of whether or not the bill was passed"))
    
    
    recordVotesAttr <- list(names = names(recordVotes4), class = "data.frame",
    					row.names = as.integer(seq(1, nrow(recordVotes4), 1)), 
    					var.labels = list(session_name = "Name of the legislative session",
    					roll_call_id = "Legiscan Roll Call ID value from the Legiscan API", 
    					bill_id = "Legiscan Bill ID from the Legiscan API",
    					date = "Date the voting on the bill took place", 
    					people_id = "Legiscan People ID value from the Legiscan API",
    					vote_id = "Numeric code indicating how the individual voted",
    					vote_text = "Text indicating the meaning of the corresponding vote_id code"))
    
    # Assign the attributes to the tables
    attributes(metaVotes4) <- metaVotesAttr
    attributes(recordVotes4) <- recordVotesAttr
    
    # Convert the list of bill metadata data frames into a single data frame 
    # object including the session name in the data frame
    billMeta <- ldply(billMeta, .id = "session_name", .parallel = TRUE, 
    				  FUN = function(x) {
    				  		rbind.fill(x, stringsAsFactors = FALSE)
    				  })
    
    # Convert the list of bill history data frames into a single data frame 
    # object including the session name in the data frame
    billHist <- ldply(billHist, .id = "session_name", .parallel = TRUE, 
    				  FUN = function(x) {
    				  		rbind.fill(x, stringsAsFactors = FALSE)
    				  })
    
    # Convert the list of bill progress data frames into a single data frame 
    # object including the session name in the data frame
    billProg <- ldply(billProg, .id = "session_name", .parallel = TRUE, 
    				  FUN = function(x) {
    						rbind.fill(x, stringsAsFactors = FALSE)
    				  })
    
    # Convert the list of bill sponsor data frames into a single data frame 
    # object including the session name in the data frame
    billSpons <- ldply(billSpons, .id = "session_name", .parallel = TRUE, 
    				   FUN = function(x) {
    						rbind.fill(x, stringsAsFactors = FALSE)
    				   })
    
    # Convert the list of bill text data frames into a single data frame 
    # object including the session name in the data frame
    billTxt <- ldply(billTxt, .id = "session_name", .parallel = TRUE, 
    				 FUN = function(x) {
    				 	rbind.fill(x, stringsAsFactors = FALSE)
    				 })
    
    detach(package:jsonlite); detach(package:plyr); detach(package:lubridate); detach(package:dplyr)
    detach(package:doMC)

}

