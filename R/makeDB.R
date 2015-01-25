makeDB <- function() {

    # Register the number of available cores on the system
    doMC::registerDoMC(cores = 32)

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
    	people[[i]] <- plyr::ldply(files[["people"]][[i]], legiscanPerson, .parallel = TRUE,
    						 .paropts = list(.errorhandling = 'pass'))
    	votes[[i]] <- plyr::llply(files[["votes"]][[i]], legiscanVotes, .parallel = TRUE,
    	                    .paropts = list(.errorhandling = 'pass'))
    	bills[[i]] <- plyr::llply(files[["bills"]][[i]], legiscanBill, 
    						fullText = "state_link", .parallel = TRUE)
    }

    # Create a single dataframe with all the people data
    allPeople <- plyr::ldply(people, .parallel = TRUE, .id = "session_name",
    				   .fun = function(x) {
    				   	dplyr::as_data_frame(x) %>% dplyr::bind_rows()
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
    votes2 <- plyr::llply(votes, unlist, recursive = FALSE, .parallel = TRUE)

    # Remove unnecessary list layer from the bills object
    bills2 <- plyr::llply(bills, unlist, recursive = FALSE, .parallel = TRUE)

    # Initiate the metaVotes and recordVotes objects that will later become
    # separate data frames
    metaVotes <- list()
    recordVotes <- list()
    billHist <- list()
    billProg <- list()
    billSpons <- list()
    billMeta <- list()
	billVotes <- list()
	billTxt <- list()

    # Loop over the sessions
    for (i in names(votes2)) {

    	# Combine all of the meta vote data frames into a single data frame
    	# per legislative session
    	metaVotes[[i]] <- plyr::ldply(votes2[[i]][names(votes2[[i]]) == "meta"], dplyr::bind_rows,
    							.parallel = TRUE)

    	# Combine all of the individual voting records into a single data frame
    	# per legislative session
    	recordVotes[[i]] <- plyr::ldply(votes2[[i]][names(votes2[[i]]) == "records"],
    							 dplyr::bind_rows, .parallel = TRUE)

    	# Combine all of the bill metadata into a single data frame per
    	# legislative session
    	billMeta[[i]] <- plyr::ldply(bills2[[i]][names(bills2[[i]]) == "billMeta"],
    	                       dplyr::bind_rows, .parallel = TRUE)

    	# Combine all of the bill history data into a single data frame per
    	# legislative session
    	billHist[[i]] <- plyr::ldply(bills2[[i]][names(bills2[[i]]) == "history"],
    						   dplyr::bind_rows, .parallel = TRUE)

    	# Combine all of the bill progress data into a single data frame per
    	# legislative session
    	billProg[[i]] <- plyr::ldply(bills2[[i]][names(bills2[[i]]) == "progress"],
    						   dplyr::bind_rows, .parallel = TRUE)

    	# Combine all of the bill sponsors data into a single data frame per
    	# legislative session
    	billSpons[[i]] <- plyr::ldply(bills2[[i]][names(bills2[[i]]) == "sponsors"],
    							dplyr::bind_rows, .parallel = TRUE)

    	billVotes[[i]] <- plyr::ldply(bills2[[i]][names(bills2[[i]]) == "votes"],
    							dplyr::bind_rows, .parallel = TRUE)
    	
    	# Combine all of the bill text data into a single data frame per
    	# legislative session
    	billTxt[[i]] <- plyr::ldply(bills2[[i]][names(bills2[[i]]) == "texts"],
    								dplyr::bind_rows, .parallel = TRUE)

    } # End of Loop

    # Remove the object i
    rm(i, bills2); gc()

    # Convert the list of voting metadata data frames into a single data frame
    # object including the session name in the data frame
    metaVotes2 <- plyr::ldply(metaVotes, .id = "session_name", .parallel = TRUE,
    				   	dplyr::bind_rows)

    # Convert the list of individual voting records data frames into a single
    # data frame object including the session name in the data frame
    recordVotes2 <- plyr::ldply(recordVotes, .id = "session_name", .parallel = TRUE,
    					 dplyr::bind_rows)

    # Remove unneeded variables from these objects
    metaVotes2 <- metaVotes2[, -2]
    recordVotes2 <- recordVotes2[, -2]


	# Recast the session name variables
    metaVotes2$session_name <- as.character(metaVotes2$session_name)
    recordVotes2$session_name <- as.character(recordVotes2$session_name)
	metaVotes2[, 2] <- as.numeric(metaVotes2[, 2])
	metaVotes2[, 3] <- as.numeric(metaVotes2[, 3])
	recordVotes2[, 2] <- as.numeric(recordVotes2[, 2])
	recordVotes2[, 3] <- as.numeric(recordVotes2[, 3])
	recordVotes2[, 5] <- as.numeric(recordVotes2[, 5])
	recordVotes2[, 6] <- as.numeric(recordVotes2[, 6])


    # Generate all of the attributes needed to assign to the voting metadata
    # data frame, generate variable labels for the variables
    # These will be used later to add column comments to the data system
    metaVotesAttr <- list(names = names(metaVotes2), class = "data.frame",
    					row.names = as.integer(seq(1, nrow(metaVotes2), 1)),
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


    recordVotesAttr <- list(names = names(recordVotes2), class = "data.frame",
    					row.names = as.integer(seq(1, nrow(recordVotes2), 1)),
    					var.labels = list(session_name = "Name of the legislative session",
    					roll_call_id = "Legiscan Roll Call ID value from the Legiscan API",
    					bill_id = "Legiscan Bill ID from the Legiscan API",
    					date = "Date the voting on the bill took place",
    					people_id = "Legiscan People ID value from the Legiscan API",
    					vote_id = "Numeric code indicating how the individual voted",
    					vote_text = "Text indicating the meaning of the corresponding vote_id code"))

    # Assign the attributes to the tables
    # attributes(metaVotes2) <- metaVotesAttr
    # attributes(recordVotes2) <- recordVotesAttr

    # Convert the list of bill metadata data frames into a single data frame
    # object including the session name in the data frame
    billMeta2 <- plyr::ldply(billMeta, .id = "session_name", .parallel = TRUE, dplyr::bind_rows)
	billMeta2 <- billMeta2[, -2]; billMeta2$session_name <- as.character(billMeta2$session_name)
    meta <- names(billMeta2); meta <- meta[-19]; billMeta2 <- billMeta2[, meta]
    	
    # Convert the list of bill history data frames into a single data frame
    # object including the session name in the data frame
    billHist2 <- plyr::ldply(billHist, .id = "session_name", .parallel = TRUE, dplyr::bind_rows)
	billHist2 <- billHist2[, -2]; billHist2$session_name <- as.character(billHist2$session_name)
    	
    # Convert the list of bill progress data frames into a single data frame
    # object including the session name in the data frame
    billProg2 <- plyr::ldply(billProg, .id = "session_name", .parallel = TRUE, dplyr::bind_rows)
	billProg2 <- billProg2[, -2]; billProg2$session_name <- as.character(billProg2$session_name)

    # Convert the list of bill sponsor data frames into a single data frame
    # object including the session name in the data frame
    billSpons2 <- plyr::ldply(billSpons, .id = "session_name", .parallel = TRUE, dplyr::bind_rows)
	billSpons2 <- billSpons2[, -2]; billSpons2$session_name <- as.character(billSpons2$session_name)

    billVotes2 <- plyr::ldply(billVotes, .id = "session_name", .parallel = TRUE, dplyr::bind_rows)
	billVotes2 <- billVotes2[, -2]; billVotes2$session_name <- as.character(billVotes2$session_name)
	billVotes2$event <- as.numeric(billVotes2$event)
	
	
	# Convert the list of bill text data frames into a single data frame
    # object including the session name in the data frame
    billTxt2 <- plyr::ldply(billTxt, .id = "session_name", .parallel = TRUE, dplyr::bind_rows)
	billTxt2 <- billTxt2[, -2]; billTxt2$session_name <- as.character(billTxt2$session_name)

}

