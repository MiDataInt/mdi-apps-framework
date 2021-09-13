
# log information about sessions for server management

logSessionMetadata <- function(sessionData){

    # further memory calculations
    sessionData$MbRAM_afterEnd  <- sum(gc()[,2]) 
    SessionMbRAM_beforeEnd <- sessionData$MbRAM_beforeEnd - sessionData$MbRAM_beforeStart
    SessionMbRAM_afterEnd  <- sessionData$MbRAM_afterEnd  - sessionData$MbRAM_beforeStart    

    ## commit the information to the log
    ## TODO: work on this
    #print(list(
    #    serverId = serverId,        # unique to a container or local server
    #    shinyId = shinyId,          # unique to a call to runApp
    #    sessionId = sessionData$id, # unique to a specific user encounter
    #    appName = sessionData$appName,
    #    sessionStartTime = sessionData$startTime,
    #    sessionDurationMins = sessionData$durationMins,
    #    MbRAM_beforeStart = sessionData$MbRAM_beforeStart,  # memory in use before app did anything in this session 
    #    MbRAM_beforeEnd = sessionData$MbRAM_beforeEnd,      # memory in use at the end of the session
    #    MbRAM_afterEnd = sessionData$MbRAM_afterEnd,        # memory in use after this session's data were cleared
    #    SessionMbRAM_beforeEnd = SessionMbRAM_beforeEnd,    # memory in use at the end that was specific to this session
    #    SessionMbRAM_afterEnd = SessionMbRAM_afterEnd,      # memory specific to this session that failed to clear
    #    nServerSessions = nServerSessions, # how many sessions this server has run (including this one)
    #    nActiveServerSessions = nActiveServerSessions, # how many sesssions were active on the server
    #    nShinySessions = nShinySessions, # how many session this shiny runApp has run
    #    nActiveShinySessions = nActiveShinySessions # how many sessions were running concurrently in this one R Shiny process
    #))
    
    # return data for auto-restart
    sessionData
}

