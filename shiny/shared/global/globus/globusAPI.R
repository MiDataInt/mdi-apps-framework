
#----------------------------------------------------------------------
# generic support for Globus API interactions; no user-specific data here
#----------------------------------------------------------------------

#----------------------------------------------------------------------
# Globus API constants
#----------------------------------------------------------------------
globusAuthEndpoints <- oauth_endpoint(
    base_url  = "https://auth.globus.org/v2/oauth2",
    authorize = "authorize",
    access    = "token"
)
globusApis <- list(
    auth     = "https://auth.globus.org",
    transfer = "https://transfer.api.globusonline.org/v0.10",
    groups   = "https://groups.api.globus.org/v2"
)
globusHelperPages <- list(
    browseEndpoint = "https://app.globus.org/file-manager/",
    logout = "https://auth.globus.org/v2/web/logout/"
)
globusPublicKey <- if(serverEnv$IS_NODE) NULL else jose::read_jwk( content(GET("https://auth.globus.org/jwk.json"))[[1]][[1]] ) # nolint

#----------------------------------------------------------------------
# httr helper functions
#----------------------------------------------------------------------
nonce <- function(){ # make a ~unique key (could also use UUID package)
    digest(paste(
        Sys.time(),
        paste0(sample(c(letters, LETTERS, 0:9), 50), collapse="")
    ))
}
getGlobusSessionFile <- function(type, key){ # standardized session file paths
    if(is.null(key)) key <- "XXXXXXXX"
    file.path(serverEnv$SESSIONS_DIR, paste(type, key, sep="-"))
}
convertGlobusTokens <- function(tokens){
    oauth2.0_token(
        endpoint    = globusAuthEndpoints,
        app         = globusClient,
        credentials = tokens,
        cache = FALSE # handled elsewhere
    )
}
parseCookie <- function(cookie){ # get 'code', 'state' and other flow control parameters from query string
    nullCookie <- list()
    if(is.null(cookie) || cookie == "") return( nullCookie )
    keyValuePairs <- strsplit(cookie, '; ')[[1]]
    if(length(keyValuePairs) == 0) return( nullCookie )
    cookie <- list()
    for(kvp in keyValuePairs){
        kvp <- strsplit(kvp, '=')[[1]]
        cookie[[kvp[1]]] <- kvp[[2]]
    }
    cookie # a named list, similar to parseQueryString() 
}

#----------------------------------------------------------------------
# Globus data object parsing
#----------------------------------------------------------------------

# convert a Globus data object (a list of lists) to a data.frame
globusToDataFrame <- function(d, colnames = NULL){
    noData <- data.frame()
    if(is.null(d) || is.null(d$DATA)) return(noData)
    nrow <- length(d$DATA)
    if(nrow == 0) return(noData)
    if(is.null(colnames)) colnames <- names(d$DATA[[1]])
    df <- data.frame(rowN = 1:nrow)
    for(col in colnames){
        df[[col]] <- sapply(d$DATA, function(x) if(is.null(x[[col]])) NA else x[[col]])
    }
    rownames(df) < df$rowN
    df
}

