#----------------------------------------------------------------------
# form elements and actions in the in-app Globus file browser
#----------------------------------------------------------------------

# TODO: IN PROGRESS

# action links to retrieve endpoint or bookmarks lists
observeEvent(input$myGlobusBookmarks,{
    reportProgress('input$myGlobusBookmarks')
    request <- 'bookmark_list?fields=name,endpoint_id,path'
    myGlobusEndpoints(getGlobusTransfer(request))
})
observeEvent(input$myGlobusEndpoints,{
    reportProgress('input$myGlobusEndpoints')
    request <- 'endpoint_search?filter_scope=my-endpoints&limit=100&fields=id,display_name,description,host_endpoint_display_name,host_path'
    myGlobusEndpoints(getGlobusTransfer(request))
})

# UI elements to display and select and endpoint/bookmark
myGlobusEndpoints <- reactiveVal(NULL)
output$myGlobusEndpointSelector <- renderUI({
    d <- myGlobusEndpoints()
    req(d)
    reportProgress('output$myGlobusEndpointSelector')    
    if(!d$success) return( NULL )
    lapply(d$contents$DATA, function(x){
        if(x$DATA_TYPE == 'endpoint'){
            cols <- c('display_name','host_endpoint_display_name','host_path')
            tags$div(HTML(paste(unlist(x[cols]), collapse="<br>")), style="border-top: 1px solid black;")
        } else if(x$DATA_TYPE == 'bookmark'){
            cols <- c('name','path')
            tags$div(HTML(paste(unlist(x[cols]), collapse="<br>")))
        }
    })
})

#$contents$DATA_TYPE
#[1] "endpoint_list"
#$contents$DATA[[1]]$DATA_TYPE
#[1] "endpoint"
#$contents$DATA[[9]]$display_name
#[1] "wilsonte-turbo-shared"
#$contents$DATA[[9]]$description
#[1] "Tom Wilson Turbo Globus share"
#$contents$DATA[[9]]$host_endpoint_display_name
#[1] "umich#flux"
#$contents$DATA[[9]]$host_path
#[1] "/nfs/turbo/path-wilsonte-turbo/globus/"
#$contents$DATA[[9]]$id
#[1] "7e735aca-6395-11e9-b7f4-0a37f382de32"

#$contents$DATA_TYPE
#[1] "bookmark_list"
#$contents$DATA[[1]]$DATA_TYPE
#[1] "bookmark"
#$contents$DATA[[1]]$name
#[1] "PathTurbo"
#$contents$DATA[[1]]$endpoint_id
#[1] "f94e0c94-f006-11e7-8219-0a208f818180"
#$contents$DATA[[1]]$path
#[1] "/nfs/turbo/path-wilsonte-turbo/"

