#' EXPLANATION OF FUNCTION
#'
#' @param tables DETAILS ABOUT THE tables PARAMETER
#'
#'
#' @return
#' TRUE if it works
#' @export
#'
#' @examples
#' EXAMPLE OF USAGE
#' import RPostgres
#' import RSQLite
#' import DBI
#' import xlsx
#' import yaml
#' 
create_schema <- function (tables = c("mtd", "ind_dat", "ind_boro_dat")){
    # cnfg <- yaml::yaml.load_file("M:/rconfig.yaml")
    conSuper <- rdb_connect()
    
    for(i in 1:length(tables)) {
        file <- read.xlsx( cnfg$schema_path, sheetName = tables[[i]] )
        
        assign( tables[[i]], file )
        print(tables[[i]])
        
        schema <- paste0(apply(file, 1, paste0, collapse = " "), collapse = ",")
        
        dbSendQuery(conSuper, paste0("DROP TABLE IF EXISTS ", tables[[i]]))
        dbSendQuery(conSuper, paste0("CREATE TABLE ",tables[[i]],"(", schema,")"))
        
        # load existing database
        file <- read.csv(paste0("E:/project_folders/apps/db/tables/",tables[[i]], ".csv"))
        file <- data.table::fread(paste0("E:/project_folders/apps/db/tables/",tables[[i]], ".csv")) 
        assign( tables[[i]], file )
        
        # convert INT to DATE type. recommend to unify notation for date columns
        if("xvardt" %in% colnames(file))
        {
            file$xvardt <- as.Date(file$xvardt, origin="1970-01-01")
        }
        
        if("x" %in% colnames(file))
        {
            file$x <- as.Date(file$x, origin="1970-01-01")
        }
        
        ## if it's the metadata file, then remove NAs
        if("dataset" %in% colnames(file)) {
            file <- file %>% filter(!is.na(dataset))
        }
        
        #Prepare query
        #1. Remove apostrophes from labels and titles
        file <-  as.data.frame(sapply(file, function(x) gsub("'", "", x)))
        
        #2. Transform values for INSERT INTO query
        values <- paste0(apply(file, 1, function(x) paste0("('", paste0(x, collapse = "', '"), "')")), collapse = ", ")
        
        #3. Correct "NULL" notation
        values <- gsub("'NULL'", "NULL", values)
        values <- gsub("'NA'", "NULL", values)
        
        dbSendQuery(conSuper, paste0("INSERT INTO ",tables[[i]],  " VALUES ", values, ";"), n = Inf )
    }
    
    dbDisconnect(conSuper)
    TRUE
}





## YOU CAN ADD MORE FUNCTIONS TO THE FILE THAT RUN IN THE BACKGROUND, 
## BUT WOULDN'T BE USED BY USERS (WOULD JUST BE CALLED WITHIN THE PACKAGE)