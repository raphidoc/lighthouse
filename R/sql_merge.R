#' @name sql_merge
#' @title sql_merge
#' @author Raphael Mabit
#'
#' @description Merge multiple sqlite data bases into a new single one.
#' Will perform the merge table and column wise (dplyr::bind_rows). A new integer
#' primary key to ensure uniqueness and data traceability. Old ID will be keep in PID column.
#'
#' @import DBI
#' @import dplyr
#' @import stringr
#' @export


sql_merge <- function(projects = c("/mnt/D/Data/WISEMan", "/mnt/D/Data/CHONe", "/mnt/D/Data/PMZA-RIKI")) {
	# have to detach package:stats to avoid C function mask
	if (any(str_detect(search(), "package:stats"))){
		detach("package:stats")
	}


	con <- dbConnect(RSQLite::SQLite(), "/mnt/D/Data/merged_db.sqlite")

	L3 <- file.path(projects,"L3")

	DBs <- list.files(L3, pattern = ".sqlite", full.names = T)

	tablist <- c()
	for (db in DBs) {
		project <- str_extract(last(str_split(db, "/")[[1]]),".+(?=.sqlite)")
		assign(project, dbConnect(RSQLite::SQLite(), db))

		tabs <- dbListTables(eval(parse(text = paste0("`",project,"`"))))

		for (tab in tabs) {
			tablist <- append(tablist,tab)

			qry_var <- paste0("SELECT * FROM ",tab,";")
			qry <- dbSendQuery(eval(parse(text = paste0("`",project,"`"))), qry_var)
			res <- dbFetch(qry)

			# ID transform to PID (Project ID)
			if (any(str_detect(names(res),"^ID$")) && any(str_detect(names(res),"^SID$"))) {
				res <- res %>% mutate(PID= str_c(project,ID,sep = "_"),
								  SPID= str_c(project,SID,sep = "_"))
			} else if (any(str_detect(names(res),"^ID$"))) {
				res <- res %>% mutate(PID= str_c(project,ID,sep = "_"))
			} else if (any(str_detect(names(res),"^SID$"))) {
				res <- res %>% mutate(SPID= str_c(project,SID,sep = "_"))
			}

			if (!exists(tab)) {
				assign(tab,res)
			} else {
				assign(tab,bind_rows(eval(parse(text = tab)),res))
			}

			dbClearResult(qry)
		}

	}

	# create new integer primary key

	data_synthesis <- data_synthesis %>% mutate(ID = seq_along(ID),
									    Project = str_extract(PID, "^[:alnum:]+(-[:alnum:]+)?(?=_)"))
	data_synthesis <- data_synthesis %>% relocate(ID,PID,Project)

	ID_frame <- data_synthesis %>% select(ID,PID)

	lab_log <- lab_log %>% select(!ID)

	lab_log <- data_synthesis %>% select(ID,PID) %>% right_join(lab_log,by="PID") %>% mutate(SID= seq_along(SID))
	lab_log <- lab_log %>% relocate(ID,SID,PID,SPID)

	SID_frame <- lab_log %>% select(ID,SID,SPID)

	tablist <- unique(tablist[str_detect(tablist, "data_synthesis|lab_log", negate = T)])

	# update new integer primary key on all table

	for (tab in tablist) {
		if (any(str_detect(names(eval(parse(text = tab))),"^ID$")) &&
		    any(str_detect(names(eval(parse(text = tab))),"^SID$"))) {
			assign(tab, eval(parse(text = tab)) %>% right_join(SID_frame, by= "SPID"))

		} else if (any(str_detect(names(eval(parse(text = tab))),"^ID$"))) {
			assign(tab, eval(parse(text = tab)) %>% select(!ID))
			assign(tab, eval(parse(text = tab)) %>% right_join(ID_frame, by= "PID"))
			assign(tab, eval(parse(text = tab)) %>% relocate(ID,PID))

		} else if (any(str_detect(names(eval(parse(text = tab))),"^SID$"))) {
			assign(tab, eval(parse(text = tab)) %>% select(!SID))
			assign(tab, eval(parse(text = tab)) %>% right_join(SID_frame, by= "SPID"))
			assign(tab, eval(parse(text = tab)) %>% relocate(SID,ID,SPID))}

		dbWriteTable(con, tab, eval(parse(text = tab)), overwrite = F)
	}
	dbWriteTable(con, "data_synthesis", data_synthesis, overwrite = F)
	dbWriteTable(con, "lab_log", lab_log, overwrite = F)

	dbDisconnect(con)
}

#
#
#
#
# 	var_table <- function(db,tabs){
# 		vars <- list()
# 		for (i in seq_along(tabs)) {
# 			vars[[i]] <- dbListFields(db, tabs[i])
# 		}
# 		names(vars) <- tabs
# 		vars
# 	}
# 	vars <- var_table(CHONe,tabs)
#
# 	qry <- dbSendQuery(CHONe,"SELECT * FROM sqlite_master
# 		WHERE type='table'
# 		ORDER BY name;")
# 	res <- dbFetch(qry)
# }
