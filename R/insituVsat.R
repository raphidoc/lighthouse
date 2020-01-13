#Function to retrive corresponding in-situ data and satellite acquisition (only Landsat 8 for now ...)
#provide the path of L2 structure L2/YYYYMMDD_StationXXXx, list by pattern station name "(\\d\\d\\d[a-z]?)",
#extract the date and convert them and made unique in a data frame
#library(rLandsat)

insituVsat <- function(L2path= ""){
	L2path <- "/home/raphael/Data/Chone/Cops/L2"
	L2path <- paste(L2path, list.files(L2path, pattern = "(\\d\\d\\d[a-z]?)"), sep = "")
	indates <- substring(L2path, regexpr("(\\d{8})", L2path, fixed = F)+1,
					 regexpr("(\\d{8})", L2path, fixed = F)+8)

	#Create a data frame with unique date
	indates <- as.Date.character(indates, format = "%Y%m%d")
	Datef <- data.frame(indates)
	Datef <- unique.data.frame(Datef)

	#look for avalible product for each date and store result
	for(i in 1:length(rownames(Datef))){
		tempdf <- landsat_search(min_date = Datef$indates[i], max_date = Datef$indates[i],
							path_master = 11, row_master = 25,
		)
		if (!exists("result") && !is.data.frame("result")){
			result <- data.frame(matrix(ncol = length(names(tempdf)), nrow = 0))
			colnames(result) <- names(tempdf)
		}
		result <- rbind(result, tempdf)
	}

}
	# #getting available products
	# prods = espa_products(result$product_id)
	# prods = prods$master
	#
	# # placing an espa order
	# result_order = espa_order(result$product_id, product = c("l1"),
	#                           file_format = "GeoTiff",
	#                           resampling_method = "cc",
	#                           projection = "lonlat",
	#                           order_note = "chone",
	#                           standard_parallel_1 = 29.5, central_meridian = -96.0,
	#                           datum = "nad83", latitude_of_origin = 23.0,
	#                           standard_parallel_2 = 45.5, false_northing = 0, false_easting = 0,
	#                           host = 'https://espa.cr.usgs.gov/api/v1/'
	# 					                )
	# espaorder_id = result_order$order_details$orderid
	#
	# # getting order status
	# durl = espa_status(order_id = espaorder_id, getSize = TRUE)
	# downurl = durl$order_details
	#
	# # download; after the order is complete
	# landsat_download(download_url = downurl$product_dload_url, dest_file = getwd())

