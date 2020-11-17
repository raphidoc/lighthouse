#' @name generate_iop_db
#' @title generate instrument databases in L3
#' @author Raphael Mabit
#'
#' @description Loop over all L2 Station QC above 0 (in iop_processing_log) to load and extract
#' all non-empty list in IOP.fitted.down.RData file.
#' Create in L3 one csv file by instrument and a global html report for all processed variables.
#'
#' TO DO: The method used here eval(parse(...)) should be changed also use check_wl_consistency to do what it says
#' Check ID consistency between L2 and processing_log
#'
#' @import dplyr
#' @import stringr
#' @export

l3_iop_gen <- function(project, mission=""){
  #From some reading in https://stackoverflow.com/questions/13649979/what-specifically-are-the-dangers-of-evalparse

  if (!exists("mission") || mission == "" ) {
    mission <- str_split(project,"/")[[1]]
    mission <- mission[length(mission)]
    message("mission name empty, taking name of the project: ",mission)
  }

# Filter setup ------------------------------------------------------------

  L2 <- file.path(project, "L2")

  LogFile <- list.files(path = file.path(project,"ProLog"), pattern = "Riops_processing_log", recursive = F, full.names = T)

  # Check file
  if (length(LogFile) == 0) {
    stop("No Riops_processing_log found in: ",file.path(project,"ProLog"))
  } else if (length(LogFile) > 1) {
    stop("Multiple Riops_processing_log found in: ",file.path(project,"ProLog"),
         "\n",str_c(LogFile, collapse = "\n"))
  }

  ProLog <- data.table::fread(file = LogFile, data.table = F, colClasses = "character")

  # List available data point in L2
  dirs <- grep("/IOP$",list.dirs(L2,recursive = T), value = T)
  IOPframe <- data.frame(dirs)

  IOPframe <- IOPframe %>%
    mutate(ID = str_extract(dirs, "(?<=/)[[:digit:]]+(?=/)"))

  # Identifies paths with ProLog
  ProLog <- ProLog %>% inner_join(IOPframe, by="ID")

  difID <- ProLog$ID[which(!ProLog$ID %in% IOPframe$ID)]

  if (!purrr::is_empty(difID)) {
  warning("ProLog ID not found in L2:\n",str_c(difID, collapse =", "))
  }

  # Filter QC
  ProLog <- ProLog %>% filter(QC > 0)


# loop over all dataset in all directories --------------------------------

  anydevice <- tibble()

  for (i in seq_along(ProLog$dirs)){
    ID <- ProLog$ID[i]
    message("Processing folder: ",ID)
    DataFile <- file.path(ProLog$dirs[i],"IOP.fitted.down.RData")
    if (file.exists(DataFile)) {
      load(DataFile)
    } else {
      message("File :",DataFile," does not exist")
    }

    devices <- purrr::map_lgl(IOP.fitted.down[-1], rlang::is_empty) == F
    device <- devices[devices]

    anydevice <- bind_rows(anydevice, data.frame(ID=ID, t(devices)))

    for (i2 in seq_along(device)){
      message("Device: ",paste0(names(device[i2]),"_",ID))
      assign(paste0(names(device[i2]),"_",ID),
             data.frame(ID=rep(ID, length(IOP.fitted.down$Depth)),
                        Depth= IOP.fitted.down$Depth))

      for (var_x in names(eval(parse(text=paste0("IOP.fitted.down$",names(device[i2])))))){
        if(var_x == "a_wl" & names(device[i2]) == "ACS"){
          a_wl_ACS <- eval(parse(text=paste0("IOP.fitted.down$",names(device[i2]),"$",var_x)))
          next()
        }
        if(var_x == "c_wl" & names(device[i2]) == "ACS"){
          c_wl_ACS <- eval(parse(text=paste0("IOP.fitted.down$",names(device[i2]),"$",var_x)))
          next()
        }
        if(var_x == "wl" & names(device[i2]) == "ASPH"){
          if (exists("wl_ASPH") && wl_ASPH != eval(parse(text=paste0("IOP.fitted.down$",names(device[i2]),"$",var_x)))){
            warning(paste("ASPH wavelength have changed:",ID))
          }
          wl_ASPH <- eval(parse(text=paste0("IOP.fitted.down$",names(device[i2]),"$",var_x)))
          next()
        }
        if(var_x == "wl" & names(device[i2]) == "BB9"){
          if (exists("wl_BB9") && wl_BB9 != eval(parse(text=paste0("IOP.fitted.down$",names(device[i2]),"$",var_x)))) {
            warning(paste("BB9 wavelength have changed:",ID))
          }
          wl_BB9 <- eval(parse(text=paste0("IOP.fitted.down$",names(device[i2]),"$",var_x)))
          next()
        }
        if(var_x == "wl" & names(device[i2]) == "HS6"){
          wl_HS6 <- eval(parse(text=paste0("IOP.fitted.down$",names(device[i2]),"$",var_x)))
          next()
        }
        eval(parse(text=paste0(names(device[i2]),"_",ID,
                               "<- cbind(", names(device[i2]),"_",ID,
                               ", IOP.fitted.down$",names(device[i2]),"$",var_x,")")))
      }
    }
  }

# Save in L3 --------------------------------------------------------------

  L3 <- file.path(project, "L3","IOP")

  # ACS
  if (!exists("a_wl_ACS")) {a_wl_ACS <-  c("399.1","403.2","407.5","411.7","415.5","420.3","425.3","429.9","434.3","438.9","443.3","448.6","453.4","458.2",
                                         "462.9","467.6","472.7","478" ,"483.1","488.1","492.8","497.7","502.6","507.8","513.2","518.5","523.5","528.9",
                                         "533.7","538.5","543.7","548.5","553.6","558.7","563.9","569","573.4","578.1","582.7","587.1","591.5","596.1",
                                         "600.6","604.8","609","613.7","618.7","623.4","628","632.7","637.2","641.8","646.6","651.2","656","660.6",
                                         "665","669.6","674","678.3","682.7","686.6","690.8","694.7","698.4","702.3","706","709.7","713.2","716.8",
                                         "720.1","723.7","726.8","730","733.4","736.7","739.3","742.4","745")}

  if (!exists("c_wl_ACS")) {c_wl_ACS <- c("400.1","404.6","408.9","412.8","417.3","422.1","427.0","431.5","435.6","440.2","445.3","450.2","455.2","459.9","464.5","469.8","474.8","480.1","485.2",
                       "490.2","494.8","499.8","504.7","509.9","515.4","520.8","525.6","530.6","535.8","540.6","545.9","550.9","556.0","561.3","566.2","571.0","575.8","580.2",
                       "584.6","589.0","593.5","598.2","602.8","607.2","611.7","616.7","621.5","626.2","630.9","635.2","640.0","644.6","649.3","654.0","658.6","663.2","667.7",
                       "672.1","676.5","680.7","685.0","688.9","693.1","696.8","700.7","704.6","708.3","711.8","715.3","718.9","722.5","725.9","729.1","732.3","735.4","738.3",
                       "741.3","744.0","746.7")}

  if (any(anydevice$ACS)) {
    DF_list <- purrr::map(mget(ls(pattern = "(ACS)_[[:digit:]]+")),
                          setNames,
                          c("ID","Depth",paste0("A_",a_wl_ACS),paste0("C_",c_wl_ACS)))
    ACS_DF <- bind_rows(DF_list)
    lighthouse::check_l3(project, L3, set="ACS")
    readr::write_csv(ACS_DF,
                     file = file.path(L3,"ACS",
                                      paste0("ACS_DB_",Sys.Date(),"_",mission,".csv")))
    ACSreport <- TRUE
  } else {ACSreport <- FALSE}


  # ASPH
  if (!exists("wl_ASPH")) {
    wl_ASPH <- c("360","361","362","363","364","365","366","367","368","369","370","371","372","373","374","375",
                 "376","377","378","379","380","381","382","383","384","385","386","387","388","389","390","391",
                 "392","393","394","395","396","397","398","399","400","401","402","403","404","405","406","407",
                 "408","409","410","411","412","413","414","415","416","417","418","419","420","421","422","423",
                 "424","425","426","427","428","429","430","431","432","433","434","435","436","437","438","439",
                 "440","441","442","443","444","445","446","447","448","449","450","451","452","453","454","455",
                 "456","457","458","459","460","461","462","463","464","465","466","467","468","469","470","471",
                 "472","473","474","475","476","477","478","479","480","481","482","483","484","485","486","487",
                 "488","489","490","491","492","493","494","495","496","497","498","499","500","501","502","503",
                 "504","505","506","507","508","509","510","511","512","513","514","515","516","517","518","519",
                 "520","521","522","523","524","525","526","527","528","529","530","531","532","533","534","535",
                 "536","537","538","539","540","541","542","543","544","545","546","547","548","549","550","551",
                 "552","553","554","555","556","557","558","559","560","561","562","563","564","565","566","567",
                 "568","569","570","571","572","573","574","575","576","577","578","579","580","581","582","583",
                 "584","585","586","587","588","589","590","591","592","593","594","595","596","597","598","599",
                 "600","601","602","603","604","605","606","607","608","609","610","611","612","613","614","615",
                 "616","617","618","619","620","621","622","623","624","625","626","627","628","629","630","631",
                 "632","633","634","635","636","637","638","639","640","641","642","643","644","645","646","647",
                 "648","649","650","651","652","653","654","655","656","657","658","659","660","661","662","663",
                 "664","665","666","667","668","669","670","671","672","673","674","675","676","677","678","679",
                 "680","681","682","683","684","685","686","687","688","689","690","691","692","693","694","695",
                 "696","697","698","699","700","701","702","703","704","705","706","707","708","709","710","711",
                 "712","713","714","715","716","717","718","719","720","721","722","723","724","725","726","727",
                 "728","729","730","731","732","733","734","735","736","737","738","739","740","741","742","743",
                 "744","745","746","747","748","749","750","751","752","753","754","755","756","757","758","759",
                 "760","761","762","763","764")}

  if (any(anydevice$ASPH)) {
    ASPH_DF <- bind_rows(mget(ls(pattern = "(ASPH)_[[:digit:]]+")))
    names(ASPH_DF) <- c("ID","Depth",paste0("A_",wl_ASPH))
    # Check that no old DB is earased accidentally
    lighthouse::check_l3(project, L3, set="ASPH")
    readr::write_csv(ASPH_DF,
                     file = file.path(L3,"ASPH",
                                      paste0("ASPH_DB_",Sys.Date(),"_",mission,".csv")))
    ASPHreport <- TRUE
  } else {ASPHreport <- FALSE}


  # BB9
  if (!exists("wl_BB9")) {wl_BB9 <- c("412","440","488","510","532","595","650","676","715")}

  if (any(anydevice$BB9)) {
    DF_list <- purrr::map(mget(ls(pattern = "(BB9)_[[:digit:]]+")),
                          setNames,
                          c("ID","Depth",paste0("Bbp_",wl_BB9),paste0("Bb_",wl_BB9),"Bbp_555","nuP"))
    BB9_DF <- bind_rows(DF_list)
    lighthouse::check_l3(project, L3, set="BB9")
    readr::write_csv(BB9_DF,
                     file = file.path(L3,"BB9",
                                      paste0("BB9_DB_",Sys.Date(),"_",mission,".csv")))
    BB9report <- TRUE
  } else {BB9report <- FALSE}



  # CTD
  if (any(anydevice$CTD)) {
    CTD_DF <- bind_rows(mget(ls(pattern = "(CTD)_[[:digit:]]+")))
    names(CTD_DF) <- c("ID","Depth","Temp","PSU")
    lighthouse::check_l3(project, L3, set="CTD")
    readr::write_csv(CTD_DF,
                     file = file.path(L3,"CTD",
                                      paste0("CTD_DB_",Sys.Date(),"_",mission,".csv")))
    CTDreport <- TRUE
  } else {CTDreport <- FALSE}

  # FLECO
  if (any(anydevice$FLECO)) {
    FLECO_DF <- bind_rows(mget(ls(pattern = "(FLECO)_[[:digit:]]+")))
    lighthouse::check_l3(project, L3, set="FLECO")
    readr::write_csv(FLECO_DF,
                     file = file.path(L3,"FLECO",
                                      paste0("FLECO_DB_",Sys.Date(),"_",mission,".csv")))
    FLECOreport <- TRUE
  } else {FLECOreport <- FALSE}

  # HS6
  if (!exists("wl_HS6")) {wl_HS6 <- c("394","420","470","532","620","700")}

  if (any(anydevice$HS6)){
    DF_list <- purrr::map(mget(ls(pattern = "(HS6)_[[:digit:]]+")),
                          setNames,
                          c("ID","Depth",paste0("Bbp_",wl_HS6),paste0("Bb_",wl_HS6),"FDOM","FCHL","Bbp_555","nuP"))
    HS6_DF <- bind_rows(DF_list)
    lighthouse::check_l3(project, L3, set="HS6")
    readr::write_csv(HS6_DF,
                     file = file.path(L3,"HS6",
                                      paste0("HS6_DB_",Sys.Date(),"_",mission,".csv")))
    HS6report <- TRUE
  } else {HS6report <- FALSE}



# HTML report -------------------------------------------------------------

  require(rmarkdown)

  report = paste0("Report_IOP_",Sys.Date(),"_",mission,".Rmd")

  cat(paste0("---\ntitle: '<center>IOP report for __",mission,"__ </center>'\n",
             "author: ''\n",
             "header-includes:\n",
             "output:\n\x20html_document:\n\x20\x20toc: true\n\x20\x20toc_float: true\n\x20\x20toc_depth: 5\n\x20\x20number_sections: true\n---\n\n"),
      file=report, append = F)

  cat("<style>\n\ntable, td, th {\n\tborder: none;\n\tpadding-left: 1em;\n\tpadding-right: 1em;\n\tmargin-left: auto;\n\tmargin-right: auto;\n\tmargin-top: 1em;\n\tmargin-bottom: 1em;\n}\n\n</style>\n\n",
      file=report, append = T)

  cat(paste0("```{r setup, include=FALSE, echo=TRUE, message=FALSE}\n",
             "require(dplyr)\nrequire(tidyr)\nrequire(ggplot2)\nrequire(latex2exp)\nrequire(plotly)\nrequire(stargazer)\n",
             "```\n"), file = report, append = T)

  cat(paste0("<center><font size='5'> Generated with Riops package __version: ",packageVersion("Riops"),"__ \n  \n",
             "Date: __",Sys.time(),"__ GMT</font></center>\n"), file = report, append=T)

  # ACS
  if (ACSreport) {
    # ACS A  spectrum plot
    cat("\n# ACS \n\n", file = report, append=T)
    cat("\n## ACS A \n\n", file = report, append=T)
    cat(paste0("```{r,echo=FALSE, message=FALSE}\n",
               "ACSplot <- ACS_DF %>% pivot_longer(cols = all_of(str_subset(names(ACS_DF),
                                                         \"([:alnum:]+_)?A(?=(_[:digit:]+))\")),
                                names_to = c(\".value\",\"Lambda\"),
                                names_pattern = \"(.+)_(.+)\") %>%
    filter(!is.na(A)) %>%
    mutate(depthKey = paste(ID,Depth,sep = \"_\"), Lambda = as.numeric(Lambda))

ACS <- highlight_key(ACSplot, ~Depth, \"Depth filter\")
    gg <- ACS %>%
             ggplot(aes(Lambda, A, group=depthKey, color=ID)) +
             geom_line(alpha=0.5) + ylab(\"C [m-1]\") +
             scale_x_continuous(breaks = c(400,500,600,770))

    plot <- highlight(ggplotly(gg, dynamicTicks=T), selectize = T, persistent=T, dynamic = F, opacityDim = 0.001)
    plot\n",
               "```\n"), file = report, append = T)

    # ACS A stats table
    cat(paste0("```{r,echo=FALSE,results='asis'}\n",
               "ACSstat <- ACS_DF %>% na.exclude %>% group_by(ID) %>%
             filter(Depth == min(Depth)) %>% ungroup() %>% select(ID,Depth,str_subset(names(ACS_DF),
                                                         \"([:alnum:]+_)?A(?=(_[:digit:]+))\")) \n",
               "stargazer(as.data.frame(ACSstat),",
               "type = \"html\", column.sep.width = \"5pt\", label = \"A summary\", title = \"A summary\")\n",
               "```\n"), file = report, append = T)

    # ACS C  spectrum plot
    cat("\n## ACS C \n\n", file = report, append=T)
    cat(paste0("```{r,echo=FALSE, message=FALSE}\n",
               "ACSplot <- ACS_DF %>% pivot_longer(cols = all_of(str_subset(names(ACS_DF),
                                                         \"([:alnum:]+_)?C(?=(_[:digit:]+))\")),
                                names_to = c(\".value\",\"Lambda\"),
                                names_pattern = \"(.+)_(.+)\") %>%
    filter(!is.na(C)) %>%
    mutate(depthKey = paste(ID,Depth,sep = \"_\"), Lambda = as.numeric(Lambda))

ACS <- highlight_key(ACSplot, ~Depth, \"Depth filter\")
    gg <- ACS %>%
             ggplot(aes(Lambda, C, group=depthKey, color=ID)) +
             geom_line(alpha=0.5) + ylab(\"C [m-1]\") +
             scale_x_continuous(breaks = c(400,500,600,770))

    plot <- highlight(ggplotly(gg, dynamicTicks=TRUE), selectize = TRUE, persistent=T, dynamic = F, opacityDim = 0.001)
    plot\n",
               "```\n"), file = report, append = T)

    # ACS C stats table
    cat(paste0("```{r,echo=FALSE,results='asis'}\n",
               "ACSstat <- ACS_DF %>% na.exclude %>% group_by(ID) %>%
             filter(Depth == min(Depth)) %>% ungroup() %>% select(ID,Depth,str_subset(names(ACS_DF),
                                                         \"([:alnum:]+_)?C(?=(_[:digit:]+))\")) \n",
               "stargazer(as.data.frame(ACSstat),",
               "type = \"html\", column.sep.width = \"5pt\", label = \"C summary\", title = \"C summary\")\n",
               "```\n"), file = report, append = T)
  }

  # ASPH spectrum plot
  if (ASPHreport) {
    cat("\n# ASPH \n\n", file = report, append=T)
    cat(paste0("```{r,echo=FALSE, message=FALSE}\n",
               "ASPHplot <- ASPH_DF %>% pivot_longer(cols = all_of(str_subset(names(ASPH_DF),
                                                         \"([:alnum:]+_)?A(?=(_[:digit:]+))\")),
                                names_to = c(\".value\",\"Lambda\"),
                                names_pattern = \"(.+)_(.+)\") %>%
    filter(!is.na(A)) %>%
    mutate(depthKey = paste(ID,Depth,sep = \"_\"), Lambda = as.numeric(Lambda))

ASPH <- highlight_key(ASPHplot, ~Depth, \"Depth filter\")
    gg <- ASPH %>%
             ggplot(aes(Lambda, A, group=depthKey, color=ID)) +
             geom_line(alpha=0.5) + ylab(\"A [m-1]\") +
             scale_x_continuous(breaks = c(350,500,600,800))

    plot <- highlight(ggplotly(gg, dynamicTicks=TRUE), selectize = TRUE, persistent=T, dynamic = F, opacityDim = 0.001)
    plot\n",
               "```\n"), file = report, append = T)

    # ASPH stats table
    cat(paste0("```{r,echo=FALSE,results='asis'}\n",
               "Astat <- ASPH_DF %>% na.exclude %>% group_by(ID) %>%
             filter(Depth == min(Depth)) %>% ungroup() %>% select(!Depth)\n",
               "stargazer(as.data.frame(Astat),",
               "type = \"html\", column.sep.width = \"5pt\", label = \"At summary\", title = \"At summary\")\n",
               "```\n"), file = report, append = T)
  }

  #BB9
  if (BB9report) {
    cat("\n# BB9 \n\n", file = report, append=T)
    # BB9 Bb spectrum plot
    cat("\n## BB9 Bb \n\n", file = report, append=T)
    cat(paste0("```{r,echo=FALSE, message=FALSE}\n",
               "BB9plot <- BB9_DF %>% pivot_longer(cols = all_of(str_subset(names(BB9_DF),
                                                         \"([:alnum:]+_)?Bb(?=(_[:digit:]+))\")),
                                names_to = c(\".value\",\"Lambda\"),
                                names_pattern = \"(.+)_(.+)\") %>%
    filter(!is.na(Bb)) %>%
    mutate(depthKey = paste(ID,Depth,sep = \"_\"), Lambda = as.numeric(Lambda))

    BB9 <- highlight_key(BB9plot, ~Depth, \"Depth filter\")
    gg <- BB9 %>%
             ggplot(aes(Lambda, Bb, group=depthKey, color=ID)) +
             geom_line(alpha=0.5) + ylab(\"Bb [m-1]\") +
             scale_x_continuous(breaks = c(400,500,600,730))

    plot <- highlight(ggplotly(gg, dynamicTicks=TRUE), selectize = TRUE, persistent=T, dynamic = F, opacityDim = 0.001)
    plot\n"
               ,
               "```\n"), file = report, append = T)

    # BB9 Bb stats table
    cat(paste0("```{r,echo=FALSE,results='asis'}\n",
               "BB9stat <- BB9_DF %>% na.exclude %>% group_by(ID) %>%
             filter(Depth == min(Depth)) %>% ungroup() %>% select(ID,Depth,str_subset(names(BB9_DF),
                                                         \"([:alnum:]+_)?Bb(?=(_[:digit:]+))\")) \n",
               "stargazer(as.data.frame(BB9stat),",
               "type = \"html\", column.sep.width = \"5pt\", label = \"Bb summary\", title = \"Bb summary\")\n",
               "```\n"), file = report, append = T)

    # BB9 Bbp
    cat("\n## BB9 Bbp \n\n", file = report, append=T)
    cat(paste0("```{r,echo=FALSE, message=FALSE}\n",
               "BB9plot <- BB9_DF %>% pivot_longer(cols = all_of(str_subset(names(BB9_DF),
                                                         \"([:alnum:]+_)?Bbp(?=(_[:digit:]+))\")),
                                names_to = c(\".value\",\"Lambda\"),
                                names_pattern = \"(.+)_(.+)\") %>%
    filter(!is.na(Bbp)) %>%
    mutate(depthKey = paste(ID,Depth,sep = \"_\"), Lambda = as.numeric(Lambda))

    BB9 <- highlight_key(BB9plot, ~Depth, \"Depth filter\")
    gg <- BB9 %>%
             ggplot(aes(Lambda, Bbp, group=depthKey, color=ID)) +
             geom_line(alpha=0.5) + ylab(\"Bbp [m-1]\") +
             scale_x_continuous(breaks = c(400,500,600,730))

    plot <- highlight(ggplotly(gg, dynamicTicks=TRUE), selectize = TRUE, persistent=T, dynamic = F, opacityDim = 0.001)
    plot\n"
               ,
               "```\n"), file = report, append = T)

    # BB9 Bbp stats table
    cat(paste0("```{r,echo=FALSE,results='asis'}\n",
               "BB9stat <- BB9_DF %>% na.exclude %>% group_by(ID) %>%
             filter(Depth == min(Depth)) %>% ungroup() %>% select(ID,Depth,str_subset(names(BB9_DF),
                                                         \"([:alnum:]+_)?Bbp(?=(_[:digit:]+))\")) \n",
               "stargazer(as.data.frame(BB9stat),",
               "type = \"html\", column.sep.width = \"5pt\", label = \"Bb summary\", title = \"Bb summary\")\n",
               "```\n"), file = report, append = T)
  }

  # CTD
  if (CTDreport) {
    cat("\n# CTD \n\n", file = report, append=T)
    # CTD plot
    cat(paste0("```{r,echo=FALSE, message=FALSE}\n",
               "CTDplot <- CTD_DF %>% na.omit() %>%
    mutate(depthKey = paste(ID,Depth,sep = \"_\"))

 meltDF <- reshape2::melt(CTDplot, id.vars=c(\"ID\",\"Depth\",\"depthKey\"))
    gg <- meltDF %>%
             ggplot(aes(x=value,y=Depth, group=ID, color=ID)) +
             geom_line() + xlab(\"Values\") +
             ylab(\"Depth [m]\")+ facet_wrap(~variable,nrow = 1,scales = \"free_x\") +
      scale_y_reverse()

    ggplotly(gg)\n"
               ,
               "```\n"), file = report, append = T)

    # CTD stats table
    cat(paste0("```{r,echo=FALSE,results='asis'}\n",
               "CTDstat <- CTD_DF %>% na.exclude\n",
               "stargazer(as.data.frame(CTDstat),",
               "type = \"html\", column.sep.width = \"5pt\", label = \"Bb summary\", title = \"Bb summary\")\n",
               "```\n"), file = report, append = T)
  }

  # HS6
  if (HS6report) {
    cat("\n# HS6 \n\n", file = report, append=T)
    # Bb
    cat("\n## HS6 Bb \n\n", file = report, append=T)
    cat(paste0("```{r,echo=FALSE, message=FALSE}\n",
               "HS6plot <- HS6_DF %>% pivot_longer(cols = all_of(str_subset(names(HS6_DF),
                                                         \"([:alnum:]+_)?Bb(?=(_[:digit:]+))\")),
                                names_to = c(\".value\",\"Lambda\"),
                                names_pattern = \"(.+)_(.+)\") %>%
    filter(!is.na(Bb) & Bb > 0) %>%
    mutate(depthKey = paste(ID,Depth,sep = \"_\"), Lambda = as.numeric(Lambda))

    HS6 <- highlight_key(HS6plot, ~Depth, \"Depth filter\")
    gg <- HS6 %>%
             ggplot(aes(Lambda, Bb, group=depthKey, color=ID)) +
             geom_line(alpha=0.5) + ylab(\"Bb [m-1]\") +
             scale_x_continuous(breaks = c(380,500,600,710))

    plot <- highlight(ggplotly(gg, dynamicTicks=TRUE), selectize = TRUE, persistent=T, dynamic = F, opacityDim = 0.001)
    plot\n"
               ,
               "```\n"), file = report, append = T)

    # HS6 Bb stats table
    cat(paste0("```{r,echo=FALSE,results='asis'}\n",
               "HS6stat <- HS6_DF %>% na.exclude %>% group_by(ID) %>%
             filter(Depth == min(Depth)) %>% ungroup() %>% select(ID,Depth,str_subset(names(HS6_DF),
                                                         \"([:alnum:]+_)?Bb(?=(_[:digit:]+))\")) \n",
               "stargazer(as.data.frame(HS6stat),",
               "type = \"html\", column.sep.width = \"5pt\", label = \"Bb summary\", title = \"Bb summary\")\n",
               "```\n"), file = report, append = T)

    # Bbp Spectrum plot
    cat("\n## HS6 Bbp \n\n", file = report, append=T)
    cat(paste0("```{r,echo=FALSE, message=FALSE}\n",
               "HS6plot <- HS6_DF %>% pivot_longer(cols = all_of(str_subset(names(HS6_DF),
                                                         \"([:alnum:]+_)?Bbp(?=(_[:digit:]+))\")),
                                names_to = c(\".value\",\"Lambda\"),
                                names_pattern = \"(.+)_(.+)\") %>%
    filter(!is.na(Bbp) & Bbp > 0) %>%
    mutate(depthKey = paste(ID,Depth,sep = \"_\"), Lambda = as.numeric(Lambda))

    HS6 <- highlight_key(HS6plot, ~Depth, \"Depth filter\")
    gg <- HS6 %>%
             ggplot(aes(Lambda, Bbp, group=depthKey, color=ID)) +
             geom_line(alpha=0.5) + ylab(\"Bbp [m-1]\") +
             scale_x_continuous(breaks = c(380,500,600,710))

    plot <- highlight(ggplotly(gg, dynamicTicks=TRUE), selectize = TRUE, persistent=T, dynamic = F, opacityDim = 0.001)
    plot\n"
               ,
               "```\n"), file = report, append = T)

    # HS6 Bb stats table
    cat(paste0("```{r,echo=FALSE,results='asis'}\n",
               "HS6stat <- HS6_DF %>% na.exclude %>% group_by(ID) %>%
             filter(Depth == min(Depth)) %>% ungroup() %>% select(ID,Depth,str_subset(names(HS6_DF),
                                                         \"([:alnum:]+_)?Bbp(?=(_[:digit:]+))\")) \n",
               "stargazer(as.data.frame(HS6stat),",
               "type = \"html\", column.sep.width = \"5pt\", label = \"Bb summary\", title = \"Bb summary\")\n",
               "```\n"), file = report, append = T)
  }


  render(report, output_dir = L3)
  file.remove(report)

}
