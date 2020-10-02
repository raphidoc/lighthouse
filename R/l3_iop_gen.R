#' @name generate_iop_db
#' @title generate instrument databases in L3
#' @author Raphael Mabit
#'
#' @import dplyr
#' @import stringr
#' @description Loop over all L2 Station QC above 0 (in iop_processing_log) to load and extract
#' all non-empty list in IOP.fitted.down.RData file.
#' Create in L3 one csv file by instrument and a global html report for all processed variables.
#' @export

l3_iop_gen <- function(project, mission="XXX", boat=c("")){
  #From some reading in https://stackoverflow.com/questions/13649979/what-specifically-are-the-dangers-of-evalparse
  #The method used here should be changed ....

# Filter setup ------------------------------------------------------------

  L2 <- file.path(project, "L2")

  LogFile <- list.files(path = file.path(project,"ProLog"), pattern = "Riops_processing_log", recursive = F, full.names = T)

  ProLog <- data.table::fread(file = LogFile, data.table = F, colClasses = "character")

  # List available data point in L2
  dirs <- grep("/IOP$",list.dirs(L2,recursive = T), value = T)
  IOPframe <- data.frame(dirs)

  IOPframe <- IOPframe %>%
    mutate(ID = str_extract(dirs, "(?<=/)[[:digit:]]+"))

  # Identifies paths with ProLog
  ProLog <- ProLog %>% inner_join(IOPframe, by="ID")

  # Filter QC
  ProLog <- ProLog %>% filter(QC > 0)


# loop over all dataset in all directories --------------------------------

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
          if (exists("wl_ASPH") && wl_ASPH != eval(parse(text=paste0("IOP.fitted.down$",names(device[i2]),"$",var_x)))) warning(paste("ASPH wavelength have changed:",ID))
          wl_ASPH <- eval(parse(text=paste0("IOP.fitted.down$",names(device[i2]),"$",var_x)))
          next()
        }
        if(var_x == "wl" & names(device[i2]) == "BB9"){
          if (exists("wl_BB9") && wl_BB9 != eval(parse(text=paste0("IOP.fitted.down$",names(device[i2]),"$",var_x)))) warning(paste("BB9 wavelength have changed:",ID))
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

  # ACS, round wl to avoid float ?
  if (any(str_detect(objects(), "ACS"))) {
    DF_list <- purrr::map(mget(ls(pattern = "(ACS)_[[:digit:]]+")),
                          setNames,
                          c("ID","Depth",paste0("A_",a_wl_ACS),paste0("C_",c_wl_ACS)))
    ACS_DF <- bind_rows(DF_list)
    lighthouse::check_l3(project, L3, set="ACS")
    readr::write_csv(ACS_DF,
                     path = file.path(L3,"ACS",
                                      paste0("ACS_DB_",Sys.Date(),"_",str_c(mission, boat, collapse = "_"),".csv")))
    ACSreport <- TRUE
  } else {ACSreport <- FALSE}


  # ASPH
  if (any(str_detect(objects(), "ASPH"))) {
    ASPH_DF <- bind_rows(mget(ls(pattern = "(ASPH)_[[:digit:]]+")))
    names(ASPH_DF) <- c("ID","Depth",paste0("A_",wl_ASPH))
    # Check that no old DB is earased accidentally
    lighthouse::check_l3(project, L3, set="ASPH")
    readr::write_csv(ASPH_DF,
                     path = file.path(L3,"ASPH",
                                      paste0("ASPH_DB_",Sys.Date(),"_",str_c(mission, boat, collapse = "_"),".csv")))
    ASPHreport <- TRUE
  } else {ASPHreport <- FALSE}


  # BB9
  if (any(str_detect(objects(), "BB9"))) {
    DF_list <- purrr::map(mget(ls(pattern = "(BB9)_[[:digit:]]+")),
                          setNames,
                          c("ID","Depth",paste0("Bbp_",wl_BB9),paste0("Bb_",wl_BB9),"Bbp_555","nuP"))
    BB9_DF <- bind_rows(DF_list)
    lighthouse::check_l3(project, L3, set="BB9")
    readr::write_csv(BB9_DF,
                     path = file.path(L3,"BB9",
                                      paste0("BB9_DB_",Sys.Date(),"_",str_c(mission, boat, collapse = "_"),".csv")))
    BB9report <- TRUE
  } else {BB9report <- FALSE}



  # CTD
  if (any(str_detect(objects(), "CTD"))) {
    CTD_DF <- bind_rows(mget(ls(pattern = "(CTD)_[[:digit:]]+")))
    names(CTD_DF) <- c("ID","Depth","Temp","PSU")
    lighthouse::check_l3(project, L3, set="CTD")
    readr::write_csv(CTD_DF,
                     path = file.path(L3,"CTD",
                                      paste0("CTD_DB_",Sys.Date(),"_",str_c(mission, boat, collapse = "_"),".csv")))
    CTDreport <- TRUE
  } else {CTDreport <- FALSE}

  # FLECO
  if (any(str_detect(objects(), "FLECO"))) {
    FLECO_DF <- bind_rows(mget(ls(pattern = "(FLECO)_[[:digit:]]+")))
    lighthouse::check_l3(project, L3, set="FLECO")
    readr::write_csv(FLECO_DF,
                     path = file.path(L3,"FLECO",
                                      paste0("FLECO_DB_",Sys.Date(),"_",str_c(mission, boat, collapse = "_"),".csv")))
    FLECOreport <- TRUE
  } else {FLECOreport <- FALSE}

  # HS6
  if (any(str_detect(objects(), "HS6"))){
    DF_list <- purrr::map(mget(ls(pattern = "(HS6)_[[:digit:]]+")),
                          setNames,
                          c("ID","Depth",paste0("Bbp_",wl_HS6),paste0("Bb_",wl_HS6),"FDOM","FCHL","Bbp_555","nuP"))
    HS6_DF <- bind_rows(DF_list)
    lighthouse::check_l3(project, L3, set="HS6")
    readr::write_csv(HS6_DF,
                     path = file.path(L3,"HS6",
                                      paste0("HS6_DB_",Sys.Date(),"_",str_c(mission, boat, collapse = "_"),".csv")))
    HS6report <- TRUE
  } else {HS6report <- FALSE}



# HTML report -------------------------------------------------------------

  require(rmarkdown)

  report = paste0("Report_IOP_DB_",Sys.Date(),"_",str_c(mission,boat,collapse = "_"),".Rmd")

  cat(paste0("---\ntitle: '<center>IOP report for __",str_c(mission,boat,collapse = " "),"__ </center>'\n",
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
