suppressMessages(library("data.table"))
suppressMessages(library("ggplot2"))
suppressMessages(library("tidyverse"))
suppressMessages(library("haven"))
suppressMessages(library("labelled"))
suppressMessages(library("here"))
suppressMessages(library("readxl"))
suppressMessages(library("zoo"))


# For `process_data.R`
suppressMessages(library("tidyverse"))
suppressMessages(library("patchwork"))
suppressMessages(library("cowplot"))
suppressMessages(library("ggsci"))
suppressMessages(library("rgeoboundaries"))
suppressMessages(library("plot3D"))
suppressMessages(library("viridis"))
suppressMessages(library("spdep"))
suppressMessages(library("rstatix"))
suppressMessages(library("scales"))
suppressMessages(library("units"))
suppressMessages(library("ggpattern"))
suppressMessages(library("magick"))





COL.REG <- c("#34B4EB", "#F4C470", "#D36167", "#B9DB7D" , "#43A278", "#FFB0B3")
names(COL.REG) <- c("Caribe", "Eje cafetero y Antioquia", "Pacífica", "Central", "Llanos", "Amazonía")

COL.PDET <- c("#D36167","#43A278")
CAT_PDET <- c("PDET", "No PDET")
names(COL.PDET) <- CAT_PDET

COL.TYPE <- c("#476314", "#71a800", "#feb502", "#ffe77e", "#0083a8", "#7ab5f5", "#bdf0ff")
CAT_LEVELS <- c("Strongly affected and persistent", "Mildly affected and persistent", "Strongly affected and disrupted", "Mildly affected and disrupted", "Strongly affected and finished", "Midly affected and finished", "With no conflicts")
names(COL.TYPE) <- CAT_LEVELS
