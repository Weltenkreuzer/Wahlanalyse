library(gdata)

get_key <- function(filename) {
  pos <- strsplit(filename, "_")
  return (pos[[1]][1])
}

get_dir <- function(directory) {
	files <- dir(directory)
	filestem <- sapply(files, get_key)
	gemkeys <- unique(filestem)
	return (gemkeys)
}

get_gemeinde <- function(directory, gemkey) {
	
		# Grunddaten einlesen
		# basedata <- read.table(paste(directory, "/", gemkeys[i],"-001z.csv"), fill=T, sep=";")
		
		# Korrekte Datenzeilen auslesen
		# valid <- !is.na(as.numeric(levels(basedata[,2]))[basedata[,2]])
		# basedata <- basedata[valid,]

		# Wahljahr auslesen
		#election <- sapply(basedata[,1], as.character)
		#election <- sapply(election, substr, 7, 10)

		# Wahlberechtigte und W?hler auslesen und in nuermisch umwandeln
		# berechtigte <- as.numeric(levels(basedata[,2]))[basedata[,2]]
		# waehler <- as.numeric(levels(basedata[,3]))[basedata[,3]]

		# Wahlbeteiligung
		# quote <- as.numeric(sub(",", ".", basedata[,4], fixed = TRUE))

  	# Gesamtstimmenzahl einlesen
	  basedata <- read.table(paste(directory, "/", gemkey,"_ges.csv", sep=""), fill=T, sep=";")
	  basedata.erst <- read.table(paste(directory, "/", gemkey,"_erst.csv", sep=""), fill=T, sep=";")
	  
    # Relevante Parteien und Wahlen auswählen
		relevant <- c(1,2,3,4,5,8,10,11)
		
    # Daten extrahieren
	  basedata <- basedata[11:25,] # Groben Ausschnitt ausschneiden
	  basedata <- basedata[relevant,] # Relevante Parteien ausschneiden
	  
    basedata.erst <- basedata.erst[11:25,] # Groben Ausschnitt ausschneiden
	  basedata.erst <- basedata.erst[relevant,] # Relevante Parteien ausschneiden
	  
    #  Variablennamen vorbereiten
	  reduce <- basedata[,-1]
    reduce.erst <- basedata.erst[,-1]
	  
    # In Vektor umwandeln (Benennung erfolgt automatisch)
		row <- unmatrix(reduce, byrow=T)
    row <- as.numeric(row)

	  row.erst <- unmatrix(reduce.erst, byrow=T)
	  row.erst <- as.numeric(row.erst)
    
    return(c(row, row.erst))
    
		# Korrekte Datenzeilen auslesen
		#valid <- basedata[,1] == "Zweitstimmen"
		#basedata <- basedata[valid,]

		# G?ltige und ung?ltige Stimmen auslesen
		#gueltig <- as.numeric(levels(basedata[,3]))[basedata[,3]]
		#ungueltig <- as.numeric(levels(basedata[,4]))[basedata[,4]]


		# Wahldaten einlesen
		#basedata <- read.table(paste(directory, "/", gemkeys[i],"-003z.csv"), fill=T, sep=";")
		
		# Korrekte Datenzeilen auslesen
		#valid <- !basedata[,2] == ""
		#basedata <- basedata[valid,]
		#basedata <- basedata[4:nrow(basedata),]
	
}

get_election <- function(directory) {
  
  # Spalten- und Zeilen-Header
  elections <- c("46", "50", "54", "58", "62", "66", "70", "74", "78", "82", "86", "90", "94", "98", "03", "08")
  parteien <- c("CSU", "SPD", "GRUEN", "FW", "FDP", "BP", "BB", "LINKE") # Reihenfolge wie 'relevant'   
  nmat <- outer(parteien, elections, paste, sep = ":")
  colnames <- c(paste(t(nmat),":GES", sep=""), paste(t(nmat),":ERST", sep=""))
  
  gemkeys <- get_dir(directory)
  data <- data.frame()
  for (i in 1:length(gemkeys)) {
    var_name <- paste("g",gemkeys[i],sep="")
    gemeinde <- get_gemeinde(directory, gemkeys[i])
    data <- rbind(data, gemeinde)
  }  
  colnames(data) <- colnames
  rownames(data) <- gemkeys
  
  return(data)
}