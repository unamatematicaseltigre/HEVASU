#FUNCIONES PARA GENERAR LOS ARCHIVO DE NOMINA
#Fecha : 11/05/2014
#Autor : José L. Romero
#-----------------------------------------------------------------------

#sobre las rutas de los archivos de nomina y los nombres de
#esas variables (esto varia con el CL seleccionado)
#VARIABLES GLOBALES
establecer_rutas_nomina <- function() {
	ruta_nomina_csv <<- paste0("nomina_regular",codigoCL,".csv")
	ruta_nomina_intro_csv <<- paste0("nomina_intro",codigoCL,".csv")
	ruta_nomina_RData <<- paste0("nomina_regular",codigoCL,".RData")
	ruta_nomina_intro_RData <<- paste0("nomina_intro",codigoCL,".RData")
}
establecer_rutas_nomina()

#-----------------------------------------------------------------------
generar_nomina_regular_CL <- 
	function(archivo_pdf,archivo_csv,codCL=codigoCL) {
		#nota: el layout del pdf ha cambiado a partir del semestre 2014-1.
		#      la 2nda columna indica la unidad de apoyo.
		#códigos de error para esta función
		#  01 - archivo pdf defectuoso
		#  03 - la data del pdf y la del csv no se corresponden
		#  04 - Debe revisar y luego reparar el archivo csv.		
		#  00 - todo bien
		mensaje <- paste0("Procesando archivo de nomina de ",
			tablaCL[codCL],": [",paste(rep("░",20),collapse=""),"]")
		svalue(barra_status) <- mensaje; Sys.sleep(0.001)
		#Convierte el listado en pdf a un archivo de texto
		error <- system(paste("pdftotext -layout",archivo_pdf,
			"matricula"))
		if (error==1) {
			svalue(barra_status) <- ""
			return(1) }		#archivo pdf defectuoso
		#cadenas de busqueda de líneas especiales y caracter de separación
		pb <- paste("UNIVERSIDAD NACIONAL ABIERTA","SECRETARIA",
			"LISTADO DE ESTUDIANTES","LAPSO [0-9]* AL:","JEFE URyCE",
			"Res\\. N","CEDULA( )+APELLIDOS","Page","Centro Local: ",
			"___",sep="|")
		#para la definición de locs y cloa, ver "editar_asesores.R"
		car_sep <- "\t"
		#Leer el archivo matricula para el primer pase
		archivo <- file("matricula", "r", blocking = FALSE)
		lineas <- readLines(archivo)
		close(archivo)
		unlink("matricula")
		#Unica pasada del archivo
		nomina <- data.frame(cedula=numeric(0),nombre=character(0),
			cloa=character(0),carrera=numeric(0),materias=character(0))
		total_tarea <- length(lineas)
		contador <- 0
		progreso_ant <- 0
		if (codigoCL=="00") {
			tmp <- tablaCL[codCL]
			nombre_centro <- substr(tmp,6,nchar(tmp))
			tmp <- as.character(centros[nombre_centro,1])
			oficinas <- strsplit(tmp,"|",fixed=TRUE)[[1]]
		} else
			oficinas <- cloas
		for (i in lineas) {
			#filtrar cada línea que no sea renglón de datos
			if (length(grep(pb,i))==0) {
				if (!i %in% c("\n","\x0c","")) {
					#eliminar el número de renglón al principio
					j <- gsub("^ *[0-9]+ +","",i)
					#sustituir multiples espacios por el caracter de separación
					j <- gsub("( )\\1+",car_sep,j)
					#sustituir por el caractér de separación en casos especiales
					j <- gsub("([A-Z])( )([0-9])",paste0("\\1",car_sep,"\\3"),j)
					j <- gsub("([A-Z])([0-9])",paste0("\\1",car_sep,"\\2"),j)
					j <- gsub("([0-9])( )([A-Z])",paste0("\\1",car_sep,"\\3"),j)
					k <- strsplit(j,car_sep)[[1]]
					ubicacion <- oficinas[
						sapply(oficinas,function(i) substr(i,6,7)==k[1])]
					ci <- as.numeric(k[2])
					nombre <- paste(k[3:4],collapse=", ")
					carrera <- as.numeric(substring(k[5],1,3))
					nomina <- rbind(nomina,data.frame(cedula=ci,nombre=nombre,
						cloa=ubicacion,carrera=carrera, materias=k[6]) )
				}
			}
			#actualiza contador y barra de estatus:
			contador <- contador + 1
			progreso <- round(contador/total_tarea*20)
			if (progreso>progreso_ant) {
				mensaje <- paste("Procesando archivo pdf de nomina de ",
					tablaCL[codCL],": [",paste(rep("█",progreso),collapse=""),
					paste(rep("░",20-progreso),collapse=""),"]",sep="")
				svalue(barra_status) <<- mensaje; Sys.sleep(0.001)
				progreso_ant <- progreso
			}
		}
		#Ahora lee el csv con los datos personales de los estudiantes
		#regulares e incorporalos a la nomina
		svalue(barra_status) <- "Incorporando datos personales ..."
		DP <- read.table(archivo_csv,header=TRUE,sep=";")
		#incorpora los datos
		indices <- sapply(nomina$cedula,function(i)
			ifelse(length(which(DP$cedula==i))==1,which(DP$cedula==i)[1],NA) )
		sexo <- DP$Sexo[indices]
		#para la fecha de nacimiento, verificar si es "1965-01-01",
		#ya que en esa fecha hubo cambio de zona horaria y generará error
		#al escribir a Excel.
		#(Ver http://en.wikipedia.org/wiki/Time_in_Venezuela)
		fecha_nacimiento <- DP$Fecha_nac[indices]
		cambio_fecha <- which(fecha_nacimiento=="1965-01-01")
		if (length(cambio_fecha)>0)
			fecha_nacimiento[cambio_fecha] <- "1965-01-02"
		correo <- limpia_correos(DP$Correo[indices])
		celular <- 	DP$Celular[indices]
		telefono <- DP$Telefono[indices]
		nomina <- cbind(nomina,sexo=sexo,fecha_nacimiento=fecha_nacimiento,
			correo=correo,celular=celular,telefono=telefono)
		nomina <- within(nomina, {
			fecha_nacimiento <- as.Date(fecha_nacimiento,"%Y-%m-%d")
			celular <- as.character(celular)
			telefono <- as.character(telefono) } )
		#antes de guardar la data a archivos, determina si se trata del
		#Nivel Central
		if (codigoCL=="00") {
			#Para el Nivel Central solo se crea el archivo csv.
			#Después se creará el archivo RData
			if (file.access(ruta_nomina_csv)!=0) {
				#si no existe el archivo csv crealo
				write.table(nomina,file=ruta_nomina_csv,sep="\t",
					col.names=TRUE,row.names=FALSE)
			} else
				write.table(nomina,file=ruta_nomina_csv,sep="\t",
					col.names=FALSE,append=TRUE,row.names=FALSE)		
		} else {
			#Guarda el archivo de nomina en csv
			write.table(nomina,file=ruta_nomina_csv,sep="\t",
				row.names=FALSE)
			#Guarda el archivo de nomina como imágen RData
			save("nomina",file=ruta_nomina_RData)
		}#if codigoCL es el Nivel Central
		svalue(barra_status) <- ""
		if (all(is.na(indices))) return(3)
		if (any(is.na(indices))) return(4)		
		return(0)
	} #generar_nomina_regular_CL(archivo_pdf,archivo_csv)

#-----------------------------------------------------------------------	
#generar_nomina_introductorio
	generar_nomina_introductorio_CL <- function(archivo_pdf,
		archivo_csv) {
	#códigos de error para esta función
		#  01 - archivo pdf defectuoso
		#  02 - la ubicacion (CL - OA) no existe, por lo tanto el pdf
		#       no es un listado nomina UNASEC válido.
		#  03 - la data del pdf y la del csv no se corresponden
		#  04 - Debe revisar y luego reparar el archivo csv.
		#  00 - todo bien
		mensaje <- paste("Procesando archivo: [",paste(rep("░",20),
			collapse=""),"]",sep="")
		svalue(barra_status) <- mensaje; Sys.sleep(0.001)
		#Convierte el listado en pdf a un archivo de texto
		error <- system(paste("pdftotext -layout",archivo_pdf,
			"matricula"))
		if (error==1) {
			svalue(barra_status) <- ""
			return(1) }		#archivo pdf defectuoso
		#cadenas de busqueda de líneas especiales y caracter de separación
		car_sep <- "\t"
		pb <- paste("UNIVERSIDAD NACIONAL ABIERTA","SECRETARIA",
			"LISTADO DE ESTUDIANTES","CURSO INTRODUCTORIO LAPSO",
			"JEFE URyCE","Res\\. N",paste0("CEDULA",car_sep,"APELLIDOS"),
			"Page","___","Centro Local: ",sep="|")
		#para la definición de locs y cloa, ver "editar_asesores.R"
		#Leer el archivo matricula para el primer pase
		archivo <- file("matricula", "r", blocking = FALSE)
		lineas <- readLines(archivo)
		close(archivo)
		unlink("matricula")
		#Unica pasada del archivo
		nomina_intro <- data.frame(cedula=numeric(0),nombre=character(0),
			cloa=character(0),carrera=numeric(0),materias=character(0))
		total_tarea <- length(lineas)
		contador <- 0
		progreso_ant <- 0
		for (i in lineas) {
			j <- gsub("( )\\1+",car_sep,i)
			#filtrar todas las líneas y dejar la locación
			if (length(grep(pb,j))>0) {
				if (length(grep(locs,j))>0) {
					result <- regexpr(locs,j)
					st <- as.numeric(result)
					long <- attr(result,"match.length")
					ubicacion <- substring(j,st,st+long-1)
				}
			}
			else {
				if (!j %in% c("\n","\x0c","")) {
					#sustituir por el caracter de separación en casos especiales
					j <- gsub("([A-Z])( )([0-9])",paste0("\\1",car_sep,"\\3"),j)
					j <- gsub("([A-Z])([0-9])",paste0("\\1",car_sep,"\\2"),j)
					j <- gsub("([0-9])( )([0-9])",paste0("\\1",car_sep,"\\3"),j)
					j <- gsub("([0-9])( )([A-Z])",paste0("\\1",car_sep,"\\3"),j)
					k <- strsplit(j,car_sep)[[1]][-1]
					ci <- as.numeric(k[1])
					nombre <- paste(k[2:3],collapse=", ")
					carrera <- as.numeric(substring(k[4],1,3))
					#si ubicacion no existe, el archivo pdf es invalido
					#retorna un error 2 en tal caso
					if (!"ubicacion"%in%ls()) {
						svalue(barra_status) <- ""
						return(2)
					} else
						nomina_intro <- rbind(nomina_intro,
							data.frame(cedula=ci,nombre=nombre,
							cloa=ubicacion,carrera=carrera,materias="000") )
				}
			}
			#actualiza contador y barra de estatus:
			contador <- contador + 1
			progreso <- round(contador/total_tarea*20)
			if (progreso>progreso_ant) {
				mensaje <- paste("Procesando archivo pdf: [",
					paste(rep("█",progreso),collapse=""),
					paste(rep("░",20-progreso),collapse=""),"]",sep="")
				svalue(barra_status) <<- mensaje; Sys.sleep(0.001)
				progreso_ant <- progreso
			}
		}
		#Ahora lee el csv con los datos personales de la gente del CI
		#e incorporalos a la nomina
		#NOTA: el otro archivo tiene ; como separador y este una , !!!!
		#NOTA: en caso de error en la lectura de este archivo, revisar si 
		#tiene secuencias de caracteres raros como &# ...
		svalue(barra_status) <- "Incorporando datos personales ..."
		DP <- read.table(archivo_csv,header=TRUE,sep=",")
		#incorpora los datos
		indices <- sapply(nomina_intro$cedula,function(i)
			ifelse(length(which(DP$cedula==i))==1,which(DP$cedula==i)[1],NA) )
		sexo <- DP$Sexo[indices]
		#para la fecha de nacimiento, verificar si es "1965-01-01",
		#ya que en esa fecha hubo cambio de zona horaria y generará error
		#al escribir a Excel.
		#(Ver http://en.wikipedia.org/wiki/Time_in_Venezuela)
		fecha_nacimiento <- DP$Fecha_nac[indices]
		cambio_fecha <- which(fecha_nacimiento=="1965-01-01")
		if (length(cambio_fecha)>0)
			fecha_nacimiento[cambio_fecha] <- "1965-01-02"		
		correo <- limpia_correos(DP$Correo[indices])
		celular <- 	DP$Celular[indices]
		telefono <- DP$Telefono[indices]
		nomina_intro <- cbind(nomina_intro,sexo=sexo,fecha_nacimiento=
			fecha_nacimiento,correo=correo,celular=celular,telefono=telefono)
		nomina <- within(nomina_intro, {
			fecha_nacimiento <- as.Date(fecha_nacimiento,"%Y-%m-%d")
			celular <- as.character(celular)
			telefono <- as.character(telefono) } )
		#Guarda el archivo de nomina en csv
		write.table(nomina_intro,file=ruta_nomina_intro_csv,
			sep="\t",row.names=FALSE)
		#Guarda el archivo de nomina como imágen RData
		save("nomina_intro",file=ruta_nomina_intro_RData)
		svalue(barra_status) <- ""
		if (all(is.na(indices))) return(3)
		if (any(is.na(indices)))	return(4)
		return(0)
	} #generar_nomina_introductorio_CL(archivo_pdf,archivo_csv)

#-----------------------------------------------------------------------
generar_nomina_nuevos <- 
	function(archivo_pdf,CLOA) {
		#códigos de error para esta función
		#  01 - archivo pdf defectuoso
		#  00 - todo bien
		#archivo_pdf es el listado que se descarga desde UNASEC.
		mensaje <- paste0("Procesando archivo: [",paste(rep("░",20),
			collapse=""),"]")
		svalue(barra_status) <- mensaje; Sys.sleep(0.001)
		#Convierte el listado en pdf a un archivo de texto
		error <- system(paste("pdftotext -layout",archivo_pdf,
			"matricula"))
		if (error==1) {
			svalue(barra_status) <- ""
			return(1) }		#archivo pdf defectuoso
		#cadenas de busqueda de líneas especiales y caracter de separación
		pb <- paste("UNIVERSIDAD NACIONAL ABIERTA","SECRETARIA","Res\\. N",
			"LISTADO DE NUEVOS INSCRITOS","LAPSO [0-9]+ AL:","JEFE URyCE",
			"CEDULA( )+APELLIDOS","Page","___","Centro Local: ",sep="|")
		#para la definición de locs y cloa, ver "editar_asesores.R"
		car_sep <- "\t"
		#Leer el archivo matricula para el primer pase
		archivo <- file("matricula", "r", blocking = FALSE)
		lineas <- readLines(archivo)
		close(archivo)
		unlink("matricula")
		#Unica pasada del archivo
		cedulas <- numeric(0)
		total_tarea <- length(lineas)
		contador <- 0
		progreso_ant <- 0
		for (i in lineas) {
			j <- gsub("( )\\1+",car_sep,i)
			#filtrar todas las líneas que no sean de data estudiantíl
			if (length(grep(pb,j))==0) {
				if (!j %in% c("\n","\x0c","")) {
					k <- strsplit(j,car_sep)[[1]]
					if (k[1]=="") k <- k[-1]
					cedulas <- c(cedulas,as.numeric(k[3]))
				}
			}
			#actualiza contador y barra de estatus:
			contador <- contador + 1
			progreso <- round(contador/total_tarea*20)
			if (progreso>progreso_ant) {
				mensaje <- paste("Procesando archivo pdf: [",
					paste(rep("█",progreso),collapse=""),
					paste(rep("░",20-progreso),collapse=""),"]",sep="")
				svalue(barra_status) <<- mensaje; Sys.sleep(0.001)
				progreso_ant <- progreso
			}
		}
		#Ahora lee la nomina regular para generar un csv con los estudiantes
		#encontrados arriba (según la cédula y el CLOA seleccionado)
		svalue(barra_status) <- "Generando el archivo csv ..."
		load(ruta_nomina_RData)
		nomina2 <- subset(nomina,(cedula %in% cedulas) &
			(cloa==CLOA),select=c(cedula,nombre,carrera:telefono) )
		nomina2 <- cbind(N=1:nrow(nomina2),nomina2)
		#Guarda el archivo de nomina en csv
		#CLOAf es el Centro Local - UA (Ej. "02 - 01") sin espacions ni
		#guiones
		CLOAf <- gsub(" |\\-","",CLOA)
		write.table(nomina2,file=paste0("nuevos_ingresos_",CLOAf,".csv"),
			sep="\t",row.names=FALSE)
		svalue(barra_status) <- ""
		return(0)
	} #generar_nomina_nuevos(archivo_pdf,CLOA)

#-----------------------------------------------------------------------
generar_nomina_reingresos <- 
	function(archivo_pdf,CLOA,narchivo) {
	#códigos de error para esta función
	#  01 - archivo pdf defectuoso
	#  00 - todo bien
	#pdf_regular es el listado que se descarga desde UNASEC.
	mensaje <- paste0("Procesando archivo: [",paste(rep("░",20),
		collapse=""),"]")
	svalue(barra_status) <- mensaje; Sys.sleep(0.001)
	#Convierte el listado en pdf a un archivo de texto
	error <- system(paste("pdftotext -layout",archivo_pdf,
		"matricula"))
	if (error==1) {
		svalue(barra_status) <- ""
		return(1) }		#archivo pdf defectuoso
	#cadenas de busqueda de líneas especiales y caracter de separación
	pb <- paste("UNIVERSIDAD NACIONAL ABIERTA","SECRETARIA","LISTADO DE",
		"INSCRITOS Y VALIDADOS LAPSO","REINGRESO DE","Page",
		"JEFE URyCE","Centro Local: ","Res\\. N","CEDULA( )+APELLIDOS",
		"___",sep="|")
	#para la definición de locs y cloa, ver "editar_asesores.R"
	car_sep <- "\t"
	#Leer el archivo matricula para el primer pase
	archivo <- file("matricula", "r", blocking = FALSE)
	lineas <- readLines(archivo)
	close(archivo)
	unlink("matricula")
	#Unica pasada del archivo
	cedulas <- numeric(0)
	total_tarea <- length(lineas)
	contador <- 0
	progreso_ant <- 0
	for (i in lineas) {
		j <- gsub("( )\\1+",car_sep,i)
		#filtrar todas las líneas que no sean de data estudiantíl
		if (length(grep(pb,j))==0) {
			if (!j %in% c("\n","\x0c","")) {
				k <- strsplit(j,car_sep)[[1]]
				cedulas <- c(cedulas,as.numeric(k[3]))
			}
		}
		#actualiza contador y barra de estatus:
		contador <- contador + 1
		progreso <- round(contador/total_tarea*20)
		if (progreso>progreso_ant) {
			mensaje <- paste("Procesando archivo pdf: [",
				paste(rep("█",progreso),collapse=""),
				paste(rep("░",20-progreso),collapse=""),"]",sep="")
			svalue(barra_status) <<- mensaje; Sys.sleep(0.001)
			progreso_ant <- progreso
		}
	}
	#Ahora lee la nomina regular para generar un csv con los estudiantes
	#encontrados arriba (según la cédula)
	svalue(barra_status) <- "Generando el archivo csv ..."
	load(ruta_nomina_RData)
	nomina2 <- subset(nomina,(cedula %in% cedulas) &
		(cloa==CLOA),select=c(cedula,nombre,carrera:telefono) )
	nomina2 <- cbind(N=1:nrow(nomina2),nomina2)
	#Guarda el archivo de nomina en csv
	#CLOAf es el código Centro Local - UA (Ej: "02 - 01") sin
	#guiones ni espacios.
	CLOAf <- gsub(" |\\-","",CLOA)
	write.table(nomina2,file=paste0(narchivo,"_",CLOAf,".csv"),
		sep="\t",row.names=FALSE)
	svalue(barra_status) <- ""
	return(0)
} #generar_nomina_reingresos(archivo_pdf,CLOA,narchivo)

#-----------------------------------------------------------------------
generar_cronograma <- function(archivo_pdf) {
	#Esta función genera el cronograma a partir del archivo pdf
	#suministrado.
	#códigos de error para esta función
	#  01 - archivo pdf defectuoso
	#  00 - todo bien
	mensaje <- paste("Procesando archivo: [",paste(rep("░",20),
		collapse=""),"]",sep="")
	svalue(barra_status) <- mensaje; Sys.sleep(0.001)
	#Convierte el listado en pdf a un archivo de texto
	error <- system(paste("pdftotext -layout",archivo_pdf,
		"cronograma"))
	if (error==1) {
		svalue(barra_status) <- ""
	return(1) }		#archivo pdf defectuoso
	#cadenas de busqueda de líneas especiales y caracter de separación
	pb <- paste("UNIVERSIDAD NACIONAL ABIERTA","1ER MOMENTO",
		" LAPSO ACADEMICO","CALENDARIO DE PRUEBAS POR ASIGNATURA",
		"DIRECCIÓN DEL CENTRO DE PROGRAMACIÓN",
		"APROBADO.+SEGÚN","COORDINACIÓN DE ESTAD",
		"^C","^( )+D",sep="|")
	#para la definición de locs y cloa, ver "editar_asesores.R"
	car_sep <- "\t"
	#Leer el archivo cronogama para el primer pase
	archivo <- file("cronograma", "r", blocking = FALSE)
	lineas <- readLines(archivo)
	close(archivo)
	unlink("cronograma")
	#Unica pasada del archivo
	cronograma <- data.frame(codigo=numeric(0),nombre=character(0),
		S1=numeric(0),F1=character(0),Tu1=character(0),Ti1=character(0),
		S2=numeric(0),F2=character(0),Tu2=character(0),Ti2=character(0),
		S3=numeric(0),F3=character(0),Tu3=character(0),Ti3=character(0),
		S4=numeric(0),F4=character(0),Tu4=character(0),Ti4=character(0))
	total_tarea <- length(lineas)
	contador <- 0
	progreso_ant <- 0
	for (i in lineas) {
		#convierte la línea a mayúsculas
		i <- toupper(i)
		#filtrar cada línea que no sea renglón de datos
		if (length(grep(pb,i))==0) {
			if (!i %in% c("\n","\x0c","")) {
				#eliminar el espacio al principio, si lo hay
				j <- gsub("^ ","",i)
				if (substr(j,5,5)!=" ") substr(j,4,4) <- car_sep
				#sustituir multiples espacios por el caracter de separación
				#esto se hace primero para trabajar con los otros espacios
				#despues
				j <- gsub("( ){2,}",car_sep,j)
				#separar los campos separados por un solo espacio
				j <- gsub("([A-Z])( )([0-9]{2})",paste0("\\1",car_sep,"\\3"),j)
				j <- gsub("([0-9])( )([0-9]{2})",paste0("\\1",car_sep,"\\3"),j)
				j <- gsub("(PM|AM)( )(I|P)",paste0("\\1",car_sep,"\\3"),j)
				j <- gsub("([0-9])( )(PM|AM)",paste0("\\1",car_sep,"\\3"),j)
				print(j)
				#ahora separa la línea según el caracter de separación
				k <- strsplit(j,car_sep)[[1]]
				S1 <- k[3]; F1 <- k[4]; Tu1 <- k[5]
				if (k[6]=="P") Ti1 <- "1P" else Ti1 <- "1Int"
				S2 <- k[7]; F2 <- k[8]; Tu2 <- k[9]
				if (k[10]=="P") Ti2 <- "2P" else Ti2 <- "2Int"
				if (length(k)>10) {
					S3 <- k[11]; F3 <- k[12]; Tu3 <- k[13]
					if (k[14]=="P") Ti3 <- "3P" else Ti3 <- "Int"
					if (length(k)>14) {
						S4 <- k[15]; F4 <- k[16]; Tu4 <- k[17]
						Ti4 <- "Int"
					} else {
						S4 <- NA; F4 <- NA; Tu4 <- NA; Ti4 <- NA
					}
				} else {
					S3 <- NA; F3 <- NA; Tu3 <- NA; Ti3 <- NA
					S4 <- NA; F4 <- NA; Tu4 <- NA; Ti4 <- NA
					}				
				cronograma <- rbind(cronograma,data.frame(codigo=k[1],
					nombre=k[2],S1=S1,F1=F1,Tu1=Tu1,Ti1=Ti1,
					S2=S2,F2=F2,Tu2=Tu2,Ti2=Ti2,
					S3=S3,F3=F3,Tu3=Tu3,Ti3=Ti3,
					S4=S4,F4=F4,Tu4=Tu4,Ti4=Ti4))
			}
		}
		#actualiza contador y barra de estatus:
		contador <- contador + 1
		progreso <- round(contador/total_tarea*20)
		if (progreso>progreso_ant) {
			mensaje <- paste("Procesando archivo pdf: [",
				paste(rep("█",progreso),collapse=""),
				paste(rep("░",20-progreso),collapse=""),"]",sep="")
			svalue(barra_status) <<- mensaje; Sys.sleep(0.001)
			progreso_ant <- progreso
		}
	} #for (i in lineas)
	write.table(cronograma,file="cronograma.csv",sep="\t",
		row.names=FALSE)
	svalue(barra_status) <- ""
	return(0)
}

