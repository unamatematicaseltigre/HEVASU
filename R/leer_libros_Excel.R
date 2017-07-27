#Funciones para leer los libros de Excel
#Fecha : 05/05/2016
#Autor : José L. Romero
#requiere los siguientes paquetes: XLConnect
#-----------------------------------------------------------------------

recorrer_libro_evaluacion <- function(asesor,
	fecha_inicio, fecha_fin) {
	#esta función recorre el libro Excel (de los asesores) para la
	#evaluación y genera un .csv cuyos renglones se corresponden a cada
	#estudiante por materia con los siguientes datos:
	#		id					- cedula
	#		carrera			- código de carrera
	#		clua				- código de centro local y unidad de apoyo
	#		asignatura	- código de asignatura
	#   ol					- cadena con data sobre los objetivos logrados
	#								  por cada estudiante. (1=logrado,0=no logrado,
	#									.=no respondió).
	#		pruebas			- cantidad de pruebas (de desarrollo) corregidas en el
	#								  rango de fecha
	#		trabajos		- cantidad de trabajos corregidos en el rango de fecha
	#fecha_inicio y fecha_fin son dadas en clase Date. Ej: 2013/10/31
	#Los renglones del csv se corresponden a eventos de evaluación
	#corregidos en ese rango de fechas.
	lapso <- svalue(selector_lapso)
	archivo_Excel <- paste0("OL_",asesor$archivo,"_",lapso,".xls")
	archivo_csv <- paste0("OL_",asesor$archivo,"_",lapso,".csv")
	#abre el archivo Excel para su lectura
	wb <- loadWorkbook(archivo_Excel, create = TRUE )
	#obtener la lista de asignaturas (cada asignatura es una hoja)
	hojas <- getSheets(wb)
	#crea el data frame
	recorrido <- data.frame()
	#recorre las hojas del libro
	for (hoja in hojas) {
		if (getLastRow(wb,hoja)>11) {
			asignatura <- as.numeric(hoja)
			svalue(barra_status) <- paste("Procesando el libro",
				archivo_Excel," Hoja:",hoja,"# de filas:",getLastRow(wb,hoja),
				"...")
			Sys.sleep(0.001)
			#obtener el número de columna a partir del cual comienzan
			#las fechas de evaluaciones
			encabezado <- as.character(readWorksheet (wb, hoja ,startRow=10,
				startCol=1, endRow=10, endCol=getLastColumn(wb,hoja), 
				header= FALSE, colTypes=rep("character",
				getLastColumn(wb,hoja)),autofitCol=FALSE) )
			col_1 <- which(encabezado=="Total")+1
			#La columna que nos interesa se encuentra 1 a la derecha de
			#donde dice "Total".
			#obtener la última columna de fecha de evaluaciones
			encabezado <- as.character( readWorksheet (wb, hoja ,startRow=11,
				startCol=1, endRow=11, endCol=getLastColumn(wb,hoja), 
				header= FALSE, colTypes=rep("character",
				getLastColumn(wb,hoja))) )
			col_n <- length(encabezado)
			#obtener los tipos de evaluaciones en cada columna de fechas
			evaluaciones <- encabezado[col_1:col_n]
			tipo_evaluacion <- ifelse(evaluaciones %in% c("1P","2P","3P",
				"Int","1Int","2Int","3Int","4Int"),"prueba","trabajo")
			#lee la data del curso (datos personales)
			data_curso <- readWorksheet(wb , hoja, startRow=12, startCol=2,
				endRow = getLastRow(wb,hoja), endCol=5, autofitRow=FALSE,
				autofitCol=FALSE,header = FALSE)
			data_curso <- subset(data_curso,select=c("Col1","Col3","Col4"))
			#lee la data de objetivos logrados y conviertelo a una cadena
			data_OL <- readWorksheet(wb , hoja, startRow=12,
				startCol=7, endRow = nrow(data_curso)+11, endCol=col_1-2, 
				autofitCol=FALSE,autofitRow=FALSE, header = FALSE)
			obj_logrados <- sapply(1:nrow(data_OL),function(i) {
				paste(sapply(data_OL[i,],function(j)
				ifelse(is.na(j),".",as.character(j))),collapse="") } )
			#lee las fechas de corrección de evaluaciones
			data_fechas <- readWorksheet(wb, hoja, startRow=12,
				startCol=col_1, endRow = nrow(data_curso)+11, endCol=col_n, 
				header = FALSE, autofitCol=FALSE, autofitRow=FALSE,
				colTypes=rep("Date",col_n-col_1+1)) 
			#poner los totales de pruebas y trabajos en 0
			pruebas <- rep(0,nrow(data_fechas))
			trabajos <- rep(0,nrow(data_fechas))
			#recorrer cada columna de evaluación
			for (i in 1:ncol(data_fechas)) {
				#actualmente, las fechas inválidas se ignoran
				fechas <- as.Date(data_fechas[,i])
				fechas <- ifelse(fechas>=fecha_inicio & fechas<=fecha_fin,1,0)
				fechas[is.na(fechas)] <- 0
				if (tipo_evaluacion[i]=="prueba") pruebas <- pruebas+fechas
				else trabajos <- trabajos+fechas
			}
			#agregar cantidad de pruebas y trabajos corregidos
			data_curso <- cbind(data_curso,rep(asignatura,nrow(data_curso)),
				obj_logrados,pruebas,trabajos)
			#renombrar las columnas
			names(data_curso) <- c("id","clua","carrera","asignatura",
				"ol","pruebas","trabajos")
			#agregarlo a la tabla general
			recorrido <- rbind(recorrido,data_curso)
		}#if hay estudiantes en esa materia
	}#for cada asignaura en el libro
	#escribe los datos al archivo csv
	write.table(recorrido,file=archivo_csv,sep="\t",row.names=FALSE)
	svalue(barra_status) <- ""; Sys.sleep(0.001)
} #recorrer_libro_evaluacion

#-----------------------------------------------------------------------

recorrer_libro_evaluacion_asesor_plan <- function(archivo_Excel,
	archivo_csv,ano) {
	#Esta función se utiliza para la Planificación Operativa.
	#Recorre el libro Excel (de un asesor) para la evaluación
	#y genera un .csv cuyos renglones se corresponden a cada
	#estudiante por materia con los siguientes datos:
	#		clua				- código de centro local y unidad de apoyo
	#		codigo_plan	- código de proyecto/acción/área del plan operativo
	#   subaccion   - código de subacción (siempre "03")
	#   producto    - código de producto (48:TP o 185:prueba corregida)
	#   mes         - més en el cual se entregó el trabajo
	#									(es la única fecha de entrega)
	#
	#archivo_csv es el archivo a donde se va a escribir todo.
	#ano es el año (no el culo) para el periodo de planificación.
	#
	#abre el archivo Excel para su lectura
	wb <- loadWorkbook(archivo_Excel, create = TRUE )
	#obtener la lista de asignaturas (cada asignatura es una hoja)
	hojas <- getSheets(wb)
	#crea el data frame
	recorrido <- data.frame()
	#recorre las hojas del libro
	for (hoja in hojas) {
		svalue(barra_status) <- paste("Procesando el libro",
			archivo_Excel," Hoja:",hoja,"...")
		Sys.sleep(0.001)
		asignatura <- readWorksheet (wb , hoja , startRow=5 , startCol=5 ,
		endRow = 5, endCol=5, header = FALSE)[1,1]
		#obtener el número de columna a partir del cual comienzan las fechas
		#de evaluaciones
		encabezado <- as.character(readWorksheet (wb, hoja ,startRow=10,
			startCol=1, endRow=10, endCol=getLastColumn(wb,hoja),
			header= FALSE, colTypes=rep("character",getLastColumn(wb,hoja)),
			autofitCol=FALSE) )
		#obtener las fechas en que se administran las evaluaciones
		primero <- which(encabezado=="Total")+1
		ultimo <- length(encabezado)
		fechas_entrega <- sapply(encabezado[primero:ultimo], function(i)
			tryCatch(as.Date(i),error=function(e) NA))
		fechas_entrega <- as.Date(as.vector(fechas_entrega),
			origin="1970-01-01")
		if (any(is.na(fechas_entrega))) {
			recorrido <- rbind(recorrido,cbind(clua="00 - 00",
				codigo_plan=paste("ERROR: hay fechas de evaluación vacias",
				"o inválidas.<br />Archivo:",archivo_Excel,"Hoja:",hoja),
				subaccion="00",producto="00",mes="01") )
		}
		#obtener la primera columna de evaluaciones
		col_1 <- which(encabezado=="Total")+1
		#La columna que nos interesa se encuentra 1 a la derecha de donde
		#dice "Total".
		#obtener la última columna de fecha de evaluaciones
		encabezado <- as.character(readWorksheet (wb, hoja ,startRow=11, 
			startCol=1, endRow=11, endCol=getLastColumn(wb,hoja), 
			header= FALSE) )
		col_n <- length(encabezado)
		#obtener los tipos de evaluaciones en cada columna de fechas
		evaluaciones <- encabezado[col_1:col_n]
		tipo_evaluacion <- ifelse(evaluaciones %in% c("1P","2P","3P",
			"Int","1Int","2Int","3Int","4Int"),185,48)
		#lee la data del curso y las fechas de las ev. presentadas
		data_curso <- readWorksheet(wb , hoja, startRow=12, startCol=4,
			endRow = getLastRow(wb,hoja), endCol=5, header = FALSE)
		if (nrow(data_curso)>0) { #hay estudiantes en esta materia
			data_fechas <- readWorksheet(wb, hoja, startRow=12, 
				startCol=col_1, endRow = getLastRow(wb,hoja), endCol=col_n,
				header = FALSE, autofitCol=FALSE,autofitRow=FALSE,
				colTypes=rep("character",col_n-col_1+1))
			clua <- data_curso[,1]
			codigo_plan <- sapply(data_curso[,2],
				function(i) id_plan(i,asignatura,archivo_Excel) )
			subaccion <- rep("03",nrow(data_fechas))	
			#recorrer cada columna (evento de prueba) de evaluación
			for (j in 1:ncol(data_fechas)) 
				if (!is.na(fechas_entrega[j])) {
					if (format(fechas_entrega[j],"%Y")==ano) {
						producto <- rep(tipo_evaluacion[j],nrow(data_fechas))
						mes <- rep(format(fechas_entrega[j],"%m"),nrow(data_fechas))
						data_curso <- cbind(clua=clua,codigo_plan=codigo_plan,
							subaccion=subaccion,producto=producto,mes=mes)
						data_curso <- subset(data_curso,
							subset=!is.na(data_fechas[,j]) )
						recorrido <- rbind(recorrido,data_curso)
					} #if y for cada fecha de evaluación de ese año
				}#if de afuera
		}# if hay filas para esta materia
	} #for cada hoja (asignatura)
	#escribe los datos al archivo csv
	#verifica si se está creando el archivo csv 
	if (file.access(archivo_csv)!=0) {
		write.table(recorrido, file=archivo_csv, sep="\t", row.names=FALSE)
	} else {
		write.table(recorrido, file=archivo_csv, sep="\t", row.names=FALSE,
		col.names=FALSE, append=TRUE)
	}
} #recorrer_libro_evaluacion_asesor_plan

#-----------------------------------------------------------------------

recorrer_libro_evaluacion_para_csv <- function(archivo_Excel) {
	#esta función recorre el libro Excel del asesor para la
	#evaluación y genera un .csv cuyos renglones se corresponden a cada
	#estudiante por materia con los siguientes datos:
	#		id					- cedula
	#   nombre      - Apellido, Nombre del estudiante
	#		carrera			- código de carrera
	#		clua				- código de centro local y unidad de apoyo
	#		asignatura	- código de asignatura
	#   ol					- cadena con data sobre los objetivos logrados
	#								  por cada estudiante. (1=logrado,0=no logrado,
	#									.=no respondió).
	#		corregido		- cadena con data sobre corrección de evaluaciones:
	#                 1=corregido,0=no corregido/no ha presentado aún.
	#                 cada caractér se corresponde a la respectiva
	#									evaluación (data en otro archivo .csv)
	#		observ			- cualquier observación cualitativa del asesor a ese
	#									estudiante
	#También crea otro csv con la información pertinente sobre las
	#materias:
	#		asignatura	-	código de la asignatura
	#		id_eval			- identificadores de las evaluaciones ("1P","Int",etc.)
	#		tipo_eval		- tipos de evaluaciones ("prueba","trabajo")
	#		f_eval			- fechas de las evaluaciones
	base_archivos <- file.path(dirname(archivo_Excel),
		gsub("^OL_","",basename(archivo_Excel)))
	base_archivos <- gsub("\\.xls$",".csv",base_archivos)
	archivo_ASIG <- file.path(dirname(base_archivos),
		paste0("ASIG_",basename(base_archivos)))
	archivo_EV <- file.path(dirname(base_archivos),
		paste0("EV_",basename(base_archivos)))
	#abre el archivo Excel para su lectura
	wb <- loadWorkbook(archivo_Excel, create = TRUE )
	#obtener la lista de asignaturas (cada asignatura es una hoja)
	hojas <- getSheets(wb)
	#crea los data frame
	recorrido <- data.frame()
	asignaturas <- data.frame()
	#recorre las hojas del libro
	for (hoja in hojas) {
		if (getLastRow(wb,hoja)>11) {
			asignatura <- as.numeric(hoja)
			svalue(barra_status) <- paste("Procesando el libro",
				basename(archivo_Excel)," Hoja:",hoja,"# de filas:",getLastRow(wb,hoja),
				"...")
			Sys.sleep(0.001)
			#obtener el número de columna a partir del cual comienzan
			#las fechas de evaluaciones
			encabezado1 <- as.character(readWorksheet (wb, hoja ,startRow=10,
				startCol=1, endRow=10, endCol=getLastColumn(wb,hoja), 
				header= FALSE, colTypes=rep("character",
				getLastColumn(wb,hoja)),autofitCol=FALSE) )
			col_1 <- which(encabezado1=="Total")+1
			#La columna que nos interesa se encuentra 1 a la derecha de
			#donde dice "Total".
			#obtener la última columna de fecha de evaluaciones
			encabezado2 <- as.character( readWorksheet (wb, hoja ,startRow=11,
				startCol=1, endRow=11, endCol=getLastColumn(wb,hoja), 
				header= FALSE, colTypes=rep("character",
				getLastColumn(wb,hoja))) )
			col_n <- length(encabezado2)-1
			#obtener los tipos de evaluaciones en cada columna de fechas
			evaluaciones <- encabezado2[col_1:col_n]
			f_evaluaciones <- encabezado1[col_1:col_n]
			tipo_evaluacion <- ifelse(evaluaciones %in% c("1P","2P","3P",
				"Int","1Int","2Int","3Int","4Int"),"prueba","trabajo")
			#agregar información de asignatura
			asignaturas <- rbind(asignaturas,
				data.frame(asignatura=asignatura,
				id_eval=paste(evaluaciones,collapse=","),
				tipo_eval=paste(tipo_evaluacion,collapse=","),
				f_eval=paste(f_evaluaciones,collapse=",")))
			#lee la data del curso (datos personales)
			data_curso <- readWorksheet(wb , hoja, startRow=12, startCol=2,
				endRow = getLastRow(wb,hoja), endCol=5, autofitRow=FALSE,
				autofitCol=FALSE,header = FALSE)
			#lee la data de objetivos logrados y conviertelo a una cadena
			data_OL <- readWorksheet(wb , hoja, startRow=12,
				startCol=7, endRow = nrow(data_curso)+11, endCol=col_1-2, 
				autofitCol=FALSE,autofitRow=FALSE, header = FALSE)
			obj_logrados <- sapply(1:nrow(data_OL),function(i) {
				paste(sapply(data_OL[i,],function(j)
				ifelse(is.na(j),".",as.character(j))),collapse="") } )
			#lee las fechas de corrección de evaluaciones
			data_fechas <- readWorksheet(wb, hoja, startRow=12,
				startCol=col_1, endRow = nrow(data_curso)+11, endCol=col_n, 
				header = FALSE, autofitCol=FALSE, autofitRow=FALSE,
				colTypes=rep("Date",col_n-col_1+1))
			#recorrer cada fila de las fechas de corrección de evaluaciones
			corregido <- character(0)
			for (i in 1:nrow(data_fechas)) {
				corregido <- c(corregido,
					paste(1-as.numeric(is.na(data_fechas[i,])),collapse=""))
			}
			#lee las observaciones (última columna)
			observ <- readWorksheet(wb , hoja, startRow=12, startCol=col_n+1,
				endRow = nrow(data_curso)+11, endCol=col_n+1, autofitRow=FALSE,
				autofitCol=FALSE,header = FALSE)
			#agregar todo
			data_curso <- cbind(data_curso,rep(asignatura,nrow(data_curso)),
				obj_logrados,corregido,observ)
			#renombrar las columnas
			names(data_curso) <- c("id","nombre","clua","carrera",
				"asignatura","ol","corregido","observ")
			#agregarlo a la tabla general
			recorrido <- rbind(recorrido,data_curso)
		}#if hay estudiantes en esa materia
	}#for cada asignaura en el libro
	#elimina los estudiantes que son NA (31/3/2017)
	data_curso <- subset(data_curso,subset=!is.na(id))
	#escribe los datos a los archivos csv
	write.table(recorrido,file=archivo_EV,sep="\t",row.names=FALSE,
		quote=FALSE)
	write.table(asignaturas,file=archivo_ASIG,sep="\t",row.names=FALSE,
		quote=FALSE)
	svalue(barra_status) <- ""; Sys.sleep(0.001)
} #recorrer_libro_evaluacion_para_csv

#-----------------------------------------------------------------------
recorrer_libros_evaluacion_para_csv <- function() {
	#CODIGOS DE ERROR 
	#0	Todo bien
	#3	No se han seleccionado asesores.
	if (length(seleccion_asesores)<1) return(3)
	#Ciclo principal - para cada asesor
	for (asesor in seleccion_asesores) {
		recorrer_libro_evaluacion_para_csv(asesor) }
	return(0)
} #recorrer_libros_evaluacion_para_csv
#-----------------------------------------------------------------------

recorrer_libro_evaluacion_orientador_plan <- function(archivo_Excel,
	archivo_csv,ano) {
	#Esta función se utiliza para la Planificación Operativa.
	#Recorre el libro Excel (de un orientador) para la evaluación
	#y genera un .csv cuyos renglones se corresponden a cada
	#estudiante por materia con los siguientes datos:
	#		clua				- código de centro local y unidad de apoyo
	#		codigo_plan - código de plan (siempre "060202")
	#   subaccion   - código de subacción (siempre "03")
	#   producto    - código de producto (siempre 48:TP)
	#   mes         - més en el cual se entregó el trabajo
	#									(es la única fecha de entrega)
	#archivo_csv es el archivo a donde se va a escribir todo.
	#ano es el año (no el culo) para el periodo de planificación.
	#recorre las hojas del libro
	hoja <- "Curso Introductorio"
	svalue(barra_status) <- paste("Procesando el libro",
		archivo_Excel," Hoja:",hoja,"...")
	Sys.sleep(0.001)
	#abre el archivo Excel para su lectura
	wb <- loadWorkbook(archivo_Excel, create = TRUE )
	#obtener las fechas en que se administran las evaluaciones
	temp <- readWorksheet (wb, hoja ,startRow=9, startCol=13, endRow=9,
		endCol=13,header= FALSE,colTypes="Date")[1,1]
	print(temp)
	fecha_adm <- tryCatch(as.Date(temp),error=function(e) NA)
	fecha_adm <- as.Date(fecha_adm,origin="1970-01-01")
	print(fecha_adm)
	print(class(fecha_adm))
	if (is.na(fecha_adm)) {
		fecha_adm <- paste("ERROR: fecha de evaluación vacia",
		"o inválida. <br />Archivo:",archivo_Excel)
	}
	#verificar si la fecha de entrega es en el año en cuestion
	if (is(fecha_adm,"Date")) {
		if (format(fecha_adm,"%Y")==ano) {
			#lee las fechas de corrección de los trabajos
			data_fechas <- readWorksheet(wb, hoja, startRow=11, startCol=13,
				endRow = getLastRow(wb,hoja), endCol=13, header = FALSE,
				autofitCol=FALSE,autofitRow=FALSE,colTypes="character")
			entregados <- ifelse(is.na(data_fechas[,1]),FALSE,TRUE)
			#lee el centro local - unidad de apoyo
			clua <- readWorksheet(wb , hoja, startRow=7, startCol=3,
				endRow = 7, endCol=3, header = FALSE)[1,1]
			#asigna el clua, código del plan, subacción, producto y més
			clua <- rep(clua,nrow(data_fechas))		
			codigo_plan <- rep("060202",nrow(data_fechas))
			subaccion <- rep("03",nrow(data_fechas))
			producto <- rep(48,nrow(data_fechas))
			mes <- rep(format(fecha_adm,"%m"),nrow(data_fechas))
			#agregar toda esta data en un data-frame
			data_curso <- cbind(clua=clua,codigo_plan=codigo_plan,
				subaccion=subaccion,producto=producto,mes=mes)
			data_curso <- subset(data_curso,subset=entregados)
			#escribe los datos al archivo csv
			if (file.access(archivo_csv)!=0) {
				write.table(data_curso, file=archivo_csv, sep="\t", 
					row.names=FALSE)
			} else {
				write.table(data_curso, file=archivo_csv, sep="\t",
					row.names=FALSE, col.names=FALSE, append=TRUE)
			} #if no existía el archivo csv
		} #if no hubo entrega de trabajos este año
	} else {
		data_curso <- cbind(clua="00 - 00",codigo_plan=fecha_adm,
			subaccion="00",producto="00",mes="01")
		if (file.access(archivo_csv)!=0) {
			write.table(data_curso, file=archivo_csv, sep="\t", 
				row.names=FALSE)
		} else {
			write.table(data_curso, file=archivo_csv, sep="\t",
				row.names=FALSE, col.names=FALSE, append=TRUE)
		} #if no existía el archivo csv	
	} #si la fecha es invalida escribió una sóla línea con el error.
} #recorrer_libro_evaluacion_orientador_plan

#-----------------------------------------------------------------------

leer_curso_introductorio <- function(archivo_Excel) {
	#lee un archivo Excel del curso introductorio (archivo_Excel) y
	#genera el archivo csv del mismo nombre pero con esa extensión.
	#el nombre del archivo csv a crear es:
	archivo_csv <- file.path(dirname(archivo_Excel),
		gsub("\\.xls$|\\.XLS$",".csv",basename(archivo_Excel)) )
	#carga el libro de Excel
	wb <- loadWorkbook(archivo_Excel, create = TRUE )
	hoja <- getActiveSheetName(wb)	
	svalue(barra_status) <- paste("Leyendo",basename(archivo_Excel),
		"# de filas:",getLastRow(wb,hoja),"...")
	Sys.sleep(0.001)	
	data_curso <- readWorksheet (wb , hoja , startRow=11 , startCol=2 ,
		endRow = getLastRow(wb,hoja), endCol=13, header = FALSE,
		autofitCol=FALSE)
	data_curso <- subset(data_curso,select=c("Col1","Col2","Col6",
		"Col11","Col12"))
	names(data_curso) <- c("cedula","nombre","carrera","aprobado","fecha")
	data_curso$aprobado <- (data_curso$aprobado==4)
	data_curso$fecha <- as.Date(data_curso$fecha)
	clua <- readWorksheet (wb , hoja , startRow=7 , startCol=3 ,
		endRow = 7, endCol=3, header = FALSE)[1,1]
	lapsoguion <- readWorksheet (wb , hoja , startRow=7 , startCol=7 ,
		endRow = 7, endCol=7, header = FALSE)[1,1]
	titulo_orientador <- readWorksheet (wb, hoja, startRow=6,
		startCol=1, endRow = 6 ,endCol=1, header = FALSE)[1,1]
	nombre_orientador <- readWorksheet (wb, hoja, startRow=6,
		startCol=3, endRow = 6, endCol=3, header = FALSE)[1,1]
	archivo <- file(archivo_csv,open="w")
	writeLines(c(titulo_orientador,nombre_orientador,clua,lapsoguion),
		con=archivo)
	close(archivo)
	write.table(data_curso,archivo_csv,quote=FALSE,row.names=FALSE,
		sep="\t", append=TRUE)
} #leer_curso_introductorio(archivo_Excel)

#-----------------------------------------------------------------------

leer_csv_asesorias <- function(archivo) {
	#esta función existe para leer y "limpiar" un poco los datos leidos
  asesorias <- read.table(archivo,header=TRUE,sep="\t")
  #en la eventualidad en que solo hay talleres (tipo="T"),
  #esto se confundiría con un vector booleano.
	asesorias$tipo <- as.character(asesorias$tipo)
	asesorias$tipo[asesorias$tipo=="TRUE"] <- "T"
	asesorias$tipo <- as.factor(asesorias$tipo)
	return(asesorias)
} #leer_csv_asesorias

#-----------------------------------------------------------------------

recorrer_hoja_asesoria <- function(asesor,
	fecha_inicio,fecha_fin) {
	#Autor : José L. Romero
	#Fecha : 18/07/2014
	#esta función recorre el libro Excel (de los asesores) para la
	#asesoria y genera los .csv's respectivos que podrian ser:
	#	asesoria_(nombreasesor)_(lapso).csv
	#	talleres_(nombreasesor)_(lapso).csv
	#	orientacion_(nombreasesor)_(lapso).csv (solo para Orientadores)
	#NOTA: estos archivos solo se generan si hubo eventos del tipo 
	#respectivo (asesorias, talleres o servicios de orientación).
	#NOTA: validar códigos de asignatura (y tipos?) [26/7/2015]
	lapso <- svalue(selector_lapso)
	archivo_Excel <- paste0("asesoria_",asesor$archivo,"_",lapso,".xls")
	#abre el archivo Excel para su lectura
	wb <- loadWorkbook(archivo_Excel, create = TRUE )	
	hoja <- "Asesorías"
	filas <- getLastRow(wb,hoja)
 	svalue(barra_status) <- 
		paste0("Procesando el libro de asesorías de ",asesor$asesor,
		" Hoja: '",hoja,"' # de filas: ",filas)
	Sys.sleep(0.001)	
	archivo_csv <- paste0("asesoria_",asesor$archivo,"_",lapso,".csv")	
	#lee la hoja de asesoría
	#Los renglones del data.frame de asesorías se corresponderán a los
	#estudiantes y sus datos:
	#   id          - cédula del estudiante (para el futuro)
	#		carrera			- código de carrera
	#		clua				- código de centro local y unidad de apoyo
	#		asignatura	- código de asignatura
	#   fecha       - fecha de la asesoría
	#		tipo        - tipo de asesoría (ver códigos de una letra)
	#   lugar       - lugar de la asesoría (CL - UA)
	#   sexo				- sexo del estudiante
	#   edad        - edad del estudiante al momento de la asesoría	
	if (filas>5) {# si hubo asesorías
		data_asesorias <- readWorksheet(wb , hoja, startRow=6, startCol=2,
			endRow = filas, endCol=11, header = FALSE, colTypes=c("numeric",
			"character","character","numeric","numeric","Date",
			"character","character","character","numeric"))
		if (nrow(data_asesorias)>0) { #realmente hubo asesorías
			data_asesorias <- subset(data_asesorias,select=c(1,3:10))
			#renombrar las columnas
			names(data_asesorias) <- c("id","clua","carrera","asignatura",
				"fecha","tipo","lugar","sexo","edad")
			#seleccionar las asesorías en el rango de fechas
			#actualmente, si hay fechas inválidas en la hoja se ignora
			#ese renglón.	
			fechas <- as.Date(data_asesorias$fecha)
			f_fechas <- ifelse(fechas>=fecha_inicio & fechas<=fecha_fin,
				TRUE,FALSE)
			data_asesorias <- subset(data_asesorias,subset=f_fechas)
		} else {
			data_asesorias <- data.frame(id=numeric(0),clua=character(0),
				carrera=numeric(0),asignatura=numeric(0),fecha=character(0),
				tipo=character(0),lugar=character(0),sexo=character(0),
				edad=numeric(0))
		}#if realmente hubo asesorias
	} else {
		data_asesorias <- data.frame(id=numeric(0),clua=character(0),
			carrera=numeric(0),asignatura=numeric(0),fecha=character(0),
			tipo=character(0),lugar=character(0),sexo=character(0),
			edad=numeric(0))
	}#if filas>5
	#escribe los datos al archivo csv
	write.table(data_asesorias,file=archivo_csv,sep="\t",
		row.names=FALSE)
	#ahora recorre la hoja de talleres para generar un archivo csv
	#con las siguientes columnas:
	#		fecha				- fecha del taller	
	#		asignatura	-	código de la asignatura del taller
	#		descripcion	- detalles sobre el contenido del taller
	hoja <- "Talleres"
	filas <- getLastRow(wb,hoja)
	svalue(barra_status) <- 
		paste0("Procesando el libro de asesorías de ",asesor$asesor,
		" Hoja: '",hoja,"' # de filas: ",filas)
	Sys.sleep(0.001)
	archivo_csv <- paste0("talleres_",asesor$archivo,"_",
		lapso,".csv")	
	if (filas>5) {#sólo si hubo talleres
		data_talleres <- readWorksheet(wb , hoja, startRow=6,
			startCol=2, endRow = filas, endCol=4, header = FALSE,
			autofitCol=FALSE, autofitRow=FALSE,
			colTypes=c("Date","numeric","character"))
		if (nrow(data_talleres)>0) {#realmente hubo talleres
			#renombrar las columnas
			names(data_talleres) <- c("fecha","asignatura","descripcion")		
			#seleccionar solo los talleres en el rango de fechas
			#si hay alguna fecha inválida, se ignora ese renglón
			fechas <- as.Date(data_talleres$fecha)
			f_fechas <- ifelse(fechas>=fecha_inicio & fechas<=fecha_fin,
				TRUE,FALSE)
			data_talleres <- subset(data_talleres,subset=f_fechas)		
		} else {
			data_talleres <- data.frame(fecha=character(0),
				asignatura=numeric(0),descripcion=character(0))
		}#if realmente hubo talleres
	} else {
		data_talleres <- data.frame(fecha=character(0),
			asignatura=numeric(0),descripcion=character(0))
	}# if filas>5
	#escribe los datos al archivo csv
	write.table(data_talleres,file=archivo_csv,sep="\t",
		row.names=FALSE)		
	#si asesor es un orientador, procesa la hoja
	#"Servicios de Orientación". Se genera un archivo csv de nombre
	#orientacion_(docente)_lapso.csv con las siguientes columnas:
	#		id 				- cédula del estudiante
	#		carrera		-	código de la carrera
	#		fecha			- fecha del servicio de orientación
	#		tipo			-	tipo de servicio de orientación
	#		sexo			- sexo del estudiante
	#		edad			- edad al momento de la orientación
	if (asesor$tipo=="Orientador") {
		archivo_csv <- paste0("orientacion_",asesor$archivo,"_",
			lapso,".csv")
		hoja <- "Servicios de Orientación"
		filas <- getLastRow(wb,hoja)
		svalue(barra_status) <- 
			paste0("Procesando el libro de asesorías de ",asesor$asesor,
			" Hoja: '",hoja,"' # de filas: ",filas)
		Sys.sleep(0.001)
		if (filas>5) {	#hubo eventos de orientación
			data_orientacion <- readWorksheet(wb , hoja, startRow=6,
				startCol=2, endRow = filas, endCol=8, header = FALSE,
				autofitCol=FALSE, autofitRow=FALSE,
				colTypes=c("numeric","character","numeric","Date",
				"character","character","numeric"))
			if (nrow(data_orientacion)>0) {#realmente hubo serv. de ori.
				data_orientacion <- subset(data_orientacion,select=c(1,3:7))
				#renombrar las columnas
				names(data_orientacion) <- c("id","carrera","fecha","tipo",
					"sexo","edad")
				#seleccionar los eventos en el rango de fechas	
				#si hay alguna fecha inválida, se ignora ese renglón
				fechas <- as.Date(data_orientacion$fecha)
				f_fechas <- ifelse(fechas>=fecha_inicio & fechas<=fecha_fin,
					TRUE,FALSE)
				data_orientacion <- subset(data_orientacion,subset=f_fechas)
			} else {
				data_orientacion <- data.frame(id=numeric(0),
					carrera=numeric(0),fecha=character(0),tipo=character(0),
					sexo=character(0),edad=charcter(0))
			}#if realmente hubo serv. de orientación
		} else {
			data_orientacion <- data.frame(id=numeric(0),
				carrera=numeric(0),fecha=character(0),tipo=character(0),
				sexo=character(0),edad=charcter(0))
		}#if hubo eventos de orientación
		#escribe los datos al archivo csv
		write.table(data_orientacion,file=archivo_csv,sep="\t",
			row.names=FALSE)		
	}#if el docente es orientador
 	svalue(barra_status) <- ""; Sys.sleep(0.001)
} #recorrer_libro_asesoria

recorrer_libro_asesoria_plan <- function(asesor,archivo_Excel,
	archivo_csv,ano) {
	#Autor : José L. Romero
	#Fecha : 18/07/2014	
	#esta función recorre el libro Excel (de los asesores) para la
	#asesoria y genera un .csv cuyos renglones se corresponden a los
	#estudiantes y sus datos:
	#	clua				- código de centro local y unidad de apoyo
	#	codigo_plan	- código de proyecto/acción/área en plan operativo
	# subaccion   - código de subacción
	#	producto    - código de producto en plan operativo
	#	mes		      - més asesoría (si es del año indicado en el argumento)
	#abre el archivo Excel para su lectura
	wb <- loadWorkbook(archivo_Excel, create = TRUE )
	#lee la hoja de asesoría
	hoja <- "Asesorías"
	filas <- getLastRow(wb,hoja)
	svalue(barra_status) <- 
		paste0("Procesando el libro ",archivo_Excel,
		" Hoja: '",hoja,"' # de filas: ",filas)
	Sys.sleep(0.001)
	if (filas>5) {# hubo asesorías
		data_asesorias <- readWorksheet(wb , hoja, startRow=6, startCol=4,
			endRow = filas, endCol=8, header = FALSE, autofitCol=FALSE,
			autofitRow=FALSE, colTypes=c("character","numeric","numeric",
			"Date","character"))
		#Validar que las asignaturas y fechas no esten en blanco.
		#No considerar las que esten en blanco dando un error.
		#NOTA: las filas que uno deje en blanco al final (con sólo
		#las fórmulas) sí generan mensajes de error aqui. En
		#'recorrer_hoja_asesoria' se ignoran.
		errores <- data.frame()
		temp <- sum(!is.na(data_asesorias[,3]))
		if (temp<nrow(data_asesorias))
			errores <- rbind(errores,
				data.frame(clua="00 - 00",codigo_plan=paste("ERROR: no indicó",
				"las materias de algunas asesorias.<br />Archivo:",
				archivo_Excel,"hoja 'Asesorías'."),subaccion="00",
				producto="00",mes="01") )
		temp <- sum(!is.na(data_asesorias[,4]))
		if (temp<nrow(data_asesorias))
			errores <- rbind(errores,
				data.frame(clua="00 - 00",codigo_plan=paste("ERROR: no indicó",
				"las fechas de algunas asesorias.<br />Archivo:",
				archivo_Excel,"hoja 'Asesorías'."),subaccion="00",
				producto="00",mes="01") )
		validas <- which(!is.na(data_asesorias[,3])&
			!is.na(data_asesorias[,4]))
		data_asesorias <- data_asesorias[validas,]
		#trabaja con lo demás					
		fechas <- data_asesorias[,4]
		mes <- format(fechas,"%m")
		f_fechas <- ifelse(format(fechas,"%Y")==ano,TRUE,FALSE)	
		clua <- data_asesorias[,1]
		codigo_plan <- sapply(1:nrow(data_asesorias),
			function(i) id_plan(data_asesorias[i,2],data_asesorias[i,3],
			archivo_Excel) )
		producto <- sapply(data_asesorias[,5], function(i) 
			id_producto(i,archivo_Excel) )
		data_asesorias <- cbind(clua=clua,codigo_plan=codigo_plan,
			subaccion=rep("02",nrow(data_asesorias)),producto=producto,
			mes=mes)
		data_asesorias <- subset(data_asesorias,subset=f_fechas)
		data_asesorias <- rbind(data_asesorias,errores)
		#escribe los datos al archivo csv (si los hay)
		if (nrow(data_asesorias)>0)
			if (file.access(archivo_csv)!=0)	{
				write.table(data_asesorias,file=archivo_csv,sep="\t",
					row.names=FALSE)
			} else {
				write.table(data_asesorias,file=archivo_csv,sep="\t",
					row.names=FALSE,col.names=FALSE,append=TRUE)
			} #existe el archivo csv
	}#if hubo asesorías
	#lee la hoja de talleres
	hoja <- "Talleres"
	filas <- getLastRow(wb,hoja)
	svalue(barra_status) <- paste0("Procesando el libro ",archivo_Excel,
		" Hoja: '",hoja,"' # de filas: ",filas)
	Sys.sleep(0.001)
	if (filas>5) {# hubo talleres
		data_hoja <- readWorksheet(wb , hoja, startRow=6, startCol=2,
			endRow = filas, endCol=3, header = FALSE, autofitCol=FALSE,
			autofitRow=FALSE, colTypes=c("Date","numeric"))
		data_talleres <- data.frame(clua=character(0),
			codigo_plan=character(0), subaccion=character(0),
			producto=character(0),mes=character(0),
			stringsAsFactors = FALSE)
		for (fila in 1:nrow(data_hoja)) {
			fecha <- as.Date(data_hoja[fila,1])
			if (!is.na(fecha) & format(fecha,"%Y")==ano) {
				#si está dentro del año continua...
				clua <- asesor$adscrito
				subaccion <- "02"
				producto <- "50" #talleres
				mes <- format(fecha,"%m")
				indices_plan <- grep(paste0("...",
					sprintf("%03d",data_hoja[fila,2])),tabla_plan$carr_mat)
				if (data_hoja[fila,2]==0) indices_plan <- indices_plan[1]
				if (length(indices_plan)==0) {#reporta materia inexistente
					data_talleres <- rbind(data_talleres,
						data.frame(clua="00 - 00",codigo_plan=paste("ERROR: materia",
						sprintf("%03d",data_hoja[fila,2]),"inexistente.<br />",
						"Archivo:",archivo_Excel,"hoja 'Talleres'."),subaccion="00",
						producto="00",mes="01") )
				} else {
					codigos_plan <- unique(tabla_plan[indices_plan,"codigo"])
					for (i in codigos_plan)
						data_talleres <- rbind(data_talleres,
							data.frame(clua=clua,codigo_plan=i,
							subaccion="02",	producto="50",mes=mes) )
				}#if materia inexistente?
			} else #if está dentro del año
				if (is.na(fecha))
					data_talleres <- rbind(data_talleres,
						data.frame(clua="00 - 00",codigo_plan=paste("ERROR: taller sin",
						"fecha.<br />Archivo:",archivo_Excel,"hoja: 'Talleres'",
						"fila:",fila+5,"."),subaccion="00",producto="00",mes="01") )
		}#for cada una de las filas leidas desde la hoja "Talleres"
		#escribe los datos al archivo csv (si los hay)
		if (nrow(data_talleres)>0)
			if (file.access(archivo_csv)!=0)	{
				write.table(data_talleres,file=archivo_csv,sep="\t",
					row.names=FALSE)
			} else {
				write.table(data_talleres,file=archivo_csv,sep="\t",
					row.names=FALSE,col.names=FALSE,append=TRUE)
			} #existe el archivo csv
	}#if hubo talleres
	#si asesor es un orientador, procesa la hoja
	#"Servicios de Orientación"
	if (asesor$tipo=="Orientador") {
		hoja <- "Servicios de Orientación"
		filas <- getLastRow(wb,hoja)
		svalue(barra_status) <- 
			paste0("Procesando el libro ",archivo_Excel,
			" Hoja: '",hoja,"' # de filas: ",filas)
		Sys.sleep(0.001)
		if (filas>5) {
			data_orientacion <- readWorksheet(wb , hoja, startRow=6,
				startCol=4, endRow = getLastRow(wb,hoja), endCol=6, 
				header = FALSE, autofitCol=FALSE, autofitRow=FALSE,
				colTypes=c("numeric","Date","character"))
			fechas <- as.Date(data_orientacion[,2])
			f_fechas <- ifelse(format(fechas,"%Y")==ano,
				TRUE,FALSE)	
			mes <- format(fechas,"%m")
			clua <- rep(asesor$adscrito,nrow(data_orientacion))
			servicio <- sapply(data_orientacion[,3],
				function(i) id_servicio(i,archivo_Excel))
			codigo_plan <- servicio["codigo_plan",]
			subaccion <- servicio["subaccion",]
			producto <- servicio["producto",]
			data_orientacion <- cbind(clua=clua,codigo_plan=codigo_plan,
				subaccion=subaccion,producto=producto,mes=mes)
			data_orientacion <- subset(data_orientacion,subset=f_fechas)
			#escribe los datos al archivo csv
			if (nrow(data_orientacion)>0)
				if (file.access(archivo_csv)!=0)	{
					write.table(data_orientacion,file=archivo_csv,sep="\t",
						row.names=FALSE)
				} else {
					write.table(data_orientacion,file=archivo_csv,sep="\t",
						row.names=FALSE,col.names=FALSE,append=TRUE)
				}#if archivo csv ya existe
		}#if hay más de 5 filas
	} #el asesor es Orientador
} #recorrer_libro_asesoria_plan

generar_archivos_plan <- function() {
	#Esta función integra las 3 funciones de recorrido del plan op.
	#Obtener el año según el selector de lapso en el menú principal.
	lapso <- svalue(selector_lapso)
	ano <- substr(lapso,1,4)
	#obtener los asesores a incluir en el reporte, según la selección
	#de asesores en el menú principal.
	quienes <- sub_lista_asesores(svalue(selector_asesores))
	#Genera una lista de todos los posibles archivos xls en el dir.
	#de trabajo que pertenzecan a los asesores seleccionados.
	#El resultado es una lista de archivos de asesoría y otra de
	#evaluación.
	nombres <- sapply(quienes,function(i) i$asesor)
	tipo_ase <- sapply(quienes,function(i) i$tipo)
	adscrito <- sapply(quienes,function(i) i$adscrito)
	archivos <- sapply(quienes,function(i) i$archivo)
	lista_archivos <- list()
	for (i in order(nombres)) {
		lista_tmp <- c(list.files(pattern=paste0("^asesoria_",
			archivos[i],"_.+\\.xls")),list.files(pattern=paste0("^OL_",
			archivos[i],"_.+\\.xls")) )		
		if (length(lista_tmp)>0)	
			lista_archivos[[length(lista_archivos)+1]] <- 
				list(nombre=nombres[i],tipo=tipo_ase[i],
					adscrito=adscrito[i],archivos=lista_tmp)
	}#for
	#si no hay archivos xls para procesar retorna un error (cod. 1)
	if (length(lista_archivos)==0) return(1)
	#borra este archivo por si acaso existe.
	unlink("plan_operativo.csv")
	archivo <- file("lista_archivos.log","w", blocking = FALSE)
	num_arch <- 1
	if (length(lista_archivos)>0) {
		for (i in lista_archivos) {
			writeLines(con=archivo,paste(i$nombre," - Rol docente:",i$tipo))
			for (j in i$archivos) {
				print(j)
				writeLines(con=archivo,paste(sprintf("%03d",num_arch),j))
				num_arch <- num_arch+1
				if (substr(j,1,2)=="OL") {
					if (i$tipo=="Asesor") {
						recorrer_libro_evaluacion_asesor_plan(archivo_Excel=j,
							archivo_csv="plan_operativo.csv",ano=ano)
					} else {
						recorrer_libro_evaluacion_orientador_plan(archivo_Excel=j,
							archivo_csv="plan_operativo.csv",ano=ano)
					}
				} else 
					recorrer_libro_asesoria_plan(asesor=i,archivo_Excel=j,
						archivo_csv="plan_operativo.csv",ano=ano)
			}#for j
			writeLines(con=archivo,"")
		}# for i
	}#if hay archivos
	writeLines(con=archivo,paste("Total: ",num_arch-1,"archivos"))
	close(archivo)
	#depuración adicional
	if (file.access("plan_operativo.csv")!=0) {
		plantmp <- data.frame(clua=character(0),codigo_plan=character(0),
			subaccion=character(0),producto=character(0),mes=character(0),
			stringsAsFactors = FALSE)
	} else {
		plantmp <- read.table("plan_operativo.csv",sep="\t",header=TRUE,
			colClasses=rep("character",5))
	}
	#agrega todos los 12 meses para cersiorarse de que las tablas
	#finales tengan 12 filas para los 12 meses
	for (mes in 1:12) {
		plantmp <- rbind(plantmp,data.frame(clua="00 - 00",
			codigo_plan="000000",subaccion=sprintf("%02d",(mes %% 3)+1),
			producto="00",mes=sprintf("%02d",mes),stringsAsFactors = FALSE))
	}# for mes	
	#extrae los errores y colócalos en un archivo de nombre
	#"errores_plan.log"
	i_errores <- which(substr(plantmp$codigo_plan,1,5)=="ERROR")
	if (length(i_errores)>0) {
		archivo <- file("errores_plan.log", "w", blocking = FALSE)
		writeLines(plantmp$codigo_plan[i_errores],archivo)
		close(archivo)
		plantmp <- plantmp[-i_errores,]
	} #si hubo errores
	write.table(plantmp,"plan_operativo.csv",sep="\t",row.names=FALSE)
	return(0)
} #generar_archivos_plan
