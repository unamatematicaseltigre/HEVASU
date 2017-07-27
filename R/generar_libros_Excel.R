#Funciones para generar las nominas Excel de cada asesor, del curso
#introductorio y de las asesorías.
#Fecha : 05/05/2016
#Autor : José L. Romero
#requiere los siguientes paquetes: XLConnect, rJava

#-----------------------------------------------------------------------
#generar_asesorias (lapso)
#genera los libros de asesorías (para materias de todos los estudiantes)
#del lapso correspondiente.
generar_asesorias <- function(lapso) {
	#CODIGOS DE ERROR 
	#0			Todo bien
	#1			No existe el archivo nomina_regularxx.RData / 
	#       nomina_introductorioxx.RData
	#2			No existe la plantilla para generar el archivo de
	#       asesoría.
	#3	    No se han seleccionado asesores.
	#función interna para escribir formulas en una hoja
	escribe_formulas <- function(hoja,tipo_asesor) {
		filas_nomina <- nrow(nomina)+5
		if (tipo_asesor!="Asesor") 
			filas_nomina <- filas_nomina+nrow(nomina_intro)
		setCellFormula(wb_ases, sheet=hoja, row=6, col=3,
			paste0("VLOOKUP(B6,'Nomina de estudiantes'!$B$6:$E$",
			filas_nomina,",2,FALSE)"))
		if (hoja=="Asesorías") {
			setCellFormula(wb_ases, sheet=hoja, row=6, col=4,
				paste0("VLOOKUP(B6,'Nomina de estudiantes'!$B$6:$E$",
				filas_nomina,",3,FALSE)"))	
			setCellFormula(wb_ases, sheet=hoja, row=6, col=5,
				paste0("VLOOKUP(B6,'Nomina de estudiantes'!$B$6:$E$",
				filas_nomina,",4,FALSE)"))
			setCellFormula(wb_ases, sheet=hoja, row=6, col=10,
				paste0("VLOOKUP(B6,'Nomina de estudiantes'!$B$6:$H$",
				filas_nomina,",6,FALSE)"))
			setCellFormula(wb_ases, sheet=hoja, row=6, col=11,
				paste0("TRUNC((G6-VLOOKUP(B6,'Nomina de estudiantes'!$B$6:$H$",
				filas_nomina,",7,FALSE))/365)") )
		} else {
			setCellFormula(wb_ases, sheet=hoja, row=6, col=4,
				paste0("VLOOKUP(B6,'Nomina de estudiantes'!$B$6:$E$",
				filas_nomina,",4,FALSE)"))
			setCellFormula(wb_ases, sheet=hoja, row=6, col=7,
				paste0("VLOOKUP(B6,'Nomina de estudiantes'!$B$6:$H$",
				filas_nomina,",6,FALSE)"))
			setCellFormula(wb_ases, sheet=hoja, row=6, col=8,
				paste0("TRUNC((E6-VLOOKUP(B6,'Nomina de estudiantes'!$B$6:$H$",
				filas_nomina,",7,FALSE))/365)") )
		}
	}
	#se requiere el archivo de nomina: "nomina_regularxx.RData"
	if (!file.access(ruta_nomina_RData,4)==0) return(1)	#no existe el
																											#archivo de nomina
	load(ruta_nomina_RData)
	#verificar si se encuentran las plantillas
	ruta_ase <- file.path(dir_programa,"plantillas","plantilla_ASE.xls")
	ruta_orientador <- file.path(dir_programa,"plantillas",
		"plantilla_ASE_ORI.xls")
	if (!file.access(ruta_ase)==0|!file.access(ruta_orientador)==0)
		return(2)
	#verificar si se han seleccionado asesores
	if (length(seleccion_asesores)<1) return(3)
	#Ciclo principal - para cada asesor
	for (asesor in seleccion_asesores) {
		#Avisar el progreso de la tarea
		svalue(barra_status) <- 
			paste0("Creando el libro Excel para ",asesor$asesor," ...")
		Sys.sleep(0.001)
		#Si el asesor es orientador o preparador y no se ha leido
		#nomina_intro, leela
		if (asesor$tipo!="Asesor" & !("nomina_intro" %in% ls()) ) {
			#se requiere el archivo de nomina: "nomina_introductorioxx.RData"
			if (!file.access(ruta_nomina_intro_RData,4)==0)
				return(1)	#no existe el archivo de nomina
			load(ruta_nomina_intro_RData)		
		}#if leer la nomina_intro o no
		#Generar el archivo Excel para las asesorías
		#hacer una copia de la plantilla
		asesoria_xls <- paste0("asesoria_",asesor$archivo,"_",lapso,".xls")
		if (asesor$tipo=="Orientador")
			system(paste("cp",ruta_orientador,asesoria_xls)) else
			system(paste("cp",ruta_ase,asesoria_xls))
		#cargar el libro excel
		wb_ases <- loadWorkbook(asesoria_xls, create=FALSE)
		setStyleAction(wb_ases,XLC$"STYLE_ACTION.NONE")
		#escribe el centro local en las hojas correspondientes
		writeWorksheet(wb_ases,paste("CL",centrolocal), header=FALSE,
			sheet="Nomina de estudiantes", startRow=3, startCol=2)
		writeWorksheet(wb_ases,
			ifelse(asesor$adscrito=="00 - 00","NIVEL CENTRAL",
				paste("CL",centrolocal)), sheet="Asesorías",
				header=FALSE, startRow=3,startCol=2)
		writeWorksheet(wb_ases,
			ifelse(asesor$adscrito=="00 - 00","NIVEL CENTRAL",
				paste("CL",centrolocal)), sheet="Talleres",
				header=FALSE, startRow=3,startCol=2)		
		if (asesor$tipo=="Orientador")
			writeWorksheet(wb_ases,paste("CL",centrolocal), header=FALSE,
				sheet="Servicios de Orientación", startRow=3, startCol=2)			
		#escribe los datos
		if (asesor$tipo=="Asesor") {
			if (asesor$adscrito=="00 - 00") {
				#La nomina de estudiantes es potencialmente muy larga
				#Escribe sólo los estudiantes de las carreras que asesora
				#el docente.
				nom <- subset(nomina,carrera %in% asesor$carreras)			
			} else {
				nom <- nomina
			}# if asesor es del Nivel Central
			#escribe la nomina de estudiantes para el asesor
			print("Policia 1")
			writeWorksheet(wb_ases,nom[,1:7],
				sheet="Nomina de estudiantes",header=FALSE,
				startRow=6, startCol=2)
			print("Policia 2a")
			writeWorksheet(wb_ases,1:(nrow(nom)),
				sheet="Nomina de estudiantes",header=FALSE,
				startRow=6, startCol=1)
			print("Policia 2b")
		} else {
			writeWorksheet(wb_ases,nomina[,1:7],
				sheet="Nomina de estudiantes", header=FALSE,
				startRow=6, startCol=2)
			writeWorksheet(wb_ases,nomina_intro[,1:7],
				sheet="Nomina de estudiantes", header=FALSE,
				startRow=6+nrow(nomina),startCol=2)
			writeWorksheet(wb_ases,1:(nrow(nomina)+
				nrow(nomina_intro)), sheet="Nomina de estudiantes",
				header=FALSE, startRow=6, startCol=1)
		}# if docente es asesor
		#escribe las fórmulas
		print("Policia 3")
		escribe_formulas("Asesorías",asesor$tipo)
		if (asesor$tipo=="Orientador")
			escribe_formulas("Servicios de Orientación","Orientador")
		#Fin - guarda el archivo
		setActiveSheet(wb_ases,sheet=2)
		saveWorkbook(wb_ases)
	}#for cada uno de los asesores seleccionados
	svalue(barra_status) <- ""
	Sys.sleep(0.001)
	return(0)
} #generar_asesorias

#-----------------------------------------------------------------------
#generar_libro_orientador
generar_libro_orientador <- function(lapso,orientador) {
	#CODIGOS DE ERROR 
	#0	Todo bien
	#1	No existe el archivo nomina_introductorioxx.RData
	#2	No existe la plantilla para generar el archivo de
	#		asesoría.
	#3	No se han seleccionado asesores.
	#se requiere el archivo de nomina: "nomina_introductorioxx.RData"
	#Función para mostrar la barra de progreso
	mostrar_progreso <- function(progreso) {
		mensaje <- paste0("Generando ",paste0("OL_",orientador$archivo,
			"_",lapso,".xls")," : [",
			paste(rep("█",progreso),collapse=""),
			paste(rep("░",19-progreso),collapse=""),"]")
		svalue(barra_status) <- mensaje; Sys.sleep(0.001)
	}
	mostrar_progreso(0)
	#si no existe el archivo de nomina retorna con error
	if (!file.access(ruta_nomina_intro_RData,4)==0) return(1)
	#todavia continuamos? ... carga la nomina:
	load(ruta_nomina_intro_RData)
	#se requiere tener la plantilla tambien
	ruta_plantilla <- file.path(dir_programa,"plantillas",
		"plantilla_INT.xls")
	if (!file.access(ruta_plantilla)==0) return(2)
	ruta_libro <- file.path(getwd(),paste0("OL_",
		orientador$archivo,"_",lapso,".xls"))
	#hacer una copia de la plantilla para el archivo del asesor
	system(paste("cp",ruta_plantilla,ruta_libro))
	mostrar_progreso(1)
	#PASO 2 :abre el libro
	wb <- loadWorkbook(ruta_libro, create = TRUE)
	#no se sobrescriben los estilos cada vez que se escribe una celda:
	setStyleAction(wb,XLC$"STYLE_ACTION.NONE")
	#carga los estilos de la plantilla
	renglon_sty <- getCellStyle(wb, "renglon")
	cedula_sty <- getCellStyle(wb,"cedula")
	nombre_sty <- getCellStyle(wb,"nombre")
	obj_sty <- getCellStyle(wb,"obj")
	fecha_sty <- getCellStyle(wb,"fecha")
	aprobado_sty <- getCellStyle(wb,"aprobado")
	bc_renglon_sty <- getCellStyle(wb, "bc.renglon")
	bc_cedula_sty <- getCellStyle(wb,"bc.cedula")
	bc_nombre_sty <- getCellStyle(wb,"bc.nombre")
	bc_obj_sty <- getCellStyle(wb,"bc.obj")
	bc_fecha_sty <- getCellStyle(wb,"bc.fecha")
	bc_aprobado_sty <- getCellStyle(wb,"bc.aprobado")
	hoja <- "Curso Introductorio"
	mostrar_progreso(2)
	#PASO 3 : Escribe info en el encabezado
	writeWorksheet(wb,paste("CL",centrolocal),sheet=hoja,header=FALSE,
		startRow=3,startCol=3)
	tmp_string <- orientador$tipo
	if (orientador$sexo=="F") tmp_string <- paste0(tmp_string,"a")
	writeWorksheet(wb,tmp_string,sheet=hoja,header=FALSE,
		startRow=6,startCol=1)
	writeWorksheet(wb, invertir_nombre_apellido(orientador$asesor),
		sheet=hoja,header=FALSE, startRow=6, startCol =3)
	oficina <- cloas[orientador$materias["000",]]
	writeWorksheet(wb, oficina, sheet=hoja,
		header=FALSE, startRow=7, startCol =3)
	writeWorksheet(wb, lapso, sheet=hoja,
		header=FALSE, startRow=7, startCol =7)
	writeWorksheet(wb, format(Sys.Date(), "%d/%m/%Y"), sheet=hoja,
		header=FALSE, startRow=7, startCol =12)
	#obten la nomina del introductorio para este orientador
	nom <- subset(nomina_intro,cloa==oficina,
		select=c(cedula,nombre,carrera))
	#define primera y última fila
	first_row <- 10+1
	last_row <- nrow(nom)+10
	mostrar_progreso(3)
	#PASO 4 : escribe la data de los estudiantes
	writeWorksheet(wb, nom[,1:2], sheet = hoja, header=FALSE,
		startRow = first_row, startCol = 2)
	writeWorksheet(wb, nom[,3], sheet = hoja, header=FALSE,
		startRow = first_row, startCol = 7)
	writeWorksheet(wb, 1:nrow(nom), sheet=hoja, header=FALSE,
		startRow = first_row, startCol = 1)
	mostrar_progreso(4)
	#PASO 5: escribe la fórmula que totaliza los objetivos ponderados
	las_formulas <- paste("SUM(H",first_row:last_row,":K",
		first_row:last_row,")",sep="")
	setCellFormula(wb, sheet=hoja, row=first_row:last_row,
		col=12, las_formulas)
	#dar formatos a las celdas
	if (last_row >= first_row) {
		sec_par <- seq(from=first_row,to=last_row,by=2)
		cols_nombres_vec_par <- as.vector(sapply(3:6,
			function(j) rep(j,length(sec_par)) ) )
		cols_obj_vec_par <- as.vector(sapply(8:11,
			function(j) rep(j,length(sec_par)) ) )
		mostrar_progreso(5)
		#PASO 6
		setCellStyle(wb, sheet = hoja, row = sec_par, col = 1,
			cellstyle = renglon_sty)
		mostrar_progreso(6)
		#PASO 7
		setCellStyle(wb, sheet = hoja, row = sec_par, col = 2,
			cellstyle = cedula_sty)
		mostrar_progreso(7)
		#PASO 8
		setCellStyle(wb, sheet = hoja, row = sec_par, col = 
			cols_nombres_vec_par, cellstyle = nombre_sty)
		mostrar_progreso(8)
		#PASO 9
		setCellStyle(wb, sheet = hoja, row = sec_par, col = 7,
			cellstyle = renglon_sty)
		mostrar_progreso(9)
		#PASO 10
		setCellStyle(wb, sheet = hoja, row = sec_par, col = 
			cols_obj_vec_par, cellstyle = obj_sty)
		mostrar_progreso(10)
		#PASO 11
		setCellStyle(wb, sheet = hoja, row = sec_par, col = 12,
			cellstyle = aprobado_sty)
		mostrar_progreso(11)	
		#PASO 12
		setCellStyle(wb, sheet = hoja, row = sec_par, col = 13,
			cellstyle = fecha_sty)
		mostrar_progreso(12)	
		if (last_row > first_row) {
			sec_impar <- seq(from=first_row+1,to=last_row,by=2)
			cols_nombres_vec_impar <- as.vector(sapply(3:6,
				function(j) rep(j,length(sec_impar)) ) )
			cols_obj_vec_impar <- as.vector(sapply(8:11,
				function(j) rep(j,length(sec_impar)) ) )
			mostrar_progreso(11)
			#PASO 13
			setCellStyle(wb, sheet = hoja, row = sec_impar, col = 1,
				cellstyle = bc_renglon_sty)
			mostrar_progreso(13)
			#PASO 14
			setCellStyle(wb, sheet = hoja, row = sec_impar, col = 2,
				cellstyle = bc_cedula_sty)
			mostrar_progreso(14)
			#PASO 15
			setCellStyle(wb, sheet = hoja, row = sec_impar, col = 
				cols_nombres_vec_impar, cellstyle = bc_nombre_sty)
			mostrar_progreso(15)
			#PASO 16
			setCellStyle(wb, sheet = hoja, row = sec_impar, col = 7,
				cellstyle = bc_renglon_sty)
			mostrar_progreso(16)
			#PASO 17
			setCellStyle(wb, sheet = hoja, row = sec_impar, col = 
				cols_obj_vec_impar, cellstyle = bc_obj_sty)
			mostrar_progreso(17)
			#PASO 18
			setCellStyle(wb, sheet = hoja, row = sec_impar, col = 12,
				cellstyle = bc_aprobado_sty)
			mostrar_progreso(18)
			#PASO 19
			setCellStyle(wb, sheet = hoja, row = sec_impar, col = 13,
				cellstyle = bc_fecha_sty)
			mostrar_progreso(19)
		}
	}
	#guarda ese libro excel
	setActiveSheet(wb,1)
	saveWorkbook(wb)
	svalue(barra_status) <- ""
	return(0)
}	#generar_libro_orientador()

#-----------------------------------------------------------------------
#generar_libro_asesor
#NOTA: la plantilla.xls tiene celdas protegidas.
#Para desproteger la hoja, la clave es 12345
generar_libro_asesor <- function(lapso,asesor) {
	#CODIGOS DE ERROR 
	#0			Todo bien
	#1			No existe el archivo nomina_regularxx.RData
	#2			No existe la plantilla para generar el archivo de
	#			  asesoría.
	#3			No se han seleccionado asesores.
	#
	#se requiere el archivo de nomina: "nomina_regularxx.RData"
	#si no existe el archivo de nomina retorna con error
	if (!file.access(ruta_nomina_RData,4)==0) return(1)
	#si existe por lo tanto cargalo
	load(ruta_nomina_RData)
	#se requiere tener la plantilla tambien
	ruta_plantilla <- file.path(dir_programa,"plantillas",
		"plantilla_OL.xls")
	if (!file.access(ruta_plantilla)==0) return(2)
	#el nombre del archivo Excel del asesor
	ruta_libro <- file.path(getwd(),
		paste0("OL_",asesor$archivo,"_",lapso,".xls"))
	#hacer una copia de la plantilla para el archivo del asesor
	system(paste("cp",ruta_plantilla,ruta_libro))
	#abre el libro
	wb <- loadWorkbook(ruta_libro, create = TRUE)
	#no se sobrescriben los estilos cada vez que se escribe una celda:
	setStyleAction(wb,XLC$"STYLE_ACTION.NONE")
	#carga los estilos de la plantilla
	barraclara_sty <- getCellStyle(wb, "Plantilla.Barraclara")
	barraclaracentrado_sty <- getCellStyle(wb,
		"Plantilla.Barraclaracentrado")
	barraclaracentradont_sty <- getCellStyle(wb,
		"Plantilla.Barraclaracentrado_nt")
	barraoscura_sty <- getCellStyle(wb, "Plantilla.Barraoscura")
	barraoscurafecha_sty <- getCellStyle(wb, "Plantilla.BarraoscuraFecha")
	barraoscuracentrado_sty <- getCellStyle(wb,
		"Plantilla.Barraoscuracentrado")
	cedula_sty <- getCellStyle(wb,"Plantilla.Cedula")
	cedulasombreada_sty <- getCellStyle(wb,"Plantilla.Cedulasombreada")
	centrado_sty <- getCellStyle(wb, "Plantilla.Centrado")
	centradont_sty <- getCellStyle(wb, "Plantilla.Centrado_nt")
	default_sty <- getCellStyle(wb, "Plantilla.Default")
	fechaev_sty <- getCellStyle(wb,"Plantilla.FechaEv")
	fechaevsombreado_sty <- getCellStyle(wb,
		"Plantilla.FechaEvsombreado")
	renglon_sty <- getCellStyle(wb,"Plantilla.Renglon")
	renglonsombreado_sty <- getCellStyle(wb,
		"Plantilla.Renglonsombreado")
	rotulostitulo_sty <- getCellStyle(wb, "Plantilla.Rotulostitulo")
	observaciones_sty <- getCellStyle(wb, "Plantilla.Observaciones")
	observacionessombreado_sty <- getCellStyle(wb,
		"Plantilla.Observacionessombreado")
	#lee el cronograma, si está disponible
	if (file.access(file.path(getwd(),"cronograma.csv"),4)==0) {
		hay_cronograma <- TRUE
		cronograma <- read.table("cronograma.csv",header=T,as.is=T,sep="\t")
	} else hay_cronograma <- FALSE
	#obtén el vector de materias que asesora
	sus_materias <- as.numeric(rownames(asesor$materias))
	#Ciclo secundario - para cada materia asesorada
	for (i in 1:length(sus_materias)) {
		#el nombre de la hoja en el libro excel
		hoja <- sprintf("%03.0f", sus_materias[i])
		#Avisar al usuario el progreso de la operación
		svalue(barra_status) <- paste("Libro: ",basename(ruta_libro),
			" Hoja: ", hoja,sep="")
		Sys.sleep(0.001)
		#obten la información sobre esta materia
		info_materia <- materia(sus_materias[i])
		#prepara un vector con las fechas para cada tipo de evaluación
		nevs <- length(info_materia$evaluaciones)		
		if (hay_cronograma) {
			mat_en_crono <- subset(cronograma,
				subset=cronograma$codigo==sus_materias[i])
			fechas_crono <- with(mat_en_crono,as.Date(c(F1,F2,F3,F4),
				"%d/%m/%Y"))
			ti_pruebas <- with(mat_en_crono,c(Ti1,Ti2,Ti3,Ti4))
			ti_pruebas[is.na(ti_pruebas)] <- "nada"
			names(fechas_crono) <- ti_pruebas
			fechas <- sapply(info_materia$evaluaciones, function(i) {
				if (i %in% ti_pruebas) {
					fechas_crono[i]
				} else NA
			})
		} else fechas <- rep(NA,nevs)
		class(fechas) <- "Date"
		#copia la plantilla a la hoja
		cloneSheet(wb, sheet="plantilla", name = hoja )
		#Escribe info en el encabezado
		writeWorksheet(wb,
			ifelse(asesor$adscrito=="00 - 00","NIVEL CENTRAL",
				paste("CL",centrolocal)),
			sheet=hoja,header=FALSE,startRow=3,startCol=3)
		tmp_string <- asesor$tipo
		if (asesor$sexo=="F") tmp_string <- paste0(tmp_string,"a")
		writeWorksheet(wb, tmp_string, sheet=hoja,
			header=FALSE, startRow=7, startCol =1)
		writeWorksheet(wb,info_materia$nombre,sheet=hoja, header=FALSE,
			startRow=5, startCol=3)
		writeWorksheet(wb, info_materia$cd, sheet=hoja, header=FALSE,
			startRow=6, startCol=3)		
		writeWorksheet(wb, c(asesor$asesor, asesor$adscrito), sheet=hoja,
			header=FALSE, startRow=7, startCol=3)		
		writeWorksheet(wb, c(info_materia$materia,info_materia$creditos),
			sheet=hoja, header=FALSE, startRow=5, startCol=5)
		writeWorksheet(wb, lapso,
			sheet=hoja, header=FALSE, startRow=7, startCol=5)
		writeWorksheet(wb, Sys.Date(), sheet=hoja,
			header=FALSE, startRow = 8, startCol = 5)
		#obten la nomina para esta materia de este asesor
		#primero elimina de la nomina los estudiantes que no inscribieron
		#materias
		estudiantes_sin_materias <- is.na(nomina$materias)
		if (any(estudiantes_sin_materias)) {
			nom <- nomina[-which(estudiantes_sin_materias),]
		} else
			nom <- nomina
		esta_materia_en <- sapply(as.character(nom$materias), function(j)
			sus_materias[i] %in% as.numeric(strsplit(j," ")[[1]]) )
		nom <- subset(nom,esta_materia_en,select=cedula:carrera)
		#agrega aquellos estudiantes de las cl-oa's que atiende pero
		#verifica si es un asesor de Nivel Central primero
		if (asesor$adscrito=="00 - 00") {
			codCL <- names(tablaCL)
			nom <- subset(nom,substr(cloa,1,2) %in% codCL[as.logical(
				asesor$materias[i,])])		
		} else {
			nom <- subset(nom,cloa %in% cloas[as.logical(
				asesor$materias[i,])])
		}# if asesor es de Nivel Central
		#obtiene los objetivos
		nobj <- nrow(info_materia$objetivos)
		los_objetivos <- 1:nobj
		#define primera y última fila
		first_row <- 11+1
		last_row <- nrow(nom)+11
		#defina primera y última columna de objetivos
		fo_col <- 6+1
		lo_col <- 6+nobj
		#defina primera y última columna de evaluaciones presentadas,
		#si aplica
		if (nevs>0) {
			fep_col <- lo_col+2
			lep_col <- nevs+fep_col-1
			comment_col <- lep_col+1 }
		else {
			fep_col <- lo_col+1
			lep_col <- fep_col
			comment_col <- lep_col
		}
		#escribe la info sobre los objetivos y sus ponderaciones
		objetivos <- rbind(los_objetivos,
			info_materia$objetivos$ponderacion)
		writeWorksheet(wb, objetivos, sheet = hoja, header=FALSE,
			startRow = 10, startCol = fo_col)
		setColumnWidth(wb,sheet=hoja,fo_col:lo_col,width=730)
		#escribe el rótulo "Total puntos" en la fila de encabezado
		writeWorksheet(wb, c("Total","puntos"), sheet = hoja,
			header=FALSE, startRow = 10, startCol = lo_col+1)
		setColumnWidth(wb,sheet=hoja,lo_col+1,width=1820)
		#escribe la parte de evaluaciones presentadas
		if (nevs>0) {
			setCellStyle(wb, sheet = hoja, row = 10,
				col = fep_col:lep_col, cellstyle = barraoscurafecha_sty)
			for (k in 1:(lep_col-fep_col+1)) {
				if (!is.na(fechas[k]))
					writeWorksheet(wb, fechas[k],	sheet = hoja, header=FALSE,
						startRow = 10, startCol = fep_col+k-1)
			}# for (escribe cada fecha de presentación por separado)
			#ahora escribe los tipos de evaluaciones en la fila de abajo
			writeWorksheet(wb,t(info_materia$evaluaciones),sheet=hoja,
				header=FALSE,startRow=11,startCol=fep_col)
			setColumnWidth(wb,sheet=hoja,fep_col:lep_col, width=10*256)
		}#if (hay evaluaciones)
		#dar formato a la barra de encabezado
		setCellStyle(wb, sheet = hoja, 
			row = 10, col = fo_col:(fep_col-1),
			cellstyle = barraoscuracentrado_sty)
		setCellStyle(wb, sheet = hoja, row=10, col=comment_col,
			cellstyle = barraoscuracentrado_sty)
		setCellStyle(wb, sheet = hoja, 
			row = 11, col = fo_col:comment_col,
			cellstyle = barraoscuracentrado_sty)
		#escribe "Observaciones" al final y ajusta el ancho de la col.
		writeWorksheet(wb,"Observaciones",sheet=hoja,header=FALSE,
			startRow=11,startCol=comment_col)
		setColumnWidth(wb,sheet=hoja,comment_col,width=80*256)
		if (first_row<=last_row) { #si hay nomina para escribir ...
			#escribe la data de los estudiantes
			writeWorksheet(wb, nom, sheet = hoja, header=FALSE,
				startRow = first_row, startCol = 2)
			writeWorksheet(wb, 1:nrow(nom), sheet=hoja, header=FALSE,
				startRow = first_row, startCol = 1)
			#escribe la fórmula que totaliza los objetivos ponderados
			las_formulas <- paste("SUMPRODUCT(",idx2col(fo_col),"$11:",
				idx2col(lo_col),"$11,",idx2col(fo_col),first_row:last_row,
				":",idx2col(lo_col),first_row:last_row,")",sep="")
			setCellFormula(wb, sheet=hoja, row=first_row:last_row,
				col=lo_col+1, las_formulas)
		} #if 
		#coloca el formato "condicional" para la celda de "total"
		centrado.CD_sty <- getCellStyle(wb,
			paste("Plantilla.CCD_",as.character(info_materia$cd),
			sep=""))
		barraclaracentrado.CD_sty <- getCellStyle(wb,
			paste("Plantilla.BCCD_",as.character(info_materia$cd),
			sep=""))
		#formatos para las filas de datos
		if (any(info_materia$objetivos$nc))
			protegidas <- (4:lo_col)[-(los_objetivos[
			info_materia$objetivos$nc]+3)]
		else
			protegidas <- c(4:lo_col)
		#si nevs=0, entonces todos los objetivos$nc son FALSE y por lo
		#tanto, todas las columnas son protegidas
		no_protegidas <- setdiff(4:lo_col,protegidas)
		if (last_row >= first_row) { #si hay nomina para escribir ...
			sec_impar <- seq(from=first_row,to=last_row,by=2)
			protegidas_vec_impar <- as.vector(sapply(protegidas,
				function(j) rep(j,length(sec_impar)) ) )
			#col1: número
			setCellStyle(wb, sheet = hoja, row = sec_impar, col = 1,
				cellstyle = renglon_sty)
			#col2: cédula
			setCellStyle(wb, sheet = hoja, row = sec_impar, col = 2,
				cellstyle = cedula_sty)
			#col3: nombre
			setCellStyle(wb, sheet = hoja, row = sec_impar, col = 3,
				cellstyle = default_sty)
			#col. de objs. protegidas
			setCellStyle(wb, sheet = hoja, row = sec_impar, 
				col = protegidas_vec_impar,
				cellstyle = centradont_sty)
			#col. de Total
			setCellStyle(wb, sheet = hoja, row = sec_impar,
				col = lo_col+1, cellstyle = centrado.CD_sty)
			if (length(no_protegidas)>0) {
				no_protegidas_vec_impar <- as.vector(sapply(
					no_protegidas, function(j) rep(j,
					length(sec_impar)) ) )
				#col. de objs. no protegidas
				setCellStyle(wb, sheet = hoja, row = sec_impar,
					col = no_protegidas_vec_impar,
					cellstyle = centrado_sty)
				}
			if (nevs>0) {
				fechasp_vec_impar <- as.vector(sapply(fep_col:lep_col,
					function(j) rep(j,length(sec_impar)) ) )
				#col. de fechas de present.
				setCellStyle(wb, sheet = hoja, row = sec_impar,
					col = fechasp_vec_impar,
					cellstyle = fechaev_sty)
				}
			#col. de observaciones
			setCellStyle(wb,sheet=hoja,row=sec_impar,col=comment_col,
					cellstyle=observaciones_sty)
			if (last_row > first_row) {
				sec_par <- seq(from=first_row+1,to=last_row,by=2)
				protegidas_vec_par <- as.vector(sapply(protegidas,
					function(j) rep(j,length(sec_par)) ) )
				#col 1 renglón
				setCellStyle(wb, sheet = hoja, row = sec_par, col = 1,
					cellstyle = renglonsombreado_sty)
				#col. de cédula
				setCellStyle(wb, sheet = hoja, row = sec_par, col = 2,
					cellstyle = cedulasombreada_sty)
				#col. de nombre
				setCellStyle(wb, sheet = hoja, row = sec_par, col = 3,
					cellstyle = barraclara_sty)
				#col. objs. prot.
				setCellStyle(wb, sheet = hoja, row = sec_par,
					col = protegidas_vec_par,
					cellstyle = barraclaracentradont_sty)
				#col. de Total
				setCellStyle(wb, sheet = hoja, row = sec_par, col = 
					lo_col+1, cellstyle = barraclaracentrado.CD_sty)
				if (length(no_protegidas)>0) {
					no_protegidas_vec_par <- as.vector(sapply (
						no_protegidas, function(j) rep(j,
						length(sec_par)) ) )
					#col. objetivos no protegidos (sombreados)
					setCellStyle(wb, sheet = hoja, row = sec_par,
						col = no_protegidas_vec_par,
						cellstyle = barraclaracentrado_sty)
				}
				if (nevs>0) {
					fechasp_vec_par <- as.vector(sapply(fep_col:lep_col,
						function(j) rep(j,length(sec_impar)) ) )
					#col. de fechas de presentación (sombreados)
					setCellStyle(wb, sheet = hoja, row = sec_par,
						col = fechasp_vec_par,
						cellstyle = fechaevsombreado_sty)
				}
				#col. de observaciones
				setCellStyle(wb,sheet=hoja,row=sec_par,col=comment_col,
					cellstyle=observacionessombreado_sty)
			}#if (first_row < last_row)
		}#if (first_row <= last_row)
	}#for (i in indices de sus materias)
	#quita la plantilla
	removeSheet(wb,sheet="plantilla")
	#guarda ese libro excel
	setActiveSheet(wb,1)
	saveWorkbook(wb)
	#sale sin error
	return(0)
}

#-----------------------------------------------------------------------
#generar_libros_xls
#NOTA: la plantilla.xls tiene celdas protegidas.
#Para desproteger la hoja, la clave es 12345
generar_libros_xls <- function(lapso) {
	#CODIGOS DE ERROR 
	#0	Todo bien
	#1	No existe el archivo nomina_regularxx.RData / 
	#		nomina_introductorioxx.RData
	#2	No existe la plantilla para generar el archivo de
	#		asesoría.
	#3	No se han seleccionado asesores.
	#4  El personal academico es un preparador.
	#se requiere el archivo de nomina:
	#"nomina_regularxx.RData" y/o "nomina_introductorioxx.RData" según
	#sea el caso.
	#verificar si se han seleccionado asesores
	if (length(seleccion_asesores)<1) return(3)
	#Ciclo principal - para cada asesor
	for (asesor in seleccion_asesores) {
		#determinar si asesor es asesor, orientador o preparador
		switch(asesor$tipo,
			Orientador = error <- generar_libro_orientador(lapso,asesor),
			Asesor = error <- generar_libro_asesor(lapso,asesor),
			Preparador = error <- 4
		)
		#si hay algún problema se sale del bucle
		if (error!=0) break
	}
	svalue(barra_status) <- ""
	Sys.sleep(0.001)
	return(error)
} #generar_libros_xls
