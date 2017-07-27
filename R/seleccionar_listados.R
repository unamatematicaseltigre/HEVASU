#FUNCIONES PARA SELECCIONAR LOS LISTADOS PDF/CSV DE UNASEC
#CON BOTON DE ACCION (invoca funciones en procesar_listados_UNASEC.R)
#Fecha : 06/07/2014
#Autor : José L. Romero
#-----------------------------------------------------------------------

#Funciones para obtener rutas de archivo plausibles
#el argumento "codigo_CL" es una cadena de 2 digitos con el código
#del Centro Local

posible_ruta_regulares <- function(codigo_CL) {
	patron0 <- paste0("listado_",codigo_CL,"_reg_INSCRITOS")
	patronb <- paste0("^",patron0,".+\\.pdf$")
	patrone <- paste0(patron0,".pdf")
	encontrados <- list.files(pattern=patronb,full.names=TRUE)
	if (length(encontrados)>0) {
		return(encontrados[1])
	} else return(patrone)
} #posible_ruta_regulares

posible_ruta_regulares_csv <- function(codigo_CL) {
	patron0 <- paste0("inscritos_",codigo_CL)
	patronb <- paste0("^",patron0,".+\\.csv$")
	patrone <- paste0(patron0,".csv")
	encontrados <- list.files(pattern=patronb,full.names=TRUE)
	if (length(encontrados)>0) {
		return(encontrados[1])
	} else return(patrone)
} #posible_ruta_regulares_csv

posible_ruta_nuevos <- function(codigo_CL) {
	patron0 <- paste0("listado_",codigo_CL,"_reg_nuevos")
	patronb <- paste0("^",patron0,".+\\.pdf$")
	patrone <- paste0(patron0,".pdf")
	encontrados <- list.files(pattern=patronb,full.names=TRUE)
	if (length(encontrados)>0) {
		return(encontrados[1])
	} else return(patrone)
} #posible_ruta_nuevos

posible_ruta_reingreso_3omas <- function(codigo_CL) {
	patron0 <- paste0("listado_",codigo_CL,"_reg_reingreso")
	patronb <- paste0("^",patron0,".+\\.pdf$")
	patrone <- paste0(patron0,".pdf")
	encontrados <- list.files(pattern=patronb,full.names=TRUE)
	if (length(encontrados)>0) {
		return(encontrados[1])
	} else return(patrone)
} #posible_ruta_reingreso_3omas

posible_ruta_reingreso_egresados <- function(codigo_CL) {
	patron0 <- paste0("listado_",codigo_CL,"_reg_egresados")
	patronb <- paste0("^",patron0,".+\\.pdf$")
	patrone <- paste0(patron0,".pdf")
	encontrados <- list.files(pattern=patronb,full.names=TRUE)
	if (length(encontrados)>0) {
		return(encontrados[1])
	} else return(patrone)
} #posible_ruta_reingreso_egresados

posible_ruta_introductorio <- function(codigo_CL) {
	patron0 <- paste0("listado_",codigo_CL,"_int_INSCRITOS")
	patronb <- paste0("^",patron0,".+\\.pdf$")
	patrone <- paste0(patron0,".pdf")
	encontrados <- list.files(pattern=patronb,full.names=TRUE)
	if (length(encontrados)>0) {
		return(encontrados[1])
	} else return(patrone)
} #posible_ruta_introductorio

posible_ruta_introductorio_csv <- function(codigo_CL) {
	#posible ruta para el archivo csv de UNASEC
	patron0 <- paste0("introductorio_",codigo_CL)
	patronb <- paste0("^",patron0,".+\\.csv$")
	patrone <- paste0(patron0,".csv")
	encontrados <- list.files(pattern=patronb,full.names=TRUE)
	if (length(encontrados)>0) {
		return(encontrados[1])
	} else return(patrone)
} #posible_ruta_introductorio_csv

posible_ruta_OLintroductorio_csv <- function() {
	#se refiere al archivo csv creado cuando se generan los
	#certificados de aprobación
	patrone <- paste0("OL_",svalue(selector_lapso),".csv")
	patronb <- paste0("^OL_.+_",svalue(selector_lapso),"\\.csv$")
	encontrados <- list.files(pattern=patronb,full.names=TRUE)
	if (length(encontrados)>0) {
		return(encontrados[1])
	} else return(patrone)
} #posible_ruta_OLintroductorio_csv()

posible_ruta_OL_xls <- function() {
	patrone <- paste0("OL_",svalue(selector_lapso),".xls")
	patronb <- paste0("OL_[a-z]+_",svalue(selector_lapso),"\\.xls")
	encontrados <- list.files(pattern=patronb,full.names=TRUE)
	if (length(encontrados)>0) {
		return(encontrados[1])
	} else return(patrone)
} #posible_ruta_OL_xls()

posible_ruta_tsv_google_drive <- function() {
	#se refiere al archivo tsv con la lista de archivos
	#en una carpeta pública de Google Drive
	patrone <- "lista_archivos.tsv"
	patronb <- ".+\\.tsv$"
	encontrados <- list.files(pattern=patronb,full.names=TRUE)
	if (length(encontrados)>0) {
		return(encontrados[1])
	} else return(patrone)
}
#-----------------------------------------------------------------------
#funciones para los GUI's de selección y generación de archivos

seleccionar_listados_regulares <- function() {
	#mensaje de espera
	svalue(barra_status) <- "Cargando el menu ..."
	Sys.sleep(0.001)
	#aqui comienza la definición del cuadro de dialogo
	wp <- gbasicdialog(title=paste("Procesar listados de Inscripción",
		"Regular"),parent=ventana,do.buttons=FALSE,
		visible=FALSE,horizontal=FALSE)
	ANCHO_X <- 515
	size(wp) <- c(ANCHO_X,250)
	wp_marco <- ggroup(horizontal=FALSE,cont=wp)
	#menu bar superior
	lista_menu <- list(
		Volver = list(
			salir=gaction(label="Volver al menú principal",icon="gtk-quit",
				handler = function(h,...) dispose(wp),
				tooltip = "Salir al menú principal")
			)
	)
	gmenu(lista_menu, cont = wp_marco)
	addSpace(wp_marco,value=5)
	#Para los archivos con data de estudiantes regulares:
	#Titulo
	marco_titulo <- ggroup(horizontal=TRUE,cont=wp_marco)
	addSpring(marco_titulo)
	logo_marco <- gimage("logouna",dirname="stock",size="button",
		cont=marco_titulo)
	titulo_mm_regular <- glabel("Inscripción Regular",
		cont=marco_titulo)
	font(titulo_mm_regular) <- list(color="black", size=12)
	size(titulo_mm_regular) <- c(ANCHO_X-60,20)
	addSpring(marco_titulo)
	gseparator(horizontal=TRUE,cont=wp_marco)		
	addSpace(wp_marco,value=10)
	#Marco de selección para listado de estudiantes regulares
	marco_regular <- gframe(text="Inscripción Regular (pdf)",
		container=wp_marco)
	lista_regular <- list(
		texto = glabel(text=posible_ruta_regulares(codigoCL),
			editable=FALSE),
		boton = gaction(label="Seleccionar ",icon="pdf",
			handler = function(h,...) 
				gfile ("Seleccione el listado pdf de estudiantes regulares",
					filter = list(".pdf" = list(patterns = c("*.pdf"))),
					initialfilename=file.path(getwd(),"listado.pdf"),
					handler=function(h,...) {
						svalue(lista_regular$texto) <- h$file
						if (file.access(h$file,4)==0) {
							font(lista_regular$texto) <- 
							list(color="darkgreen",size=8)
						} else
							font(lista_regular$texto) <- list(color="red",size=8) }  
				)
			)
	)
	toolbar_regular <- gtoolbar(lista_regular,cont=marco_regular)
	size(toolbar_regular) <- c(ANCHO_X-15,40)
	size(lista_regular$texto) <- c(ANCHO_X-65,32)
	#dar color según exista el archivo pdf
	if (file.access(svalue(lista_regular$texto),4)==0)
		font(lista_regular$texto) <- list(color="darkgreen",size=8) else
		font(lista_regular$texto) <- list(color="red", size=8)
	#Marco de selección - listado de data personal de est. regulares
	marco_datos_regulares <- 
		gframe(text="Data personal de los estudiantes regulares (csv)",
		container=wp_marco)
	lista_datos_regulares <- list(
		texto = glabel(text=posible_ruta_regulares_csv(codigoCL),
			editable=FALSE),
		boton = gaction(label="Seleccionar ",icon="textcsv",
			handler = function(h,...) 
				gfile (paste("Seleccione el archivo csv con la data",
					"personal de los estudiantes regulares"),
					filter = list(".csv" = list(patterns = c("*.csv"))),
					initialfilename=file.path(getwd(),"listado.csv"),
					handler=function(h,...) {
						svalue(lista_datos_regulares$texto) <- h$file
						if (file.access(h$file,4)==0) {
							font(lista_datos_regulares$texto) <- 
								list(color="darkgreen",size=8)
						} else
							font(lista_datos_regulares$texto) <- 
								list(color="red",size=8) }  
					)
				)
		)
	toolbar_datos_regulares <- gtoolbar(lista_datos_regulares,
		cont=marco_datos_regulares)
	size(toolbar_datos_regulares) <- c(ANCHO_X-15,40)
	size(lista_datos_regulares$texto) <- c(ANCHO_X-65,32)
	#dar color según exista el archivo csv
	if (file.access(svalue(lista_datos_regulares$texto),4)==0) {
		font(lista_datos_regulares$texto) <- 
			list(color="darkgreen",size=8)
	} else
		font(lista_datos_regulares$texto) <- list(color="red", size=8)
	#Botón para procesar los listados (llamar a la función procesar)
	marco_boton <- ggroup(horizontal=TRUE, cont=wp_marco)
	addSpring(marco_boton)
	addSpace(marco_boton,value=200,horizontal=TRUE)
	go_button <- gbutton("gtk-execute",border=TRUE,
		handler=function(h,...) {
			archivo_pdf <- svalue(lista_regular$texto)
			archivo_csv <- svalue(lista_datos_regulares$texto)
			if (file.access(archivo_pdf,4)==0 & 
				file.access(archivo_csv,4)==0) {
				error <- generar_nomina_regular_CL(archivo_pdf,archivo_csv)
				if (error==1) 
					error_msg <- gmessage("El archivo no es un pdf válido.",
						title="Error",icon="error")
				if (error==2)
					error_msg <- gmessage(paste("El archivo no contiene",
						"data UNASEC"),title="Error",icon="error")
				if (error==3)
					error_msg <- gmessage(paste("La data del archivo",
						"pdf no se corresponde\ncon la data del archivo csv."),
						title="Error",icon="error")
				if (error==4)
					error_msg <- gmessage(paste("Debe revisar y luego",
						"reparar el archivo csv."),
						title="Aviso",icon="warning")
				if (error==4|error==0) dispose(wp)
			} else
				error_msg <- 
					gmessage(paste0("Seleccione los archivos pdf",
						" y csv con la data\nde los estudiantes regulares",
						" primero."),title="Aviso",icon="warning")
		}, cont=marco_boton)
	tooltip(go_button) <- paste("Haga clic aquí para procesar",
		"los archivos\nindicados y generar la data de la nomina",
		"de\ninscritos regulares.")
	addSpace(marco_boton,value=200,horizontal=TRUE)
	addSpring(marco_boton)
	#---------------------
	svalue(barra_status) <- ""
	Sys.sleep(0.001)
	visible(wp) <- TRUE
} #seleccionar_listados_regulares()

procesar_listados_regulares_NC <- function() {
	ejecutar <- function(h,...) {
		if (!any(existen_ambos)) {
			gmessage(paste("No hay Centros Locales con data completa.",
				"Descargue los listados desde la página de UNASEC."),
				title="Error", icon = "error" )
			return(1)
		}# if
		if (!all(existen_ambos)) {
			gmessage(paste("Hay Centros Locales con data incompleta.",
				"Se generará la nomina sólo para aquellos Centros Locales con",
				"la data completa."),title="Aviso", icon = "warning" )
		}# if
		if (file.access(ruta_nomina_csv)==0) unlink(ruta_nomina_csv)
		cualesCL <- which(existen_ambos)
		for (i in cualesCL) {
			#procesa el listado pdf del CL i-ésimo ...
			error <- generar_nomina_regular_CL(archivo_pdf=rutas_pdf[i],
				archivo_csv=rutas_csv[i],codCL=names(tablaCL)[i])
			#y luego indica si hubo errores. El usuario debe tomar nota.
			if (error==1) 
				error_msg <- gmessage("El archivo no es un pdf válido.",
					title="Error",icon="error")
			if (error==3)
				error_msg <- gmessage(paste("La data del archivo",
					"pdf no se corresponde\ncon la data del archivo csv."),
					title="Error",icon="error")
			if (error==4)
				error_msg <- gmessage(paste("Debe revisar y luego",
					"reparar el archivo csv."),title="Aviso",icon="warning")
						
		}# for i in Centros Locales con ambos listados
		#lee el archivo csv y genera la nómina en RData
		nomina <- read.table(file=ruta_nomina_csv,sep="\t",header=TRUE)
		save("nomina",file=ruta_nomina_RData)
		dispose(wp) #no hay errores en cuanto a los CL sin listados
	}# ejecutar
	#mensaje de espera
	svalue(barra_status) <- "Cargando el menu ..."
	Sys.sleep(0.001)
	#primero crea la tabla para los nombres de los CL
	tabla_centros <- rownames(centros)[-1]
	tabla_centros <- paste(substr(centros[-1,1],1,2),"-",tabla_centros)
	names(tabla_centros) <- substr(centros[-1,1],1,2)
	#para cada CL, detecta la presencia de los posibles archivos pdf y csv
	rutas_csv <- sapply(names(tablaCL),function(i) 
		posible_ruta_regulares_csv(i))
	rutas_pdf <- sapply(names(tablaCL),function(i) 
		posible_ruta_regulares(i))
	existe_csv <- sapply(rutas_csv, function(i)
		file.access(i)==0)
	existe_pdf <- sapply(rutas_pdf, function(i)
		file.access(i)==0)
	existen_ambos <- existe_csv & existe_pdf
	iconos_csv <- ifelse(existe_csv,"⬛","⬜")
	iconos_pdf <- ifelse(existe_pdf,"⬛","⬜")
	#aqui comienza la definición del cuadro de dialogo
	wp <- gbasicdialog(title=paste("Procesar listados de Inscripción",
		"Regular"),parent=ventana,do.buttons=FALSE,
		visible=FALSE,horizontal=FALSE)
	size(wp) <- c(540,380)
	wp_marco <- ggroup(horizontal=FALSE,cont=wp)
	addSpace(wp_marco,value=10,horizontal=FALSE)
	marco_titulo <- ggroup(horizontal=TRUE,cont=wp_marco)
	addSpring(marco_titulo)
	logo_marco <- gimage("logouna",dirname="stock",size="button",
		cont=marco_titulo)
	titulo_marco <- glabel("Listados disponibles para cada Centro Local",
		cont=marco_titulo)
	size(titulo_marco) <- c(466,20)
	font(titulo_marco) <- list(color="black",size=12)
	addSpring(marco_titulo)
	sep1_marco <- gseparator(horizontal=TRUE,cont=wp_marco)
	marco_tabla <- ggroup(horizontal=TRUE,cont=wp_marco)
	addSpring(marco_tabla)
	tabla_archivos <- gtable(
		data.frame(CL1=as.character(tablaCL[1:11]),
			pdf1=iconos_pdf[1:11],csv2=iconos_csv[1:11],
			CL2=as.character(tablaCL[12:22]),pdf2=iconos_pdf[12:22],
			csv2=iconos_csv[12:22]),container=marco_tabla)
	#cambia los encabezados de las columnas
	gtk_tabla <- getToolkitWidget(tabla_archivos)
	for (i in 0:5) {
		assign(paste0("col",i),gtkTreeViewGetColumn(gtk_tabla, i))
		gtkTreeViewColumnSetClickable(get(paste0("col",i)), FALSE)
		assign(paste0("rend",i), 
			gtkCellLayoutGetCells(get(paste0("col",i))) )
		if (i %in% c(1,2,4,5)) 
			gtkCellRendererSetAlignment(get(paste("rend",i,sep=""))[[1]],
				xalign=0.5,yalign=0.5)
		assign(paste0("col",i,"_header"),gtkLabelNew(switch((i %% 3)+1,
			"Centro Local","pdf","csv"),show=TRUE))
		gtkTreeViewColumnSetWidget(get(paste0("col",i)), 
			widget = get(paste0("col",i,"_header")))
	}#for
	size(tabla_archivos) <- c(496,272)
	addSpring(marco_tabla)
	sep2_marco <- gseparator(horizontal=TRUE,cont=wp_marco)
	addSpring(wp_marco)
	marco_boton <- ggroup(horizontal=TRUE,cont=wp_marco)
	addSpring(marco_boton)
	go_button <- gbutton("gtk-execute",border=TRUE,
		handler=ejecutar, cont=marco_boton)
	addSpring(marco_boton)
	addSpring(wp_marco)
	#--------------------------
	svalue(barra_status) <- ""
	Sys.sleep(0.001)
	visible(wp) <- TRUE
}#procesar_listados_regulares_NC

#-----------------------------------------------------------------------
seleccionar_listados_introductorio <- function() {
	#mensaje de espera
	svalue(barra_status) <- "Cargando el menu ..."
	Sys.sleep(0.001)
	#aqui comienza la definición del cuadro de dialogo
	wp <- gbasicdialog(title=paste("Procesar listados de Inscripción",
		"Introductorio"),parent=ventana,do.buttons=FALSE,
		visible=FALSE,horizontal=FALSE)
	ANCHO_X <- 515
	size(wp) <- c(ANCHO_X,250)
	wp_marco <- ggroup(horizontal=FALSE,cont=wp)
	#menu bar superior
	lista_menu <- list(
		Volver = list(
			salir=gaction(label="Volver al menú principal",icon="gtk-quit",
				handler = function(h,...) dispose(wp),
				tooltip = "Salir al menú principal")
			)
	)
	gmenu(lista_menu, cont = wp_marco)
	addSpace(wp_marco,value=5)
	marco_titulo <- ggroup(horizontal=TRUE,cont=wp_marco)
	addSpring(marco_titulo)
	logo_marco <- gimage("logouna",dirname="stock",size="button",
		cont=marco_titulo)
	titulo_mm_introductorio <- glabel(
		"Inscripción del Curso Introductorio",cont=marco_titulo)
	font(titulo_mm_introductorio) <- list(color="black", size=12)
	size(titulo_mm_introductorio) <- c(ANCHO_X-60,20)
	addSpring(marco_titulo)
	gseparator(horizontal=TRUE,cont=wp_marco)		
	addSpace(wp_marco,value=10)
	#Marco de selección - listado de data personal de est. del CI
	marco_introductorio <- 
		gframe(text="Listado de estudiantes curso introductorio",
		container=wp_marco)
	lista_intro <- list(
		texto = glabel(text=posible_ruta_introductorio(codigoCL),
			editable=FALSE),
		boton = gaction(label="Seleccionar ",icon="pdf",
			handler = function(h,...) 
				gfile (paste0("Seleccione el listado pdf de estudiantes ",
					"del curso introductorio"),
					filter = list(".pdf" = list(patterns = c("*.pdf"))),
					initialfilename=file.path(getwd(),"listado.pdf"),
					handler=function(h,...) {
					svalue(lista_intro$texto) <- h$file
					if (file.access(h$file,4)==0)
						font(lista_intro$texto) <- 
						list(color="darkgreen",size=8) else
						font(lista_intro$texto) <- list(color="red",size=8) }  
				)
			)
	)
	toolbar_intro <- gtoolbar(lista_intro,cont=marco_introductorio)
	size(toolbar_intro) <- c(ANCHO_X-15,40)
	size(lista_intro$texto) <- c(ANCHO_X-65,32)
	#dar color según exista el archivo pdf
	if (file.access(svalue(lista_intro$texto),4)==0)
		font(lista_intro$texto) <- list(color="darkgreen",size=8) else
		font(lista_intro$texto) <- list(color="red", size=8)
	#Marco de selección - listado de data personal de est. del CI
	marco_datos_introductorio <- 
		gframe(text="Data personal de los estudiantes del CI",
		container=wp_marco)
	lista_datos_introductorio <- list(
		texto = glabel(posible_ruta_introductorio_csv(codigoCL),
			editable=FALSE),
		boton = gaction(label="Seleccionar ",icon="textcsv",
			handler = function(h,...) 
				gfile (paste("Seleccione el archivo csv con la data",
					"personal de los estudiantes del CI."),
					filter = list(".csv" = list(patterns = c("*.csv"))),
					initialfilename=file.path(getwd(),"listado.csv"),
					handler=function(h,...) {
						svalue(lista_datos_introductorio$texto) <- h$file
						if (file.access(h$file,4)==0) {
							font(lista_datos_introductorio$texto) <- 
								list(color="darkgreen",size=8)
						} else
							font(lista_datos_introductorio$texto) <- 
								list(color="red",size=8) }  
				)
			)
	)
	toolbar_datos_introductorio <- gtoolbar(lista_datos_introductorio,
		cont=marco_datos_introductorio)
	size(toolbar_datos_introductorio) <- c(ANCHO_X-15,40)
	size(lista_datos_introductorio$texto) <- c(ANCHO_X-65,32)
	#dar color según exista el archivo csv
	if (file.access(svalue(lista_datos_introductorio$texto),4)==0) {
		font(lista_datos_introductorio$texto) <- 
			list(color="darkgreen",size=8)
	} else
		font(lista_datos_introductorio$texto) <- list(color="red", size=8)
	#Botón para procesar los listados (llamar a la función procesar)
	marco_boton <- ggroup(horizontal=TRUE, cont=wp_marco)
	addSpring(marco_boton)
	addSpace(marco_boton,value=200,horizontal=TRUE)
	go_button <- gbutton("gtk-execute",border=TRUE,
		handler=function(h,...) {
			archivo_pdf <- svalue(lista_intro$texto)
			archivo_csv <- svalue(lista_datos_introductorio$texto)
			if (file.access(archivo_pdf,4)==0 & 
				file.access(archivo_csv,4)==0) {
				error <- generar_nomina_introductorio_CL(archivo_pdf,
					archivo_csv)
				if (error==1) 
					error_msg <- gmessage("El archivo no es un pdf válido.",
						title="Error",icon="error")
				if (error==2)
					error_msg <- gmessage(paste("El archivo no contiene",
						"data UNASEC"),title="Error",icon="error")
				if (error==3)
					error_msg <- gmessage(paste("La data del archivo",
						"pdf no se corresponde\ncon la data del archivo csv."),
						title="Error",icon="error")
				if (error==4)
					error_msg <- gmessage(paste("Debe revisar y luego",
						"reparar el archivo csv."),
						title="Aviso",icon="warning")
				if (error==4|error==0) dispose(wp)
			} else
				error_msg <- 
					gmessage(paste0("Seleccione los archivos pdf",
						" y csv con la data\nde los estudiantes regulares",
						" primero."),title="Aviso",icon="warning")
		}, cont=marco_boton)
	tooltip(go_button) <- paste("Haga clic aquí para procesar",
		"los archivos\nindicados y generar la data de la nomina",
		"de\ninscritos en el CI.")
	addSpace(marco_boton,value=200,horizontal=TRUE)
	addSpring(marco_boton)		
	#--------------------------
	svalue(barra_status) <- ""
	Sys.sleep(0.001)
	visible(wp) <- TRUE
} #seleccionar_listados_introductorio()

#-----------------------------------------------------------------------
seleccionar_listados_nuevos <- function() {
	#mensaje de espera
	svalue(barra_status) <- "Cargando el menu ..."
	Sys.sleep(0.001)
	#aqui comienza la definición del cuadro de dialogo
	wp <- gbasicdialog(title=paste("Generar listado csv de Nuevos",
		"Ingresos"),parent=ventana,do.buttons=FALSE,
		visible=FALSE,horizontal=FALSE)
	ANCHO_X <- 515
	size(wp) <- c(ANCHO_X,180)
	wp_marco <- ggroup(horizontal=FALSE,cont=wp)
	#menu bar superior
	lista_menu <- list(
		Volver = list(
			salir=gaction(label="Volver al menú principal",icon="gtk-quit",
				handler = function(h,...) dispose(wp),
				tooltip = "Salir al menú principal")
			)
	)
	gmenu(lista_menu, cont = wp_marco)
	addSpace(wp_marco,value=5)
	#Para los archivos con data de estudiantes regulares:
	#Titulo
	marco_titulo <- ggroup(horizontal=TRUE,cont=wp_marco)
	addSpring(marco_titulo)
	logo_marco <- gimage("logouna",dirname="stock",size="button",
		cont=marco_titulo)
	titulo_mm_nuevos <- glabel("Listado de Nuevos Inscritos",
		cont=marco_titulo)
	font(titulo_mm_nuevos) <- list(color="black", size=12)
	size(titulo_mm_nuevos) <- c(ANCHO_X-60,20)
	addSpring(marco_titulo)
	gseparator(horizontal=TRUE,cont=wp_marco)		
	addSpace(wp_marco,value=10)
	#Marco de selección para listado de estudiantes regulares
	marco_nuevos <- gframe(text="Nuevos Inscritos (pdf)",
		container=wp_marco)
	lista_nuevos <- list(
		texto = glabel(text=posible_ruta_nuevos(codigoCL),
			editable=FALSE),
		boton = gaction(label="Seleccionar ",icon="pdf",
			handler = function(h,...) 
				gfile ("Seleccione el listado pdf de nuevos inscritos",
					filter = list(".pdf" = list(patterns = c("*.pdf"))),
					initialfilename=file.path(getwd(),"listado.pdf"),
					handler=function(h,...) {
						svalue(lista_nuevos$texto) <- h$file
						if (file.access(h$file,4)==0) {
							font(lista_nuevos$texto) <- 
								list(color="darkgreen",size=8)
						} else
							font(lista_nuevos$texto) <- list(color="red",size=8) }  
				)
			)
	)
	toolbar_nuevos <- gtoolbar(lista_nuevos,cont=marco_nuevos)
	size(toolbar_nuevos) <- c(ANCHO_X-15,40)
	size(lista_nuevos$texto) <- c(ANCHO_X-65,32)
	#dar color según exista el archivo pdf
	if (file.access(svalue(lista_nuevos$texto),4)==0)
		font(lista_nuevos$texto) <- list(color="darkgreen",size=8) else
		font(lista_nuevos$texto) <- list(color="red", size=8)
	#Botón para procesar los listados (llamar a la función procesar)
	marco_boton <- ggroup(horizontal=TRUE, cont=wp_marco)
	addSpring(marco_boton)
	addSpace(marco_boton,value=200,horizontal=TRUE)	
	go_button <- gbutton("gtk-execute",border=TRUE,
		handler=function(h,...) {
			archivo_pdf <- svalue(lista_nuevos$texto)
			if (file.access(archivo_pdf,4)==0) {
				if (file.access(ruta_nomina_RData,4)==0) {
					error <- generar_nomina_nuevos(archivo_pdf,
						svalue(selector_oficina))
					if (error==1) { 
						error_msg <- gmessage("El archivo no es un pdf válido.",
							title="Error",icon="error")
					} else dispose(wp)
				} else
					error_msg <- gmessage(paste("No ha generado los datos",
						"de la nomina estudiantil. Seleccione un archivo pdf/csv",
						"válido con el listado UNASEC y luego",
						"'Procesar listado de estudiantes regulares'."),
						title="Error",icon="error")					
			} else
				error_msg <- 
					gmessage(paste0("Seleccione el archivos pdf",
						" con la data de los estudiantes\nregulares",
						" primero."),title="Aviso",icon="warning")
		}, cont=marco_boton)
	tooltip(go_button) <- paste("Haga clic aquí para generar",
		"el listado de\nnuevos ingresos para el CL-UA seleccionado.")
	addSpace(marco_boton,value=200,horizontal=TRUE)
	addSpring(marco_boton)
	#--------------------------
	svalue(barra_status) <- ""
	Sys.sleep(0.001)
	visible(wp) <- TRUE
} #seleccionar_listados_nuevos()

#-----------------------------------------------------------------------
seleccionar_listados_reingresos <- function() {
	#mensaje de espera
	svalue(barra_status) <- "Cargando el menu ..."
	Sys.sleep(0.001)
	#aqui comienza la definición del cuadro de dialogo
	wp <- gbasicdialog(title="Generar listado csv de Reingresos",
		parent=ventana,do.buttons=FALSE, visible=FALSE,horizontal=FALSE)
	ANCHO_X <- 515
	size(wp) <- c(ANCHO_X,180)
	wp_marco <- ggroup(horizontal=FALSE,cont=wp)
	#menu bar superior
	lista_menu <- list(
		Volver = list(
			salir=gaction(label="Volver al menú principal",icon="gtk-quit",
				handler = function(h,...) dispose(wp),
				tooltip = "Salir al menú principal")
			)
	)
	gmenu(lista_menu, cont = wp_marco)
	addSpace(wp_marco,value=5)
	#Titulo
	marco_titulo <- ggroup(horizontal=TRUE,cont=wp_marco)
	addSpring(marco_titulo)
	logo_marco <- gimage("logouna",dirname="stock",size="button",
		cont=marco_titulo)
	titulo_mm_reingresos <- glabel("Listado de Reingresos",
		cont=marco_titulo)
	font(titulo_mm_reingresos) <- list(color="black", size=12)
	size(titulo_mm_reingresos) <- c(ANCHO_X-60,20)
	addSpring(marco_titulo)
	gseparator(horizontal=TRUE,cont=wp_marco)		
	addSpace(wp_marco,value=10)
	#Marco de selección para listado de estudiantes de reingreso
	marco_reingresos <- gframe(text="Reingresos (pdf)",
		container=wp_marco)
	lista_reingresos <- list(
		texto = glabel(text=posible_ruta_reingreso_3omas(codigoCL),
			editable=FALSE),
		boton = gaction(label="Seleccionar",icon="pdf",
			handler = function(h,...) 
				gfile ("Seleccione el listado pdf de estudiantes de reingreso",
					filter = list(".pdf" = list(patterns = c("*.pdf"))),
					initialfilename=file.path(getwd(),"listado.pdf"),
					handler=function(h,...) {
						svalue(lista_reingresos$texto) <- h$file
						if (file.access(h$file,4)==0) {
							font(lista_reingresos$texto) <- 
								list(color="darkgreen",size=8)
						} else
							font(lista_reingresos$texto) <- list(color="red",size=8) }  
				)
			)
	)
	toolbar_reingresos <- gtoolbar(lista_reingresos,cont=marco_reingresos)
	size(toolbar_reingresos) <- c(ANCHO_X-15,40)
	size(lista_reingresos$texto) <- c(ANCHO_X-65,32)
	#dar color según exista el archivo pdf
	if (file.access(svalue(lista_reingresos$texto),4)==0) {
		font(lista_reingresos$texto) <- 
			list(color="darkgreen",size=8)
	} else
		font(lista_reingresos$texto) <- list(color="red", size=8)
	#Botón para procesar los listados (llamar a la función procesar)
	marco_boton <- ggroup(horizontal=TRUE, cont=wp_marco)
	addSpring(marco_boton)
	addSpace(marco_boton,value=200,horizontal=TRUE)		
	go_button <- gbutton("gtk-execute",border=TRUE,
		handler=function(h,...) {
			archivo_pdf <- svalue(lista_reingresos$texto)
			if (file.access(archivo_pdf,4)==0) {
				if (file.access(ruta_nomina_RData,4)==0) {
					error <- generar_nomina_reingresos(archivo_pdf,
						svalue(selector_oficina),"listado_reingresos")
					if (error==1) {
						error_msg <- gmessage("El archivo no es un pdf válido.",
							title="Error",icon="error")
					} else dispose(wp)
				} else
					error_msg <- gmessage(paste("No ha generado los datos",
						"de la nomina estudiantil. Seleccione un archivo pdf/csv",
						"válido con el listado UNASEC y luego",
						"'Procesar listado de estudiantes regulares'."),
						title="Error",icon="error")					
			} else
				error_msg <- 
					gmessage(paste0("Seleccione el archivo pdf con la data de",
						" los estudiantes\n de reingreso primero"),
						title="Aviso",icon="warning")
		}, cont=marco_boton)
	tooltip(go_button) <- paste("Haga clic aquí para generar",
		"el listado de\nreingresos para el CL-UA seleccionado.")
	addSpace(marco_boton,value=200,horizontal=TRUE)
	addSpring(marco_boton)	
	#--------------------------
	svalue(barra_status) <- ""
	Sys.sleep(0.001)
	visible(wp) <- TRUE
} #seleccionar_listados_reingresos()

#-----------------------------------------------------------------------
seleccionar_listados_reing_egre <- function() {
	#mensaje de espera
	svalue(barra_status) <- "Cargando el menu ..."
	Sys.sleep(0.001)
	#aqui comienza la definición del cuadro de dialogo
	wp <- gbasicdialog(title=paste("Generar listado csv de",
		"Reingresos (Egresados)"),
		parent=ventana,do.buttons=FALSE, visible=FALSE,horizontal=FALSE)
	ANCHO_X <- 515
	size(wp) <- c(ANCHO_X,180)
	wp_marco <- ggroup(horizontal=FALSE,cont=wp)
	#menu bar superior
	lista_menu <- list(
		Volver = list(
			salir=gaction(label="Volver al menú principal",icon="gtk-quit",
				handler = function(h,...) dispose(wp),
				tooltip = "Salir al menú principal")
			)
	)
	gmenu(lista_menu, cont = wp_marco)
	addSpace(wp_marco,value=5)
	#Titulo
	marco_titulo <- ggroup(horizontal=TRUE,cont=wp_marco)
	addSpring(marco_titulo)
	logo_marco <- gimage("logouna",dirname="stock",size="button",
		cont=marco_titulo)
	titulo_mm_reing_egre <- glabel("Listado de Reingresos (Egresados)",
		cont=marco_titulo)
	font(titulo_mm_reing_egre) <- list(color="black", size=12)
	size(titulo_mm_reing_egre) <- c(ANCHO_X-60,20)
	addSpring(marco_titulo)
	gseparator(horizontal=TRUE,cont=wp_marco)		
	addSpace(wp_marco,value=10)
	#Marco de selección para listado de estudiantes de reingreso (egr.)
	marco_reing_egre <- gframe(text="Reingreso de Egresados (pdf)",
		container=wp_marco)
	lista_reing_egre <- list(
		texto = glabel(text=posible_ruta_reingreso_egresados(codigoCL),
			editable=FALSE),
		boton = gaction(label="Seleccionar ",icon="pdf",
			handler = function(h,...) 
				gfile ("Seleccione el listado pdf de reingresos (egresados)",
					filter = list(".pdf" = list(patterns = c("*.pdf"))),
					initialfilename=file.path(getwd(),"listado.pdf"),
					handler=function(h,...) {
						svalue(lista_reing_egre$texto) <- h$file
						if (file.access(h$file,4)==0) {
							font(lista_reing_egre$texto) <- 
								list(color="darkgreen",size=8)
						} else
							font(lista_reing_egre$texto) <- list(color="red",size=8) }  
				)
			)
	)
	toolbar_reing_egre <- gtoolbar(lista_reing_egre,cont=marco_reing_egre)
	size(toolbar_reing_egre) <- c(ANCHO_X-15,40)
	size(lista_reing_egre$texto) <- c(ANCHO_X-65,32)
	#dar color según exista el archivo pdf
	if (file.access(svalue(lista_reing_egre$texto),4)==0) {
		font(lista_reing_egre$texto) <- 
			list(color="darkgreen",size=8)
	} else
		font(lista_reing_egre$texto) <- list(color="red", size=8)
	#Botón para procesar los listados (llamar a la función procesar)
	marco_boton <- ggroup(horizontal=TRUE, cont=wp_marco)
	addSpring(marco_boton)
	addSpace(marco_boton,value=200,horizontal=TRUE)	
	go_button <- gbutton("gtk-execute",border=TRUE,
		handler=function(h,...) {
			archivo_pdf <- svalue(lista_reing_egre$texto)
			if (file.access(archivo_pdf,4)==0) {
				if (file.access(ruta_nomina_RData,4)==0) {
					error <- generar_nomina_reingresos(archivo_pdf,
						svalue(selector_oficina),"listado_reing_egresados")
					if (error==1) {
						error_msg <- gmessage("El archivo no es un pdf válido.",
							title="Error",icon="error")
					} else dispose(wp)
				} else
					error_msg <- gmessage(paste("No ha generado los datos",
						"de la nomina estudiantil. Seleccione un archivo pdf/csv",
						"válido con el listado UNASEC y luego",
						"'Procesar listado de estudiantes regulares'."),
						title="Error",icon="error")					
			} else
				error_msg <- 
					gmessage(paste0("Seleccione el archivo pdf con la data",
						" de los estudiantes egresados (reingreso)",
						" primero."),title="Aviso",icon="warning")
		}, cont=marco_boton)
	tooltip(go_button) <- paste("Haga clic aquí para generar",
		"el listado de\nreingresos (de egreados) para el\n",
		"CL-UA seleccionado.")
	addSpace(marco_boton,value=200,horizontal=TRUE)
	addSpring(marco_boton)
	#--------------------------
	svalue(barra_status) <- ""
	Sys.sleep(0.001)
	visible(wp) <- TRUE
} #seleccionar_listados_reing_egre()

#-----------------------------------------------------------------------

seleccionar_cronograma <- function() {
	#mensaje de espera
	svalue(barra_status) <- "Cargando el menu ..."
	Sys.sleep(0.001)
	#aqui comienza la definición del cuadro de dialogo 
	wp <- gbasicdialog(title="Leer Calendario de Pruebas",parent=ventana,
		do.buttons=FALSE,visible=FALSE,horizontal=FALSE)
	ANCHO_X <- 515
	size(wp) <- c(ANCHO_X,250)
	wp_marco <- ggroup(horizontal=FALSE,cont=wp)
	#menu bar superior
	lista_menu <- list(
		Volver = list(
			salir=gaction(label="Volver al menú principal",icon="gtk-quit",
				handler = function(h,...) dispose(wp),
				tooltip = "Salir al menú principal")
			)
	)
	gmenu(lista_menu, cont = wp_marco)
	addSpace(wp_marco,value=5)
	#Titulo
	marco_titulo <- ggroup(horizontal=TRUE,cont=wp_marco)
	addSpring(marco_titulo)
	logo_marco <- gimage("logouna",dirname="stock",size="button",
		cont=marco_titulo)
	titulo_mm_cronograma <- glabel("Calendario de Pruebas por Asignatura",
		cont=marco_titulo)
	size(titulo_mm_cronograma) <- c(ANCHO_X-60,20)
	font(titulo_mm_cronograma) <- list(color="black", size=12)
	addSpring(marco_titulo)
	gseparator(horizontal=TRUE,cont=wp_marco)		
	addSpace(wp_marco,value=10)
	#Marco de selección para el cronograma (pdf)
	marco_cronograma <- gframe(text="Calendario de pruebas (pdf)",
		container=wp_marco)
	lista_cronograma <- list(
		texto = glabel(text=file.path(getwd(),"cronograma.pdf"),
			editable=FALSE),
		boton = gaction(label="Seleccionar ",icon="pdf",
			handler = function(h,...) 
				gfile ("Seleccione el calendario de pruebas",
					filter = list(".pdf" = list(patterns = c("*.pdf"))),
					initialfilename=file.path(getwd(),"cronograma.pdf"),
					handler=function(h,...) {
						svalue(lista_cronograma$texto) <- h$file
						if (file.access(h$file,4)==0) {
							font(lista_cronograma$texto) <- 
								list(color="darkgreen",size=8)
						} else
							font(lista_cronograma$texto) <- list(color="red",size=8) }  
				)
			)
	)
	toolbar_cronograma <- gtoolbar(lista_cronograma,cont=marco_cronograma)
	size(toolbar_cronograma) <- c(ANCHO_X-15,40)
	size(lista_cronograma$texto) <- c(ANCHO_X-65,32)
	#dar color según exista el archivo pdf
	if (file.access(svalue(lista_cronograma$texto),4)==0) {
		font(lista_cronograma$texto) <- 
			list(color="darkgreen",size=8)
	} else
		font(lista_cronograma$texto) <- list(color="red", size=8)
	#Botón para procesar los listados (llamar a la función procesar)
	marco_boton <- ggroup(horizontal=TRUE, cont=wp_marco)
	addSpring(marco_boton)
	addSpace(marco_boton,value=200,horizontal=TRUE)	
	go_button <- gbutton("gtk-execute",border=TRUE,
		handler=function(h,...) {
			archivo_pdf <- svalue(lista_cronograma$texto)
			if (file.access(archivo_pdf,4)==0) {
				error <- generar_cronograma(archivo_pdf)
				if (error==1) {
					error_msg <- gmessage("El archivo no es un pdf válido.",
						title="Error",icon="error")
				}
				if (error==0) dispose(wp)
			} else
				error_msg <- 
					gmessage(paste0("Seleccione el archivo pdf con el",
						" calendario de pruebas por asignatura",
						" primero."),title="Aviso",icon="warning")
		}, cont=marco_boton)
	tooltip(go_button) <- paste("Haga clic aquí para generar",
		"el archivo csv\ndel calendario de pruebas")
	addSpace(marco_boton,value=200,horizontal=TRUE)
	addSpring(marco_boton)
	#--------------------------
	svalue(barra_status) <- ""
	Sys.sleep(0.001)
	visible(wp) <- TRUE
} #seleccionar_cronograma()

#-----------------------------------------------------------------------
seleccionar_datos_fichas <- function() {
	#mensaje de espera
	svalue(barra_status) <- "Cargando el menu ..."
	Sys.sleep(0.001)
	#aqui comienza la definición del cuadro de dialogo 
	wp <- gbasicdialog(title="Generar fichas académicas",parent=ventana,
		do.buttons=FALSE,visible=FALSE,horizontal=FALSE)
	ANCHO_X <- 515
	size(wp) <- c(ANCHO_X,300)
	wp_marco <- ggroup(horizontal=FALSE,cont=wp)
	#menu bar superior
	lista_menu <- list(
		Volver = list(
			salir=gaction(label="Volver al menú principal",icon="gtk-quit",
				handler = function(h,...) dispose(wp),
				tooltip = "Salir al menú principal")
			)
	)
	gmenu(lista_menu, cont = wp_marco)
	addSpace(wp_marco,value=5)
	#Titulo
	marco_titulo <- ggroup(horizontal=TRUE,cont=wp_marco)
	addSpring(marco_titulo)
	logo_marco <- gimage("logouna",dirname="stock",size="button",
		cont=marco_titulo)
	titulo_mm_fichas <- glabel("Generación de fichas académicas",
		cont=marco_titulo)
	size(titulo_mm_fichas) <- c(ANCHO_X-60,20)
	font(titulo_mm_fichas) <- list(color="black", size=12)
	addSpring(marco_titulo)
	gseparator(horizontal=TRUE,cont=wp_marco)		
	addSpace(wp_marco,value=10)
	#Marco de entrada de datos para generación fichas
	flinea1 <- paste("1) Asegúrese de cambiar el directorio de trabajo",
		"a la carpeta donde tiene los\n archivos de fichas .REP.",
		"Las fichas se generarán en esa carpeta. ",
		"El directorio de\ntrabajo actual es:")
	flinea3 <- paste("2) Asegúrese de seleccionar en el menú principal",
		"el lapso correcto y el centro lo-\ncal/oficina de apoyo para",
		"la cual se están generando las fichas.")
	flinea4 <- paste("3) Si lo desea, puede indicar el url de la página",
		"web donde se publicarán las fichas\na continuación:")
	fichas_linea1 <- glabel(text=flinea1,editable=FALSE,cont=wp_marco)
	fichas_linea2 <- glabel(text=getwd(), editable=FALSE,cont=wp_marco)
	fichas_linea3 <- glabel(text=flinea3,editable=FALSE,cont=wp_marco)
	fichas_linea4 <- glabel(text=flinea4,editable=FALSE,cont=wp_marco)
	paginaweb <- glabel(text="http://unaorientacioneltigre.blogspot.com",
		editable=TRUE,cont=wp_marco)
	font(fichas_linea2) <- list(size=9)
	#Botón para procesar los listados (llamar a la función procesar)
	marco_boton <- ggroup(horizontal=TRUE, cont=wp_marco)
	addSpring(marco_boton)
	addSpace(marco_boton,value=200,horizontal=TRUE)	
	go_button <- gbutton("gtk-execute",border=TRUE,
		handler=function(h,...) {
			error <- generar_fichas(svalue(paginaweb))
			if (error==1) {
				error_msg <- gmessage("No hay archivos de fichas .REP.",
					title="Error",icon="error") }
			if (error==2) {
				error_msg <- 
					gmessage(paste("No hay archivos de fichas .REP",
						"válidos."),title="Aviso",icon="warning")}
			if (error==0) {
				svalue(barra_status) <- ""
				dispose(wp)
			}
		}, cont=marco_boton)
	tooltip(go_button) <- paste("Haga clic aquí para generar",
		"las fichas")
	addSpace(marco_boton,value=200,horizontal=TRUE)
	addSpring(marco_boton)
	#--------------------------
	svalue(barra_status) <- ""
	Sys.sleep(0.001)
	visible(wp) <- TRUE
} #seleccionar_datos_fichas

#-----------------------------------------------------------------------

seleccionar_tsv_fichas <- function() {
	#mensaje de espera
	svalue(barra_status) <- "Cargando el menu ..."
	Sys.sleep(0.001)
	#aqui comienza la definición del cuadro de dialogo 
	wp <- gbasicdialog(title="Generar html para consulta de fichas",
		parent=ventana,do.buttons=FALSE,visible=FALSE,horizontal=FALSE)
	ANCHO_X <- 515
	size(wp) <- c(ANCHO_X,250)
	wp_marco <- ggroup(horizontal=FALSE,cont=wp)
	#menu bar superior
	lista_menu <- list(
		Volver = list(
			salir=gaction(label="Volver al menú principal",icon="gtk-quit",
				handler = function(h,...) dispose(wp),
				tooltip = "Salir al menú principal")
			)
	)
	gmenu(lista_menu, cont = wp_marco)
	addSpace(wp_marco,value=5)
	#Titulo
	marco_titulo <- ggroup(horizontal=TRUE,cont=wp_marco)
	addSpring(marco_titulo)
	logo_marco <- gimage("logouna",dirname="stock",size="button",
		cont=marco_titulo)
	titulo_mm_fichas <- glabel("Generar HTML para consulta de fichas",
		cont=marco_titulo)
	size(titulo_mm_fichas) <- c(ANCHO_X-60,20)
	font(titulo_mm_fichas) <- list(color="black", size=12)
	addSpring(marco_titulo)
	gseparator(horizontal=TRUE,cont=wp_marco)		
	addSpace(wp_marco,value=10)
	#Marco de selección para la lista de archivos (.tsv)
	marco_fichas <- gframe(text="Seleccione de archivo tsv",
		container=wp_marco)
	lista_fichas <- list(
		texto = glabel(text=file.path(getwd(),"lista_archivos.tsv"),
			editable=FALSE),
		boton = gaction(label="Seleccionar ",icon="textocsv",
			handler = function(h,...) 
				gfile ("Seleccione el archivo tsv generado por Google Drive",
					filter = list(".tsv" = list(patterns = c("*.tsv"))),
					initialfilename=file.path(getwd(),"lista_archivos.tsv"),
					handler=function(h,...) {
						svalue(lista_fichas$texto) <- h$file
						if (file.access(h$file,4)==0) {
							font(lista_fichas$texto) <- 
								list(color="darkgreen",size=8)
						} else
							font(lista_fichas$texto) <- list(color="red",size=8) }  
				)
			)
	)
	toolbar_fichas <- gtoolbar(lista_fichas,cont=marco_fichas)
	size(toolbar_fichas) <- c(ANCHO_X-15,40)
	size(lista_fichas$texto) <- c(ANCHO_X-65,32)
	#dar color según exista el archivo pdf
	if (file.access(svalue(lista_fichas$texto),4)==0) {
		font(lista_fichas$texto) <- 
			list(color="darkgreen",size=8)
	} else
		font(lista_fichas$texto) <- list(color="red", size=8)
	#Botón para procesar los listados (llamar a la función procesar)
	marco_boton <- ggroup(horizontal=TRUE, cont=wp_marco)
	addSpring(marco_boton)
	addSpace(marco_boton,value=200,horizontal=TRUE)	
	go_button <- gbutton("gtk-execute",border=TRUE,
		handler=function(h,...) {
			archivo_tsv <- svalue(lista_fichas$texto)
			if (file.access(archivo_tsv,4)==0) {
				generar_html_fichas(archivo_tsv)
				dispose(wp)
			} else
				error_msg <- 
					gmessage(paste("Seleccione el archivo tsv con la",
						"lista de fichas (.pdf) en Google Drive"),
						title="Aviso",icon="warning")
		}, cont=marco_boton)
	tooltip(go_button) <- paste("Haga clic aquí para generar",
		"el archivo html para la publicación de las fichas en su blog.")
	addSpace(marco_boton,value=200,horizontal=TRUE)
	addSpring(marco_boton)
	#--------------------------
	svalue(barra_status) <- ""
	Sys.sleep(0.001)
	visible(wp) <- TRUE
} #seleccionar_tsv_fichas()

#-----------------------------------------------------------------------

seleccionar_archivos_para_certificados <- function() {
	#mensaje de espera
	svalue(barra_status) <- "Cargando el menu ..."
	Sys.sleep(0.001)
	#aqui comienza la definición del cuadro de dialogo
	wp <- gbasicdialog(title=paste("Generar HTML para la",
		"publicación de los certificados de aprobación del CI"),
		parent=ventana,do.buttons=FALSE,visible=FALSE,horizontal=FALSE)
	ANCHO_X <- 515
	size(wp) <- c(ANCHO_X,250)
	wp_marco <- ggroup(horizontal=FALSE,cont=wp)
	#menu bar superior
	lista_menu <- list(
		Volver = list(
			salir=gaction(label="Volver al menú principal",icon="gtk-quit",
				handler = function(h,...) dispose(wp),
				tooltip = "Salir al menú principal")
			)
	)
	gmenu(lista_menu, cont = wp_marco)
	addSpace(wp_marco,value=5)
	#Titulo
	marco_titulo <- ggroup(horizontal=TRUE,cont=wp_marco)
	addSpring(marco_titulo)
	logo_marco <- gimage("logouna",dirname="stock",size="button",
		cont=marco_titulo)
	titulo_mm_htmlcert <- glabel(paste("HTML para publicar certificados",
		"de aprobación del CI"),cont=marco_titulo)
	font(titulo_mm_htmlcert) <- list(color="black", size=12)
	size(titulo_mm_htmlcert) <- c(ANCHO_X-60,20)
	addSpring(marco_titulo)
	gseparator(horizontal=TRUE,cont=wp_marco)		
	addSpace(wp_marco,value=10)
	#Marco de selección para el archivo csv del curso introductorio
	marco_dataintro <- gframe(text="Data curso introductorio (csv)",
		container=wp_marco)
	lista_dataintro <- list(
		texto = glabel(text=posible_ruta_OLintroductorio_csv(),
			editable=FALSE),
		boton = gaction(label="Seleccionar ",icon="textcsv",
			handler = function(h,...) 
				gfile (paste("Seleccione el archivo csv creado con los",
					"certificados de aprobación del CI"),
					filter = list(".csv" = list(patterns = c("*.csv"))),
					initialfilename=file.path(getwd(),
						posible_ruta_OLintroductorio_csv()),
					handler=function(h,...) {
						svalue(lista_dataintro$texto) <- h$file
						if (file.access(h$file,4)==0) {
							font(lista_dataintro$texto) <- 
							list(color="darkgreen",size=8)
						} else
							font(lista_dataintro$texto) <- list(color="red",size=8) }  
				)
			)
	)
	toolbar_dataintro <- gtoolbar(lista_dataintro,cont=marco_dataintro)
	size(toolbar_dataintro) <- c(ANCHO_X-15,40)
	size(lista_dataintro$texto) <- c(ANCHO_X-65,32)
	#dar color según exista el archivo csv
	if (file.access(svalue(lista_dataintro$texto),4)==0)
		font(lista_dataintro$texto) <- list(color="darkgreen",size=8) else
		font(lista_dataintro$texto) <- list(color="red", size=8)
	#Marco de selección - tsv con directorio carpeta google drive
	marco_tsvgoogledrive <- 
		gframe(text=paste("Lista de archivos en carpeta pública",
			"de Google Drive (tsv)"),container=wp_marco)
	lista_tsvgoogledrive <- list(
		texto = glabel(text=posible_ruta_tsv_google_drive(),
			editable=FALSE),
		boton = gaction(label="Seleccionar ",icon="textcsv",
			handler = function(h,...) 
				gfile (paste("Seleccione el archivo tsv con el directorio",
					"de archivos en la carpeta pública de Google Drive"),
					filter = list(".tsv" = list(patterns = c("*.tsv"))),
					initialfilename=file.path(getwd(),
						posible_ruta_tsv_google_drive()),
					handler=function(h,...) {
						svalue(lista_tsvgoogledrive$texto) <- h$file
						if (file.access(h$file,4)==0) {
							font(lista_tsvgoogledrive$texto) <- 
								list(color="darkgreen",size=8)
						} else
							font(lista_tsvgoogledrive$texto) <- 
								list(color="red",size=8) }  
					)
				)
		)
	toolbar_tsvgoogledrive <- gtoolbar(lista_tsvgoogledrive,
		cont=marco_tsvgoogledrive)
	size(toolbar_tsvgoogledrive) <- c(ANCHO_X-15,40)
	size(lista_tsvgoogledrive$texto) <- c(ANCHO_X-65,32)
	#dar color según exista el archivo csv
	if (file.access(svalue(lista_tsvgoogledrive$texto),4)==0) {
		font(lista_tsvgoogledrive$texto) <- 
			list(color="darkgreen",size=8)
	} else
		font(lista_tsvgoogledrive$texto) <- list(color="red", size=8)
	#Botón para procesar los listados (llamar a la función procesar)
	marco_boton <- ggroup(horizontal=TRUE, cont=wp_marco)
	addSpring(marco_boton)
	addSpace(marco_boton,value=200,horizontal=TRUE)
	go_button <- gbutton("gtk-execute",border=TRUE,
		handler=function(h,...) {
			archivo_csv <- svalue(lista_dataintro$texto)
			archivo_tsv <- svalue(lista_tsvgoogledrive$texto)
			if (file.access(archivo_csv,4)==0 & 
				file.access(archivo_tsv,4)==0) {
				publicar_certificados(archivo_csv,archivo_tsv)
				dispose(wp)
			} else
				error_msg <- 
					gmessage(paste0("Seleccione los archivos csv",
						" y tsv con la data\nde los certificados de",
						" aprobación primero."),title="Aviso",icon="warning")
		}, cont=marco_boton)
	tooltip(go_button) <- paste("Haga clic aquí para procesar",
		"los archivos\nindicados y generar el código HTML para",
		"publicar\nlos certificados de aprobación.")
	addSpace(marco_boton,value=200,horizontal=TRUE)
	addSpring(marco_boton)
	#---------------------
	svalue(barra_status) <- ""
	Sys.sleep(0.001)
	visible(wp) <- TRUE
} #seleccionar_archivos_para_certificados()

#-----------------------------------------------------------------------

seleccionar_archivos_publicar_OL <- function() {
	#mensaje de espera
	svalue(barra_status) <- "Cargando el menu ..."
	Sys.sleep(0.001)
	#aqui comienza la definición del cuadro de dialogo
	wp <- gbasicdialog(title=paste("Generar HTML para publicar objetivos",
		"logrados"),parent=ventana,do.buttons=FALSE,
		visible=FALSE,horizontal=FALSE)
	ANCHO_X <- 640
	size(wp) <- c(ANCHO_X,450)
	wp_marco <- ggroup(horizontal=FALSE,cont=wp)
	#menu bar superior
	lista_menu <- list(
		Volver = list(
			salir=gaction(label="Volver al menú principal",icon="gtk-quit",
				handler = function(h,...) dispose(wp),
				tooltip = "Salir al menú principal")
			)
	)
	gmenu(lista_menu, cont = wp_marco)
	addSpace(wp_marco,value=5)
	#Titulo
	marco_titulo <- ggroup(horizontal=TRUE,cont=wp_marco)
	addSpring(marco_titulo)
	logo_marco <- gimage("logouna",dirname="stock",size="button",
		cont=marco_titulo)
	titulo_mm_regular <- glabel("HTML para publicar objetivos logrados",
		cont=marco_titulo)
	font(titulo_mm_regular) <- list(color="black", size=12)
	size(titulo_mm_regular) <- c(ANCHO_X-60,20)
	addSpring(marco_titulo)
	gseparator(horizontal=TRUE,cont=wp_marco)		
	addSpace(wp_marco,value=10)
	#Marco de selección del archivo OL_...xls
	marco_xls <- gframe(text="Selección del archivo OL (xls)",
		container=wp_marco)
	lista_xls <- list(
		texto = glabel(text=posible_ruta_OL_xls(),
			editable=FALSE),
		boton = gaction(label="Seleccionar ",icon="excel",
			handler = function(h,...) 
				gfile ("Seleccione el archivo OL (Excel) del asesor",
					filter = list(".xls" = list(patterns = c("*.xls"))),
					initialfilename=file.path(getwd(),"OL.xls"),
					handler=function(h,...) {
						svalue(lista_xls$texto) <- h$file
						if (file.access(h$file,4)==0) {
							font(lista_xls$texto) <- 
							list(color="darkgreen",size=8)
							archivo_dat <- nombre_archivo_datos_googleforms(
								svalue(lista_xls$texto))
							if (file.access(archivo_dat,4)==0) {
								lista_dgf <- lee_archivo_datos_googleforms(archivo_dat)
								svalue(campo_url) <- lista_dgf$url
								svalue(campo_cedula) <- lista_dgf$cedula
								svalue(campo_asignatura) <- lista_dgf$asignatura
							} else {
								svalue(campo_url) <- ""
								svalue(campo_cedula) <- ""
								svalue(campo_asignatura) <- ""
							}
						} else {
							font(lista_xls$texto) <- list(color="red",size=8)
							svalue(campo_url) <- ""
							svalue(campo_cedula) <- ""
							svalue(campo_asignatura) <- ""
						}
				 }  
				)
			)
	)
	toolbar_xls <- gtoolbar(lista_xls,cont=marco_xls)
	size(toolbar_xls) <- c(ANCHO_X-15,40)
	size(lista_xls$texto) <- c(ANCHO_X-65,32)
	#Marco de datos para habilitar registro de consulta de objetivos
	#logrados mediante Google Forms
	marco_registrar_consulta <- 
		gframe(text=paste("Opciones para registro de consulta",
			"de objetivos logrados"),container=wp_marco,horizontal=FALSE)
	addSpace(marco_registrar_consulta,value=5)
	selector_google_forms <- gradio(c(paste("Sin registro de",
		"consulta de objetivos"),paste("Registro de consulta",
		"de objetivos")),handler=function(h,...) {
			enabled(marco_data_googleforms) <- svalue(h$obj)==
			"Registro de consulta de objetivos"
		},horizontal=FALSE,cont=marco_registrar_consulta)
	addSpace(marco_registrar_consulta,value=5)
	#Submarco para datos de archivo OL_asesor.dat
	#Este archivo contiene la data necesaria para registrar
	#la consulta del estudiante mediante Google Forms
	marco_data_googleforms <- ggroup(horizontal=FALSE,
		cont=marco_registrar_consulta)
	linea1_mdgf <- ggroup(horizontal=TRUE,cont=marco_data_googleforms)
	linea2_mdgf <- ggroup(horizontal=TRUE,cont=marco_data_googleforms)
	linea3_mdgf <- ggroup(horizontal=TRUE,cont=marco_data_googleforms)
	addSpring(linea1_mdgf)
	glabel("URL Google Forms",width=120,editable=FALSE,cont=linea1_mdgf)
	addSpace(linea1_mdgf,value=5)
	campo_url <- gtext("",width=475,height=60,cont=linea1_mdgf)
	addSpring(linea1_mdgf)
	addSpring(linea2_mdgf)
	glabel("campo cédula",width=120,editable=FALSE,cont=linea2_mdgf)
	addSpace(linea2_mdgf,value=5)
	campo_cedula <- gtext("",width=165,height=40,cont=linea2_mdgf)
	addSpace(linea2_mdgf,value=40)
	glabel("campo asignatura",width=120,editable=FALSE,cont=linea2_mdgf)
	addSpace(linea2_mdgf,value=5)
	campo_asignatura <- gtext("",width=165,height=40,cont=linea2_mdgf)
	addSpring(linea2_mdgf)
	addSpace(marco_data_googleforms,value=5)
	addSpring(linea3_mdgf)
	boton_guardar_dat <- gbutton("gtk-save",border=TRUE,
		handler=function(...) {
			if ((file.access(svalue(lista_xls$texto),4)==0)) {
				archivo_dat <- nombre_archivo_datos_googleforms(
					svalue(lista_xls$texto))
				lista_dgf <- list(url=svalue(campo_url),cedula=
					svalue(campo_cedula),asignatura=svalue(campo_asignatura))
				escribe_archivo_datos_googleforms(lista_dgf,archivo_dat)
			} else {
				gmessage(paste("No hay ruta asociada para el archivo de data.",
					"\nSeleccione un archivo OL primero."),title="Error",
					icon="error")
			}
		},tooltip = paste("Guardar los datos arriba en",
		"el archivo .dat"), cont=linea3_mdgf)
	addSpring(linea3_mdgf)
	enabled(marco_data_googleforms) <- svalue(selector_google_forms)==
		"Registro de consulta de objetivos"
	#dar color verde o rojo según exista el archivo xls
	if (file.access(svalue(lista_xls$texto),4)==0) {
		font(lista_xls$texto) <- list(color="darkgreen",size=8)
		archivo_dat <- nombre_archivo_datos_googleforms(
			svalue(lista_xls$texto))
		if (file.access(archivo_dat,4)==0) {
			lista_dgf <- lee_archivo_datos_googleforms(archivo_dat)
			svalue(campo_url) <- lista_dgf$url
			svalue(campo_cedula) <- lista_dgf$cedula
			svalue(campo_asignatura) <- lista_dgf$asignatura
		} else {
			svalue(campo_url) <- ""
			svalue(campo_cedula) <- ""
			svalue(campo_asignatura) <- ""
		}
	} else {
		font(lista_xls$texto) <- list(color="red", size=8)
		svalue(campo_url) <- ""
		svalue(campo_cedula) <- ""
		svalue(campo_asignatura) <- ""
	}
	#Botón para generar el html
	marco_boton <- ggroup(horizontal=TRUE, cont=wp_marco)
	addSpring(marco_boton)
	addSpace(marco_boton,value=200,horizontal=TRUE)
	go_button <- gbutton("gtk-execute",border=TRUE,
		handler=function(h,...) {
			archivo_xls <- svalue(lista_xls$texto)
			archivo_dat <- nombre_archivo_datos_googleforms(archivo_xls)
			con_googleforms <- svalue(selector_google_forms)==
				"Registro de consulta de objetivos"
			if (file.access(archivo_xls,4)==0) {
				if (con_googleforms & file.access(archivo_dat,4)!=0) {
					gmessage(paste("No existe el archivo .dat para",
						"Google Forms. Debe crear\nel formulario primero y",
						"luego indicar los datos solicitados arriba."),
						title="Error",icon="error")
					return(1)
				}
				recorrer_libro_evaluacion_para_csv(archivo_xls)
				crear_html_objetivos_logrados(archivo_xls,con_googleforms)
				dispose(wp)
			} else
				gmessage(paste("Seleccione el archivo Excel con los",
					"objetivos logrados del asesor."),title="Aviso",
					icon="warning")
		}, cont=marco_boton)
	tooltip(go_button) <- paste("Haga clic aquí para procesar",
		"los archivos\nindicados y generar la data de la nomina",
		"de\ninscritos regulares.")
	addSpace(marco_boton,value=200,horizontal=TRUE)
	addSpring(marco_boton)
	#---------------------
	svalue(barra_status) <- ""
	Sys.sleep(0.001)
	visible(wp) <- TRUE
} #seleccionar_archivos_publicar_OL()
