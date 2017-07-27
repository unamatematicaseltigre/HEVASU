#Datos sobre el centro local y el lapso + 
#ventana de diálogo para su selección/edición
#Fecha : 03/07/2014
#Autor : José L. Romero
#requiere los siguientes paquetes: gWidgets, RGtk2
#-----------------------------------------------------------------------

#lee el archivo de lapsos academicos
archivo <- file(file.path(dir_programa,"data","lapsos"),"r",
	blocking = FALSE)
lapsos <- readLines(archivo)
close(archivo)

leer_centros <- function() {
	#función para cargar la data sobre el centro local seleccionado
	#desde el archivo respectivo.
	#Lee la información sobre el centro local y sus oficinas de apoyo
	archivo <- file(file.path(dir_programa,"data","centros_UNA"),"r",
		blocking = FALSE)
	#obtiene el nombre del centro local (VARIABLE GLOBAL)
	centrolocal <<- readLines(archivo,n=1)
	close(archivo)
	centros <<- read.table(file.path(dir_programa,"data","centros_UNA"),
	header=TRUE,skip=1,row.names=1,sep="\t")
	#obtiene los códigos de los cl-oa (VARIABLES GLOBALES)
	locs <<- as.character(centros[centrolocal,])
	cloas <<- strsplit(locs,"|",fixed=TRUE)[[1]]
	codigoCL <<- substr(locs,1,2)
}# leer_centros

texto_titulo_UNA <- function() {
	#función para crear la etiqueta en la parte izq superior del
	#menú principal
	if (centrolocal=="NIVEL CENTRAL") {
		texto <- paste0("UNIVERSIDAD NACIONAL ABIERTA\n",
			"UNIDAD ACADEMICA\n",centrolocal)
	} else
		texto <- paste0("UNIVERSIDAD NACIONAL ABIERTA\n",
			"UNIDAD ACADEMICA\nCL ",centrolocal)
	return(texto)
}# texto_titulo_UNA

redibujar_menu_principal <- function() {
	#esta función se utiliza para deshabilitar ciertos elementos
	#en el toolbar del menú principal, según sea el CL seleccionado
	#el NIVEL CENTRAL o no.
	if (centrolocal=="NIVEL CENTRAL") {
		enabled(menubar_list$Archivo$Procesar_int) <- F
		enabled(menubar_list$Archivo$Procesar_nuevos) <- F
		enabled(menubar_list$Archivo$Procesar_reingresos) <- F
		enabled(menubar_list$Archivo$Procesar_reing_egre) <- F
		enabled(menubar_list$Archivo$Reparar_int) <- F
		enabled(menubar_list$"Reportes y Certificados"$certificados) <- F
		svalue(menubar_list$Archivo$Procesar_reg) <- 
			"Procesar listados de estudiantes regulares"
	} else {
		enabled(menubar_list$Archivo$Procesar_int) <- T
		enabled(menubar_list$Archivo$Procesar_nuevos) <- T
		enabled(menubar_list$Archivo$Procesar_reingresos) <- T
		enabled(menubar_list$Archivo$Procesar_reing_egre) <- T
		enabled(menubar_list$Archivo$Reparar_int) <- T
		enabled(menubar_list$"Reportes y Certificados"$certificados) <- T
		svalue(menubar_list$Archivo$Procesar_reg) <- 
			"Procesar listado de estudiantes regulares"
	}#if
}#redibujar_menu_principal

leer_centros() #desde una vez carga la data del CL en memoria
#genera una tabla con todos los centros locales
tablaCL <- rownames(centros)[-1]
tablaCL <- paste(substr(centros[-1,1],1,2),"-",tablaCL)
names(tablaCL) <- substr(centros[-1,1],1,2)

ventana_mod_CL_lapsos <- function() {

	#dialogo de salida
	dialogo_salida <- function() {
		dialogo_salida.env <- environment()
		sub_wp <- gbasicdialog(title="Salir de edición de lapsos/sel. CL",
			parent=wp, do.buttons=FALSE,visible=FALSE,horizontal=TRUE)
		sub_wp_frame <- ggroup(horizontal=FALSE,container=sub_wp)
		addSpace(sub_wp_frame,value=10,horizontal=FALSE)
		linea_mensaje <- ggroup(horizontal=TRUE,container=sub_wp_frame)
		addSpring(linea_mensaje)
		gimage("gtk-dialog-question",dirname="stock",size="dialog",
			container=linea_mensaje)
		glabel(paste("Usted está a punto de salir de la ventana de edición",
			"de lapsos/selección de CL. ¿Desea guardar los cambios",
			"antes de salir?",sep="\n"),container=linea_mensaje)
		addSpring(linea_mensaje)
		addSpace(sub_wp_frame,value=10,horizontal=FALSE)
		gseparator(horizontal=TRUE,cont=sub_wp_frame)
		linea_botones <- ggroup(horizontal=TRUE,container=sub_wp_frame)
		addSpace(linea_botones,value=10,horizontal=TRUE)
		boton1 <- gbutton("Guardar cambios y salir",container=linea_botones, 
			handler=function(h,...) {
				assign("cond",1,envir=dialogo_salida.env)
				dispose(sub_wp)
			}
		)
		boton2 <- gbutton("Salir sin guardar",container=linea_botones,
			handler=function(h,...) {
				assign("cond",2,envir=dialogo_salida.env)
				dispose(sub_wp)
			}
		)
		boton3 <- gbutton("Cancelar (no salir)",container=linea_botones,
			handler=function(h,...) {
				assign("cond",3,envir=dialogo_salida.env)
				dispose(sub_wp)
			}
		)
		addSpace(linea_botones,value=10,horizontal=TRUE)
		visible(sub_wp) <- TRUE
		return(cond)
	}

	guardar <- function() {
		#función que guarda los cambios en lapsos y en centros locales.
		#actualiza el selector de lapsos
		lapsos <<- as.character(lista_lapsos[])
		selector_lapso[] <- lapsos
		svalue(selector_lapso) <- tail(lapsos,1)
		update(selector_lapso)
		#actualiza la VARIABLES GLOBAL centrolocal
		centrolocal <<- svalue(selector_centro)
		#si hay cambios, entonces actualiza otras cosas y da un aviso.
		if (centrolocal_viejo!=centrolocal|
			lapso_viejo!=svalue(selector_lapso)) {
			#avisa al usuario
			gmessage(paste("Recuerde que debe indicar otro directorio de\n",
				"trabajo y leer/procesar nuevamente las nómina de\n",
				"estudiantes en caso necesario."),title="Aviso",
				icon = "warning" )
			#borra lo objetos "nomina" y "nomina_intro" si los hay
			if ("nomina_intro" %in% ls()) rm("nomina_intro")
			if ("nomina" %in% ls()) rm("nomina")
			#reescribe los archivos "data/lapsos" y "data/centros_UNA"
			archivo <- file(file.path(dir_programa,"data","lapsos"),"w",
				blocking = FALSE)
			writeLines(lapsos,con=archivo)
			close(archivo)
			#cambia la 1era línea de centros_UNA, que es el CL seleccionado 
			archivo <- file(file.path(dir_programa,"data","centros_UNA"),"r",
				blocking = FALSE)
			lineas <- readLines(archivo)
			lineas[1] <- centrolocal
			close(archivo)
			archivo <- file(file.path(dir_programa,"data","centros_UNA"),"w",
				blocking = FALSE)		
			writeLines(lineas,con=archivo)
			close(archivo)
			#vuelve a leer el archivo para actualizar la data del CL
			leer_centros()
			#al cambiar el centrolocal, cambia tambien la base de datos de los
			#asesores y la ruta de ese archivo:
			leer_asesores() #(en "editar_asesores.R")
			actualizar_indices_asesores()
			#así como tambien las rutas de los archivos de nómina...
			establecer_rutas_nomina()
			#actualiza otros widgets: el texto en "titulo_una",
			#el "selector_oficina" y el combobox de "selector_asesores"
			svalue(titulo_una) <- texto_titulo_UNA()
			update(titulo_una)
			selector_oficina[] <- cloas
			svalue(selector_oficina) <- cloas[1]
			update(selector_oficina)
			seleccion_asesores <<- bd_asesores
			selector_asesores[] <<- names(indice_asesores)
			svalue(selector_asesores) <<- names(indice_asesores)
			update(selector_asesores)
			#habilita/dehabilita ciertos elementos del menú ppal
			redibujar_menu_principal()
		}# if
	}# guardar

	#mensaje de espera
	svalue(barra_status) <- "Cargando el menu ..."
	Sys.sleep(0.001)
	#guardar los valores viejos de "lapso" y "centrolocal"
	centrolocal_viejo <- centrolocal
	lapso_viejo <- svalue(selector_lapso)
	#crear la ventana
	wp <- gbasicdialog(
		title="Selección de Centro Local / Edición de Lapso",
		parent=ventana,do.buttons=FALSE,visible=FALSE,horizontal=FALSE)
	size(wp) <- c(460,540)
	wp_marco <- ggroup(horizontal=FALSE,cont=wp)
	#menu bar superior
	lista_menu <- list(
		Volver = list(
			salir=gaction(label="Volver al menú principal",icon="gtk-quit",
				handler = function(h,...) {
					condicion <- dialogo_salida()
					switch(condicion,
						{	#guardar en lapsos, centros locales y salir
							guardar()
							dispose(wp) },
						{ #salir de edición materias sin guardar
							dispose(wp) },
						{ #no salio de edición de materias
							} )
				},tooltip = "Salir al menú principal")
			)
	)
	gmenu(lista_menu, cont = wp_marco)
	marco_titulo <- ggroup(horizontal=TRUE,cont=wp_marco)
	addSpring(marco_titulo)
	logo_marco <- gimage("logouna",dirname="stock",size="button",
		cont=marco_titulo)
	titulo_marco <- glabel(
		"Modificación de lapsos / Selección de Centro Local",cont=marco_titulo)
	font(titulo_marco) <- list(color="black", size=12)
	size(titulo_marco) <- c(420,20)
	addSpring(marco_titulo)
	gseparator(horizontal=TRUE,cont=wp_marco)
	addSpace(wp_marco,value=5)
	wp_submarco <- ggroup(horizontal=TRUE,cont=wp_marco)
	#Marco de modificación de lapso
	mod_lapso_marco <- gframe(text="Modificación de lapsos\nacadémicos",
		horizontal=FALSE,container=wp_submarco)
	size(mod_lapso_marco) <- c(180,455)
	lista_lapsos <- gdf(lapsos,container=mod_lapso_marco)
	size(lista_lapsos) <- c(160,360)
	gtk_lista_lapsos <- getToolkitWidget(lista_lapsos)
	gtkTreeViewSetGridLines(gtk_lista_lapsos,grid.lines=3)
	col0 <- gtkTreeViewGetColumn(gtk_lista_lapsos, 0)
	col1 <- gtkTreeViewGetColumn(gtk_lista_lapsos, 1)
	gtkTreeViewColumnSetClickable(col1, FALSE)
	rend1 <- gtkCellLayoutGetCells(col1)
	rend1[[1]]$set(xalign = 0.5)
	gtkTreeViewRemoveColumn(gtk_lista_lapsos, col0)
	col1_header <- gtkLabelNew("Lapsos académicos",show = TRUE)
	gtkLabelSetWidthChars(col1_header, 18)
	gtkLabelSetLineWrap(col1_header, TRUE)
	gtkTreeViewColumnSetWidget(col1, widget = col1_header)
	toolbar_inferior_list <- list(
		Ins_lapso = gaction("Agregar lapso",icon="add",
			handler=function(h,...) {
				campo <- svalue(toolbar_inferior_list$Lapso)
				if (is.na(campo)) {
					gmessage("Indique un lapso válido a agregar (ej. 2014-1).",
						title="Aviso", icon = "warning" )
					return(1) }
				if (is.na(as.numeric(substr(campo,1,4)))|
					substr(campo,5,5)!="-") {
					gmessage("Indique un lapso válido a agregar (ej. 2014-1).",
						title="Aviso", icon = "warning" )
					return(2) }
				if (campo %in% lista_lapsos[]) {
					gmessage(paste0("Ya existe el lapso '",campo,"'."),
						title="Aviso", icon = "warning" )
					return(3) }				
				n <- length(lista_lapsos[])
				lista_lapsos[] <- c(lista_lapsos[],campo)
				lista_lapsos[] <- sort(lista_lapsos[])
				update(lista_lapsos)
			},tooltip="Agregar el lapso indicado"),
		Lapso = gedit("",width=6),
		Elim_lapso = gaction("Eliminar último lapso", icon="remove",
			handler=function(h,...) {
				n <- length(lista_lapsos[])
				if (n>0) {
					lista_lapsos[] <- lista_lapsos[-n]
					update(lista_lapsos)
				}
			},tooltip="Eliminar el último lapso en la lista")
		)
	toolbar_inferior <- gtoolbar(toolbar_inferior_list,
		container=mod_lapso_marco)
	size(toolbar_inferior) <- c(160,40)
	addSpring(wp_submarco)
	#marco de selección Centro Local
	sel_CL_marco <- gframe(text="Selección de Centro Local",
		container=wp_submarco)
	size(sel_CL_marco) <- c(260,455)
	selector_centro <- gradio(items=rownames(centros),selected=
		which(rownames(centros)==centrolocal)[1],horizontal=FALSE,
		tooltip="Seleccione un Centro Local o el Nivel Central",
		container=sel_CL_marco)
	#------------------
	svalue(barra_status) <- ""
	Sys.sleep(0.001)
	visible(wp) <- TRUE
} #ventana_mod_CL_lapsos

