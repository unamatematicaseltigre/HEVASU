#Funciones para el manual de usuario, creditos y la licencia
#Fecha : 27/03/2017
#Autor : José L. Romero
#requiere los siguientes paquetes: rJava, gWidgets, RGtk2
#usa también LaTeX/texlive

acerca_de <- function() {
	#mensaje de espera
	svalue(barra_status) <- "Cargando la ventana ..."
	Sys.sleep(0.001)
	#aqui comienza la definición del cuadro de dialogo
	wp <- gbasicdialog(title="Acerca de",parent=ventana,
		do.buttons=FALSE,visible=FALSE,horizontal=FALSE)
	ANCHO_X <- 515
	size(wp) <- c(ANCHO_X,400)
	wp_marco <- ggroup(horizontal=FALSE,height=400,cont=wp)
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
	#crear el cuaderno
	cuaderno <- gnotebook(cont=wp_marco,expand=TRUE)
	#página Información
	informacion <- ggroup(horizontal=FALSE,cont=cuaderno,
		label="Información")
	addSpace(informacion,value=10)
	linea1 <- ggroup(horizontal=TRUE,cont=informacion)
	addSpring(linea1)
	titulo_linea1 <- glabel("HEVASU 2.1",cont=linea1)
	font(titulo_linea1) <- list(color="black", size=14,weight="bold")
	addSpring(linea1)
	addSpace(informacion,value=5)
	linea2 <- ggroup(horizontal=TRUE,cont=informacion)
	addSpring(linea2)
	titulo_linea2 <- glabel(paste("Herramienta de apoyo a la",
		"EValuación y ASesoría academica en la Una"),cont=linea2)
	font(titulo_linea2) <- list(color="black", size=9,weight="bold")
	addSpring(linea2)
	addSpace(informacion,value=5)
	linea3 <- ggroup(horizontal=TRUE,cont=informacion)
	addSpring(linea3)
	glabel(paste("Última modificación del código fuente:",
		"30/03/2017"),cont=linea3)
	addSpring(linea3)
	addSpace(informacion,value=10)
	linea4 <- ggroup(horizontal=TRUE,cont=informacion)
	addSpring(linea4)
	glabel("(c) 2014-2017",cont=linea4)
	addSpring(linea4)
	linea5 <- ggroup(horizontal=TRUE,cont=informacion)
	addSpring(linea5)
	glabel("Todos los derechos reservados",cont=linea5)
	addSpring(linea5)
	addSpace(informacion,value=10)
	linea6 <- ggroup(horizontal=TRUE,cont=informacion)
	addSpring(linea6)
	titulo_linea6 <- glabel("Diseño y programación en R:",
		cont=linea6)
	font(titulo_linea6) <- list(color="black", size=9,weight="bold")
	addSpring(linea6)
	addSpace(informacion,value=5)
	linea7 <- ggroup(horizontal=TRUE,cont=informacion)
	addSpring(linea7)
	glabel("José L. Romero P.",cont=linea7)
	addSpring(linea7)
	linea8 <- ggroup(horizontal=TRUE,cont=informacion)
	addSpring(linea8)
	glabel("jromero@una.edu.ve",cont=linea8)
	addSpring(linea8)
	#página Licencia
	licencia <- ggroup(horizontal=FALSE,cont=cuaderno,
		label="Licencia")
	addSpace(licencia,value=5)
	logo_licencia <- gimage(file.path(dir_programa,"gplv3.png"),
		cont=licencia)
	addSpace(licencia,value=5)
	archivo_licencia <- file(file.path(dir_programa,"LICENSE"),open="")
	gtext(paste(readLines(archivo_licencia),collapse="\n"),
		cont=licencia,expand=TRUE)
	close(archivo_licencia)
	svalue(cuaderno) <- 1	
	#--------------------------
	svalue(barra_status) <- ""
	Sys.sleep(0.001)
	visible(wp) <- TRUE
} #acerca_de()
