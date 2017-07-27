#!usr/bin/env Rscript
#HEVASU - Herramienta de apoyo a la EValuación y ASesoría de la Una.
#MAIN R script para la aplicación
#Fecha : 05/05/2016
#Autor : José L. Romero
#-----------------------------------------------------------------------
#Carga los paquetes de R necesarios
require(gWidgets)
options(guiToolkit="RGtk2")
require(RGtk2)
require(XLConnect)
options(stringsAsFactors = FALSE)

#---------------- definición de funciones de utilería ------------------

homedir <- function() {
	#determina la ruta de los programas y del directorio raiz
	temp <- system("dirname ~/.",intern=TRUE)
	return(temp)
}# homedir

dir_programa <- {
	file.path(homedir(),"HEVASU")
}# dir_programa  (VARIABLE GLOBAL)

#función para invertir Apellido, Nombre por Nombre Apellido
invertir_nombre_apellido <- function(x) {
	return(gsub("(.+),( *)(.+)","\\3 \\1",x) )
}# invertir_nombre_apellido
	
#otra función útil - convertir fecha
convertir_fecha <- function(cadena) {
	cadena <- gsub("\\-","/",cadena)
	result <- regexpr("[0-9]+$",cadena)
	st <- as.numeric(result)
	long <- attr(result,"match.length")
	if (st<1) return(NA)
	ano <- as.numeric(substring(cadena,st,st+long-1))
	if (ano<1000) ano <- ano+2000
	ano <- as.character(ano)
	fecha_correcta <- paste0(substring(cadena,1,st-1),ano)
	return(as.Date(fecha_correcta,"%d/%m/%Y"))
}# convertir_fecha

limpia_correos <- function(email) {
	#función para acomodar un poco los correos
	email <- tolower(email)
	email <- sapply(email,function(i)
		ifelse(length(grep("^[-a-z0-9._]+@[a-z0-9]+[a-z.]*\\.[a-z]+$",
			i))==0,NA,i) )
	return(as.character(email))
} #limpia_correos

loffice_abierto <- function() {
	#función que retorna TRUE si el LibreOffice está abierto
	temp <- system("ps cx -o cmd| grep -v grep | grep soffice\\.bin ",
		intern=TRUE)
	return(length(temp)>0)
}# loffice_abierto

#------------ fin de definición de funciones de utilería ---------------

#define el colores de los rotulo (ver colors() )
#estilo de texto explicativo
estilo_rotulo1 <- list(color="slategrey",weight="light")
#estilo de etiquetas de toolbars
estilo_rotulo2 <- list(color="slategray4", weight="light")
#estilo de las etiquetas de campos
estilo_rotulo3	<- list(color="steelblue3", weight="bold")
#agrega varios iconos
addStockIcons("logouna",file.path(dir_programa,"logo.png"))
addStockIcons("excel", file.path(dir_programa,"excel.png"))
addStockIcons("word", file.path(dir_programa,"word.png"))
addStockIcons("LOwriter", file.path(dir_programa,"LOwriter.png"))
addStockIcons("pdf", file.path(dir_programa,"application-pdf.png"))
addStockIcons("textcsv",file.path(dir_programa,"text-csv.png"))
addStockIcons("texthtml",file.path(dir_programa,"text-html.png"))
addStockIcons("autores",file.path(dir_programa,"text-x-authors.png"))
addStockIcons("logogpl",file.path(dir_programa,"gplv3.png"))
#incorpora todos los sub-programas
source(file.path(dir_programa,"R","editar_CL_lapsos.R"))
source(file.path(dir_programa,"R","editar_materias.R"))
source(file.path(dir_programa,"R","editar_asesores.R"))
source(file.path(dir_programa,"R","procesar_listados_UNASEC.R"))
source(file.path(dir_programa,"R","seleccionar_listados.R"))
source(file.path(dir_programa,"R","generar_libros_Excel.R"))
source(file.path(dir_programa,"R","leer_libros_Excel.R"))
source(file.path(dir_programa,"R","certificados_introductorio.R"))
source(file.path(dir_programa,"R","reporte_actividades.R"))
source(file.path(dir_programa,"R","plan_operativo.R"))
source(file.path(dir_programa,"R","fichas.R"))
source(file.path(dir_programa,"R","objetivos_logrados.R"))
source(file.path(dir_programa,"R","ayuda.R"))
#crear la ventana
ventana <- gbasicdialog("HEVASU",visible=FALSE, do.buttons = FALSE)
marco <- ggroup(horizontal=FALSE, cont=ventana)
encabezado <- ggroup(horizontal=TRUE, cont=marco)
logo_una <- gimage(file.path(dir_programa,"logo.png"),cont=encabezado)
titulo_una <- glabel(texto_titulo_UNA(), cont=encabezado)
barra_sep <- gseparator(horizontal = TRUE, container = marco)
#El menú
menubar_list <- list(
	Archivo=list(
		Carpeta=gaction("Seleccionar carpeta de trabajo",
			icon="directory", handler=function(...)
			gfile("Seleccione la carpeta de trabajo",
				type="selectdir",handler=function(h,...) {
					setwd(h$file)
					svalue(dir_trabajo) <- h$file
				} )
		),
		linea_sep1 = gseparator(),
		Procesar_reg= gaction("Procesar listado de estudiantes regulares",
			icon="convert", handler=function(...)
			if (centrolocal=="NIVEL CENTRAL") {
				procesar_listados_regulares_NC()
			} else
				seleccionar_listados_regulares()
		),
		Procesar_int= gaction(
			"Procesar listado de estudiantes curso introductorio",
			icon="convert", handler=function(...)
				seleccionar_listados_introductorio()
		),
		Procesar_nuevos = gaction(
			"Generar listado csv de nuevos ingresos (regulares)",
			icon="convert", handler=function(...)
				seleccionar_listados_nuevos()
		),
		Procesar_reingresos = gaction(
			"Generar listado csv de reingresos (regulares)",
			icon="convert", handler=function(...)
				seleccionar_listados_reingresos()
		),
		Procesar_reing_egre = gaction(
			"Generar listado csv de reingresos de egresados (regulares)",
			icon="convert", handler=function(...)
				seleccionar_listados_reing_egre()
		),
		Generar_calendario = gaction(
			"Generar csv con calendario de pruebas por asignatura",
			icon="convert", handler=function(...)
				seleccionar_cronograma()
		),
		linea_sep2 = gseparator(),
		Reparar_reg = gaction(
			"Reparar datos de estudiantes regulares (desde csv)",
			icon="gtk-revert-to-saved", handler=function(...) {
				if (file.access(ruta_nomina_csv,4)==0) {
					nomina <- read.table(ruta_nomina_csv,header=TRUE,sep="\t")
					nomina$fecha_nacimiento <- as.Date(nomina$fecha_nacimiento)
					save("nomina",file=ruta_nomina_RData)
				} else
					error_msg <- 
						gmessage(paste0("No existe el archivo '",
							ruta_nomina_csv,"'"), title="Aviso",icon="warning")
			}
		),
		Reparar_int = gaction(
			"Reparar datos de estudiantes de introductorio (desde csv)",
			icon="gtk-revert-to-saved", handler=function(...) {
				if (file.access(ruta_nomina_intro_csv,4)==0) {
					nomina_intro <- read.table(ruta_nomina_intro_csv,header=TRUE,
						sep="\t")
					nomina_intro$fecha_nacimiento <- 
						as.Date(nomina_intro$fecha_nacimiento)
					save("nomina_intro",file=ruta_nomina_intro_RData)
				} else
					error_msg <- 
						gmessage(
							paste0("No existe el archivo '",ruta_nomina_intro_csv,
							"'"), title="Aviso", icon="warning")
			}
		),
		linea_sep3 = gseparator(),
		Salir=gaction("Salir", icon="quit", handler=function(...)
			dispose(ventana))
	),
	Editar = list(
		Asignaturas = gaction("Editar datos de asignaturas",
			icon="gtk-edit", handler=function(h,...) {
					ventana_edicion_materias()
				}
			),
		Asesores = gaction("Editar datos de asesores",icon="gtk-edit",
			handler=function(h,...) {
				if (centrolocal=="NIVEL CENTRAL") {
					ventana_edicion_asesores_NC()
				} else
					ventana_edicion_asesores()
				actualizar_indices_asesores()
				seleccion_asesores <<- bd_asesores
				selector_asesores[] <<- names(indice_asesores)
				svalue(selector_asesores) <<- names(indice_asesores)
				update(selector_asesores)
			}),
		Lapso_CL = gaction("Editar/seleccionar lapsos y CL",icon="gtk-edit",
			handler=function(h,...) {
				ventana_mod_CL_lapsos()
			})
	),
	"Generar xls" = list(
		"OL"=gaction("Control de objetivos logrados",
			icon="excel",
			handler=function(...) {
				error <- generar_libros_xls(svalue(selector_lapso))
				if (error==1)
					error_msg <- gmessage(paste("No hay datos de la nomina",
						"estudiantil. Seleccione el directorio de trabajo",
						"apropiado y seleccione 'Procesar listado",
						"estudiantes regulares/curso introductorio' en el submenu",
						"'Archivos' en caso de no haberlo hecho aún."),
						title="Error",icon="error")
				if (error==2)
					error_msg <- gmessage(paste("No se pudo encontrar",
						"la plantilla de Excel para generar el libro de",
						"control de objetivos logrados.",
						"Comuníquese con el administrador."),
						title="Error",icon="error")
				if (error==3)
					error_msg <- gmessage(paste("Debe seleccionar por lo",
						"menos un asesor en el cuadro inferior."),
						title="Error",icon="error")
				if (error==4)
					error_msg <- gmessage(paste("El personal académico",
						"seleccionado es un(a) preparador(a)."),
						title="Error",icon="error")
			}),
		"Asesoria"=gaction("Control de asesorias",
			icon="excel",
			handler=function(...) {
				error <- generar_asesorias(svalue(selector_lapso))
				if (error==1)
					error_msg <- gmessage(paste("No hay datos de la nomina",
						"estudiantil. Seleccione el directorio de trabajo",
						"apropiado y seleccione 'Procesar listado",
						"estudiantes regulares/curso introductorio' en el submenu",
						"'Archivos' en caso de no haberlo hecho aún."),
						title="Error",icon="error")
				if (error==2)
					error_msg <- gmessage(paste("No se pudo encontrar la",
						"plantilla de Excel para generar el librería de asesoría.",
						"Comuníquese con el administrador."),
						title="Error",icon="error")
			})
	),
	"Reportes y Certificados"=list(
		"certificados"=gaction("Generar certificados de aprobación CI",
			icon="pdf",handler=function(...) {
				archivo_Excel <- gfile (paste("Seleccione el libro xls",
					"del orientador"),filter = list(".xls" = list(
					patterns = c("*.xls"))), initialfilename=getwd(),
					handler=function(h,...) return(h$file)
				)
				if (!is.na(archivo_Excel)) {
					leer_curso_introductorio(archivo_Excel)
					generar_certificados(archivo_Excel)
				}
			}
		),
		"fichas académicas"=gaction("Generar fichas académicas",
			icon="pdf",handler=function(...) {
				seleccionar_datos_fichas()}
		),
		"reporte actividades"=gaction("Generar reporte de actividades",
			icon="word",handler=function(...) {
				if (length(seleccion_asesores)>0) {
					generar_reportes_actividad()
				} else 
					gmessage("No hay asesores seleccionados.",icon="warning")
			}
		),
		"reporte planificacion"=gaction("Generar reporte de planificación",
			icon="pdf",handler=function(...) {
				if (length(seleccion_asesores)>0) {
					generar_reporte_planificacion()
				} else 
					gmessage("No hay asesores seleccionados.",icon="warning")
			}
		)
	),
	"Publicar en la web"=list(
		"publicar objetivos logrados"=gaction(paste("Publicar objetivos",
			"logrados"),icon="texthtml",handler=function(...) {
				seleccionar_archivos_publicar_OL()
			}
		),
#		"reportes objetivos logrados"=gaction(paste("Generar reportes",
#			"csv para consulta de objetivos logrados."),
#			icon="textcsv",handler=function(...) {
#				error <- recorrer_libros_evaluacion_para_csv()
#				if (error==3)
#				error_msg <- gmessage(paste("Debe seleccionar por lo",
#					"menos un asesor en el cuadro inferior."),
#					title="Error",icon="error") }
#		),
		"publicar certificados"=gaction(paste("Generar HTML para",
			"publicación de certificados del CI"),icon="texthtml",
			handler=function(...) {
				seleccionar_archivos_para_certificados() }
		),
		"publicar fichas"=gaction(paste("Generar HTML para publicación de",
			"fichas"),icon="texthtml",handler=function(...) {
				seleccionar_tsv_fichas() }
		)
	),
	"Ayuda"=list(
		"manual de usuario"=gaction("Manual de usuario HEVASU",
			icon="pdf",handler=function(...) {
				system(paste("gnome-open",file.path(dir_programa,
					"documentacion","manual.pdf")))
			}),
		"acerca de"=gaction("Acerca de ...",
			icon="autores",handler=function(...) {
				acerca_de()
			})
	)
)
gmenu(menubar_list, cont = marco)
redibujar_menu_principal()
addSpace(marco,value=20,horizontal=FALSE)
#el directorio de trabajo
dir_trabajo_marco <- gframe(text="Carpeta de trabajo",
	container=marco)
addSpace(dir_trabajo_marco,value=5,horizontal=TRUE)
dir_trabajo <- glabel(text=getwd(), editable=FALSE,
	cont=dir_trabajo_marco)
font(dir_trabajo) <- list(size=9)
addSpace(dir_trabajo_marco,value=5,horizontal=TRUE)
addSpace(marco,value=20,horizontal=FALSE)
#parametros adicionales: el lapso, la unidad de apoyo y
#la lista de asesores
parametros <- ggroup(horizontal=TRUE, cont=marco)
addSpring(parametros)
#el lapso
lapso_marco <- gframe(text="Lapso Academico", container=parametros)
addSpring(lapso_marco)
#el selector de lapso se ubica en el último lapso (renglón) en el
#archivo de lapsos
selector_lapso <- gcombobox(items=lapsos,editable=FALSE,selected=
	length(lapsos),container=lapso_marco)
addSpring(lapso_marco)
addSpring(parametros)
#la unidad de apoyo
oficina_marco <- gframe(text="Unidad/Oficina",container=parametros)
addSpring(oficina_marco)
selector_oficina <- gcombobox(items=cloas,editable=FALSE,
selected=1,container=oficina_marco)
addSpring(oficina_marco)
addSpring(parametros)
#la lista de asesores
seleccion_asesores <- bd_asesores
asesores_marco <- gframe(text="Seleccione el/los asesores",
	container=parametros)
selector_asesores <- gcheckboxgroup(names(indice_asesores),
	checked = TRUE, horizontal = FALSE, use.table=TRUE,
	container=asesores_marco)
size(selector_asesores) <- c(400,200)
addHandlerChanged(selector_asesores,handler=function(h,...) {
	seleccion_asesores <<- sub_lista_asesores(svalue(selector_asesores))
} )
addSpring(parametros)
addSpace(marco,value=20,horizontal=FALSE)
#agregar barra de estatus
barra_status <- gstatusbar("",ventana)
font(barra_status) <- list(color="darkgreen",size=11)
size(ventana) <- c(800,550)
visible(ventana) <- TRUE
