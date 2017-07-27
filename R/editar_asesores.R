#Edición de la base de datos de asesores
#Fecha : 20/10/2013
#Autor : José L. Romero
#requiere los siguientes paquetes: gWidgets, RGtk2
#-----------------------------------------------------------------------
#"bd_asesores" es una lista cargada desde el directorio de data cuyos
#elementos son los asesores, cada uno con la siguiente data (en orden
#consecutivo y colocándome como ejemplo):
#  ..$ asesor  : chr "Romero, José L."
#  ..$ archivo : chr "joseromero"
#  ..$ adscrito: chr "02 - 01"
#  ..$ materias: logi [1:12, 1:3] FALSE FALSE FALSE FALSE ...
#  .. ..- attr(*, "dimnames")=List of 2
#  .. .. ..$ : chr [1:12] "175" "176" "177" "178" ...
#  .. .. ..$ : chr [1:3] "02 - 00" "02 - 01" "02 - 03"
#  ..$ area    : chr "Matemáticas" ...
#  ..$ tipo    : chr "Asesor"
#  ..$ sexo    : chr "M"

#-----------------------------------------------------------------------

#La base de datos de los asesores
#Si se selecciona otro CL, debe modificarse ruta_bd_asesores
leer_asesores <- function() {
	ruta_bd_asesores <<- file.path(dir_programa,"data",
		paste0("bd_asesores",codigoCL,".RData") )
	if (file.access(ruta_bd_asesores)==0) {
		load(ruta_bd_asesores,envir=.GlobalEnv)
	} else {
		bd_asesores <<- list()
		save(bd_asesores,file=ruta_bd_asesores)
	}
}# leer_asesores

leer_asesores()

#-----------------------------------------------------------------------
#inicializa (o re-inicializa) los indices de los asesores
#esto se hara: 1) al principio y 2) cuando se agrega/quita un asesor
actualizar_indices_asesores <- function() {
	if (length(bd_asesores)>0) {
		indtmp <<- 1:length(bd_asesores)
		names(indtmp) <- 
			sapply(indtmp, function(i) bd_asesores[[c(i,1)]] )
		indice_asesores <<- order(names(indtmp))
		names(indice_asesores) <<- sort(names(indtmp)) } else
	{
		indice_asesores <<- numeric(0)
	}
}

actualizar_indices_asesores()

#función que retorna una (sub) lista de asesores a partir de un
#vector de nombres
sub_lista_asesores <- function(quienes) {
	lapply(indice_asesores[quienes],function(i) bd_asesores[[i]])
}

#función para normalizar los nombres de archivos
normalizar <- function(x) {
	x <- invertir_nombre_apellido(x)
	x <- tolower(x)
	x <- gsub("á","a",x)
	x <- gsub("é","e",x)
	x <- gsub("í","i",x)
	x <- gsub("ó","o",x)
	x <- gsub("ú","u",x)
	x <- gsub("ñ","n",x)
	x <- gsub(",|_|\\.|\\s","",x)
	return(x)
}
		
#-----------------------------------------------------------------------
ventana_edicion_asesores <- function() {
	#mensaje de espera
	svalue(barra_status) <- "Cargando el menu ..."
	Sys.sleep(0.001)
	#crea un nuevo environment para referencia
	this.env <- environment()
	#tmp_asesores es una copia (de trabajo) de la base de datos de
	#los asesores
	tmp_asesores <<- bd_asesores
	#sel_asesor es el selector de los asesores:
	#(valores de 0 a length(tmp_asesores))
	if (length(tmp_asesores)>0)
		sel_asesor <<- 1 else
		sel_asesor <<- 0
	#inicializa (o re-inicializa) los indices de asesores
	#esto se hara: 1) al principio, o 
	#2) cuando se agrega o se quita un asesor
	actualizar_indices_asesores_tmp <- function() {
		if (length(tmp_asesores)>0) {
			indtmp <- 1:length(tmp_asesores)
			names(indtmp) <- sapply(indtmp, function(i)
				tmp_asesores[[c(i,1)]] )
			indice_asesores_tmp <<- order(names(indtmp))
			names(indice_asesores_tmp) <<- sort(names(indtmp)) } else
		{
			indice_asesores_tmp <<- numeric(0)
		}
	}
	
	actualizar_indices_asesores_tmp()
	#info_asesor contiene la información del asesor seleccionado
	#info_asesor es una versión de un registro de tmp_asesores
	#compatible con la de los controles en pantalla. (25/10/13)
	if (sel_asesor>0) {
		info_asesor <<- tmp_asesores[[ indice_asesores_tmp[sel_asesor] ]]
		m1 <- info_asesor$materias
		mtmp <- matrix(as.integer(m1),ncol=ncol(m1), byrow=FALSE)
		colnames(mtmp) <- colnames(m1)
		rownames(mtmp) <- rownames(m1)
		info_asesor$materias <<- mtmp
	} else
		info_asesor <<- list(asesor="",tipo="Asesor",sexo="M",
			archivo="",adscrito=cloas[1],area="",
			materias={tmp_matrix <- matrix(0,nrow=0,ncol=length(cloas))
				colnames(tmp_matrix) <- cloas
				tmp_matrix}
		)
			
	#copia la información sobre la materia seleccionada desde los
	#widgets del formulario a la base de datos provisional.
	actualizar_asesor_seleccionado <- function() {
		i <-  indice_asesores_tmp[sel_asesor] 
		update(nombre_asesor)
		update(nombre_archivo)
		update(tipo_asesor)
		update(sexo_asesor)
		update(adscrito)
		update(area_asesoria)
		update(materias)
		tmp_asesores[[i]]$asesor <<- svalue(nombre_asesor)
		tmp_asesores[[i]]$tipo <<- svalue(tipo_asesor)
		tmp_asesores[[i]]$sexo <<- svalue(sexo_asesor)
		tmp_asesores[[i]]$archivo <<- svalue(nombre_archivo)
		tmp_asesores[[i]]$adscrito <<- svalue(adscrito)
		tmp_asesores[[i]]$area <<- svalue(area_asesoria)
		#cuando hay una sola materia materias[] se convierte en una lista!
		#de lo contrario es un data frame
		if (class(materias[])=="list") 
			tmp2 <- as.logical(materias[]) else
			tmp2 <- as.logical(as.matrix(materias[]))
		mtmp <- matrix(tmp2,ncol=length(cloas),byrow=FALSE)
		colnames(mtmp) <- colnames(info_asesor$materias)
		rownames(mtmp) <- rownames(info_asesor$materias)
		tmp_asesores[[i]]$materias <<- mtmp
	}
	#copia la información sobre el asesor seleccionado desde la base de
	#datos a los widgets del formulario
	actualizar_vista_asesor <- function() {
		#i <-  indice_asesores_tmp[sel_asesor] <---- (25/10/13)
		if (sel_asesor>0) {
			info_asesor <<- tmp_asesores[[ indice_asesores_tmp[sel_asesor] ]]
			m1 <- info_asesor$materias
			mtmp <- matrix(as.integer(m1),ncol=length(cloas),
				byrow=FALSE)
			colnames(mtmp) <- colnames(m1)
			rownames(mtmp) <- rownames(m1)
			info_asesor$materias <<- mtmp
			enabled(nombre_frame) <- TRUE
			enabled(nombre_archivo_frame) <- TRUE			
			enabled(tipo_frame) <- TRUE
			enabled(sexo_frame) <- TRUE
			enabled(adscrito_frame) <- TRUE
			enabled(area_asesoria_frame) <- TRUE			
			enabled(materias) <- TRUE
			enabled(toolbar_materias) <- TRUE
		} else {
			info_asesor <<- list(asesor="",tipo="Asesor",sexo="M",
				adscrito=cloas[1],area="",archivo="",
				materias={tmp_matrix <- matrix(0,nrow=0,ncol=length(cloas))
						colnames(tmp_matrix) <- cloas
						tmp_matrix})
			enabled(nombre_frame) <- FALSE
			enabled(nombre_archivo_frame) <- FALSE	
			enabled(tipo_frame) <- FALSE
			enabled(sexo_frame) <- FALSE
			enabled(adscrito_frame) <- FALSE
			enabled(area_asesoria_frame) <- FALSE
			enabled(materias) <- FALSE
			enabled(toolbar_materias) <- FALSE
		}
		svalue(nombre_asesor) <- info_asesor$asesor
		svalue(tipo_asesor) <- info_asesor$tipo
		svalue(sexo_asesor) <- info_asesor$sexo
		svalue(nombre_archivo) <- info_asesor$archivo
		svalue(adscrito) <- info_asesor$adscrito
		svalue(area_asesoria) <- info_asesor$area
		materias[] <- info_asesor$materias
		update(nombre_asesor)
		update(nombre_archivo)
		update(tipo_asesor)
		update(sexo_asesor)
		update(adscrito)
		update(area_asesoria)
		update(materias)
	}
	#-------------------------------------------------------------------
	#aqui comienza la definición del cuadro de dialogo de edición
	#de asesores
	wp <- gbasicdialog(title="Editar información de asesores",
		parent=ventana,do.buttons=FALSE,visible=FALSE,horizontal=FALSE)
	size(wp) <- c(640,446)
	wp_marco <- ggroup(horizontal=FALSE,cont=wp)
	#-------------------------------------------------------------------
	#dialogo de salida
	dialogo_salida <- function() {
		dialogo_salida.env <- environment()
		sub_wp <- gbasicdialog(title="Salir de edición de asesores",
			parent=wp, do.buttons=FALSE,visible=FALSE,horizontal=TRUE)
		sub_wp_frame <- ggroup(horizontal=FALSE,container=sub_wp)
		addSpace(sub_wp_frame,value=10,horizontal=FALSE)
		linea_mensaje <- ggroup(horizontal=TRUE,container=sub_wp_frame)
		addSpring(linea_mensaje)
		gimage("gtk-dialog-question",dirname="stock",size="dialog",
			container=linea_mensaje)
		glabel(paste("Usted está a punto de salir de la ventana de edición",
			"de asesores. ¿Desea guardar los cambios a la base",
			"de datos antes de salir?",sep="\n"),container=linea_mensaje)
		addSpring(linea_mensaje)
		addSpace(sub_wp_frame,value=10,horizontal=FALSE)
		gseparator(horizontal=TRUE,cont=sub_wp_frame)
		linea_botones <- ggroup(horizontal=TRUE,container=sub_wp_frame)
		addSpace(linea_botones,value=10,horizontal=TRUE)
		boton1 <- gbutton("Guardar cambios y salir",
			container=linea_botones, handler=function(h,...) {
				assign("cond",1,envir=dialogo_salida.env)
				dispose(sub_wp)
			}		)
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
	#continua con la definición del dialogo de edición de asesores
	#a continuación los toolbars superiores
	barra_sup1 <- ggroup(horizontal=TRUE,cont=wp_marco)
	barra_sup2 <- ggroup(horizontal=TRUE,cont=wp_marco)
	addSpace(barra_sup1,value=5,horizontal=TRUE)
	addSpace(barra_sup2,value=5,horizontal=TRUE)
	logo_UNA <- gimage("logouna",dirname="stock",size="button",
		container=barra_sup1)
	toolbar_sup1_1_list <- list(
		Primera=gaction(label="Primero",icon="gtk-goto-first",
			handler = function(h,...) {
				assign("sel_asesor",1,envir=this.env)
				actualizar_vista_asesor()
			},
			tooltip="Ir al primer docente de la base de datos"),
		Anterior=gaction(label="Anterior",icon="gtk-go-back",
			handler = function(h,...) {
				if (sel_asesor>1) {
					actualizar_asesor_seleccionado()
					assign("sel_asesor",sel_asesor-1,envir=this.env)
					actualizar_vista_asesor()
				}
			},
			tooltip="Ir al asesor previo"),
		Siguiente=gaction(label="Siguiente", icon="gtk-go-forward",
			handler = function(h,...) {
				if (sel_asesor<length(tmp_asesores)) {
					actualizar_asesor_seleccionado()
					assign("sel_asesor",sel_asesor+1,envir=this.env)
					actualizar_vista_asesor()
				}
			},
			tooltip="Ir al asesor siguiente"),
		Ultima=gaction(label="Última", icon="gtk-goto-last",
			handler = function(h,...) {
				assign("sel_asesor",length(indice_asesores_tmp),
					envir=this.env)
				actualizar_vista_asesor()
			},
			tooltip="Ir al último asesor de la base de datos")
	)
	toolbar_sup1_1 <- gtoolbar(toolbar_sup1_1_list,
		container=barra_sup1)
	toolbar_sup1_2_list <- list(
		titulo_buscar=glabel("Buscar ", editable=FALSE),
		quien_buscar=gtext("",width=246,height=30),
		Buscar = gaction(label="Buscar",icon="gtk-find",
			handler = function(h,...) {
				campo <- svalue(toolbar_sup1_2_list$quien_buscar)
				if (is.na(campo)) {
						gmessage("Indique un nombre válido para buscar.",
							title="Aviso", icon = "warning" )
						return(1) }
					nombres <- names(indice_asesores_tmp)
					n <- length(nombres)
					if (n>0) {
						if (campo>nombres[n]) i <- n else
							i <- min(which(nombres>=campo))
						assign("sel_asesor",i,envir=this.env)
						actualizar_vista_asesor()
					}
					svalue(quien_buscar) <- ""
					return(0)
				},
				tooltip = "Buscar el docente indicado")
		)
	font(toolbar_sup1_2_list$titulo_buscar) <- estilo_rotulo2
	toolbar_sup1_2 <- gtoolbar(toolbar_sup1_2_list,
		container=barra_sup1)
	#toolbar_sup1_3
	toolbar_sup1_3_list <- list(
		titulo_salir=glabel("Salir",editable=FALSE),
		Salir = gaction(label="Salir",icon="gtk-quit",
			handler = function(h,...) {
				condicion <- dialogo_salida()
				switch(condicion,
					{	#actualizar tmp_asesores 1ero:
						if (sel_asesor>0) actualizar_asesor_seleccionado()
						bd_asesores <<- tmp_asesores
						actualizar_indices_asesores()
						save(bd_asesores,file=ruta_bd_asesores)
						dispose(wp) },
					{ dispose(wp) },
					{ } )
			},
			tooltip = "Salir al menú principal")
	)
	font(toolbar_sup1_3_list$titulo_salir) <- estilo_rotulo2
	size(toolbar_sup1_3_list$titulo_salir) <- c(55,40)
	toolbar_sup1_3 <- gtoolbar(toolbar_sup1_3_list,
		container=barra_sup1)
	toolbar_sup2_1_list <- list(
		titulo_crear = glabel("Crear\nnuevo  ",editable=FALSE),
		quien_crear = gtext("",width=270,height=40),
		Crear = gaction(label="Crear",icon="gtk-new",
			handler = function(h,...) {
				campo <- svalue(toolbar_sup2_1_list$quien_crear)
				if (is.na(campo)|campo=="") {
					gmessage("Indique un nombre válido para el docente.",
						title="Aviso", icon = "warning" )
					return(1) }
				if (campo %in% names(indice_asesores)) {
					gmessage("El asesor indicado ya existe.",
						title="Aviso", icon = "warning" )
					return(2) 
				}
				campo_archivo <- normalizar(campo)
				info_asesor <<- list(asesor=campo,sexo="M",
					tipo="Asesor",adscrito=cloas[1],
					archivo=campo_archivo,area="",
					materias={tmp_matrix <- matrix(logical(0),nrow=0,
						ncol=length(cloas))
						colnames(tmp_matrix) <- cloas
						tmp_matrix
					}
				)
				n <- length(indice_asesores_tmp)+1
				tmp_asesores[[n]] <<- info_asesor
				actualizar_indices_asesores_tmp()
				assign("sel_asesor",which(names(indice_asesores_tmp)==campo),
					envir=this.env)
				actualizar_vista_asesor()
				svalue(toolbar_sup2_1_list$quien_crear) <- ""
			},
			tooltip = "Crear el docente con el nombre indicado")
	)
	font(toolbar_sup2_1_list$titulo_crear) <- estilo_rotulo2
	toolbar_sup2_1 <- gtoolbar(toolbar_sup2_1_list,
		container=barra_sup2)
	toolbar_sup2_2_list <- list(
		titulo_eliminar = glabel("Eliminar",editable=FALSE),
		Eliminar = gaction(label="Eliminar",icon="gtk-delete",
			handler = function(h,...) {
				#solo se elminia si hay más de un asesor
				cuantos_hay <- length(indice_asesores_tmp)
				if (cuantos_hay>0) {
					n <- indice_asesores_tmp[sel_asesor]
					if (sel_asesor==cuantos_hay) 
						assign("sel_asesor",sel_asesor-1,envir=this.env)
					tmp_asesores <<- tmp_asesores[-n]
					actualizar_indices_asesores_tmp()
					actualizar_vista_asesor()
				}
			},
			tooltip = "Elimina el docente actualmente visible")
	)
	font(toolbar_sup2_2_list$titulo_eliminar) <- estilo_rotulo2
	size(toolbar_sup2_2_list$titulo_eliminar) <- c(57,40)
	toolbar_sup2_2 <- gtoolbar(toolbar_sup2_2_list,
		container=barra_sup2)
	#addSpace(barra_sup2,value=32,horizontal=TRUE)
	toolbar_sup2_3_list <- list(
		titulo_guardar = glabel("Guardar\ncambios",editable=FALSE),
		Guardar = gaction(label="Guardar",icon="gtk-save",
			handler = function(h,...) {
				#actualizar tmp_asesores 1ero (si hay asesores)
				if (sel_asesor>0) actualizar_asesor_seleccionado() 
				bd_asesores <<- tmp_asesores
				actualizar_indices_asesores()
				save(bd_asesores,file=ruta_bd_asesores)
			},
			tooltip = "Guarda los cambios a la base de datos")
	)
	font(toolbar_sup2_3_list$titulo_guardar) <- estilo_rotulo2
	size(toolbar_sup2_3_list$titulo_guardar) <- c(55,40)
	toolbar_sup2_3 <- gtoolbar(toolbar_sup2_3_list,
		container=barra_sup2)
	addSpace(barra_sup1,value=5,horizontal=TRUE)
	addSpace(barra_sup2,value=5,horizontal=TRUE)
	size(toolbar_sup1_1) <- c(145,40)
	size(toolbar_sup1_2) <- c(334,40)
	size(toolbar_sup1_3) <- c(100,40)
	size(toolbar_sup2_1) <- c(403,40)
	size(toolbar_sup2_2) <- c(100,40)
	size(toolbar_sup2_3) <- c(100,40)
	#ahora los widgets de abajo
	marco_en1 <- ggroup(horizontal=TRUE,cont=wp_marco)
	addSpace(marco_en1,value=5,horizontal=TRUE)
	nombre_frame <- gframe(text="Apellidos, Nombres",horizontal=FALSE,
		container=marco_en1)
	size(nombre_frame) <- c(224,50)
	nombre_asesor <- glabel(info_asesor$asesor,editable=FALSE,
		container=nombre_frame)
	size(nombre_asesor) <- c(270,30)
	tipo_frame <- gframe(text="Tipo de personal académico",
		horizontal=FALSE,container=marco_en1)
	size(tipo_frame) <- c(301,50)
	tipo_asesor <- gradio(c("Asesor","Orientador","Preparador"),
		horizontal=TRUE,container=tipo_frame)
	svalue(tipo_asesor) <- info_asesor$tipo
	sexo_frame <- gframe(text="Sexo",horizontal=FALSE,container=marco_en1)
	size(sexo_frame) <- c(80,50)
	sexo_asesor <- gradio(c("F","M"),horizontal=TRUE,container=sexo_frame)
	svalue(sexo_asesor) <- info_asesor$sexo
	marco_en2 <- ggroup(horizontal=TRUE,cont=wp_marco)
	addSpace(marco_en2,value=5,horizontal=TRUE)
	nombre_archivo_frame <- gframe(text="Sufijo para archivos",
		horizontal=FALSE,container=marco_en2)
	size(nombre_archivo_frame) <- c(224,50)
	nombre_archivo <- gtext(info_asesor$archivo,width=220,height=30,
		container=nombre_archivo_frame)
	area_asesoria_frame <- gframe(text="Área de asesoría",
		horizontal=FALSE,container=marco_en2)
	size(area_asesoria_frame) <- c(280,50)
	area_asesoria <- gtext(info_asesor$area,width=276,height=30,
		container=area_asesoria_frame)
	adscrito_frame <- gframe(text="Adscrito a",horizontal=FALSE,
		container=marco_en2)
	size(adscrito_frame) <- c(100,50)
	adscrito <- gcombobox(items=cloas,editable=FALSE,
		selected=which(cloas==info_asesor$adscrito),
		container=adscrito_frame)
	addSpace(wp_marco,value=10)
	#ahora el editor de las materias y los cloas donde corrige
	marco_materias <- ggroup(horizontal=TRUE, cont=wp_marco)
	addSpring(marco_materias,horizontal=TRUE)
	materias <- gdf(info_asesor$materias,container=marco_materias)
	size(materias) <- c(96+length(cloas)*52,170)
	gtk_materias <- getToolkitWidget(materias)
	gtkTreeViewSetGridLines(gtk_materias,grid.lines=3)
	columnas <- 0:length(cloas)
	for (i in columnas) {
		assign(paste0("col",i),
			gtkTreeViewGetColumn(gtk_materias, i) )
		gtkTreeViewColumnSetClickable(get(paste0("col",i)), FALSE)
		assign(paste0("rend",i), gtkCellLayoutGetCells(
			get(paste0("col",i))) )
		gtkCellRendererSetAlignment(get(paste0("rend",i))[[1]],
			xalign=0.5,yalign=0.5)
	}
	col0_header <- gtkLabelNew("Asignatura", show = TRUE)
	gtkTreeViewColumnSetWidget(col0, widget = col0_header)
	addSpring(marco_materias,horizontal=TRUE)
	#toolbar para agregar o eliminar materias de asesoría
	toolbar_materias_list <- list(
		Ins_label = glabel("Agregar asignatura con código   ",
			editable=FALSE),
		Asignatura = gedit("",width=3,coerce.with=as.numeric),
		Ins_asig = gaction("Agregar asignatura",icon="add",
			handler=function(h,...) {
				codigo_asig <- sprintf("%03.0f",
					svalue(toolbar_materias_list$Asignatura) )
				if (!(codigo_asig %in% names(indice_materias))|
					is.na(codigo_asig)) {
					gmessage("Indique un código de asignatura de 3 digitos válido.",
						title="Aviso", icon = "warning" )
					return(1)
				}
				if (codigo_asig %in% rownames(materias[])) {
					gmessage("Esta materia ya está incluida entre las asesoradas",
						title="Aviso", icon = "warning" )
					return(2)
				}
				#materias[] se convierte en una lista cuando
				#tiene una sola fila !!!
				tmp2 <-rbind(info_asesor$materias,{
					tmp <- rep(0,length(cloas))
					names(tmp) <- cloas
					tmp}
				)
				rownames(tmp2) <- c(rownames(info_asesor$materias),codigo_asig)
				info_asesor$materias <<- tmp2
				materias[] <- tmp2
					update(materias)
				svalue(toolbar_materias_list$Asignatura) <- ""
			},
			tooltip="Agregar asignatura"),
		sep1=glabel(paste(rep(" ",12),collapse=""),editable=FALSE),
		Elim_label = glabel("Eliminar asignatura",editable=FALSE),
		Elim_asig = gaction("Eliminar asignatura", icon="remove",
			handler=function(h,...) {
				n <- nrow(info_asesor$materias)
				if (n>0) {
					tmpdimnames <- list(rownames(info_asesor$materias)[-n],
						colnames(info_asesor$materias))
					mtmp <- matrix(info_asesor$materias[-n,], ncol=length(cloas))
					dimnames(mtmp) <- tmpdimnames
					info_asesor$materias <<- mtmp 
					materias[] <- info_asesor$materias
					update(materias)
				}
			},
			tooltip="Eliminar la última asignatura en el cuadro de arriba")
	)
	toolbar_materias_frame <- ggroup(horizontal=TRUE,container=wp)
	addSpring(toolbar_materias_frame)
	toolbar_materias <- gtoolbar(toolbar_materias_list,
		container=toolbar_materias_frame)
	size(toolbar_materias) <- c(600,40)
	addSpring(toolbar_materias_frame)
	#---------------------
	svalue(barra_status) <- ""
	Sys.sleep(0.001)
	actualizar_vista_asesor()
	visible(wp) <- TRUE
} #ventana edicion asesores

#-----------------------------------------------------------------------
ventana_edicion_asesores_NC <- function() {
	#mensaje de espera
	svalue(barra_status) <- "Cargando el menu ..."
	Sys.sleep(0.001)
	#abrevia los codigos de los CL - estos se usaran en vez de las cloas
	codCL <- names(tablaCL)
	#crea un nuevo environment para referencia
	this.env <- environment()
	#tmp_asesores es una copia (de trabajo) de la base de datos de
	#los asesores
	tmp_asesores <<- bd_asesores
	#sel_asesor es el selector de los asesores:
	#(valores de 0 a length(tmp_asesores))
	if (length(tmp_asesores)>0)
		sel_asesor <<- 1 else
		sel_asesor <<- 0
	#inicializa (o re-inicializa) los indices de asesores
	#esto se hara: 1) al principio, o 
	#2) cuando se agrega o se quita un asesor
	actualizar_indices_asesores_tmp <- function() {
		if (length(tmp_asesores)>0) {
			indtmp <- 1:length(tmp_asesores)
			names(indtmp) <- sapply(indtmp, function(i)
				tmp_asesores[[c(i,1)]] )
			indice_asesores_tmp <<- order(names(indtmp))
			names(indice_asesores_tmp) <<- sort(names(indtmp)) } else
		{
			indice_asesores_tmp <<- numeric(0)
		}
	}
	
	actualizar_indices_asesores_tmp()
	#info_asesor contiene la información del asesor seleccionado
	#info_asesor es una versión de un registro de tmp_asesores
	#compatible con la de los controles en pantalla. (25/10/13)
	if (sel_asesor>0) {
		info_asesor <<- tmp_asesores[[ indice_asesores_tmp[sel_asesor] ]]
		m1 <- info_asesor$materias
		mtmp <- matrix(as.integer(m1),ncol=ncol(m1), byrow=FALSE)
		colnames(mtmp) <- colnames(m1)
		rownames(mtmp) <- rownames(m1)
		info_asesor$materias <<- mtmp
	} else
		info_asesor <<- list(asesor="",tipo="Asesor",sexo="M",
			archivo="",adscrito=cloas[1],area="",
			materias={tmp_matrix <- matrix(1,nrow=0,ncol=length(codCL))
				colnames(tmp_matrix) <- codCL
				tmp_matrix}
		)
			

	actualizar_asesor_seleccionado <- function() {
		#copia la información sobre el asesor seleccionado desde los
		#widgets del formulario a la base de datos provisional.	
		i <-  indice_asesores_tmp[sel_asesor]
		update(nombre_asesor)
		update(nombre_archivo)
		update(sexo_asesor)
		update(area_asesoria)
		update(selector_carreras)
		update(materias)
		tmp_asesores[[i]]$asesor <<- svalue(nombre_asesor)
		tmp_asesores[[i]]$sexo <<- svalue(sexo_asesor)
		tmp_asesores[[i]]$archivo <<- svalue(nombre_archivo)
		tmp_asesores[[i]]$area <<- svalue(area_asesoria)
		tmp_asesores[[i]]$carreras <<- svalue(selector_carreras)
		#cuando hay una sola materia materias[] se convierte en una lista!
		#de lo contrario es un data frame
		if (class(materias[])=="list") 
			tmp2 <- as.logical(materias[]) else
			tmp2 <- as.logical(as.matrix(materias[]))
		mtmp <- matrix(tmp2,ncol=length(codCL),byrow=FALSE)
		colnames(mtmp) <- colnames(info_asesor$materias)
		rownames(mtmp) <- rownames(info_asesor$materias)
		tmp_asesores[[i]]$materias <<- mtmp
	}

	actualizar_vista_asesor <- function() {
		#copia la información sobre el asesor seleccionado desde la base de
		#datos a los widgets del formulario
		if (sel_asesor>0) {
			info_asesor <<- tmp_asesores[[ indice_asesores_tmp[sel_asesor] ]]
			m1 <- info_asesor$materias
			mtmp <- matrix(as.integer(m1),ncol=length(codCL),
				byrow=FALSE)
			colnames(mtmp) <- colnames(m1)
			rownames(mtmp) <- rownames(m1)
			info_asesor$materias <<- mtmp
			enabled(nombre_frame) <- TRUE
			enabled(nombre_archivo_frame) <- TRUE
			enabled(area_asesoria_frame) <- TRUE
			enabled(sexo_frame) <- TRUE
			enabled(materias) <- TRUE
			enabled(toolbar_materias) <- TRUE
			enabled(selector_carreras) <- TRUE
		} else {
			info_asesor <<- list(asesor="",tipo="Asesor",sexo="M",
				adscrito=cloas[1],area="",carreras=numeric(0),archivo="",
				materias={tmp_matrix <- matrix(1,nrow=0,ncol=length(codCL))
						colnames(tmp_matrix) <- codCL
						tmp_matrix})
			enabled(nombre_frame) <- FALSE
			enabled(nombre_archivo_frame) <- FALSE
			enabled(sexo_frame) <- FALSE
			enabled(area_asesoria_frame) <- FALSE
			enabled(materias) <- FALSE
			enabled(toolbar_materias) <- FALSE
			enabled(selector_carreras) <- FALSE
		}# if
		svalue(nombre_asesor) <- info_asesor$asesor
		svalue(sexo_asesor) <- info_asesor$sexo
		svalue(nombre_archivo) <- info_asesor$archivo
		svalue(area_asesoria) <- info_asesor$area
		svalue(selector_carreras) <- info_asesor$carreras
		update(nombre_asesor)
		update(nombre_archivo)
		update(sexo_asesor)
		update(area_asesoria)
		update(selector_carreras)
		materias[] <- info_asesor$materias
		update(materias)
	}
	#-------------------------------------------------------------------
	#aqui comienza la definición del cuadro de dialogo de edición
	#de asesores
	wp <- gbasicdialog(title="Editar información de asesores",
		parent=ventana,do.buttons=FALSE,visible=FALSE,horizontal=FALSE)
	size(wp) <- c(640,446)
	wp_marco <- ggroup(horizontal=FALSE,cont=wp)
	#-------------------------------------------------------------------
	#dialogo de salida
	dialogo_salida <- function() {
		dialogo_salida.env <- environment()
		sub_wp <- gbasicdialog(title="Salir de edición de asesores",
			parent=wp, do.buttons=FALSE,visible=FALSE,horizontal=TRUE)
		sub_wp_frame <- ggroup(horizontal=FALSE,container=sub_wp)
		addSpace(sub_wp_frame,value=10,horizontal=FALSE)
		linea_mensaje <- ggroup(horizontal=TRUE,container=sub_wp_frame)
		addSpring(linea_mensaje)
		gimage("gtk-dialog-question",dirname="stock",size="dialog",
			container=linea_mensaje)
		glabel(paste("Usted está a punto de salir de la ventana de edición",
			"de asesores. ¿Desea guardar los cambios a la base",
			"de datos antes de salir?",sep="\n"),container=linea_mensaje)
		addSpring(linea_mensaje)
		addSpace(sub_wp_frame,value=10,horizontal=FALSE)
		gseparator(horizontal=TRUE,cont=sub_wp_frame)
		linea_botones <- ggroup(horizontal=TRUE,container=sub_wp_frame)
		addSpace(linea_botones,value=10,horizontal=TRUE)
		boton1 <- gbutton("Guardar cambios y salir",
			container=linea_botones, handler=function(h,...) {
				assign("cond",1,envir=dialogo_salida.env)
				dispose(sub_wp)
			}		)
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
	#continua con la definición del dialogo de edición de asesores
	#a continuación los toolbars superiores
	barra_sup1 <- ggroup(horizontal=TRUE,cont=wp_marco)
	barra_sup2 <- ggroup(horizontal=TRUE,cont=wp_marco)
	addSpace(barra_sup1,value=5,horizontal=TRUE)
	addSpace(barra_sup2,value=5,horizontal=TRUE)
	logo_UNA <- gimage("logouna",dirname="stock",size="button",
		container=barra_sup1)
	toolbar_sup1_1_list <- list(
		Primera=gaction(label="Primero",icon="gtk-goto-first",
			handler = function(h,...) {
				assign("sel_asesor",1,envir=this.env)
				actualizar_vista_asesor()
			},
			tooltip="Ir al primer docente de la base de datos"),
		Anterior=gaction(label="Anterior",icon="gtk-go-back",
			handler = function(h,...) {
				if (sel_asesor>1) {
					actualizar_asesor_seleccionado()
					assign("sel_asesor",sel_asesor-1,envir=this.env)
					actualizar_vista_asesor()
				}
			},
			tooltip="Ir al asesor previo"),
		Siguiente=gaction(label="Siguiente", icon="gtk-go-forward",
			handler = function(h,...) {
				if (sel_asesor<length(tmp_asesores)) {
					actualizar_asesor_seleccionado()
					assign("sel_asesor",sel_asesor+1,envir=this.env)
					actualizar_vista_asesor()
				}
			},
			tooltip="Ir al asesor siguiente"),
		Ultima=gaction(label="Última", icon="gtk-goto-last",
			handler = function(h,...) {
				assign("sel_asesor",length(indice_asesores_tmp),
					envir=this.env)
				actualizar_vista_asesor()
			},
			tooltip="Ir al último asesor de la base de datos")
	)
	toolbar_sup1_1 <- gtoolbar(toolbar_sup1_1_list,
		container=barra_sup1)
	toolbar_sup1_2_list <- list(
		titulo_buscar=glabel("Buscar ", editable=FALSE),
		quien_buscar=gtext("",width=246,height=30),
		Buscar = gaction(label="Buscar",icon="gtk-find",
			handler = function(h,...) {
				campo <- svalue(toolbar_sup1_2_list$quien_buscar)
				if (is.na(campo)) {
						gmessage("Indique un nombre válido para buscar.",
							title="Aviso", icon = "warning" )
						return(1) }
					nombres <- names(indice_asesores_tmp)
					n <- length(nombres)
					if (n>0) {
						if (campo>nombres[n]) i <- n else
							i <- min(which(nombres>=campo))
						assign("sel_asesor",i,envir=this.env)
						actualizar_vista_asesor()
					}
					svalue(quien_buscar) <- ""
					return(0)
				},
				tooltip = "Buscar el docente indicado")
		)
	font(toolbar_sup1_2_list$titulo_buscar) <- estilo_rotulo2
	toolbar_sup1_2 <- gtoolbar(toolbar_sup1_2_list,
		container=barra_sup1)
	#toolbar_sup1_3
	toolbar_sup1_3_list <- list(
		titulo_salir=glabel("Salir",editable=FALSE),
		Salir = gaction(label="Salir",icon="gtk-quit",
			handler = function(h,...) {
				condicion <- dialogo_salida()
				switch(condicion,
					{	#actualizar tmp_asesores 1ero:
						if (sel_asesor>0) actualizar_asesor_seleccionado()
						bd_asesores <<- tmp_asesores
						actualizar_indices_asesores()
						save(bd_asesores,file=ruta_bd_asesores)
						dispose(wp) },
					{ dispose(wp) },
					{ } )
			},
			tooltip = "Salir al menú principal")
	)
	font(toolbar_sup1_3_list$titulo_salir) <- estilo_rotulo2
	size(toolbar_sup1_3_list$titulo_salir) <- c(55,40)
	toolbar_sup1_3 <- gtoolbar(toolbar_sup1_3_list,
		container=barra_sup1)
	toolbar_sup2_1_list <- list(
		titulo_crear = glabel("Crear\nnuevo  ",editable=FALSE),
		quien_crear = gtext("",width=270,height=40),
		Crear = gaction(label="Crear",icon="gtk-new",
			handler = function(h,...) {
				campo <- svalue(toolbar_sup2_1_list$quien_crear)
				if (is.na(campo)|campo=="") {
					gmessage("Indique un nombre válido para el docente.",
						title="Aviso", icon = "warning" )
					return(1) }
				if (campo %in% names(indice_asesores)) {
					gmessage("El asesor indicado ya existe.",
						title="Aviso", icon = "warning" )
					return(2) 
				}
				campo_archivo <- normalizar(campo)
				info_asesor <<- list(asesor=campo,sexo="M",
					tipo="Asesor",adscrito=cloas[1],
					archivo=campo_archivo,area="",
					materias={tmp_matrix <- matrix(logical(0),nrow=0,
						ncol=length(codCL))
						colnames(tmp_matrix) <- codCL
						tmp_matrix
					}
				)
				n <- length(indice_asesores_tmp)+1
				tmp_asesores[[n]] <<- info_asesor
				actualizar_indices_asesores_tmp()
				assign("sel_asesor",which(names(indice_asesores_tmp)==campo),
					envir=this.env)
				actualizar_vista_asesor()
				svalue(toolbar_sup2_1_list$quien_crear) <- ""
			},
			tooltip = "Crear el docente con el nombre indicado")
	)
	font(toolbar_sup2_1_list$titulo_crear) <- estilo_rotulo2
	toolbar_sup2_1 <- gtoolbar(toolbar_sup2_1_list,
		container=barra_sup2)
	toolbar_sup2_2_list <- list(
		titulo_eliminar = glabel("Eliminar",editable=FALSE),
		Eliminar = gaction(label="Eliminar",icon="gtk-delete",
			handler = function(h,...) {
				#solo se elminia si hay más de un asesor
				cuantos_hay <- length(indice_asesores_tmp)
				if (cuantos_hay>0) {
					n <- indice_asesores_tmp[sel_asesor]
					if (sel_asesor==cuantos_hay) 
						assign("sel_asesor",sel_asesor-1,envir=this.env)
					tmp_asesores <<- tmp_asesores[-n]
					actualizar_indices_asesores_tmp()
					actualizar_vista_asesor()
				}
			},
			tooltip = "Elimina el docente actualmente visible")
	)
	font(toolbar_sup2_2_list$titulo_eliminar) <- estilo_rotulo2
	size(toolbar_sup2_2_list$titulo_eliminar) <- c(57,40)
	toolbar_sup2_2 <- gtoolbar(toolbar_sup2_2_list,
		container=barra_sup2)
	toolbar_sup2_3_list <- list(
		titulo_guardar = glabel("Guardar\ncambios",editable=FALSE),
		Guardar = gaction(label="Guardar",icon="gtk-save",
			handler = function(h,...) {
				#actualizar tmp_asesores 1ero (si hay asesores)
				if (sel_asesor>0) actualizar_asesor_seleccionado() 
				bd_asesores <<- tmp_asesores
				actualizar_indices_asesores()
				save(bd_asesores,file=ruta_bd_asesores)
			},
			tooltip = "Guarda los cambios a la base de datos")
	)
	font(toolbar_sup2_3_list$titulo_guardar) <- estilo_rotulo2
	size(toolbar_sup2_3_list$titulo_guardar) <- c(55,40)
	toolbar_sup2_3 <- gtoolbar(toolbar_sup2_3_list,
		container=barra_sup2)
	addSpace(barra_sup1,value=5,horizontal=TRUE)
	addSpace(barra_sup2,value=5,horizontal=TRUE)
	size(toolbar_sup1_1) <- c(145,40)
	size(toolbar_sup1_2) <- c(334,40)
	size(toolbar_sup1_3) <- c(100,40)
	size(toolbar_sup2_1) <- c(403,40)
	size(toolbar_sup2_2) <- c(100,40)
	size(toolbar_sup2_3) <- c(100,40)
	#ahora los widgets de abajo
	marcos_abajo <- ggroup(horizontal=TRUE,cont=wp_marco)
	addSpace(marcos_abajo,value=5,horizontal=TRUE)
	marco_izq <- ggroup(horizontal=FALSE,cont=marcos_abajo)
	size(marco_izq) <- c(408,104)
	addSpring(marcos_abajo)
	marco_der <- ggroup(horizontal=FALSE,cont=marcos_abajo)
	size(marco_der) <- c(208,104)
	addSpace(marcos_abajo,value=5,horizontal=TRUE)
	marco_en1 <- ggroup(horizontal=TRUE,cont=marco_izq)
	nombre_frame <- gframe(text="Apellidos, Nombres",horizontal=FALSE,
		container=marco_en1)
	size(nombre_frame) <- c(320,40)
	nombre_asesor <- glabel(info_asesor$asesor,editable=FALSE,
		container=nombre_frame)
	size(nombre_asesor) <- c(320,25)
	sexo_frame <- gframe(text="Sexo",horizontal=FALSE,container=marco_en1)
	size(sexo_frame) <- c(80,40)
	sexo_asesor <- gradio(c("F","M"),horizontal=TRUE,
		selected=ifelse(info_asesor$sexo=="F",1,2), container=sexo_frame)
	addSpring(marco_izq)
	marco_en2 <- ggroup(horizontal=TRUE,cont=marco_izq)
	nombre_archivo_frame <- gframe(text="Sufijo para archivos",
		horizontal=FALSE,container=marco_en2)
	size(nombre_archivo_frame) <- c(200,50)	
	nombre_archivo <- gtext(info_asesor$archivo,width=200,height=30,
		container=nombre_archivo_frame)		
	area_asesoria_frame <- gframe(text="Área de asesoría",
		horizontal=FALSE,container=marco_en2)
	size(area_asesoria_frame) <- c(200,50)
	area_asesoria <- gtext(info_asesor$area,width=200,height=30,
		container=area_asesoria_frame)
	carreras_marco <- gframe(text="Carreras atendidas", horizontal=FALSE,
		container=marco_der)
	size(carreras_marco) <- c(204,104)
	selector_carreras <- gcheckboxgroup(c(126,236,237,280,281,430,440,
		508,521,542,610,612,613),checked = FALSE, horizontal = FALSE,
		use.table=TRUE, container=carreras_marco)
	addSpace(wp_marco,value=10)
	#ahora el editor de las materias y los centros locales donde corrige
	marco_materias <- ggroup(horizontal=TRUE, cont=wp_marco)
	addSpring(marco_materias,horizontal=TRUE)
	materias <- gdf(info_asesor$materias,container=marco_materias)
	size(materias) <- c(68+length(codCL)*25,170)
	gtk_materias <- getToolkitWidget(materias)
	gtkTreeViewSetGridLines(gtk_materias,grid.lines=3)
	columnas <- 0:length(cloas)
	for (i in columnas) {
		assign(paste0("col",i),
			gtkTreeViewGetColumn(gtk_materias, i) )
		gtkTreeViewColumnSetClickable(get(paste0("col",i)), FALSE)
		assign(paste0("rend",i), gtkCellLayoutGetCells(
			get(paste0("col",i))) )
		gtkCellRendererSetAlignment(get(paste0("rend",i))[[1]],
			xalign=0.5,yalign=0.5)
	}
	col0_header <- gtkLabelNew("Asig.", show = TRUE)
	gtkTreeViewColumnSetWidget(col0, widget = col0_header)
	addSpring(marco_materias,horizontal=TRUE)
	#toolbar para agregar o eliminar materias de asesoría
	toolbar_materias_list <- list(
		Ins_label = glabel("Agregar asignatura con código   ",
			editable=FALSE),
		Asignatura = gedit("",width=3,coerce.with=as.numeric),
		Ins_asig = gaction("Agregar asignatura",icon="add",
			handler=function(h,...) {
				codigo_asig <- sprintf("%03.0f",
					svalue(toolbar_materias_list$Asignatura) )
				if (!(codigo_asig %in% names(indice_materias))|
					is.na(codigo_asig)) {
					gmessage("Indique un código de asignatura de 3 digitos válido.",
						title="Aviso", icon = "warning" )
					return(1)
				}
				if (codigo_asig %in% rownames(materias[])) {
					gmessage("Esta materia ya está incluida entre las asesoradas",
						title="Aviso", icon = "warning" )
					return(2)
				}
				#materias[] se convierte en una lista cuando
				#tiene una sola fila !!!
				tmp2 <-rbind(info_asesor$materias,{
					tmp <- rep(1,length(codCL))
					names(tmp) <- codCL
					tmp}
				)
				rownames(tmp2) <- c(rownames(info_asesor$materias),codigo_asig)
				info_asesor$materias <<- tmp2
				materias[] <- tmp2
					update(materias)
				svalue(toolbar_materias_list$Asignatura) <- ""
			},
			tooltip="Agregar asignatura"),
		sep1=glabel(paste(rep(" ",12),collapse=""),editable=FALSE),
		Elim_label = glabel("Eliminar asignatura",editable=FALSE),
		Elim_asig = gaction("Eliminar asignatura", icon="remove",
			handler=function(h,...) {
				n <- nrow(info_asesor$materias)
				if (n>0) {
					tmpdimnames <- list(rownames(info_asesor$materias)[-n],
						colnames(info_asesor$materias))
					mtmp <- matrix(info_asesor$materias[-n,], ncol=length(codCL))
					dimnames(mtmp) <- tmpdimnames
					info_asesor$materias <<- mtmp 
					materias[] <- info_asesor$materias
					update(materias)
				}
			},
			tooltip="Eliminar la última asignatura en el cuadro de arriba")
	)
	toolbar_materias_frame <- ggroup(horizontal=TRUE,container=wp)
	addSpring(toolbar_materias_frame)
	toolbar_materias <- gtoolbar(toolbar_materias_list,
		container=toolbar_materias_frame)
	size(toolbar_materias) <- c(610,30)
	addSpring(toolbar_materias_frame)
	#---------------------
	svalue(barra_status) <- ""
	Sys.sleep(0.001)
	actualizar_vista_asesor()
	visible(wp) <- TRUE
} #ventana edicion asesores_NC
