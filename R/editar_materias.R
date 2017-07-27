#Edición de la base de datos de las materias
#Fecha : 05/01/2014
#Autor : José L. Romero
#requiere los siguientes paquetes: gWidgets, RGtk2
#-----------------------------------------------------------------------

	#lee los tipos de evaluaciones
	archivo <- file(file.path(dir_programa,"data","tipos_evaluacion"),
		"r", blocking = FALSE)
	tipos_evaluacion <- readLines(archivo)
	close(archivo)

	#carga la base de datos de las materias
	ruta_bd_materias <- file.path(dir_programa,"data","bd_materias.RData") 
	if (file.access(ruta_bd_materias)==0) {
		load(ruta_bd_materias)
	} else {
		bd_materias <- list()
		save(bd_materias,file=ruta_bd_materias)
	}

	#Funciones para manipular la base de datos de materias
	#verifica si existe la materia con código numerico x
	existe_materia <- function(x) {
		return(sprintf("%03.0f",x)%in%names(indice_materias))
	}
	#la función definida a continuación retorna la información en
	#bd_materias correspondiente a una materia x (x es el código numérico)
	materia <- function(x) {
		indice <- as.numeric(indice_materias[sprintf("%03.0f",x)])
		return(bd_materias[[indice]])
	}
	#inicializa (o re-inicializa) los indices de las materias
	#esto se hara: 1) al principio, o 
	#2) cuando se agrega o se quita una materia
	actualizar_indices_materias <- function() {
		indtmp <- 1:length(bd_materias)
		names(indtmp) <- sapply(indtmp, function(i)
			sprintf("%03.0f", bd_materias[[c(i,1)]]) )
		indice_materias <<- order(names(indtmp))
		names(indice_materias) <<- sort(names(indtmp))
	}
	actualizar_indices_materias()

	ventana_edicion_materias <- function() {
		#mensaje de espera
		svalue(barra_status) <- "Cargando el menu ..."
		Sys.sleep(0.001)
		#crea un nuevo environment para referencia
		this.env <- environment()
		#tmp_materias es una copia (de trabajo) de la base de datos de
		#las materias
		tmp_materias <<- bd_materias
		#sel_materia es el selector de las materias:
		#(valores de 1 a length(tmp_materias))
		sel_materia <<- 1
		#inicializa (o re-inicializa) los indices de las materias
		#esto se hara: 1) al principio, o 
		#2) cuando se agrega o se quita una materia
		actualizar_indices_materias_tmp <- function() {
			indtmp <- 1:length(tmp_materias)
			names(indtmp) <- sapply(indtmp, function(i)
				sprintf("%03.0f", tmp_materias[[c(i,1)]]) )
			indice_materias_tmp <<- order(names(indtmp))
			names(indice_materias_tmp) <<- sort(names(indtmp))
		}
		actualizar_indices_materias_tmp()
		#info_materia contiene la información de la materia seleccionada
		info_materia <<- tmp_materias[[ indice_materias_tmp[sel_materia] ]]
		#la siguiente función transforma la data de los objetivos a numericos
		editarobjetivos_df <- function(x) {
			#x es la materia completa
			tmp_df <- x$objetivos
			ponde <- tmp_df$ponderacion
			eva <- ifelse(tmp_df$nc,1,0)
			return(data.frame(pond=ponde,ev=eva))
		}
		#copia la información sobre la materia seleccionada desde los
		#widgets del formulario a la base de datos provisional.
		actualizar_materia_seleccionada <- function() {
			i <-  indice_materias_tmp[sel_materia] 
			tmp_materias[[i]]$nombre <<- svalue(nombre_asignatura)
			tmp_materias[[i]]$creditos <<- svalue(creditos_asignatura)
			evas <- as.character(evaluaciones_asignatura[])
			tmp_materias[[i]]$evaluaciones <<- evas
			tmp_materias[[i]]$cd <<- svalue(criterio_asignatura)
			tmp_df <- data.frame(
				ponderacion=as.integer(objetivos_asignatura[]$pond),
				nc = as.logical(objetivos_asignatura[]$ev) )
			#si no hay evaluaciones, no hay objetivos con nivel corrector
			if (length(evas)==0) tmp_df$nc <- rep(F,nrow(tmp_df))
			tmp_materias[[i]]$objetivos <<- tmp_df
		}
		#copia la información sobre la materia seleccionada desde la base de
		#datos a los widgets del formulario
		actualizar_vista_materia <- function() {
			i <-  indice_materias_tmp[sel_materia]
			info_materia <<- tmp_materias[[i]]
			svalue(codigo_asignatura) <- names(indice_materias_tmp)[sel_materia]
			svalue(nombre_asignatura) <- info_materia$nombre
			svalue(creditos_asignatura) <- info_materia$creditos
			objetivos_asignatura[] <- editarobjetivos_df(info_materia)
			svalue(criterio_asignatura) <- info_materia$cd
			evaluaciones_asignatura[] <- info_materia$evaluaciones
			svalue(total_pond) <-  sum(objetivos_asignatura[,1])
			update(codigo_asignatura)
			update(nombre_asignatura)
			update(creditos_asignatura)
			update(objetivos_asignatura)
			update(criterio_asignatura)
			update(evaluaciones_asignatura)
			update(total_pond)
		}
		#-------------------------------------------------------------------
		#aqui comienza la definición del cuadro de dialogo de edición
		#de materias
		wp <- gbasicdialog(title="Editar información de asignaturas",
			parent=ventana,do.buttons=FALSE,visible=FALSE,horizontal=FALSE)
		size(wp) <- c(640,396)
		wp_marco <- ggroup(horizontal=FALSE,cont=wp)
		#-------------------------------------------------------------------
		#dialogo de salida
		dialogo_salida <- function() {
			dialogo_salida.env <- environment()
			sub_wp <- gbasicdialog(title="Salir de edición de asignaturas",
				parent=wp, do.buttons=FALSE,visible=FALSE,horizontal=TRUE)
			sub_wp_frame <- ggroup(horizontal=FALSE,container=sub_wp)
			addSpace(sub_wp_frame,value=10,horizontal=FALSE)
			linea_mensaje <- ggroup(horizontal=TRUE,container=sub_wp_frame)
			addSpring(linea_mensaje)
			gimage("gtk-dialog-question",dirname="stock",size="dialog",
				container=linea_mensaje)
			glabel(paste("Usted está a punto de salir de la ventana de edición",
				"de asignaturas. ¿Desea guardar los cambios a la base",
				"de datos antes de salir?",sep="\n"),container=linea_mensaje)
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
		#continua con la definición del dialogo de edición de asignaturas
		#a continuación los toolbars superiores
		barra_sup1 <- ggroup(horizontal=TRUE,cont=wp_marco)
		barra_sup2 <- ggroup(horizontal=TRUE,cont=wp_marco)
		addSpace(barra_sup1,value=5,horizontal=TRUE)
		addSpace(barra_sup2,value=5,horizontal=TRUE)
		logo_UNA <- gimage("logouna",dirname="stock",size="button",
		container=barra_sup1)
		addSpace(barra_sup1,value=5,horizontal=TRUE)
		etiqueta_sup1 <- glabel("Ir a la asignatura",editable=FALSE,
			container=barra_sup1)
		etiqueta_sup2 <- glabel("Crear o eliminar\nasignaturas",
			editable=FALSE,container=barra_sup2)
		font(etiqueta_sup1) <- estilo_rotulo1
		font(etiqueta_sup2) <- estilo_rotulo1
		addSpring(barra_sup1,horizontal=TRUE)
		addSpring(barra_sup2,horizontal=TRUE)
		toolbar_sup1_1_list <- list(
			Primera=gaction(label="Primera",icon="gtk-goto-first",
				handler = function(h,...) {
					assign("sel_materia",1,envir=this.env)
					actualizar_vista_materia()
				},
				tooltip="Ir a la primera asignatura de la base de datos"),
			Anterior=gaction(label="Anterior",icon="gtk-go-back",
				handler = function(h,...) {
					if (sel_materia>1) {
						actualizar_materia_seleccionada()
						assign("sel_materia",sel_materia-1,envir=this.env)
						actualizar_vista_materia()
					}
				},
				tooltip="Ir a la asignatura previa"),
			Siguiente=gaction(label="Siguiente", icon="gtk-go-forward",
				handler = function(h,...) {
					if (sel_materia<length(tmp_materias)) {
						actualizar_materia_seleccionada()
						assign("sel_materia",sel_materia+1,envir=this.env)
						actualizar_vista_materia()
					}
				},
				tooltip="Ir a la asignatura siguiente"),
			Ultima=gaction(label="Última", icon="gtk-goto-last",
				handler = function(h,...) {
					assign("sel_materia",length(indice_materias_tmp),envir=this.env)
					actualizar_vista_materia()
				},
				tooltip="Ir a la última asignatura de la base de datos")
		)
		toolbar_sup1_1 <- gtoolbar(toolbar_sup1_1_list,
			container=barra_sup1)
		toolbar_sup1_2_list <- list(
			titulo_buscar=glabel("Buscar ", editable=FALSE),
			cual_buscar = gedit("",width=3,coerce.with=as.numeric),
			Buscar = gaction(label="Buscar",icon="gtk-find",
				handler = function(h,...) {
					campo <- svalue(toolbar_sup1_2_list$cual_buscar)
					if (is.na(campo)) {
						gmessage("Indique un código de asignatura de 3 digitos válido para buscar.",
							title="Aviso", icon = "warning" )
						return(1) }
					codigo <- sprintf("%03.0f",campo)
					codigos <- names(indice_materias_tmp)
					if (codigo<codigos[1]) i <- 1 else
						i <- max(which(codigos<=codigo))
					assign("sel_materia",i,envir=this.env)
					actualizar_vista_materia()
					svalue(toolbar_sup1_2_list$cual_buscar) <- ""
					return(0)
				},
				tooltip = "Buscar la asignatura indicada")
		)
		font(toolbar_sup1_2_list$titulo_buscar) <- estilo_rotulo2
		size(toolbar_sup1_2_list$titulo_buscar) <- c(75,30)
		toolbar_sup1_2 <- gtoolbar(toolbar_sup1_2_list,
			container=barra_sup1)
		addSpring(barra_sup1,horizontal=TRUE)
		toolbar_sup1_3_list <- list(
			titulo_salir=glabel("Salir",editable=FALSE),
			Salir = gaction(label="Salir",icon="gtk-quit",
				handler = function(h,...) {
					condicion <- dialogo_salida()
					switch(condicion,
						{	#guardar en bd_materias y salir
							actualizar_materia_seleccionada()
							bd_materias <<- tmp_materias
							actualizar_indices_materias()
							save(bd_materias,file=ruta_bd_materias)
							dispose(wp) },
						{ #salir de edición materias sin guardar
							dispose(wp) },
						{ #no salio de edición de materias
							} )
				},
				tooltip = "Salir al menú principal")
		)
		font(toolbar_sup1_3_list$titulo_salir) <- estilo_rotulo2
		size(toolbar_sup1_3_list$titulo_salir) <- c(90,30)
		toolbar_sup1_3 <- gtoolbar(toolbar_sup1_3_list,
			container=barra_sup1)
		toolbar_sup2_1_list <- list(
			titulo_crear = glabel("Crear\nnueva  ",editable=FALSE),
			cual_crear = gedit("",width=3,coerce.with=as.numeric),
			Crear = gaction(label="Crear",icon="gtk-new",
				handler = function(h,...) {
					campo <- svalue(toolbar_sup2_1_list$cual_crear)
					if (is.na(campo)) {
						gmessage("Indique un código de asignatura de 3 digitos válido.",
							title="Aviso", icon = "warning" )
						return(1) }
					codigo <- sprintf("%03.0f",campo)
					codigos <- names(indice_materias_tmp)
					if (codigo %in% codigos) {
						gmessage("La materia indicada ya existe.",
							title="Aviso", icon = "warning" )
						return(2) 
					}
					info_materia <<- list(materia=campo,
						nombre="",creditos=3,cd=5,
						evaluaciones=c("1P","2P","Int"),
						objetivos=data.frame(
							ponderacion=rep(1,8),
							nc = rep(TRUE,8) )
						)
					n <- length(indice_materias_tmp)+1
					tmp_materias[[n]] <<- info_materia
					actualizar_indices_materias_tmp()
					assign("sel_materia", 
						which(names(indice_materias_tmp)==sprintf("%03.0f",campo)),
						envir=this.env)
					actualizar_vista_materia()
					svalue(toolbar_sup2_1_list$cual_crear) <- ""
				},
				tooltip = "Crear la asignatura con el código indicado")
		)
		font(toolbar_sup2_1_list$titulo_crear) <- estilo_rotulo2
		toolbar_sup2_1 <- gtoolbar(toolbar_sup2_1_list,
			container=barra_sup2)
		toolbar_sup2_2_list <- list(
			titulo_eliminar = glabel("Eliminar",editable=FALSE),
			Eliminar = gaction(label="Eliminar",icon="gtk-delete",
				handler = function(h,...) {
					#solo se elminia si hay más de una materia
					cuantas_hay <- length(indice_materias_tmp)
					if (cuantas_hay>1) {
						n <- indice_materias_tmp[sel_materia]
						if (sel_materia==cuantas_hay) 
							assign("sel_materia",sel_materia-1,envir=this.env)
						tmp_materias <<- tmp_materias[-n]
						actualizar_indices_materias_tmp()
						actualizar_vista_materia()
					}
				},
				tooltip = "Elimina la asignatura actualmente visible")
		)
		font(toolbar_sup2_2_list$titulo_eliminar) <- estilo_rotulo2
		size(toolbar_sup2_2_list$titulo_eliminar) <- c(110,30)
		toolbar_sup2_2 <- gtoolbar(toolbar_sup2_2_list,
			container=barra_sup2)
		addSpring(barra_sup2,horizontal=TRUE)
		toolbar_sup2_3_list <- list(
			titulo_guardar = glabel("Guardar\ncambios",editable=FALSE),
			Guardar = gaction(label="Guardar",icon="gtk-save",
				handler = function(h,...) {
					actualizar_materia_seleccionada()
					bd_materias <<- tmp_materias
					actualizar_indices_materias()
					save(bd_materias,file=ruta_bd_materias )
				},
				tooltip = "Guarda los cambios a la base de datos")
		)
		font(toolbar_sup2_3_list$titulo_guardar) <- estilo_rotulo2
		size(toolbar_sup2_3_list$titulo_guardar) <- c(90,30)
		toolbar_sup2_3 <- gtoolbar(toolbar_sup2_3_list,
			container=barra_sup2)
		addSpace(barra_sup1,value=5,horizontal=TRUE)
		addSpace(barra_sup2,value=5,horizontal=TRUE)
		size(etiqueta_sup1) <- c(98,40)
		size(etiqueta_sup2) <- c(110,40)
		size(toolbar_sup1_1) <- c(140,40)
		size(toolbar_sup2_1) <- c(160,40)
		size(toolbar_sup1_2) <- c(160,40)
		size(toolbar_sup2_2) <- c(160,40)
		size(toolbar_sup1_3) <- c(130,40)
		size(toolbar_sup2_3) <- c(130,40)
		
		#el marco de encabezado
		marco_en1 <- ggroup(horizontal=TRUE,cont=wp_marco)
		addSpace(marco_en1,value=5,horizontal=TRUE)
		codigo_frame <- gframe(text="Código",horizontal=FALSE,
			container=marco_en1)
		
		size(codigo_frame) <- c(70,60)
		codigo_asignatura <- glabel(names(indice_materias_tmp)[sel_materia],
			editable=FALSE,container=codigo_frame)
		nombre_frame <- gframe(text="Nombre",horizontal=FALSE,
			container=marco_en1)
		size(nombre_frame) <- c(280,60)
		nombre_asignatura <- gtext(info_materia$nombre,
			width=128,height=40,container=nombre_frame)
		credito_frame <- gframe(text="Creditos",horizontal=FALSE,
			container=marco_en1)
		size(credito_frame) <- c(80,60)
		addSpace(credito_frame,value=2,horizontal=FALSE)
		credito_line_frame <- ggroup(horizontal=TRUE,container=credito_frame)
		addSpring(credito_line_frame)
		creditos_asignatura <- gedit(info_materia$creditos,
			width=2,coerce.with=as.numeric,container=credito_line_frame)
		size(creditos_asignatura) <- c(35,30)
		addSpring(credito_line_frame)
		cdedom_frame <- gframe(text="Criterio de dominio",horizontal=FALSE,
			align=0.5,container=marco_en1)
		size(cdedom_frame) <- c(170,60)
		addSpace(cdedom_frame,value=2,horizontal=FALSE)
		cdedom_line_frame <- ggroup(horizontal=TRUE,container=cdedom_frame)
		addSpring(cdedom_line_frame)
		criterio_asignatura <- gedit(info_materia$cd, width=2,
			coerce.with=as.numeric,container=cdedom_line_frame)
		size(criterio_asignatura) <- c(35,30)
		glabel(" de ",editable=FALSE,container=cdedom_line_frame)
		#el marco de tablas contiene información sobre los objetivos
		#(ponderaciones y nivel corrector) y también sobre cuales son las
		#evaluaciones corregidas por el asesor.
		#cuadro de objetivos de la asignatura
		marco_tablas <- ggroup(horizontal=TRUE, cont=wp_marco)
		addSpace(marco_tablas,value=5,horizontal=TRUE)
		objetivos_asignatura <- gdf(editarobjetivos_df(info_materia), 
			container=marco_tablas)
		size(objetivos_asignatura) <- c(316,195)
		gtk_objetivos_asignatura <- getToolkitWidget(objetivos_asignatura)
		gtkTreeViewSetGridLines(gtk_objetivos_asignatura,grid.lines=3)
		col0 <- gtkTreeViewGetColumn(gtk_objetivos_asignatura, 0)
		col1 <- gtkTreeViewGetColumn(gtk_objetivos_asignatura, 1)
		col2 <- gtkTreeViewGetColumn(gtk_objetivos_asignatura, 2)
		gtkTreeViewColumnSetClickable(col0, FALSE)
		gtkTreeViewColumnSetClickable(col1, FALSE)
		gtkTreeViewColumnSetClickable(col2, FALSE)
		rend1 <- gtkCellLayoutGetCells(col1)
		rend1[[1]]$set(xalign = 0.5)
		rend2 <- gtkCellLayoutGetCells(col2)
		rend2[[1]]$set(xalign = 0.5)
		col0_header <- gtkLabelNew("Objetivo", show = TRUE)
		col1_header <- gtkLabelNew("Ponderación", show = TRUE)
		col2_header <- gtkLabelNew("¿Evaluado por asesor?", show = TRUE)
		gtkLabelSetWidthChars(col2_header, 10)
		gtkLabelSetLineWrap(col2_header, TRUE)
		gtkTreeViewColumnSetWidget(col0, widget = col0_header)
		gtkTreeViewColumnSetWidget(col1, widget = col1_header)
		gtkTreeViewColumnSetWidget(col2, widget = col2_header)
		#ahora que tenemos la información de los objetivos, podemos 
		#calcular el total de las ponderaciones
		total_pond <- glabel(sum(objetivos_asignatura[,1]),editable=FALSE,
			container=cdedom_line_frame)
		addSpring(cdedom_line_frame)
		#cuadro de evaluaciones de asignatura
		addSpring(marco_tablas)
		addSpace(marco_tablas,value=4,horizontal=TRUE)
		gseparator(container=marco_tablas,horizontal=FALSE)
		addSpring(marco_tablas)
		evaluaciones_asignatura <- gdf(info_materia$evaluaciones,
			container=marco_tablas)
		size(evaluaciones_asignatura) <- c(230,195)
		gtk_evaluaciones_asignatura <- getToolkitWidget(evaluaciones_asignatura)
		gtkTreeViewSetGridLines(gtk_evaluaciones_asignatura,grid.lines=3)
		col0 <- gtkTreeViewGetColumn(gtk_evaluaciones_asignatura, 0)
		col1 <- gtkTreeViewGetColumn(gtk_evaluaciones_asignatura, 1)
		gtkTreeViewColumnSetClickable(col1, FALSE)
		rend1 <- gtkCellLayoutGetCells(col1)
		rend1[[1]]$set(xalign = 0.5)
		gtkTreeViewRemoveColumn(gtk_evaluaciones_asignatura, col0)
		col1_header <- gtkLabelNew("Pruebas / trabajos corregidos por el asesor",
			show = TRUE)
		gtkLabelSetWidthChars(col1_header, 20)
		gtkLabelSetLineWrap(col1_header, TRUE)
		gtkTreeViewColumnSetWidget(col1, widget = col1_header)
		addSpace(marco_tablas,value=6,horizontal=TRUE)
		#el marco eliminar/insertar sirve para eliminar o insertar
		#objetivos y/o evaluaciones
		toolbar_inferior_izq_list <- list(
			sep0 = glabel(paste(rep(" ",20),collapse=""),editable=FALSE),
			Ins_obj = gaction("Insertar objetivo",icon="add",
				handler=function(h,...) {
					#objetivos_asignatura se convierte en una lista cuando
					#tiene una sola fila !!!
					objetivos_asignatura[] <-rbind(objetivos_asignatura[],
						c(pond=1L,ev=1L) )
					update(objetivos_asignatura)
					svalue(total_pond) <- sum(objetivos_asignatura[,1])
					update(total_pond)
				},
				tooltip="Agregar objetivo"),
			sep1 = glabel(paste(rep(" ",8),collapse=""),editable=FALSE),
			Elim_obj = gaction("Eliminar objetivo",icon="remove",
				handler=function(h,...) {
					#objetivos_asignatura se convierte en una lista cuando
					#tiene una sola fila !!!
					if (class(objetivos_asignatura[])=="data.frame") {
						n <- nrow(objetivos_asignatura[])
						if (n>1) {
							objetivos_asignatura[] <-
								as.data.frame(objetivos_asignatura[-n,])
							update(objetivos_asignatura)
							svalue(total_pond) <- sum(objetivos_asignatura[,1])
							update(total_pond)
						}
					}
				},
				tooltip = "Elimina el último objetivo" )
		)
		toolbar_inferior_der_list <- list(
			Ins_eva = gaction("Agregar evaluación",icon="add",
				handler=function(h,...) {
					n <- length(evaluaciones_asignatura[])
					evaluaciones_asignatura[] <- c(evaluaciones_asignatura[],
						svalue(toolbar_inferior_der_list$Tipo_eva) )
					update(evaluaciones_asignatura)
				},
				tooltip="Agregar evaluación"),
			Tipo_eva = gcombobox(items=tipos_evaluacion,editable=FALSE,
				selected=1),
			sep3=glabel(paste(rep(" ",6),collapse=""),editable=FALSE),
			Elim_eva = gaction("Eliminar evaluación", icon="remove",
				handler=function(h,...) {
					n <- length(evaluaciones_asignatura[])
					if (n>0) {
						evaluaciones_asignatura[] <- evaluaciones_asignatura[-n]
						update(evaluaciones_asignatura)
					}
				},
				tooltip="Eliminar la última evaluación en la lista")
		)
		toolbar_inferior_izq <- gtoolbar(toolbar_inferior_izq_list)
		toolbar_inferior_der <- gtoolbar(toolbar_inferior_der_list)
		size(toolbar_inferior_izq) <- c(363,35)
		size(toolbar_inferior_der) <- c(277,35)
		barra_inferior <- gpanedgroup(toolbar_inferior_izq,
			toolbar_inferior_der, container=wp_marco)
		#-----------------
		svalue(barra_status) <- ""
		Sys.sleep(0.001)
		visible(wp) <- TRUE
	} #ventana edicion materias
