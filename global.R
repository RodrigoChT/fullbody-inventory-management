#### Install packages ----
# install.packages('shiny')
# install.packages('shinyjs')
# #install.packages('rmarkdown')
# install.packages('gtools')
# install.packages('knitr')
# install.packages('pander')
# install.packages('xtable')
# install.packages('shinythemes')
# install.packages('magrittr')
# install.packages('dplyr')
# install.packages('DT')
# install.packages('lubridate')

library(shiny)
library(shinyjs)
library(rmarkdown)
library(gtools)
library(knitr)
library(pander)
library(xtable)
library(shinythemes)
library(magrittr)
library(dplyr)
# new libraries
library(purrr)
library(stringr)
library(openxlsx)

#### Parameters ----
dt.options <- list(dom = 'Bfrtip',
                   buttons = c('csv',
                               'excel',
                               'print'),
                   pageLength = 20
                   )


doc.types <- c('Boletas',
               'Facturas',
               'Guias',
               'Nota de credito',
               'Proforma'
)

default.price.type <- 'Profesor'

geo.zones <- list(lima.districts = read.csv('./Data/geo.sites.csv'),
                  peru.depts = c('-',
                                 'Amazonas',
                                 'Áncash',
                                 'Apurímac',
                                 'Arequipa',
                                 'Ayacucho',
                                 'Cajamarca',
                                 'Callao',
                                 'Cuzco',
                                 'Huancavelica',
                                 'Huánuco',
                                 'Ica',
                                 'Junín',
                                 'La Libertad',
                                 'Lambayeque',
                                 'Lima',
                                 'Loreto',
                                 'Madre de Dios',
                                 'Moquegua',
                                 'Pasco',
                                 'Piura',
                                 'Puno',
                                 'San Martín',
                                 'Tacna',
                                 'Tumbes',
                                 'Ucayali',
                                 'Extranjero')
)
geo.zones$lima.districts <- c('-', as.character(geo.zones$lima.districts$lima.districts))

counterpart.vars <- c('Tipo',
                      'Nombre',
                      'Entidad',
                      'Documento',
                      'Contacto',
                      'Provincia',
                      'Distrito')

inventory.info.cols <- c('Producto',
                         'Tipo',
                         'Nombre',
                         'Total')

transactions.classes <- list(no.price = c('transfer',
                                          'hold',
                                          'wildcard'),
                             price.number = c('supplier',
                                              'supplier2',
                                              'credit'),
                             price.type = c('sale'))

transactions.translations <- c('sale' = 'Venta',
                               'supplier' = 'Proveedor Recepcion',
                               'supplier2' = 'Proveedor Devolucion',
                               'transfer' = 'Traslado',
                               'hold' = 'Separacion',
                               'credit' = 'Nota de credito',
                               'wildcard' = 'Regularizacion')

transactions.translations2 <- c('Venta' = 'sale',
                                'Proveedor Recepcion'='supplier',
                                 'Proveedor Devolucion'='supplier2',
                                 'Traslado'='transfer',
                                 'Separacion'='hold',
                                'Nota de credito' = 'credit',
                                'Regularizacion' = 'wildcard')


ids.translations <- c('sale' = 'V',
                      'supplier' = 'PR',
                      'supplier2' = 'PD',
                      'transfer' = 'T',
                      'hold' = 'S',
                      'credit' = 'NC',
                      'wildcard' = 'RE')

ids.translations2 <- c('V' = 'sale',
                      'PR' = 'supplier',
                      'PD' = 'supplier2',
                      'T' = 'transfer',
                      'S' = 'hold',
                      'NC' = 'credit',
                      'RE' = 'wildcard')

transaction.columns <- data.frame(internal = c('quantity',
                                               'prod',
                                               'price',
                                               'price.actual'),
                                  external = c('Cantidad',
                                               'Producto',
                                               'Precio.Tipo',
                                               'Precio')
                                  )

mod.inv.basic <- c('sale',
                        'hold',
                        'transfer')
mod.inv.basic.reason <- data.frame(type = 'supplier')

transaction.columns <- data.frame(lapply(transaction.columns, as.character),
                                  stringsAsFactors = F)

transaction.direction <- list(incoming = c('supplier', 'credit'),
                              outgoing = c('sale', 'supplier2', 'transfer'),
                              hold = c('hold'),
                              depends.reason = c('wildcard'))

psswrd <- 'condoRito64' # password is stored this way because the feature is not really used

sidebar.size <- 2

credit.notes.date.limit <- 15

#### Utils ----
operation_datatable <- function(cols, obs) {
  operation <- data.table(placeholder = NA)
  operation[, (cols) := NA,]
  operation[, placeholder := NULL]
  
  for (i in 1:length(obs)) {
    operation[i, (cols) := isolate(get(paste0(cols)))]
  }
  
}

simple_cap <- function(x) {
  s <- tolower(strsplit(x, " ")[[1]])
  paste(toupper(substring(s, 1,1)), substring(s, 2),
        sep = "", collapse = " ")
}

input_widgets <- function(transaction, 
                          category = NULL,
                          check = F) {
  
  tagList(inputPanel(
    uiOutput(paste0(transaction, '.date')),
    uiOutput(paste0(transaction, '.id')),
    uiOutput(paste0(transaction, '.notes')),
    uiOutput(paste0(transaction, '.store')),
    uiOutput(paste0(transaction, '.counterpart')),
    movement.reason.input(paste0(transaction, '.reason'),
                          transaction),
    actionButton(paste0(transaction, '.start'),
                 'Empezar transacción'),
    actionButton(paste0(transaction, '.process'),
                 'Procesar transacción',
                 icon = icon('usd',
                             lib = 'glyphicon')),
    if (check == T) {
      actionButton(paste0(transaction, '.check'),
                   'Revisar transacción'
      )
    }
    )
  )
    
}

product_widgets <- function(transaction) {
  tagList(uiOutput(paste0(transaction, '.product')),
  actionButton(paste0(transaction, '.add'), 
               'Añadir producto',
               icon = icon('plus')),
  actionButton(paste0(transaction, '.cancel'),
               'Cancelar transaccion')
  )
}

input_widgets_counterparts <- function(transaction) {
  tagList(
    selectizeInput(paste0(transaction, '.type'),
                   'Cliente o proveedor:',
                   choices = c('Cliente', 'Proveedor'),
                   selected = 'Cliente'),
    conditionalPanel(
      condition = "input.registerCounterpartAM == 'Modificar'",
      uiOutput('modify.counterpart')
    ),
    textInput(paste0(transaction, '.name'),
              'Nombre:'),
    textInput(paste0(transaction, '.entity.code'),
              'Código de entidad:'),
    textInput(paste0(transaction, '.phone'),
              'Telefono:'),
    textInput(paste0(transaction, '.id'),
              'DNI / RUC:'),
    selectizeInput('newClientDept',
                   'Departamento:',
                   choices = geo.zones$peru.depts,
                   selected = geo.zones$peru.depts[1]),
    conditionalPanel(
      condition = "input.newClientDept == 'Lima'",
      selectizeInput(paste0(transaction, '.district'),
                     'Distrito limeño:',
                     choices = geo.zones$lima.districts,
                     selected = geo.zones$lima.districts[1])
    ),
    actionButton(paste0(transaction, '.process'),
                 'Registrar')
  )
  
}

input_widgets_price_types <- function() {
  tagList(
    conditionalPanel(
      condition = "input.priceTypesAction == 'Agregar'",
      textInput('price.type.new',
                'Precio')
    ),
    conditionalPanel(
      condition = "input.priceTypesAction == 'Eliminar'",
      uiOutput('price.type.existing')
    )
  )
}

#### Static input functions ----
payment.input <- function(id) {
  selectizeInput(id,  # cant use "."
                 'Medio de pago:',
                 choices = c('Contado',
                             'Credito',  # TODO: chequear tildes
                             'Visa',
                             'Mixto'))
}

product.dummy.input <- function(id, selected) {
  selectizeInput(id,
                 'Producto',
                 choices  = mixedsort(selected),
                 selected = selected)
}

movement.reason.input <- function(id = NULL, type = NULL) {
  show.input <- F
  if (type == 'supplier2') {
    choices <- c('Arreglo', 'Sin arreglo', 'Muestra')
    show.input <- T 
  } else if (type == 'supplier') {
    choices <- c('Ingreso nuevo', 'Devolución de arreglo', 'Devolución de muestra')
    show.input <- T
  }
  if (show.input) {
    selectizeInput(id,
                   'Razón',
                   choices = choices)
  }
  
}

#### Warnings ----
warning.ongoing.transaction <- function() {
  showModal(modalDialog(
    title     = 'ERROR',
    'Hay otra transacción empezada. Cancélala o procésala primero.',
    footer    = modalButton('OK', icon = icon('ok', lib = "glyphicon")),
    easyClose = T
  ))
}

warning.duplicated.object <- function(object.type) {
  showModal(modalDialog(
    title     = 'ERROR',
    paste0('Ya existe un ', object.type, ' con ese nombre.'),
    footer    = modalButton('OK', icon = icon('ok', lib = "glyphicon")),
    easyClose = T
  ))
}

warning.generic.problem <- function(message = NA_character_) {
  if (is.na(message)) {
    message <- paste0('Algo salió mal, contactar a Rodrigo Ch.')
  }
  showModal(modalDialog(
    title     = 'ERROR',
    message,
    footer    = modalButton('OK', icon = icon('ok', lib = "glyphicon")),
    easyClose = T
  ))
}

#### Confirmations ----
confirmation.registered <- function(object.type) {
  showModal(modalDialog(
    title     = 'Confirmación',
    paste0(object.type, ' registrado.'),
    footer    = modalButton('OK', icon = icon('ok', lib = "glyphicon")),
    easyClose = T
  ))
}

confirmation.operation <- function() {
  showModal(modalDialog(
    title     = 'Confirmación',
    'Operación exitosa.',
    footer    = modalButton('OK', icon = icon('ok', lib = "glyphicon")),
    easyClose = T
  ))
}