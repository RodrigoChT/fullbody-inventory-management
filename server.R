shinyServer(function(input, output, session) {
  
  #### Reactive utils ----
  start_transaction <- function(type) {
    
    # Get common values
    if (type == 'credit') {
      store <- unique(operations$credit$transaction$Origen)
      counterpart <- unique(operations$credit$transaction$Destino)
      reason <- unique(operations$credit$transaction$Razon)

    } else {
      store <- input[[paste0(type, '.store')]]
      counterpart <- input[[paste0(type, '.counterpart')]]
      reason <- input[[paste0(type, '.reason')]]
    }
    
    if (type == 'wildcard') {
      entity <- '' 
    } else {
      entity <- reac.data$counterparts$Entidad[reac.data$counterparts$Nombre == counterpart]
    }
    
    # Get id of previous transaction
    previous.transaction <- max(as.numeric(gsub(ids.translations[[type]], 
                                     '',
                                     operations$transactions$ID[grep(ids.translations[[type]],
                                                                     operations$transactions$ID)]
                                          )
                                ))
    
    previous.transaction <- as.numeric(max(c(previous.transaction, 0), na.rm = T))
    
    next.transaction <- paste0(ids.translations[[type]],
                               previous.transaction + 1)
    
    # Initialize current transaction
    operations[[type]]$items <- data.frame(# Common and repeated info
                                           Fecha = format(rep(input[[paste0(type, '.date')]],
                                                              length(inserted))),
                                           ID = next.transaction,
                                           Entidad = entity,
                                           Notas = input[[paste0(type, '.notes')]],
                                           Documento.Interno = input[[paste0(type, '.id')]],
                                           Transaccion = transactions.translations[[type]],
                                           
                                           # Common and non repeated info
                                           Producto = NA,
                                           
                                           # Info specific to transaction type
                                           Medio.Pago = NA,
                                           Razon = NA,
                                           Revertida = 0,
                                           Credito = NA,
                                           Relacion = NA
                                           )
    
    if (type %in% c('sale')) {
      operations[[type]]$items$Medio.Pago <- input[[paste0(type, 'Payment')]]
    } else if (type %in% c('supplier','supplier2')) {
      operations[[type]]$items$Razon <- reason
    } else if (type %in% c('hold')) {
      operations[[type]]$items$Razon <- counterpart
    }
    
    if (type %in% 'credit') {
      operations[[type]]$items$Relacion <- unique(operations$credit$transaction$ID)
    }
    
    if (type %in% transactions.classes$no.price) {
      registered.transactions <- transaction.columns[!transaction.columns$internal %in% c('price',
                                                                                          'price.actual'),]
      operations[[type]]$items$Precio <- NA
      operations[[type]]$items$Precio.Tipo <- NA
    } else if (type %in% transactions.classes$price.number) {
      # NOTE: the column Precio.Tipo is going to be cut to the column Precio in this case
      registered.transactions <- transaction.columns[!transaction.columns$internal %in% c('price.actual'), ]
      operations[[type]]$items$Precio.Tipo <- NA
    } else if (type %in% transactions.classes$price.type) {
      registered.transactions <- transaction.columns
    }
    
    # Determine incoming and outgoind locations
    if (type %in% transaction.direction$incoming) {
      operations[[type]]$items$Destino <- store
      operations[[type]]$items$Origen <- counterpart
    } else if (type %in% transaction.direction$outgoing) {
      operations[[type]]$items$Origen <- store
      operations[[type]]$items$Destino <- counterpart
    } else if (type %in% transaction.direction$hold) {
      operations[[type]]$items$Origen <- store
      operations[[type]]$items$Destino <- 'Separaciones'
    } else if (type %in% transaction.direction$depends) {
      if (input[[paste0(type, '.action')]] == 'Disminuir') {
        operations[[type]]$items$Origen <- input[[paste0(type, '.store.provider')]]
        operations[[type]]$items$Destino <- NA
      } else if (input[[paste0(type, '.action')]] == 'Aumentar') {
        operations[[type]]$items$Destino <- input[[paste0(type, '.store.provider')]]
        operations[[type]]$items$Origen <- NA
      }
    }
        
    # Insert non identical values
    for (i in 1:dim(registered.transactions)[1]) {
      if (registered.transactions$internal[i] == 'price.actual') {
        
        # For the rest...
        for (j in 1:length(inserted)) {
          
          current.price <- input[[paste0(type,
                                         '.price.id',
                                         inserted[j])]]
          if (current.price == 'Otro') {
            operations[[type]]$items$Precio[j] <- input[[paste0(type,
                                                                '.price.other.id',
                                                                inserted[j])]]
          } else if (current.price == 'Gratuito') {
            operations[[type]]$items$Precio[j] <- 0
            
          } else {
            operations[[type]]$items$Precio[j] <- reac.data$products[[current.price]][reac.data$products$Codigo ==
                                                                                        input[[paste0(type,
                                                                                                      '.prod.id',
                                                                                                      inserted[j])]]]
          }
          
          
          
        }
      } else {
        for (j in 1:length(inserted)) {
          # Insert price for credit note
          if (type == 'credit' & registered.transactions$internal[i] == 'price') {
            operations[[type]]$items$Precio[j] <- operations$credit$transaction[operations$credit$transaction$Producto == input[[paste0(type,
                                                                                                                                        '.',
                                                                                                                                        'prod',
                                                                                                                                        '.id',
                                                                                                                                        inserted[j])
                                                                                                                                 ]],
                                                                                'Precio'
                                                                                ]
          } else {
            operations[[type]]$items[[registered.transactions$external[i]]][j] <- input[[paste0(type,
                                                                                                '.',
                                                                                                registered.transactions$internal[i],
                                                                                                '.id',
                                                                                                inserted[j]
            )]]
          }
          
          
          
        }
      }
      
    }
    
    # Rename price columns for some transactions
    if (type %in% setdiff(transactions.classes$price.number, 'credit')) {
      operations[[type]]$items$Precio <- operations[[type]]$items$Precio.Tipo
      operations[[type]]$items$Precio.Tipo <- NA
    }

    # Calculate total per product
    operations[[type]]$items$Total <- as.numeric(operations[[type]]$items$Precio) * 
      as.numeric(operations[[type]]$items$Cantidad)
  }
  
  clean_inputs <- function(type) {
    # Close ongoing transaction
    operations$ongoing.transaction <- F
    
    # Reset date
    updateDateInput(session, paste0(type, '.date'), value = Sys.Date())
    
    # Remove items from inserted list
    for (item in inserted) {
      removeUI(selector = paste0('#', type, 'ItemId', item))
    }
    
    # Clean inserted list
    inserted <<- c()
  }
  
  
  finish_transaction <- function(type, process = F) {
  
      if (process == T) {

      # Get ID of previous transaction to modify
      if (type %in% 'credit') {
        ID <- input$credit.trans
      } else {
        ID <- NULL
      }
      
      # Modify inventory
      operation.result <- modify_inventory(type,
                                           ID = ID)
      
      if (operation.result == 'valid') {
        # Register transaction
        operations[[type]]$items <- operations[[type]]$items[, order(names(operations$transactions))]
        operations$transactions <- rbind(operations$transactions,
                                         operations[[type]]$items)
        
        # Save transaction file
        write.csv(operations$transactions,
                  './Data/transactions.csv',
                  row.names = F)
        
        file.copy('./Data/transactions.csv',
                  './Data compartida/Transacciones_vista.csv',
                  overwrite = T)
        
        # Clean inputs
        clean_inputs(type)
        
        # Message box
        showModal(modalDialog(
          title     = 'Confirmacion',
          'Transaccion procesada.',
          footer    = modalButton('OK', icon = icon('ok', lib = "glyphicon")),
          easyClose = T
        ))
      } else {
        return(NULL)
      }}
    
    # Clean inputs
    clean_inputs(type)
    operations[[type]]$items <- NULL
    
    # Unblock stores
    if (type %in% c('supplier')) {
      shinyjs::enable(paste0(type, '.reason'))
      shinyjs::enable(paste0(type, '.counterpart'))
    } else if (type %in% c('sale')) {
      shinyjs::enable(paste0(type, '.store'))
      shinyjs::enable(paste0(type, '.default.price'))
    } else if (type %in% c('wildcard')) {
      shinyjs::enable(paste0(type, '.store.provider'))
      shinyjs::enable(paste0(type, '.action'))
    } else {
      shinyjs::enable(paste0(type, '.store'))
    }
    
    operations[[type]]$valid <- FALSE
    
  }
  
  modify_inventory <- function(type, revert = F, ID = NULL){

    if (type == 'credit') {
      revert <- T
      type.orig <- 'credit'
    } else {
      type.orig <- type
    }
    
    # Get transaction data
    if (revert) {
      
      if (type == 'credit') {
        transaction <- operations[[type]]$items
        type <- transactions.translations2[[as.character(unique(transaction$Transaccion))]]
      } else {
        transaction <- operations$transactions[operations$transactions$ID %in% ID, ]
      }
    } else {
      transaction <- operations[[type]]$items
    }
    
    # Extract data from transaction
    origin <- as.character(unique(transaction$Origen))
    reason <- as.character(unique(transaction$Razon))
    counterpart <- as.character(unique(transaction$Destino))
    prods <- transaction$items$Producto
    operations$inventory <- merge(operations$inventory,
                                  transaction[, names(transaction) %in% c('Producto', 'Cantidad')],
                                  by = 'Producto',
                                  all = T)
    operations$inventory$Cantidad[is.na(operations$inventory$Cantidad)] <- 0
    move.from.origin <- type %in% c('sale',
                                    'hold',
                                    'transfer',
                                    'supplier2') | 
                       (type == 'supplier' & reason %in% c('Devolución de arreglo', 'Devolución de muestra')) | #NOTE: hardcoded
                       (type == 'wildcard' & input$wildcard.action == 'Disminuir' & revert == F) |	
                       (type == 'wildcard' & revert == T & !is.na(origin))
    move.to.counterpart <- type %in% c('hold',
                                       'supplier',
                                       'transfer',
                                       'credit') |
                           (type %in% 'supplier2' & reason %in% c('Arreglo', 'Muestra')) |
                           (type == 'wildcard' & input$wildcard.action == 'Aumentar' & revert == F)	
                           (type == 'wildcard' & revert == T & !is.na(counterpart))
    
    if (move.from.origin){
      if (revert) {
        operations$inventory[[origin]] <- operations$inventory[[origin]] + 
          operations$inventory$Cantidad
      } else {
        operations$inventory[[origin]] <- operations$inventory[[origin]] - 
          operations$inventory$Cantidad
      }
      
      
    }

    if (move.to.counterpart) {
      if (revert & !type %in% 'credit') {
        operations$inventory[[counterpart]] <- operations$inventory[[counterpart]] -
          operations$inventory$Cantidad
      } else {
        operations$inventory[[counterpart]] <- operations$inventory[[counterpart]] +
          operations$inventory$Cantidad
      }
      
    }
    
    # Check that there are no negative numbers
    negative.stock.products <- apply(operations$inventory[, !names(operations$inventory) %in% inventory.info.cols],
                                     1,
                                     min) < 0
    
    if (any(negative.stock.products)) {
      # Message box
      showModal(modalDialog(
        title     = 'ERROR',
        paste0('El resultado de la transaccion produce valores negativos en el inventario y/o el inventario ya contenia valores negativos. ',
               'Producto(s): ',
               paste0(operations$inventory$Producto[negative.stock.products],
                      collapse = ', '),
               '.'),
        footer    = modalButton('OK', icon = icon('ok', lib = "glyphicon")),
        easyClose = T
      ))
      
      # Recover original inventory
      operations$inventory <- read.csv('./Data/Inventario.csv')
      
      return('invalid')
    }
    
    # Get rid of quantity column
    operations$inventory$Cantidad <- NULL
    
    # Recalculate total
    operations$inventory$Total <- rowSums(operations$inventory[, !names(operations$inventory) %in% c('Nombre',
                                                                                                     'Tipo',
                                                                                                     'Producto',
                                                                                                     'Total')])
    
    # Save inventory file
    write.csv(operations$inventory,
              './Data/Inventario.csv',
              row.names = F)
    
    file.copy('./Data/Inventario.csv',
              './Data compartida/Inventario_vista.csv',
              overwrite = T)
    
    
    # Mark as reverted in transaction
    if (revert) {
      if (type == 'credit') {
        operations$transactions$Credito[operations$transactions$ID %in% ID] <- 1
      } else {
        operations$transactions$Revertida[operations$transactions$ID %in% ID] <- 1
      }
      
      write.csv(operations$transactions,
                './Data/transactions.csv',
                row.names = F)
      
      file.copy('./Data/transactions.csv',
                './Data compartida/Transacciones_vista.csv',
                overwrite = T)
    }

    if (revert & type == 'credit') {
      render(input = "report.Rmd",
             output_format = "pdf_document",
             output_file = paste0(unique(transaction$ID), '.pdf'),
             output_dir = "Reportes",
             params = list(trs    = transaction,
                           revert = FALSE))

    } else if (!revert) {
      render(input = "report.Rmd",
             output_format = "pdf_document",
             output_file = paste0(unique(transaction$ID), '.pdf'),
             output_dir = "Reportes",
             params = list(trs    = transaction,
                           revert = revert))
    }

    return('valid')
    
  }
  
  update_sales_summary <- function(type, store, dates) {

    pre_filter_data <- operations$transactions %>% 
      filter(Fecha <= dates[2] & Fecha >= dates[1] & (Origen == store | Destino == store) & Revertida == 0)
    
    
    if ('Ventas' %in% type) {
      only_sales <- pre_filter_data %>%
      filter(Transaccion %in% c('Venta')) %>%
      group_by(ID) %>%
      summarise(
        Fecha = unique(Fecha),
        Tienda = unique(Origen),
        Cliente = unique(Destino),
        Total = sum(Total),
        Notas = unique(Notas)
      )
    } else {
      only_sales <- NULL
    }
    
    if ('Notas de credito' %in% type) {
      only_credit_notes <- pre_filter_data %>%
      filter(Transaccion %in% c('Nota de credito')) %>%
      group_by(ID) %>%
      summarise(
        Fecha = unique(Fecha),
        Tienda = unique(Destino),
        Cliente = unique(Origen),
        Total = sum(Total),
        Notas = unique(Notas)
      )
    } else {
      only_credit_notes <- NULL
    }
    
    if (is.null(only_sales)) {
      operations$sales.summary <- only_credit_notes
    } else if (is.null(only_credit_notes)) {
      operations$sales.summary <- only_sales
    } else {
      operations$sales.summary <- rbind(only_sales, only_credit_notes)
    }
    
  }
  
  update_clothes_summary <- function(type, store, dates) {
    pre_filter_data <- operations$transactions %>% 
      filter(Fecha <= dates[2] & Fecha >= dates[1] & (Origen == store | Destino == store) & Revertida == 0)
    
    if ('Ventas' %in% type) {
      only_sales <- pre_filter_data %>%
        filter(Transaccion %in% c('Venta')) %>%
        group_by(ID, Producto) %>%
        summarise(
          Fecha = unique(Fecha),
          Tienda = unique(Origen),
          Cliente = unique(Destino),
          Cantidad = sum(Cantidad),
          Notas = unique(Notas)
        )
    } else {
      only_sales <- NULL
    }
    
    if ('Notas de credito' %in% type) {
      only_credit_notes <- pre_filter_data %>%
        filter(Transaccion %in% c('Nota de credito')) %>%
        group_by(ID, Producto) %>%
        summarise(
          Fecha = unique(Fecha),
          Tienda = unique(Destino),
          Cliente = unique(Origen),
          Cantidad = sum(Cantidad),
          Notas = unique(Notas)
        )
    } else {
      only_credit_notes <- NULL
    }
    
    if (is.null(only_sales)) {
      operations$clothes.summary <- only_credit_notes
    } else if (is.null(only_credit_notes)) {
      operations$clothes.summary <- only_sales
    } else {
      operations$clothes.summary <- rbind(only_sales, only_credit_notes)
    }

    if (dim(operations$clothes.summary[1]) > 0) {
      ids <- unique(operations$clothes.summary$ID)
      idx <- match(ids, operations$clothes.summary$ID)
      idx_erase <- setdiff(1:dim(operations$clothes.summary)[1], idx)
      operations$clothes.summary$ID[idx_erase] <- ''
    }
    
    
  }
  
  #### Load data ----
  transactions <- read.csv('./Data/transactions.csv',
                           stringsAsFactors = F)
  
  if (!'Notas' %in% names(transactions)) {
    transactions$Notas <- NA  
  }
  
  inventory <- read.csv('./Data/Inventario.csv')

  if (!'Total' %in% names(inventory)) {
    inventory$Total <- 0  
  }
  stores <- read.csv('./Data/Locales.csv')
  products <- read.csv('./Data/Productos.csv') %>% 
    arrange(desc(Codigo))  # TODO: Import pipeline operator
  counterparts <- read.csv('./Data/Contrapartes.csv',
                           stringsAsFactors = F,
                           colClasses = 'character',
                           encoding = 'latin1')
  prod.types <- read.csv('./Data/Tipos de producto.csv')
  
  ## Extract some specific info
  price.types <- names(products)[!names(products) %in% c('Tipo', 'Nombre', 'Codigo')]
  
  #### Initialize reactive values ----
  operations <- reactiveValues(sale = list(),
                               transactions = transactions,
                               sales.summary = NA,
                               clothes.summary = NA,
                               inventory = inventory,
                               tests = list(),
                               ongoing.transaction = F
  )
  reac.data <- reactiveValues(counterparts = counterparts,
                              prod.types = prod.types,
                              products = products,
                              price.types.input = NA,
                              price.types = price.types, 
                              stores = stores)
  
  updateActionButton(session,'sale.summary.create')
  
  #### Static inputs ----
  date.input <- function(id) {
    renderUI(
      dateInput(id,
                'Fecha de la transacción:',
                format = 'dd-mm-yyyy',
                language = 'es')
    )
  }
  
  
  id.input <- function(id) {
    renderUI(
      textInput(id,
                'Documento interno:')
    )
  }
  
  notes.input <- function(id) {
    renderUI(
      textInput(id,
                'Nota:')
    )
  }

  add.price.input <- function(type2, id) {
    numericInput(paste0(type2, '.price.', id),
                 label = id,
                 min = 0,
                 value = NA)
  }
  
  #### Dynamic inputs ----
  store.input <- function(id, role = NULL, stores.choices = NULL, include.sep = F, mult = F, default = NA) {
    if (is.null(role)) {
      mess <- 'Tienda: '
    } else if (role == 'receiving') {
      mess <- 'Tienda recibiendo: '
    } else if (role == 'giving') {
      mess <- 'Tienda entregando: '
    }
  
    if (is.null(stores.choices)) {
      if (include.sep) {
        renderUI(
          selectizeInput(id, 
                         mess,
                         choices = reac.data$stores$Nombre,
                         multiple = mult,
                         selected = default)
        )

      } else {
        renderUI(
          selectizeInput(id, 
                         mess,
                         choices = reac.data$stores$Nombre,
                         multiple = mult,
                         selected = default)
        )
        
      }
    } else {
      renderUI(
        selectizeInput(id, 
                       mess,
                       choices = mixedsort(stores.choices),
                         multiple = mult,
                       selected = default)
      )
    }
    
    
  }
  
  # NOTE: careful with this (usar proveedor o cliente en el resultado del input)
  counterpart.input <- function(id, type) {
    renderUI(
      selectizeInput(id,
                     paste0('Elegir ', type, ':'),
                     choices = mixedsort(reac.data$counterparts$Nombre[reac.data$counterparts$Tipo == type]))
    )
  } 
  
  
  product.input <- function(id, store, has.stock, selected = NULL) {
    if (has.stock) {
      renderUI(selectizeInput(id,
                              label    = 'Producto',
                              choices  = mixedsort(operations$inventory$Producto[operations$inventory[[store]] > 0]),
                              selected = selected))
      
    } else {
      renderUI(selectizeInput(id,
                              label    = 'Producto',
                              choices  = mixedsort(operations$inventory$Producto),
                              selected = selected))
    }
    
  }
  
  product.input.simple <- function(id, store, has.stock, selected = NULL) {
    
    if (has.stock) {
      selectizeInput(id,
                              label    = 'Producto',
                              choices  = mixedsort(operations$inventory$Producto[operations$inventory[[store]] > 0]),
                              selected = selected)
      
    } else {
      selectizeInput(id,
                              label    = 'Producto',
                              choices  = mixedsort(operations$inventory$Producto),
                              selected = selected)
    }
    
  }
  
  product.input.credit <- function(id, has.stock, transaction.items) {
    if (has.stock) {
      renderUI(selectizeInput(id,
                              label = 'Producto',
                              choices = mixedsort(transaction.items$Producto[transaction.items$Cantidad > 0])))
    } else {
      renderUI(selectizeInput(id,
                              label = 'Producto',
                              choices = mixedsort(transaction.items$Producto)))
    }
    
  }

  price.input <- function(id, type, selected = NULL) {
    if (type %in% c('supplier', 'supplier2')) {
      numericInput(id,
                   label = 'Precio',
                   value = 0)
    } else {
      choices <- c(reac.data$price.types,
                   'Otro',
                   'Gratuito')
      if (is.null(selected)) {
        selected <- default.price.type
      }
      selectizeInput(id,
                     label = 'Precio',
                     choices = mixedsort(choices),
                     selected = selected
                     )
    }
  }
  
  price.other.input <- function(id) {
    numericInput(id,
                 label = '',
                 value = 0)
  }
  
  
  quantity.input <- function(id, origin = NA, prod = NA, max.quantity = FALSE) {    # TODO: detect stock of prod in store
    
    if (max.quantity) {
      if (origin == 'prev.sale.trans') {
        top.limit <-  operations$credit$clean.transaction$Cantidad[operations$credit$transaction$Producto == prod]
      } else {
        top.limit <- isolate(operations$inventory[[origin]][operations$inventory$Producto %in% prod])
      }
    } else if (!max.quantity) {
      top.limit <- NA
    }
    numericInput(id,
                 label = ' Cantidad',
                 value = 1,
                 min   = 1,
                 max   = top.limit,
                 step  = 1
    )

  }
  
  trans.input <- function(id, type = NULL) {
    
    if (is.null(type)) {
      pattern <- ''
    } else {
      pattern <- ids.translations[[type]]
    }
    
    if (id == 'revert.trans') {
      choices <- grep(pattern,
                      unique(operations$transactions$ID[!operations$transactions$Revertida %in% 1 &
                                                          !operations$transactions$Credito %in% 1 &
                                                          !operations$transactions$Transaccion %in% 'Nota de credito'
                                                        ]),
                      value = T) %>% rev()
    } else {
      choices <- grep(pattern,
                      unique(operations$transactions$ID[!operations$transactions$Revertida %in% 1 &
                                                          as.Date(operations$transactions$Fecha) >=  Sys.Date() - credit.notes.date.limit
                                                        ]),
                      value = T) %>% 
        rev()
    }
      
    selectizeInput(id,
                   'Elegir transacción:',
                   choices = choices
                   )
  }
  
  prod.type.input <- function(id) {
    renderUI(selectizeInput(id,
                            'Tipo:',
                            choices = mixedsort(reac.data$prod.types$Codigo)))
  }
  
  inventory.cols.input <- function(id) {
    renderUI(selectizeInput(id,
                            'Stock por modificar:',
                            choices = mixedsort(names(operations$inventory)[!names(operations$inventory) %in% inventory.info.cols])
                            ))
  }
  
  #### Sale's input----
  inserted <- c()
  
  get_inputs <- function(type, max.quantity) {
    observeEvent(input[[paste0(type,'.add')]], {

      add <- input[[paste0(type, '.add')]]
      item.id <- paste0(type, 'ItemId', add)
      prod.id <- paste0(type, '.prod.id', add)
      quantity.id <- paste0(type, '.quantity.id', add)
      price.id <- paste0(type, '.price.id', add)
      price.id.amount <- paste0(type, '.price.id.amount', add)
      price.other.id <- paste0(type, '.price.other.id', add)
      remove.id <- paste0(type, '.remove.id', add)
      has.stock <- T
      
      # If the limit needs to come from the counterpart or another place
      if (type == 'supplier' & input$supplier.reason %in% c('Devolución de arreglo', 
                                                            'Devolución de muestra')) {  #NOTE: hardcoded
        max.quantity <- T
        origin <- isolate(input[[paste0(type, '.counterpart')]])
      } else if (type == 'credit') {
        origin <- 'prev.sale.trans'
      } else if (type == 'wildcard' & input$wildcard.action == 'Disminuir') {
        origin <- isolate(input[[paste0(type, '.store.provider')]])
        max.quantity <- T
      } else if (type == 'wildcard' & input$wildcard.action == 'Aumentar') {
        has.stock <- F
      } else {
        origin <- isolate(input[[paste0(type, '.store')]])
      }
      
      # Get default price
      if (type %in% c('sale')) {
        default.price <- input[[paste0(type, '.default.price')]]
      } else {
        default.price <- NULL
      }
      
      if (type %in% c('sale')) {
        
        insertUI(
          selector = paste0('#', type, 'List'),
          ui = tags$div(id = item.id,
                        tags$style(type='text/css', ".selectize-input {height: 42px; }"),
                        tags$div(style = 'margin-top: 20px; display:inline-block;vertical-align:top',
                                 product.input.simple(prod.id,
                                               origin,
                                               has.stock = T)
                                 ),
                        tags$div(style = 'margin-top: 20px; display:inline-block;vertical-align:top',
                                 quantity.input(quantity.id,
                                                max.quantity = FALSE)),
                        tags$div(style = 'margin-top: 20px; display:inline-block;vertical-align:top',
                                 price.input(price.id,
                                             type,
                                             default.price)),
                        tags$div(style = 'margin-top: 20px; display:inline-block;vertical-align:top',
                                 price.other.input(price.other.id)
                                 ),
                        tags$div(style = 'margin-top: 45px; display:inline-block;vertical-align:top',
                                 actionButton(remove.id, 
                                              label = '',
                                              icon = icon('remove-circle',
                                                          lib = 'glyphicon'))
                                 )
          )
        )
        
      } else if (type %in% c('supplier', 'supplier2')) {
        
        insertUI(
          selector = paste0('#', type, 'List'),
          ui = tags$div(id = item.id,
                        tags$style(type='text/css', ".selectize-input {height: 42px; }"),
                        tags$div(style = 'margin-top: 20px; display:inline-block;vertical-align:top',
                                 product.input.simple(prod.id,
                                                      origin,
                                                      has.stock = T)
                        ),
                        tags$div(style = 'margin-top: 20px; display:inline-block;vertical-align:top',
                                 quantity.input(quantity.id,
                                                max.quantity = FALSE)),
                        tags$div(style = 'margin-top: 20px; display:inline-block;vertical-align:top',
                                 price.input(price.id,
                                             type,
                                             default.price)),
                        tags$div(style = 'margin-top: 45px; display:inline-block;vertical-align:top',
                                 actionButton(remove.id, 
                                              label = '',
                                              icon = icon('remove-circle',
                                                          lib = 'glyphicon'))
                        )
          )
        )
        
      } else if (type %in% 'credit') {
        insertUI(
          selector = paste0('#', type, 'List'),
          ui = tags$div(id = item.id,
                        tags$div(style = 'display:inline-block',
                                 product.input.credit(prod.id,
                                                      has.stock = T,
                                                      operations$credit$clean.transaction)
                                 ),
                        tags$div(style = 'display:inline-block',
                                 quantity.input(quantity.id,
                                                max.quantity = FALSE)),
                        tags$div(style = 'display:inline-block',
                                 actionButton(remove.id, 
                                              label = '',
                                              icon = icon('remove-circle',
                                                          lib = 'glyphicon')))
          )
        )
      } else {
        insertUI(
          selector = paste0('#', type, 'List'),
          ui = tags$div(id = item.id,
                        tags$div(style = 'display:inline-block',
                                 #product.dummy.input(prod.id, isolate(input[[paste0(type, '.product')]]))
                                 product.input.simple(prod.id,
                                                      origin,
                                                      has.stock = has.stock)
                        ),
                        tags$div(style = 'display:inline-block',
                                 quantity.input(quantity.id,
                                                max.quantity = FALSE)),
                        tags$div(style = 'display:inline-block',
                                 actionButton(remove.id, 
                                              label = '',
                                              icon = icon('remove-circle',
                                                          lib = 'glyphicon')))
          )
        )
      }
      
      # Remove specific elements
      observeEvent(input[[remove.id]], {
        removeUI(selector = paste0('#', item.id))
        inserted <<- setdiff(inserted, add)
      })
      
      # Update prices
      observeEvent(c(input[[price.id]],
                     input[[prod.id]]) , {
        if (!is.null(input[[price.id]])) {
          if (input[[price.id]] == 'Otro') {
            value <- NA
          } else {
            value <- reac.data$products[reac.data$products$Codigo == input[[prod.id]],
                                        input[[price.id]]]
          }
          updateNumericInput(session,
                             price.other.id,
                             value = value)
        } 
        
      })
      
      inserted <<- c(inserted, add)
      
    })
  }

  #### Transaction Buttons ----
  start.transaction.button <- function(transaction, has.stock) {
    observeEvent(input[[paste0(transaction, '.start')]], {
      if (!operations$ongoing.transaction) {
        if (transaction %in% c('supplier')) {
          shinyjs::disable(paste0(transaction, '.reason'))
          shinyjs::disable(paste0(transaction, '.counterpart'))
        } else if (transaction == 'credit') {
          shinyjs::disable(paste0(transaction, '.trans'))
        } else if (transaction == 'wildcard') {
          shinyjs::disable('wildcard.store.provider')
          shinyjs::disable('wildcard.action')
        } else if(transaction == 'sale') {
          shinyjs::disable(paste0(transaction, '.store'))
          shinyjs::disable(paste0(transaction, '.default.price'))
        } else {
          shinyjs::disable(paste0(transaction, '.store'))
        }
        operations[[transaction]]$valid <- TRUE
        operations$ongoing.transaction <- T
        
        if (transaction == 'supplier' & input$supplier.reason %in% c('Devolución de arreglo', 'Devolución de muestra')) { #NOTE: hardcoded
          has.stock = T
          origin <- '.counterpart'
        } else if (transaction == 'wildcard' & input$wildcard.action == 'Disminuir') {
          has.stock = T
          origin <- '.store.provider'
        } else {
          origin <- '.store'
        }
        
        if (transaction == 'credit') {
          #transaction.items <- operations$transactions[operations$transactions$ID == input$credit.trans, ]
          # output[[paste0(transaction, '.product')]] <- product.input.credit(paste0(transaction, '.product'),
          #                                                                   has.stock = T,
          #                                                                   operations$credit$clean.transaction)
        } else {

          # output[[paste0(transaction, '.product')]] <- product.input(paste0(transaction, '.product'), 
          #                                                            input[[paste0(transaction, origin)]],
          #                                                            has.stock = has.stock)
        }
        
        
      } else {
        warning.ongoing.transaction()
      }
      
    })
  }
  
  cancel.transaction.button <- function(transaction) {
    observeEvent(input[[paste0(transaction, '.cancel')]], {
      if (transaction %in% c('supplier')) {
        shinyjs::enable(paste0(transaction, '.reason'))
        shinyjs::enable(paste0(transaction, '.counterpart'))
      } else if (transaction == 'credit') {
        shinyjs::enable(paste0(transaction, '.trans'))
      } else if (transaction == 'wildcard') {
        shinyjs::enable('wildcard.store.provider')
        shinyjs::enable('wildcard.action')
      } else if(transaction == 'sale') {
        shinyjs::enable(paste0(transaction, '.store'))
        shinyjs::enable(paste0(transaction, '.default.price'))
      } else {
        shinyjs::enable(paste0(transaction, '.store'))
      }
      finish_transaction(transaction, process = F)
      operations$ongoing.transaction <- F 
    })
  }
  
  add.item.transaction.button <- function(transaction) {
    observeEvent(input[[paste0(transaction, '.add')]], {
      
      # Ids
      add <- isolate(input[[paste0(transaction, '.add')]])
      item.id <- paste0(transaction, 'ItemId', add)
      prod.id <- paste0('prod.id', add)
      quantity.id <- paste0('quantity.id', add)
      remove.id <- paste0('remove.id', add)
      
      insertUI(
        selector = paste0('#', transaction, 'List'),
        ui = tags$div(id = item.id,
                     tags$div(style = 'display:inline-block',
                              renderText('asdf' #isolate(input[[paste0(transaction, 'product')]])
                                         ))
      ))})
  }
  
  process.transaction.button <- function(transaction,
                                         action = 'process',
                                         password = FALSE) {
    observeEvent(input[[paste0(transaction, '.', action)]], {
      
      # Check password
      if (any(c(!password,
                all(password, input[[paste0(transaction, '.password')]] %in% psswrd)))) {
        
        # Check that all prods are unique
        prod.list <- c()
        for (i in 1:length(inserted)) {
          prod.list <- c(prod.list, input[[paste0(transaction,
                                                  '.prod.id',
                                                  inserted[i])]])
        }
        
        if (transaction %in% c('credit',
                               'wildcard')) {
          origin.condition <- F
        } else {
          origin.condition <- input[[paste0(transaction,
                                            '.store')]] %in% input[[paste0(transaction, 
                                                                           '.counterpart')]]
        }
        
        if (length(inserted) > 0) {
          
          if (any(duplicated(prod.list))) {
            
            showModal(modalDialog(
              title     = 'ERROR',
              'Productos repetidos.',
              footer    = modalButton('OK', icon = icon('ok', lib = "glyphicon")),
              easyClose = T
            ))
            
          } else if (origin.condition) {
            showModal(modalDialog(
              title     = 'ERROR',
              'La tienda de origen y de destino son la misma.',
              footer    = modalButton('OK', icon = icon('ok', lib = "glyphicon")),
              easyClose = T
            ))
          
          } else {
            # Set ID and initialize output data frame
            start_transaction(transaction)
            
            if (action == 'check') {
              n.items <- dim(operations[[transaction]]$items)[1]
              operations[[transaction]]$items[n.items + 1, ] <- ''
              operations[[transaction]]$items$Total[n.items + 1] <- sum(as.numeric(operations[[transaction]]$items$Total),
                                                                        na.rm = T)
              operations[[transaction]]$items$Cantidad[n.items + 1] <- sum(as.numeric(operations[[transaction]]$items$Cantidad),
                                                                           na.rm = T)
              
              return()
            }
            
            if (transaction == 'sale') {
              if (sum(as.numeric(operations[[transaction]]$items$Total), na.rm = T) != 
                  sum(c(input$sale.cash,
                        input$sale.card,
                        input$sale.deposit.1,
                        input$sale.deposit.2,
                        input$sale.credit,
                        input$sale.advance),
                      na.rm = T)) {
                
                showModal(modalDialog(
                  title     = 'ERROR',
                  'El monto cobrado no cuadra con el precio total.',
                  footer    = modalButton('OK', icon = icon('ok', lib = "glyphicon")),
                  easyClose = T
                ))
                return()
              }
            }
            
            # Save new transactions and clean up
            finish_transaction(transaction, process = T)
            
            # Clean transaction table, but don't clear inputs
            operations[[transaction]]$items <- NULL
          }
          
        } else {
          showModal(modalDialog(
            title     = 'ERROR',
            'Elige por lo menos 1 producto.',
            footer    = modalButton('OK', icon = icon('ok', lib = "glyphicon")),
            easyClose = T
          ))
        }
          
        } else {
          # Wrong password
          showModal(modalDialog(
            title     = 'ERROR',
            'Clave equivocada!',
            footer    = modalButton('OK', icon = icon('ok', lib = "glyphicon")),
            easyClose = T
          ))
        }
      
      
      shinyjs::reset(paste0(transaction, '.password'))
      
        })
  }
  
  add.change.product <- function(type2, password = F) {
    
    observeEvent(input[[paste0(type2, '.process')]], {
      
      # Check password
      if (all(password,
              !input[[paste0(type2, '.password')]] %in% psswrd)) {
        
        showModal(modalDialog(
          title     = 'ERROR',
          'Clave equivocada!',
          footer    = modalButton('OK', icon = icon('ok', lib = "glyphicon")),
          easyClose = T)
        )
        
      } else {
        
        if (type2 == 'new.prod') {
          code <- paste0(input$new.prod.type,
                         '-',
                         simple_cap(input$new.prod.name))
        } else if (type2 == 'change.prices') {
          code <- input$change.prices.prod
        }
        
        if (operations$ongoing.transaction) {
          
          # There is an ongoing transaction, close it first.
          warning.ongoing.transaction()
          
          # Check for duplicates
        } else if (type2 == 'new.prod' & check.duplicate(code, 
                                                         reac.data$products$Codigo)) {
          warning.duplicated.object('producto')
          
        } else {
          
          if (type2 == 'new.prod') {
            
            # Add new product
            new.prod.df <- data.frame(Tipo = input$new.prod.type,
                                      Nombre = simple_cap(input$new.prod.name),
                                      Producto = code
            )
            
            # Write new inventory
            operations$inventory <- bind_rows(operations$inventory, new.prod.df)
            operations$inventory[dim(operations$inventory)[1], ][is.na(operations$inventory[dim(operations$inventory)[1], ])] <- 0
            write.csv(operations$inventory,
                      './Data/Inventario.csv',
                      row.names = F)
            
            file.copy('./Data/Inventario.csv',
                      './Data compartida/Inventario_vista.csv',
                      overwrite = T)
            
            names(new.prod.df)[names(new.prod.df) == 'Producto'] <- 'Codigo'
            
            for (i in reac.data$price.types) {
              new.prod.df[i] <- NA
            }
            
            reac.data$products <- rbind(reac.data$products, 
                                        new.prod.df)
          }
          
          for (i in reac.data$price.types) {
            input.name <- paste0(paste0(type2, '.price.'), i)
            reac.data$products[[i]][reac.data$products$Codigo == code] <- input[[input.name]]
          }
          
          # Write new prod table
          write.csv(reac.data$products,
                    './Data/Productos.csv',
                    row.names = F)
          
          # Sucess message
          showModal(modalDialog(
            title     = 'Operacion realizada',
            'El producto fue registrado.',
            footer    = modalButton('OK', icon = icon('ok', lib = "glyphicon")),
            easyClose = T
          ))
        }
        
        # Clean inputs
        updateTextInput(session,
                        'new.prod.name',
                        value = '')
        
        if (type2 == 'new.prod') {
          for (i in reac.data$price.types) {
            input.name <- paste0(paste0(type2, '.price.'), i)
            updateTextInput(session,
                            input.name,
                            value = '')
          }
        }
        
      }
      shinyjs::reset(paste0(type2, '.password'))
    })
  }

  #### Checks ----
  
  check.duplicate <- function(current.name, 
                              existing.names) {
    if (tolower(current.name) %in% tolower(existing.names)) {
      return(T)
    } else {
      return(F)
    }
  }
  
  
  #### Reports ----
  ## Transactions table
  output$transactions.table <- DT::renderDataTable({
    operations$transactions[seq(dim(operations$transactions)[1], 1), ]
  },
  rownames = F,
  server = T,
  filter = 'top',
  options = dt.options)
  
  # Download transactions table (table too large for using standard buttons)
  output$transactions.download <- downloadHandler(
    filename = function(){'transacciones.csv'}, 
    content = function(fname){
      write.csv(operations$transactions[seq(dim(operations$transactions)[1], 1), ],
                fname,
                row.names = F)
    }
  )
  
  ## ______Sales summary table ----
  output$sale.summary.store <- store.input('sale.summary.store', mult = T, default = 'Miraflores')

  observeEvent(input$sale.summary.create, {
    update_sales_summary(type = input$sale.summary.type,
                         store = input$sale.summary.store,
                         dates = c(input$sale.summary.start,
                                   input$sale.summary.end))
  })
  
  output$sales.summary.table <- DT::renderDataTable({

    operations$sales.summary#[seq(dim(operations$sales.summary)[1], 1), ]
  },
  rownames = F,
  server = T,
  filter = 'top',
  options = dt.options)

  # Download sales summary table (table too large for using standard buttons)
  output$sale.summary.download <- downloadHandler(
    filename = function(){'resumen_ventas.csv'},
    content = function(fname){
      write.csv(operations$sales.summary,
                fname,
                row.names = F)
    }
  )
  
  ## ______Clothes summary ----
  
  output$clothes.summary.store <- store.input('clothes.summary.store', mult = T, default = 'Miraflores')
  
  observeEvent(input$clothes.summary.create, {
    update_clothes_summary(type = input$clothes.summary.type,
                         store = input$clothes.summary.store,
                         dates = c(input$clothes.summary.start,
                                   input$clothes.summary.end))
    
  })
  
  output$clothes.summary.table <- DT::renderDataTable({
    
    operations$clothes.summary
  },
  rownames = F,
  server = T,
  filter = 'top',
  options = dt.options)
  
  # Download clothes summary table (table too large for using standard buttons)
  output$clothes.summary.download <- downloadHandler(
    filename = function(){'resumen_prendas.csv'},
    content = function(fname){
      write.csv(operations$clothes.summary,
                fname,
                row.names = F)
    }
  )
  
  ## Inventory table
  output$inventory.table <- DT::renderDataTable({
    
    df <- operations$inventory[order(operations$inventory$Producto) & operations$inventory$Total != 0, ]
    df[, c('Producto',
           'Tipo',
           'Nombre',
           as.character(reac.data$stores$Nombre),
           mixedsort(reac.data$counterparts$Nombre[reac.data$counterparts$Tipo == 'Proveedor']), 
           'Total')]
  },
  rownames = F,
  server = F,
  filter = 'top',
  extensions = c('Buttons'),
  options = dt.options)
  
  #### Transactions ----
  ## __Sale ----
  # Buttons
  start.transaction.button('sale', has.stock = T)
  cancel.transaction.button('sale')
  output$saleValid <- reactive({
    operations$sale$valid
  })
  outputOptions(output, "saleValid", suspendWhenHidden = FALSE)
  process.transaction.button('sale', 'process')
  process.transaction.button('sale', 'check')
  get_inputs('sale', max.quantity = T)
  
  
  # Static inputs
  output$sale.date <- date.input('sale.date')
  output$sale.store <- store.input('sale.store', include.sep = T)  # NOTE: avoid hardcoding
  output$sale.id <- id.input('sale.id')
  output$sale.notes <- notes.input('sale.notes')
  output$sale.counterpart <- counterpart.input('sale.counterpart', 'Cliente')
  output$sale.default.price <- renderUI({price.input('sale.default.price',
                                           'sale')})
  
  ## __ Credit note ----
  output$credit.trans <- renderUI(trans.input('credit.trans', type = 'sale'))
  output$credit.notes <- notes.input('credit.notes')
  
  observeEvent(input$credit.trans, {
    operations$credit$transaction <- operations$transactions[operations$transactions$ID %in% input$credit.trans, ]
    operations$credit$previous.orig <- operations$transactions[operations$transactions$Relacion %in% input$credit.trans, ]
    if (all(dim(operations$credit$previous.orig)) > 0) {
      operations$credit$previous <- summarise(group_by(operations$credit$previous.orig, Producto),
                                              'prev.cn' = sum(Cantidad, na.rm = T))
      operations$credit$clean.transaction <- merge(operations$transactions[operations$transactions$ID == input$credit.trans, ],
                                                   operations$credit$previous[, c('Producto', 'prev.cn')],
                                                   by = 'Producto',
                                                   all = T)
      operations$credit$clean.transaction$prev.cn[is.na(operations$credit$clean.transaction$prev.cn)] <- 0
      operations$credit$clean.transaction$Cantidad <-  operations$credit$clean.transaction$Cantidad -  operations$credit$clean.transaction$prev.cn
      operations$credit$clean.transaction$prev.cn <- NULL
    } else {
      operations$credit$clean.transaction <- operations$credit$transaction
    }
    
  })
  
  # Show transaction items
  output$transaction.to.credit <- renderDataTable({
    operations$credit$transaction
  })
  
  # Previous credit notes associated to that sale
  output$previous.credit.notes <- renderDataTable({
    operations$credit$previous.orig
  })
  
  start.transaction.button('credit', has.stock = T)
  output$creditValid <- reactive({
    operations$credit$valid
  })
  
  output$credit.date <- date.input('credit.date')
  output$credit.id <- id.input('credit.id')
  outputOptions(output, "creditValid", suspendWhenHidden = FALSE)
  cancel.transaction.button('credit')
  get_inputs('credit', max.quantity = T)
  process.transaction.button('credit')
  
  ## __Suplier, incoming ----
  # Buttons
  start.transaction.button('supplier', has.stock = F)  # NOTE: turns to true in a sub category
  cancel.transaction.button('supplier')
  output$supplierValid <- reactive({
    operations$supplier$valid
  })
  outputOptions(output, "supplierValid", suspendWhenHidden = FALSE)
  process.transaction.button('supplier')
  get_inputs('supplier', max.quantity = F)  # NOTE: turns to true in a sub category

  # Static inputs
  output$supplier.date <- date.input('supplier.date')
  output$supplier.store <- store.input('supplier.store')
  output$supplier.id <- id.input('supplier.id')
  output$supplier.notes <- notes.input('supplier.notes')
  output$supplier.counterpart <- counterpart.input('supplier.counterpart', 'Proveedor')
  
  ## __Suplier, returning ----
  # Buttons
  start.transaction.button('supplier2', has.stock = T)
  cancel.transaction.button('supplier2')
  output$supplier2Valid <- reactive({
    operations$supplier2$valid
  })
  outputOptions(output, "supplier2Valid", suspendWhenHidden = FALSE)
  process.transaction.button('supplier2')
  get_inputs('supplier2', max.quantity = T)

  # Static inputs
  output$supplier2.date <- date.input('supplier2.date')
  output$supplier2.store <- store.input('supplier2.store')
  output$supplier2.id <- id.input('supplier2.id')
  output$supplier2.notes <- notes.input('supplier2.notes')
  output$supplier2.counterpart <- counterpart.input('supplier2.counterpart', 'Proveedor')
  
  ## __Transfer ----
  # Buttons
  start.transaction.button('transfer', has.stock = T)
  cancel.transaction.button('transfer')
  output$transferValid <- reactive({
    operations$transfer$valid
  })
  outputOptions(output, "transferValid", suspendWhenHidden = FALSE)
  process.transaction.button('transfer')
  process.transaction.button('transfer', 'check')
  get_inputs('transfer', max.quantity = T)
  
  # Static inputs
  output$transfer.date <- date.input('transfer.date')
  output$transfer.store <- store.input('transfer.store', 'giving')
  output$transfer.id <- id.input('transfer.id')
  output$transfer.notes <- notes.input('transfer.notes')
  output$transfer.counterpart <- store.input('transfer.counterpart', 'receiving')
  
  ## __Hold ----
  # Buttons
  start.transaction.button('hold', has.stock = T)
  cancel.transaction.button('hold')
  output$holdValid <- reactive({
    operations$hold$valid
  })
  outputOptions(output, "holdValid", suspendWhenHidden = FALSE)
  process.transaction.button('hold')
  get_inputs('hold', max.quantity = T)
  
  # Static inputs
  output$hold.date <- date.input('hold.date')
  output$hold.store <- store.input('hold.store', stores.choices = 'Miraflores')
  output$hold.id <- id.input('hold.id')
  output$hold.notes <- notes.input('hold.notes')
  output$hold.counterpart <- counterpart.input('hold.counterpart', 'Cliente')
  #output$hold.counterpart <- counterpart.input('hold.counterpart', 'Proveedor')

  ## __Wild card ----
  output$wildcard.date <- date.input('wildcard.date')
  output$wildcard.id <- id.input('wildcard.id')
  output$wildcard.store.provider <- inventory.cols.input('wildcard.store.provider')
  output$wildcard.notes <- notes.input('wildcard.notes')
  # output$wildcard.product <- product.input('wildcard.product',
  #                                          has.stock = F)  # NOTE: turns to true in a sub category

  start.transaction.button('wildcard', has.stock = F)  # NOTE: turns to true in a sub category
  cancel.transaction.button('wildcard')
  output$wildCardValid <- reactive({
    operations$wildcard$valid
  })
  outputOptions(output, "wildCardValid", suspendWhenHidden = FALSE)
  process.transaction.button('wildcard', 'process', password = TRUE)
  get_inputs('wildcard', max.quantity = F)
  
  #### Check ongoing sale ----
  output$sale.check0 <- renderTable(
    
    operations[['sale']]$items[, c('Producto',
                                   'Cantidad',
                                   'Precio',
                                   'Total')]
    
  )
  
  output$sale.check1 <- renderText(
    cat(as.character('\n',operations$sale$items$Transaccion[1]), as.character(operations$sale$items$ID[1]))
  )
  
  #### Check transfer sale ----
  output$transfer.check0 <- renderTable(
    
    operations[['transfer']]$items[, c('Producto',
                                   'Cantidad')]
    
  )
  
  output$transfer.check1 <- renderText(
    cat(as.character('\n',operations$transfer$items$Transaccion[1]), as.character(operations$transfer$items$ID[1]))
  )
  
  #### Revert transactions ----
  # Choose transaction
  output$revert.trans <- renderUI(trans.input('revert.trans'))
  
  # Show transaction items
  output$transaction.to.revert <- renderDataTable({
    operations$transactions[operations$transactions$ID == input$revert.trans, ]
  })
  
  observeEvent(input$process.revert.button, {
    
    # Check is there is an ongoing transaction
    if (!operations$ongoing.transaction) {
      
      # Check password
      if (any(input$revert.password %in% psswrd,
              as.numeric(difftime(lubridate::today(),
                                  operations$transactions[operations$transactions$ID == input$revert.trans, 'Fecha'],
                                  'days')) <= 15 
              )) {

        modify_inventory(type = ids.translations2[gsub('[0-9]+$', '', input$revert.trans)],
                         revert = T,
                         ID = input$revert.trans)
        
        showModal(modalDialog(
          title     = 'Éxito',
          'Transacción revertida',
          footer    = modalButton('OK', icon = icon('ok', lib = "glyphicon")),
          easyClose = T
        ))


      } else {
        # Wrong password
        showModal(modalDialog(
          title     = 'ERROR',
          'Ingresar la clave correcta para revertir operaciones de hace más de 15 días.',
          footer    = modalButton('OK', icon = icon('ok', lib = "glyphicon")),
          easyClose = T
        ))

      }
      
      
    } else {
      
      # There is an ongoing transaction, close it first.
      warning.ongoing.transaction()
      
    }
  
  })
  
  #### Register new client or provider ----
  observeEvent(input$new.client.process, {
    
    # There is an ongoing transaction, close it first.
    if (operations$ongoing.transaction) {
      warning.ongoing.transaction()
    } else {
      
      # Format counterpart name
      if (input$new.client.type == 'Proveedor') {
        counterpart.formatted.name <- gsub(' ', '.', simple_cap(input$new.client.name))
      } else if (input$new.client.type == 'Cliente') {
        counterpart.formatted.name <- simple_cap(input$new.client.name)
      } else {
        warning.generic.problem()
        
      }
      
      # Check for duplicate registries, it is valid for new and modified counterparts.
      if (check.duplicate(counterpart.formatted.name,
                          reac.data$counterparts$Nombre[reac.data$counterparts$Tipo == input$new.client.type])) {
        warning.duplicated.object(tolower(input$new.client.type))
      } else {
        
        # Counterpart new or updated info
        counterpart.data <- data.frame(Tipo = input$new.client.type,
                                       Nombre = counterpart.formatted.name,
                                       Entidad = as.character(input$new.client.entity.code),
                                       Documento = as.character(input$new.client.id),
                                       Contacto = as.character(input$new.client.phone),
                                       Distrito = as.character(input$new.client.district),
                                       Provincia = as.character(input$newClientDept),
                                       stringsAsFactors = F)
        counterpart.data[counterpart.data == '-' | counterpart.data == ''] <- NA_character_
        
        # Check that dimension is right, just in case
        if (dim(counterpart.data)[1] > 1) {
          warning.generic.problem()
        } else {
          
          if (input$registerCounterpartAM == 'Agregar') {
            
            # Add new counterpart
            reac.data$counterparts <- rbind(reac.data$counterparts, counterpart.data)
            
            # If new provider, modify inventory
            if (input$new.client.type == 'Proveedor') {
              operations$inventory[[counterpart.formatted.name]] <- 0
              
              # Save inventory file
              write.csv(operations$inventory,
                        './Data/Inventario.csv',
                        row.names = F)
              
              file.copy('./Data/Inventario.csv',
                        './Data compartida/Inventario_vista.csv',
                        overwrite = T)
              
            }
            
          } else if (input$registerCounterpartAM == 'Modificar') {
            # NOTE: the order matters
            updated.vars <- c(setdiff(counterpart.vars, c('Tipo', 'Nombre')), 'Nombre')
            
            # Update counterparts database 
            for (elem in updated.vars) {
              new.value <- counterpart.data[1, elem]
              if (!is.na(new.value)) {
                reac.data$counterparts[reac.data$counterparts$Nombre %in% input$modify.counterpart &
                                         reac.data$counterparts$Tipo %in% input$new.client.type,
                                       elem] <- new.value
              }
            }
            
            # Ifcounterpart name changed
            if (!is.na(counterpart.data$Nombre)) {
              
              # Update inventory if counterpart name is changed for a provider
              if (input$new.client.type == 'Proveedor') {
                transaction.column <- 'Origen'
                
                names(operations$inventory)[names(operations$inventory) == input$modify.counterpart] <- counterpart.formatted.name
                
                write.csv(operations$inventory,
                          './Data/Inventario.csv',
                          row.names = F)
                
                file.copy('./Data/Inventario.csv',
                          './Data compartida/Inventario_vista.csv',
                          overwrite = T)
                
              } else {
                transaction.column <- 'Destino'
              }
              
              # Update transactions file
              operations$transactions[transaction.column][operations$transactions[transaction.column] == input$modify.counterpart] <- counterpart.formatted.name
              
              write.csv(operations$transactions,
                        './Data/transactions.csv',
                        row.names = F)
              
              file.copy('./Data/transactions.csv',
                        './Data compartida/Transacciones_vista.csv',
                        overwrite = T)
              
              
              
            }
            
            # Ifcounterpart name changed
            if (!is.na(counterpart.data$Entidad) & input$new.client.type == 'Cliente') {
              
              # Update transactions file
              operations$transactions$Entidad[operations$transactions$Destino == input$modify.counterpart] <- as.character(input$new.client.entity.code)
              
              write.csv(operations$transactions,
                        './Data/transactions.csv',
                        row.names = F)
              
              file.copy('./Data/transactions.csv',
                        './Data compartida/Transacciones_vista.csv',
                        overwrite = T)
              
            }

          
          }
          
          # Confirmation message
          confirmation.registered(input$new.client.type)
          
          # Save new version of counterparts DB
          write.csv(reac.data$counterparts,
                    './Data/Contrapartes.csv',
                    row.names = F)
          
          # Clean inputs (only if operation went through)
          updateTextInput(session,
                          'new.client.name',
                          value = '')
          
          updateTextInput(session,
                          'new.client.id',
                          value = '')
          
          updateTextInput(session,
                          'new.client.phone',
                          value = '')
          
          updateSelectizeInput(session,
                               'new.client.district',
                               selected = geo.zones$lima.districts[1])
          
          updateSelectizeInput(session,
                               'newClientDept',  # NOTE: Javascript compatibility
                               selected = geo.zones$peru.depts[1])
          
          updateSelectizeInput(session,
                               'new.client.type',
                               selected = 'Cliente')
          
        }
      }
    }
  })
  
  # Modify client or provider
  observeEvent(input$new.client.type , {
    output$modify.counterpart <-
    counterpart.input('modify.counterpart', input$new.client.type)
  })

  #### Add new product ----
  output$new.prod.type <- prod.type.input('new.prod.type')
  
  output$new.prod.prices <- renderUI({
    tagList(
      lapply(reac.data$price.types, function(i) {
        add.price.input('new.prod', i)
      })
    )
  })
  
  add.change.product(type2 = 'new.prod',
                     password = T)
  
  #### Change product prices ----
  # Inputs
    output$change.prices.prod <- product.input('change.prices.prod',
                                                         store = NA,
                                                         has.stock = F)
  
  output$change.prices.prices <- renderUI({
    tagList(
      lapply(reac.data$price.types, function(i) {
        add.price.input('change.prices', i)
      })
    )
  })
  
  observeEvent(input$change.prices.prod, {
    lapply(reac.data$price.types, function(i) {
      updateNumericInput(session,
                         paste0('change.prices', '.price.', i),
                         value = reac.data$products[reac.data$products$Codigo == input$change.prices.prod,
                                                    i])
    })
  })
  
  add.change.product(type2 = 'change.prices', password = T)
  
  
  
  #### Add new type of prod ----
  observeEvent(input$new.type.prod.process, {
    
    # There is an ongoing transaction, close it first.
    if (operations$ongoing.transaction) {
      
      warning.ongoing.transaction()
      
      # Check that there are input values
    } else if (any(c(input$new.type.prod.name,
                           input$new.type.prod.code) == '')) {
      
      showModal(modalDialog(
        title     = 'ERROR',
        'Alguno de los campos esta vacio.',
        footer    = modalButton('OK', icon = icon('ok', lib = "glyphicon")),
        easyClose = T
      ))
      
      # Check for duplicates
    } else if (check.duplicate(simple_cap(input$new.type.prod.name), 
                               reac.data$prod.types$Tipo)) {
      warning.duplicated.object('tipo de producto')
      
    } else if (check.duplicate(toupper(input$new.type.prod.code), 
                               reac.data$prod.types$Codigo)) {
      warning.duplicated.object('codigo de producto')
      
      # If everything is OK, process the product
    } else {
      reac.data$prod.types <- rbind(reac.data$prod.types,
                                    data.frame(Tipo = simple_cap(input$new.type.prod.name),
                                               Codigo = toupper(input$new.type.prod.code),
                                               Activo = 1))
      
      write.csv(reac.data$prod.types,
                './Data/Tipos de producto.csv',
                row.names = F)
      
      # Clean inputs
      updateTextInput(session,
                      'new.type.prod.name',
                      value = '')
      
      
      updateTextInput(session,
                      'new.type.prod.code',
                      value = '')
      
      # Sucess message
      showModal(modalDialog(
        title     = 'Operacion realizada',
        'El tipo de producto fue registrado.',
        footer    = modalButton('OK', icon = icon('ok', lib = "glyphicon")),
        easyClose = T
      ))
      
      
    }
      
  })
  

  #### Add or remove price type ----
  output$price.type.existing <- renderUI(price.input('price.type.existing',
                                                     'a')) # PENDING2: check if can leave empty)
  
  observeEvent(input$price.type.process, {
    
    # There is an ongoing transaction, close it first.
    if (operations$ongoing.transaction) {
      
      warning.ongoing.transaction()
      
    } else {
      
      if (input$priceTypesAction == 'Agregar') {
        
        # Check if price already exists
        if (check.duplicate(input$price.type.new,
                            reac.data$price.types)) {
          warning.duplicated.object('tipo de precio')
          
        } else {
          # Add the price type
          reac.data$price.types <- c(reac.data$price.types,
                                     input$price.type.new)
          
          reac.data$products[input$price.type.new] <- NA_real_
          write.csv(reac.data$products, 
                    './Data/Productos.csv',
                    row.names = F)
          
          shinyjs::reset('price.type.new')
          
          confirmation.registered('Tipo de precio')
        }
        
      } else if (input$priceTypesAction == 'Eliminar') {
        # Eliminate price type
        reac.data$price.types <- setdiff(reac.data$price.types,
                                         input$price.type.existing)
        
        reac.data$products[input$price.type.existing] <- NULL
        write.csv(reac.data$products, 
                  './Data/Productos.csv',
                  row.names = F)
        
        confirmation.operation()
      }
    }
    
  })
  
  #### Add stores ----
  observeEvent(input$add.store.process,  {
    
    # There is an ongoing transaction, close it first.
    if (operations$ongoing.transaction) {
      
      warning.ongoing.transaction()
      
    } else {
    
      name <- gsub(' ', '.', simple_cap(input$add.store.name))
      
      # check if store already exists 
      if (name %in% names(reac.data$stores)) {
        warning.duplicated.object('local')
        
      } else {
        # modified and save stores table
        reac.data$stores <- rbind(reac.data$stores,
                                  data.frame(Nombre = name,
                                             Codigo = NA_integer_))
        
        write.csv(reac.data$stores, 
                  './Data/Locales.csv',
                  row.names = F)
        
        # modify and save inventory table
        operations$inventory[[name]] <- 0
        
        write.csv(operations$inventory,
                  './Data/Inventario.csv',
                  row.names = F)
        
        file.copy('./Data/Inventario.csv',
                  './Data compartida/Inventario_vista.csv',
                  overwrite = T)
        
        # confirmation message and cleanup
        confirmation.registered('Local')
        shinyjs::reset('add.store.name')
      }
    }
    
  })
  
  #### Eliminate elements ----
  # Lists of the selected type of element to eliminate
  observeEvent(c(input$delete.element.type,
                 input$delete.element.process,
                 input$new.client.process, 
                 input$new.prod.process,
                 input$new.store.process), {
                   
    if (input$delete.element.type == 'Proveedor') {
      choices <- reac.data$counterparts$Nombre[reac.data$counterparts$Tipo == 'Proveedor']
    } else if (input$delete.element.type == 'Cliente') {
      choices <- reac.data$counterparts$Nombre[reac.data$counterparts$Tipo == 'Cliente']
    } else if (input$delete.element.type == 'Producto') {
      choices <- reac.data$products$Codigo
    } else if (input$delete.element.type == 'Tienda') {
      choices <- reac.data$stores$Nombre
    }
  
    output$delete.element.selection <- renderUI(
      selectizeInput('delete.element.selection',
                     input$delete.element.type,
                     choices = choices)
    )
  })
  
  observeEvent(input$delete.element.process,  {
    
    # There is an ongoing transaction, close it first.
    if (operations$ongoing.transaction) {
      
      warning.ongoing.transaction()
      
    } else {
      
      if (input$delete.element.type == 'Proveedor') {
        
        # check if the provider has no stock in the inventory
        if (all(operations$inventory[[input$delete.element.selection]] %in% 0)) {
          
          # delete provider from inventory table and save changes
          operations$inventory[[input$delete.element.selection]] <- NULL
          write.csv(operations$inventory,
                    './Data/Inventario.csv',
                    row.names = F)
          
          file.copy('./Data/Inventario.csv',
                    './Data compartida/Inventario_vista.csv',
                    overwrite = T)
          
          # click provider from list and save changes
          reac.data$counterparts <- reac.data$counterparts[!(reac.data$counterparts$Tipo == 'Proveedor' & 
                                                               reac.data$counterparts$Nombre == input$delete.element.selection), ]
          write.csv(reac.data$counterparts,
                    './Data/Contrapartes.csv',
                    row.names = F)
          
          # reset element input shock
          shinyjs::reset('delete.element.selection')
          
          # confirmation message
          confirmation.operation()
          
        } else {
          # error message
          warning.generic.problem('Este proveedor aún tiene stock asignado!')
        }
        
      } else if (input$delete.element.type == 'Cliente') {
        
        # click client from list and save changes
        reac.data$counterparts <- reac.data$counterparts[!(reac.data$counterparts$Tipo == 'Cliente' & 
                                                             reac.data$counterparts$Nombre == input$delete.element.selection), ]
        write.csv(reac.data$counterparts,
                  './Data/Contrapartes.csv',
                  row.names = F)
        
        # reset element input shock
        shinyjs::reset('delete.element.selection')
        
        # confirmation message
        confirmation.operation()
        
        } else if (input$delete.element.type == 'Producto') {
        
        # check that there is no stock of the product in the inventory
        if (all(operations$inventory[operations$inventory$Producto %in% input$delete.element.selection, 
                                     !names(operations$inventory) %in% c('Producto',
                                                                         'Tipo',
                                                                         'Nombre')] %in% 0)) {
          
          # delete product from list and save changes
          reac.data$products <- reac.data$products[!reac.data$products$Codigo %in% input$delete.element.selection, ]
          
          write.csv(reac.data$products,
                    './Data/Productos.csv',
                    row.names = F)
          
          # delete product from inventory and save changes
          operations$inventory <- operations$inventory[!operations$inventory$Producto %in% input$delete.element.selection, ]
          
          write.csv(operations$inventory,
                    './Data/Inventario.csv',
                    row.names = F)
          
          file.copy('./Data/Inventario.csv',
                    './Data compartida/Inventario_vista.csv',
                    overwrite = T)
          
          # reset element input shock
          shinyjs::reset('delete.element.selection')
          
          # confirmation message
          confirmation.operation()
          
        } else {
          
          # error message
          warning.generic.problem('Este producto aún tiene stock asignado!')
          
        }
        
        
      } else if (input$delete.element.type == 'Tienda') {
        
        # check that the store does not have any stock
        if (all(operations$inventory[[input$delete.element.selection]] %in% 0)) {
          
          # delete store from inventory table and save changes
          operations$inventory[[input$delete.element.selection]] <- NULL
          
          write.csv(operations$inventory,
                    './Data/Inventario.csv',
                    row.names = F)
          
          file.copy('./Data/Inventario.csv',
                    './Data compartida/Inventario_vista.csv',
                    overwrite = T)
          
          # delete store from list and safe changes
          reac.data$stores <- reac.data$stores[!reac.data$stores$Nombre %in% input$delete.element.selection, ]
          
          write.csv(reac.data$stores, 
                    './Data/Locales.csv',
                    row.names = F)
          
          # confirmation message
          confirmation.operation()
          
        } else {
          
          # error message
          warning.generic.problem('Esta tienda aún tiene stock asignado!')
          
        }
        
      }
    }  
    
  })

  #### See databases ----
  # Providers
  output$providers <- DT::renderDataTable({
    reac.data$counterparts[reac.data$counterparts$Tipo == 'Proveedor',
                           !names(reac.data$counterparts) %in% c('Tipo')]
  },
  rownames = F,
  server = F,
  filter = 'top',
  extensions = c('Buttons'),
  options = dt.options)
  
  # Clients
  output$clients <- DT::renderDataTable({
    reac.data$counterparts[reac.data$counterparts$Tipo == 'Cliente',
                           #!names(reac.data$counterparts) %in% c('Tipo')
                           c('Nombre',
                             'Entidad',
                             'Documento',
                             'Contacto',
                             'Distrito',
                             'Provincia')]
  },
  rownames = F,
  server = F,
  filter = 'top',
  extensions = c('Buttons'),
  options = dt.options)
  
  # Stores
  output$stores <- DT::renderDataTable({
    reac.data$stores
  },
  rownames = F,
  server = F,
  filter = 'top',
  extensions = c('Buttons'),
  options = dt.options)
  
  # Products
  output$products <- DT::renderDataTable({
    reac.data$products[order(reac.data$products$Codigo), ]
  },
  rownames = F,
  server = F,
  filter = 'top',
  extensions = c('Buttons'),
  options = dt.options)
  
  # Type of products
  output$prod.types <- DT::renderDataTable({
    reac.data$prod.types[order(reac.data$prod.types$Tipo), ]
  },
  rownames = F,
  server = F,
  filter = 'top',
  extensions = c('Buttons'),
  options = dt.options)
  
  })

