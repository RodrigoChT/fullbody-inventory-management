shinyUI(navbarPage('Sistema de Inventario - Full Body',
                   
                   theme = shinytheme('flatly'),
                   tabPanel('Ventas',
                            navlistPanel(
                              #### Ventas ----
                              tabPanel('Venta regular',
                                       inputPanel(
                                         uiOutput('sale.date'),
                                         uiOutput('sale.store'),
                                         uiOutput('sale.id'),
                                         uiOutput('sale.notes'),
                                         selectizeInput('sale.id.type',
                                                        'Tipo de doc.:',
                                                        choices = doc.types),
                                         uiOutput('sale.default.price'),
                                         uiOutput('sale.counterpart'),
                                         numericInput('sale.cash',
                                                      'Efectivo',
                                                      value = NA),
                                         numericInput('sale.card',
                                                      'Tarjeta',
                                                      value = NA),
                                         numericInput('sale.deposit.1',
                                                      'Depósito 1',
                                                      value = NA),
                                         numericInput('sale.deposit.2',
                                                      'Depósito 2',
                                                      value = NA),
                                         numericInput('sale.credit',
                                                      'Crédito',
                                                      value = NA),
                                         numericInput('sale.advance',
                                                      'Adelanto',
                                                      value = NA),
                                         payment.input('salePayment'),
                                         conditionalPanel(
                                           condition = "input.salePayment == 'Mixto'",
                                           sliderInput('sale.mix.payment',
                                                       'Porcentaje pagado al contado',
                                                       min = 0,
                                                       max = 100,
                                                       value = 50)
                                         ),
                                         actionButton('sale.start',
                                                      'Empezar venta'),
                                        
                                         actionButton('sale.process', 
                                                      'Procesar venta',
                                                      icon = icon('usd',
                                                                  lib = 'glyphicon'))
                                       ),
                                       
                                       conditionalPanel(
                                          condition = "output.saleValid",
                                         uiOutput('sale.product'),
                                         tags$div(id = 'tsList'),
                                         tags$div(id = 'saleList'),
                                         actionButton('sale.add', 
                                                      'Añadir producto',
                                                      icon = icon('plus')),
                                         actionButton('sale.cancel',
                                                      'Cancelar'),
                                         actionButton('sale.check',
                                                      'Revisar venta'),
                                         textOutput('sale.check1'),
                                         tableOutput('sale.check0')
                                       )
                              ),
                              
                              ## Notas de credito
                              tabPanel('Nota de crédito',
                                       inputPanel(uiOutput('credit.date'),
                                                  uiOutput('credit.id'),
                                                  uiOutput('credit.notes'),
                                                  uiOutput('credit.trans'),
                                                  actionButton('credit.start',
                                                               'Seleccionar'),
                                                  actionButton('credit.process',
                                                               'Procesar')),
                                       h3('Venta original:'),
                                       dataTableOutput('transaction.to.credit'),
                                       h3('Notas de credito anteriores:'),
                                       dataTableOutput('previous.credit.notes'),
                                       mainPanel(conditionalPanel(
                                         condition = "output.creditValid",
                                         product_widgets('credit')),
                                         tags$div(id = 'creditList')
                                         )
                                       ),
                              
                              ## Separaciones
                              tabPanel('Separaciones',
                                       input_widgets('hold'),
                                       conditionalPanel(
                                         condition = "output.holdValid",
                                         product_widgets('hold')
                                       ),
                                       tags$div(id = 'holdList')),
                              widths = c(sidebar.size, 12 - sidebar.size)
                            )
                            ),
                   
                   #### Proveedores ----
                   tabPanel('Proveedores',
                            navlistPanel(
                              ## Incoming
                              tabPanel('Recepción',
                                       input_widgets('supplier'),
                                       conditionalPanel(
                                         condition = "output.supplierValid",
                                         tags$div(id = 'supplierList'),
                                         product_widgets('supplier')
                                       ),
                                       ),
                              ## Returning
                              tabPanel('Devolución',
                                       input_widgets('supplier2'),
                                       conditionalPanel(
                                         condition = "output.supplier2Valid",
                                         tags$div(id = 'supplier2List'),
                                         product_widgets('supplier2')
                                       ),
                                       ),
                              widths = c(sidebar.size, 12 - sidebar.size))

                           ),
                   #### Traslados ----
                   tabPanel('Traslados',
                            input_widgets('transfer',
                                          check = T),
                            conditionalPanel(
                              condition = "output.transferValid",
                              product_widgets('transfer'),
                              textOutput('transfer.check1'),
                              tableOutput('transfer.check0')
                            ),
                            tags$div(id = 'transferList')),

                   #### Inventory ----
                   tabPanel('Inventario',
                            DT::dataTableOutput('inventory.table')
                            ),
                   
                   #### Reports ----
                   tabPanel('Movimientos',
                            downloadButton('transactions.download',
                                           'Descargar'),
                            DT::dataTableOutput('transactions.table')
                            ),
                   
                   #### Special ops -----
                   tabPanel('Operaciones esp.',
                            navlistPanel(
                              
                              ## Modify inventory, wild card
                              tabPanel('Regularización',
                                       inputPanel(uiOutput('wildcard.date'),
                                                  uiOutput('wildcard.store.provider'),
                                                  selectizeInput('wildcard.action',
                                                                 'Aumentar o disminuir:',
                                                                 choices = c('Aumentar',
                                                                             'Disminuir')),
                                                  uiOutput('wildcard.id'),
                                                  uiOutput('wildcard.notes'),
                                                  passwordInput('wildcard.password',
                                                                'Clave',
                                                                value = ''),
                                                  actionButton('wildcard.start',
                                                               'Empezar transacción'),
                                                  actionButton('wildcard.process',
                                                               'Procesar transacción',
                                                               icon = icon('usd',
                                                                           lib = 'glyphicon'))
                                                  ),
                                       mainPanel(
                                         conditionalPanel(
                                           condition = "output.wildCardValid",
                                           #uiOutput('wildcard.product'),
                                           actionButton('wildcard.add', 
                                                        'Añadir producto',
                                                        icon = icon('plus')),
                                           actionButton('wildcard.cancel',
                                                        'Cancelar venta')
                                         ),
                                         tags$div(id = 'wildcardList')
                                       )
                                       
                                       ),
                              
                              ## Revert transactions
                              tabPanel('Revertir',
                                       inputPanel(uiOutput('revert.trans'),
                                                  passwordInput('revert.password',
                                                                'Clave',
                                                                value = ''),
                                                  actionButton('process.revert.button',
                                                               'Revertir')),
                                       mainPanel(dataTableOutput('transaction.to.revert'))),
                                       
                              
                              
                              widths = c(sidebar.size, 12 - sidebar.size)
                            )),
                   
                   #### Show secondary tables and reports ----
                   tabPanel('Reportes',
                            navlistPanel(
                              tabPanel('Resumen de ventas',
                                       inputPanel(
                                         dateInput('sale.summary.start',
                                                   'Inicio',
                                                   value = lubridate::today()-7),
                                         dateInput('sale.summary.end',
                                                   'Final'),
                                         uiOutput('sale.summary.store'),
                                         selectizeInput('sale.summary.type',
                                                        'Tipo',
                                                        choices = c('Ventas',
                                                                    'Notas de credito'),
                                                        selected = 'Ventas',
                                                        multiple = T),
                                         textInput('sale.summary.import.start.id',
                                                   'Código inicial (imp)'),
                                         textInput('sale.summary.import.month',
                                                   'Mes (imp)'),
                                         actionButton('sale.summary.create',
                                                      'Generar'),
                                         downloadButton('sale.summary.download',
                                                        'Descargar'),
                                         downloadButton('sale.summary.import.download',
                                                        'Importaciones')
                                       ),
                                       
                                       DT::dataTableOutput('sales.summary.table')),
                              tabPanel('Resumen de prendas',
                                       inputPanel(
                                         dateInput('clothes.summary.start',
                                                   'Inicio',
                                                   value = lubridate::today()-7),
                                         dateInput('clothes.summary.end',
                                                   'Final'),
                                         uiOutput('clothes.summary.store'),
                                         selectizeInput('clothes.summary.type',
                                                        'Tipo',
                                                        choices = c('Ventas',
                                                                    'Notas de credito'),
                                                        selected = 'Ventas',
                                                        multiple = T),
                                         actionButton('clothes.summary.create',
                                                      'Generar'),
                                         downloadButton('clothes.summary.download',
                                                        'Descargar')
                                       ),
                                       
                                       DT::dataTableOutput('clothes.summary.table')),
                            widths = c(sidebar.size, 12 - sidebar.size))),
                   
                   #### Modify databases ----
                   tabPanel('Modificar bases',
                            navlistPanel(
                              ## Add clients
                              tabPanel('Registrar cliente o proveedor',
                                       selectizeInput('registerCounterpartAM',
                                                      'Acción',
                                                      choices = c('Agregar',
                                                                  'Modificar'),
                                                      selected = 'Agregar'),
                                       input_widgets_counterparts('new.client')
                              ),
                              
                              ## Add new product
                              tabPanel('Nuevo producto',
                                       uiOutput('new.prod.type'),
                                       textInput('new.prod.name',
                                                 'Nombre:',
                                                 value = NA),
                                       uiOutput('new.prod.prices'),
                                       passwordInput('new.prod.password',
                                                     'Clave',
                                                     value = ''),
                                       actionButton('new.prod.process',
                                                    'Registrar')
                              ),
                              
                              ## Modify product prices
                              tabPanel('Modificar precios',
                                       uiOutput('change.prices.prod'),
                                       uiOutput('change.prices.prices'),
                                       passwordInput('change.prices.password',
                                                     'Clave',
                                                     value = ''), 
                                       actionButton('change.prices.process',
                                                    'Registrar')
                              ),
                              
                              
                              ## Add new type of product
                              tabPanel('Nuevo tipo de prod.',
                                       textInput('new.type.prod.name',
                                                 'Nombre'),
                                       textInput('new.type.prod.code',
                                                 'Código'),
                                       actionButton('new.type.prod.process',
                                                    'Registrar')),
                              
                              ## Add or remove price types
                              tabPanel('Tipos de precio',
                                       selectizeInput('priceTypesAction',
                                                      'Acción',
                                                      choices = c('Agregar',
                                                                  'Eliminar'),
                                                      selected = 'Agregar'),
                                       input_widgets_price_types(),
                                       actionButton('price.type.process',
                                                    'Registrar')
                                       ),
                              
                              ## Add stores
                              tabPanel('Agregar tiendas',
                                       textInput('add.store.name',
                                                 'Nombre'),
                                       actionButton('add.store.process',
                                                    'Registrar')
                                       ), 
                              
                              ## Delete elements
                              tabPanel('Eliminaciones',
                                       selectizeInput('delete.element.type',
                                                      'Tipo de elemento',
                                                      c('Proveedor',
                                                        'Cliente',
                                                        'Producto',
                                                        'Tienda')),
                                       uiOutput('delete.element.selection'),
                                       actionButton('delete.element.process',
                                                    'Eliminar')),
                              widths = c(sidebar.size, 12 - sidebar.size)
                            )),
                   
                   #### See databases -----
                   tabPanel('Bases', 
                            navlistPanel(
                              tabPanel('Clientes',
                                       DT::dataTableOutput('clients')),
                              tabPanel('Proveedores',
                                       DT::dataTableOutput('providers')),
                              tabPanel('Tiendas',
                                       DT::dataTableOutput('stores')),
                              tabPanel('Productos',
                                       DT::dataTableOutput('products')),
                              tabPanel('Tipos de prod',
                                       DT::dataTableOutput('prod.types')),
                              widths = c(sidebar.size, 12 - sidebar.size)
                            )),
                   shinyjs::useShinyjs()
))