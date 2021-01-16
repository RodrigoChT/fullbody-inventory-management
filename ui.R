shinyUI(navbarPage('Sistema de Inventario - Full Body',
                   
                   theme = shinytheme('flatly'),
                   
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