program Inventario

    use InventarioModule
    use LeerArchi
    use Movimientos_inventario_module
    use Crear_Informe
    implicit none
    integer :: opcion_menu, max_lineas, i
    character(len=100) :: contenido(100)
    character(len=100) :: ruta_archivo, ruta_instrucciones,ruta_archivo_editado,ruta_archivo_movimientos
    character(len=100) :: delimitador
    
    type(producto),allocatable :: productos(:)
    i=1

    max_lineas = 100
    !ruta_archivo="inventario.inv"
    delimitador=","
    ruta_archivo=""
    ruta_instrucciones=""
    ruta_archivo_editado=""
    ruta_archivo_movimientos=""
    allocate(productos(0))

    ! Leer archivo
    !call LeerArchivo(max_lineas,contenido,ruta_archivo)

   

     do  
         print *, "Practica 1: Inventario de productos"
         print *, "Menu de opciones de inventario de productos"
         print *, "1. Cargar Inventario Inicial"
         print *, "2. Cargar Instrucciones de Movimientos"
         print *, "3. Crear Informe de Inventario"
         print *, "4. Salir"
          print *, "Ingrese una opcion: "
          read *, opcion_menu
           ruta_archivo_editado=ruta_archivo
             ruta_archivo_movimientos=ruta_instrucciones

            !  ruta_archivo=""
            !  ruta_instrucciones=""

          select case(opcion_menu)
             case(1)
                !call LeerArchivo(max_lineas,contenido,ruta_archivo)
                 !do i=1, max_lineas
                    
                call Cargarinventario(ruta_archivo)
                ruta_archivo_editado=ruta_archivo
                !end do
             case(2)
                 !call LeerArchivo(max_lineas,contenido,ruta_archivo)
                call cargar_movimientos(ruta_archivo_editado,ruta_instrucciones)
                ruta_archivo_movimientos=ruta_instrucciones
                !ruta_instrucciones=""

             case(3)
                 call Crearinforme(ruta_archivo,ruta_archivo_movimientos,ruta_archivo_editado)
             case(4)
                 exit
             case default
                 print *, "Opcion invalida"
         end select
    end do
    
end program Inventario
