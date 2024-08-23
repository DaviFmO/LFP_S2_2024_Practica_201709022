module Crear_Informe

    implicit none
    contains
    subroutine Crearinforme(ruta_archivo,ruta_instrucciones,ruta_archivo_editado)
        character(len=100), intent(in) :: ruta_archivo
        character(len=100), intent(inout) :: ruta_instrucciones
        character(len=100) :: ruta_archivo_editado
        character(len=100) :: linea, palabra1, palabra2, palabra3, palabra4, palabra5
        character::signo
        integer:: unit, ios,pos1,pos2,pos3,pos4,pos5, valornumerico 
        integer,dimension(100)::cantidad
        real,dimension(100)::precio
        real:: total    
        character(len=100),dimension(100):: equipo,ubicacion
        integer:: contador,i
        !inicializar el contador
        unit=10
        contador=0
        total=0
        signo=""
        !abrir el archivo de movimientos
        open(unit=10, file=trim(ruta_archivo), status='old', action='read', iostat=ios)
        if(ios/=0) then
            print*, "Error al abrir al abrir el archivo de movimientos: ", trim(ruta_archivo)   
            return
        end if
        !leer el archivo de movimientos
        do
            read(10,'(A)',iostat=ios) linea
            if(ios/=0) exit
            
            !separar la linea en palabras
            pos1=index(linea," ")
            pos2=index(linea(pos1+1:),";")+pos1
            pos3=index(linea(pos2+1:),";")+pos2
            pos4=index(linea(pos3+1:),";")+pos3

            !extraer las palabras
            palabra1=linea(1:pos1-1)
            palabra2=linea(pos1+1:pos2-1)
            palabra3=linea(pos2+1:pos3-1)
            palabra4=linea(pos3+1:pos4-1)
            palabra5=linea(pos4+1:)
            !almacenar los datos en el arreglo productos
            contador=contador+1
            equipo(contador)=trim(palabra2)
            read(palabra3,*) cantidad(contador)
            read(palabra4,*) precio(contador)
            ubicacion(contador)=trim(palabra5)
        end do
        close(10)
        !solicitar al usuario que ingrese el nombre del archivo de instrucciones
        ! print*, "Ingrese el nombre del archivo de instrucciones de movimientos"
        ! read(*,"(A)") ruta_instrucciones
        !abrir el archivo de instrucciones
        open(unit=11, file=trim(ruta_instrucciones), status='old', action='read', iostat=ios)
        if(ios/=0) then
            print*, "Error al abrir al abrir el archivo de instrucciones: ", trim(ruta_instrucciones)   
            return
        end if
        !leer el archivo de instrucciones y actualizar el inventario
        do
            read(11,'(A)',iostat=ios) linea
            if(ios/=0) exit
            !separar la linea en palabras
            pos1=index(linea," ")
            pos2=index(linea(pos1+1:),";")+pos1
            pos3=index(linea(pos2+1:),";")+pos2
            pos4=index(linea(pos3+1:),";")+pos3
            !pos5=index(linea(pos4+1:),";")+pos4
            !extraer las palabras
            palabra1=linea(1:pos1-1)
            palabra2=linea(pos1+1:pos2-1)
            palabra3=linea(pos2+1:pos3-1)
            palabra4=linea(pos3+1:pos4-1)
            palabra5=linea(pos4+1:)
            !ver instruccion y actualizar el inventario
            if(trim(palabra1)== "agregar_stock") then
                !buscar el producto en el arreglo productos
                do i=1,contador
                    if(trim(equipo(i))==trim(palabra2) .and. trim(ubicacion(i))==trim(palabra5)) then
                       read(palabra3,*) valornumerico
                        !actualizar el cantidad
                        cantidad(i)=cantidad(i)+valornumerico
                       ! print*, "Se agrego ", palabra3, " unidades de ", palabra2
                        exit
                    end if
            end do
            if( trim(ubicacion(i))/=trim(palabra5)) then
               ! print*, "No se encontro el producto ", palabra2, " en la ubicacion ", palabra5
            end if
        else if(trim(palabra1)== "eliminar_equipo") then
                !buscar el producto en el arreglo productos
                do i=1,contador
                    if(trim(equipo(i))==trim((palabra2)).and.trim(ubicacion(i))==trim(palabra5)) then
                        !verificar si la cantidad a eliminar es mayor a la cantidad en inventario
                        read(palabra3,*) valornumerico
                       if(cantidad(i)>=valornumerico) then
                        
                           cantidad(i)=cantidad(i)-valornumerico
                           !print*, "Se agrego ", palabra3, " unidades de ", palabra2
                           
                        else
                            !print*,"No hay suficiente inventario para eliminar ", palabra3, " unidades de ", palabra2
                        end if
                        exit
                    end if
                end do
            if ( trim(ubicacion(i))/=trim(palabra5)) then
               ! print *, 'Error: El equipo', trim(palabra2), 'no existe en la ubicación', trim(palabra5)
            end if
        end if
    end do
    close(11)
    ! Solicita al usuario ingresar la ruta del archivo de informe
    print *, 'Ingrese la ruta del archivo de informe (.txt):'
    read(*, '(A)') ruta_archivo_editado
     !guardar el inventario actualizado en el archivo de inventario
      open(unit=12, file=trim(ruta_archivo_editado), status='replace', action='write', iostat=ios)
          if(ios/=0) then
             print*, "Error al abrir al abrir el archivo de movimientos: ", trim(ruta_archivo)   
             return
          end if
          write(12,'(A)') "Informe de inventario"
          write(12,'(A)') "Equipo  Cantidad   Precio Unitario    valor    Ubicacion"
          write(12,'(A)') "------------------------------------------------------"
          print*, "Informe de inventario"
          print*, "Equipo    Cantidad    Precio_Unitario     valor       Ubicacion"
          print*, "--------------------------------------------------------------"
      do i=1,contador
        total=cantidad(i)*precio(i)
        signo="Q"
          write(12,'(A,2X,I5,7X,F10.2,5X,A,F10.2,2x,A)') trim(equipo(i)), cantidad(i), precio(i),"Q",total, trim(ubicacion(i))
          print*, trim(equipo(i)), cantidad(i), precio(i),signo,cantidad(i)*precio(i), ubicacion(i)
      end do
     print*, "Inventario actualizado guardado en el archivo ", trim(ruta_archivo_editado)
    print*, "Fin de la actualizacion de inventario"
    close(12)
    end subroutine Crearinforme
end module Crear_Informe


