module LeerArchi
    use InventarioModule
    implicit none
    contains
    subroutine LeerArchivo(max_lineas,contenido,ruta_archivo)
        implicit none
        integer, intent(in) :: max_lineas
        character(len=100), dimension(max_lineas), intent(out) :: contenido
        character(len=100), intent(in) :: ruta_archivo
        integer :: i, unidad, ios
        character(len=100) :: linea
        logical :: fin_archivo

        i=1
        fin_archivo = .false.
        contenido= ""
        unidad = 10

        !abrir archivo para lectura
        open(unit=unidad, file=trim(ruta_archivo), status='old', action='read', iostat=ios)
        if(ios /= 0) then
            print *, "Error al abrir el archivo"
            stop
        end if
        !leer archivo linea por linea
        do 
            read(unidad, '(A)', iostat=ios) linea
            if(ios /= 0) exit
            if(i <= max_lineas) then
                print *, linea
                contenido(i) = linea
                i = i + 1
            end if
        end do
        close(unidad)  
    end subroutine LeerArchivo
    
    subroutine Cargarinventario(ruta_archivo)
        implicit none
      
        character(len=100), intent(inout) :: ruta_archivo
        character(len=100) :: linea, palabra1, palabra2, palabra3, palabra4, palabra5
        integer:: unit, ios,pos1,pos2,pos3,pos4,pos5
        integer,dimension(100)::cantidad
        real,dimension(100)::precio
        character(len=100),dimension(100):: equipo,ubicacion
        integer:: contador,i
        !inicializar el contador
        !nombre=""
        unit=10
        contador=0
        !abrir el archivo 
        print*, "Ingrese el nombre del archivo o la ruta"
        read(*,"(A)") ruta_archivo
        open(unit=unit,file=trim(ruta_archivo),status='old',action='read',iostat=ios)
        if(ios/=0) then
            print*, "Error al abrir al abrir el inventario: ", trim(ruta_archivo)   
            return
        end if
        !leer el archivo y almacenar los datos en el arreglo productos  
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
        !mostrar los datos almacenados
        print*, "Datos almacenados en el arreglo productos"
        print*, "Nombre, Cantidad, Precio, Ubicacion"
        do i=1,contador
            print*, trim(equipo(i)),cantidad(i), precio(i),trim(ubicacion(i))
        end do

    end subroutine Cargarinventario
end module LeerArchi
