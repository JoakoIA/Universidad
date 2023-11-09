def digitarCantidadIngresos():
    global N
    while True:
        try:
            N = int(input("Ingrese la cantidad de certificados a solicitar: "))
            if N > 0:
                return N
            else:
                print("Debe ingresar un valor valido") 
        except ValueError:
            print("Ingrese la cantidad de certificados a solicitar:")

def digitarPrecios():
    while True:
        try:
            precio = int(input("Ingrese el precio del certificado: "))
            if 0 <= precio <= 50000:
                precios.append(precio)
                break
            else:
                print("Debe precio entre $0 y $50.000") 
        except ValueError:
            print("Ingrese el precio del certificado:")

def digitarTipos():
    while True:
        try:
            Tramite = int(input("Ingrese el tipo de tramite\n 1- Presencial \n 2- Online\n: "))
            if Tramite in (1, 2):
                tramistes.append(Tramite)
                return Tramite
            else:
                print("El tipo de tramite seleccionado no existe") 
        except ValueError:
            print("Ingrese el tipo de tramite\n 1- Online \n 2- Presencial\n:")

def digitarEspecificacion():
    for i in (0,N):
        while True:
            try:
                Especificacion = int(input("Ingrese la especificacion del certificado\n 1- Nacimiento \n 2- Defuncion\n 3- Matrimonio\n: "))
                if Especificacion in [1, 2, 3]:
                    especificaciones.append(Especificacion)
                    return Especificacion
                else:
                    print("La especificacion seleccionada no existe") 
            except ValueError:
                print("Ingrese la especificacion del certificado\n 1- Nacimiento \n 2- Defuncion\n 3- Matrimonio\n: ")

def presentarResultados():
    global precios
    precios = []
    global tramistes
    tramistes = []
    global especificaciones
    especificaciones = []
    n = digitarCantidadIngresos()
    for i in range(0, n):
        digitarPrecios()
        digitarTipos()
        digitarEspecificacion()

    
    total_certificados = n
    total_presenciales = tramistes.count(1)
    total_online = tramistes.count(2)
    promedio_precio_presenciales = sum(precios[i] for i in range(total_presenciales)) / total_presenciales if total_presenciales > 0 else 0
    promedio_precio_online = sum(precios[i] for i in range(total_presenciales, total_certificados)) / total_online if total_online > 0 else 0
    total_nacimiento = especificaciones.count(1)
    total_defuncion = especificaciones.count(2)
    total_matrimonio = especificaciones.count(3)
    promedio_precio_nacimiento = sum(precios[i] for i in range(total_certificados) if especificaciones[i] == 1) / total_nacimiento if total_nacimiento > 0 else 0
    promedio_precio_defuncion = sum(precios[i] for i in range(total_certificados) if especificaciones[i] == 2) / total_defuncion if total_defuncion > 0 else 0
    promedio_precio_matrimonio = sum(precios[i] for i in range(total_certificados) if especificaciones[i] == 3) / total_matrimonio if total_matrimonio > 0 else 0
    presencial_nacimiento = sum(1 for i in range(total_presenciales) if especificaciones[i] == 1)
    presencial_defuncion = sum(1 for i in range(total_presenciales) if especificaciones[i] == 2)
    presencial_matrimonio = sum(1 for i in range(total_presenciales) if especificaciones[i] == 3)
    online_nacimiento = sum(1 for i in range(total_presenciales, total_certificados) if especificaciones[i] == 1)
    online_defuncion = sum(1 for i in range(total_presenciales, total_certificados) if especificaciones[i] == 2)
    online_matrimonio = sum(1 for i in range(total_presenciales, total_certificados) if especificaciones[i] == 3)

    print("\nResultados:")
    print(f"Cantidad total de certificados: {total_certificados}")
    print(f"Cantidad de certificados presenciales solicitados: {total_presenciales}")
    print(f"Cantidad de certificados online solicitados: {total_online}")
    print(f"Promedio de precios de certificados presenciales solicitados: {promedio_precio_presenciales:.2f}")
    print(f"Promedio de precios de certificados online solicitados: {promedio_precio_online:.2f}")
    print(f"Cantidad de certificados de nacimiento solicitados: {total_nacimiento}")
    print(f"Cantidad de certificados de defunción solicitados: {total_defuncion}")
    print(f"Cantidad de certificados de matrimonio solicitados: {total_matrimonio}")
    print(f"Promedio de precios de certificados de nacimiento solicitados: {promedio_precio_nacimiento:.2f}")
    print(f"Promedio de precios de certificados de defunción solicitados: {promedio_precio_defuncion:.2f}")
    print(f"Promedio de precios de certificados de matrimonio solicitados: {promedio_precio_matrimonio:.2f}")
    print(f"Cantidad de certificados presenciales y de nacimiento solicitados: {presencial_nacimiento}")
    print(f"Cantidad de certificados presenciales y de defunción solicitados: {presencial_defuncion}")
    print(f"Cantidad de certificados presenciales y de matrimonio solicitados: {presencial_matrimonio}")
    print(f"Cantidad de certificados online y de nacimiento solicitados: {online_nacimiento}")
    print(f"Cantidad de certificados online y de defunción solicitados: {online_defuncion}")
    print(f"Cantidad de certificados online y de matrimonio solicitados: {online_matrimonio}")

presentarResultados()