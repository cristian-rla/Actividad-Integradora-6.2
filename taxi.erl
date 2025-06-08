-module(taxi).
-export([registra_taxi/2, elimina_taxi/1, consultar_estado/1, servicio_iniciado/1, servicio_terminado/1, taxi/3]).

central()->
    'nodoCentral@LAPTOP-ULGC39GQ'.

% Esta función solo se manda a llamar en funciones con previa verificación de la existencia de la central
verifica_previo_registro(NombreTaxi)->
    {central, central()} ! {lista_taxis, self()},
    receive
        {lista_otorgada, Taxis}->
            lists:any(fun({ActTaxiId, _}) -> NombreTaxi == ActTaxiId end, Taxis)
    after 5000 ->
        failed_to_connect
    end
.

% Se confía en que el registro del taxi se haga con una id que ya tenga un sistema de nomenclatura especifico, porque se recibe como parámetro.
registra_taxi(TaxiId, UbicacionInicial)->

    case verifica_previo_registro(TaxiId) of
        false ->
            {central, central()} ! {registra_taxi, TaxiId, spawn(?MODULE, taxi, [disponible, UbicacionInicial, undefined])},  % Está bien esta llamada porque así ya recibió algo y no se queda esperando, y si no hay taxi, puedo terminar el proceso desde ahí
            io:format("El taxi ~p ha sido registrado correctamente~n", [TaxiId]);
        true ->
            io:format("El taxi ~p ya está registrado ~n", [TaxiId])
    end

.

elimina_taxi(TaxiId)->
    case encuentra_taxi(TaxiId) of 
        false ->
            io:format("El taxi ~p no existe", [TaxiId]);
        {_, TaxiPid}->
            case consultar_estado(TaxiPid) of
                {disponible, _} ->
                    {central, central()} ! {elimina_taxi, {TaxiId, TaxiPid}};
                {ocupado, _} ->
                    io:format("No se puede terminar el taxi, está ocupado");
                _ ->
                    io:format("No se pudo establecer la conexión con el taxi")
            end
    end

.

encuentra_taxi(TaxiId)->
    {central, central()} ! {lista_taxis, self()},
    receive 
        {lista_otorgada, Taxis}->
            lists:keyfind(TaxiId, 1, Taxis)
    end

.
consultar_estado(TaxiPid) when is_pid(TaxiPid)->
    TaxiPid ! {consulta_estado, self()},
    receive
        {estado, Estado, Ubicacion} ->
            {Estado, Ubicacion};
        _ ->
            {undefined, undefined}    
    after 2000 ->
        {undefined, undefined} 
    end
;

consultar_estado(TaxiId) when is_atom(TaxiId)->

    case encuentra_taxi(TaxiId) of
        false ->
            io:format("El taxi ~p no está registrado ~n", [TaxiId]);
        {_, PidTaxi} ->
            PidTaxi ! {consulta_estado, self()},
            receive
                {estado, Estado, Ubicacion} ->
                    {Estado, Ubicacion};
                _ ->
                    {undefined, undefined}    
            after 2000 ->
                {undefined, undefined} 
            end
    end
.

servicio_iniciado(TaxiId) -> % La segunda manera de asignar un viaje a un taxi. Aunque usualmente se asigna al asignarle tal taxi al viajero desde la central.
    case encuentra_taxi(TaxiId) of 
        false ->
            io:format("El taxi no se encuentra registrado~n");
        {_, TaxiPid} ->
            case consultar_estado(TaxiPid) of
                {disponible, _} ->
                    TaxiPid ! servicio_iniciado;
                {ocupado, _} ->
                    io:format("El taxi está ocupado, no se pudo inicializar un servicio~n")
            end
    end

.

servicio_terminado(TaxiId) ->

    case encuentra_taxi(TaxiId) of 
        false ->
            io:format("El taxi no se encuentra registrado~n");
        {_, TaxiPid} ->
            case consultar_estado(TaxiPid) of
                {ocupado, _} ->
                    {central, central()} ! {servicio_terminado, {TaxiId, TaxiPid}};
                {disponible, _} ->
                    io:format("El taxi está disponible, no se pudo terminar ningún servicio~n")
            end
    end
.   

% Creo que una buena alternativa para aceptar atomos es hacer una lista de tuplas que tengan ubicaciones aceptadas. Esto también sirve para el 
% Hay una diferencia entre activo y disponible/ocupado. La combinación activo y ocupado es posible, inactivo es para los taxis que en algun momento estuvieron en servicio. Pero eso no se guarda en los taxis, porque un taxi inactivo implica que el proceso ha cerrado. 
taxi(Estado, Ubicacion, ViajePid) ->
    case ViajePid of 
        undefined ->
            receive
                terminar_taxi ->
                    case Estado of
                        {disponible, _}->
                            io:format("Se ha terminado el taxi~n");
                        {ocupado, _} ->
                            io:format("No se puede terminar el taxi, está ocupado~n"),
                            taxi(Estado, Ubicacion, ViajePid)
                    end;
                {viajero_asignado, NuevoViaje} ->
                    io:format("Se le ha asignado un viajero al taxi~n"),
                    taxi(Estado, Ubicacion, NuevoViaje);
                {consulta_estado, From} ->
                    From ! {estado, Estado, Ubicacion},
                    taxi(Estado, Ubicacion, ViajePid);
                servicio_iniciado ->
                    io:format("No se puede iniciar un servicio en taxis sin viaje asignado~n"),
                    taxi(Estado, Ubicacion, ViajePid);
                _->
                    io:format("Mensaje sin reconocer~n"),
                    taxi(Estado,Ubicacion, ViajePid)

            end;
        _ ->
            receive 
                terminar_taxi ->
                    case Estado of
                        {disponible, _}->
                            io:format("Se ha terminado el taxi~n");
                        {ocupado, _} ->
                            io:format("No se puede terminar el taxi, está ocupado~n"),
                            taxi(Estado, Ubicacion, ViajePid)
                    end;
                servicio_iniciado-> % Cambia la disponibilidad a ocupado.
                    io:format("Se ha recogido al viajero~n"),
                    taxi(ocupado, Ubicacion, ViajePid);
                viaje_cancelado->
                    io:format("El viajero ha cancelado el viaje~n"),
                    taxi(disponible, Ubicacion, undefined);
                {viaje_terminado, NuevaUbicacion} ->
                    io:format("Se ha culminado el viaje ~p~n",[ViajePid]),
                    taxi(disponible, NuevaUbicacion, undefined);
                {consulta_estado, From} ->
                    From ! {estado, Estado, Ubicacion},
                    taxi(Estado, Ubicacion, ViajePid)
            end
        end
.